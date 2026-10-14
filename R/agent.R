# An agent is a model, instructions and tools; run() executes one episode:
# ask the model, run the tools it asks for, show it the results, repeat,
# until it gives a final answer or a budget runs out.

#' Create an agent
#'
#' An agent bundles a model, a name, instructions and tools. Nothing is sent
#' until you [run()] it.
#'
#' @param name A short name (shown in traces and used when the agent is a tool
#'   of another agent).
#' @param instructions The system prompt: the agent's job, its rules, and what
#'   a good answer looks like.
#' @param model A `"provider:model"` string; see [set_default_model()].
#' @param tools A list of tools from [define_tool()], [builtin_tool()] or the
#'   research tools such as [tool_openalex()].
#' @param description One sentence saying what the agent does (used by
#'   [as_tool()], routers and teams).
#' @param output_schema Optional structure for the final answer: a named list
#'   of argument descriptions (see [arg_string()], [arg_array()],
#'   [arg_object()]). The agent then finishes by calling a `final_answer`
#'   tool whose arguments are checked in R; an invalid answer is sent back to
#'   the model as an error to fix (counted, never silently repaired).
#' @param context How the conversation is trimmed before each model call; see
#'   [ctx_full()] and the other `ctx_*()` strategies.
#' @param max_tokens Maximum output tokens per model call.
#' @param effort Reasoning effort for reasoning models, or `NULL`.
#' @return An object of class `agent`.
#' @examples
#' word_count <- function(text) length(strsplit(text, "\\s+")[[1]])
#' counter <- agent(
#'   name = "counter",
#'   instructions = "Count words with the word_count tool and report the number.",
#'   model = "groq:openai/gpt-oss-20b",
#'   tools = list(define_tool(word_count, "Count the words in a text.",
#'                            args = list(text = arg_string("The text"))))
#' )
#' counter
#' @export
agent <- function(name, instructions, model = default_model("lead"), tools = list(),
                  description = "", output_schema = NULL, context = ctx_full(),
                  max_tokens = 4096, effort = NULL) {
  parse_model(model)
  if (inherits(tools, c("agent_tool", "builtin_tool"))) tools <- list(tools)
  structure(list(
    name = name, instructions = instructions, model = model, tools = tools,
    description = description, output_schema = output_schema, context = context,
    max_tokens = max_tokens, effort = effort
  ), class = "agent")
}

#' @export
print.agent <- function(x, ...) {
  cat("<agent>", x$name, "|", x$model, "\n")
  if (nzchar(x$description)) cat(x$description, "\n")
  cat("Instructions:", .shorten(x$instructions, 160), "\n")
  tn <- vapply(x$tools, `[[`, character(1), "name")
  cat("Tools:", if (length(tn)) paste(tn, collapse = ", ") else "(none)", "\n")
  if (!is.null(x$output_schema)) cat("Final answer: structured (", paste(names(x$output_schema), collapse = ", "), ")\n")
  cat("Context:", x$context$name, "\n")
  invisible(x)
}

#' Set the limits of an episode
#'
#' Every run stops at the first limit reached, and the episode records which
#' one it was. Set budgets before tuning an agent, not after.
#'
#' @param max_steps Maximum number of model calls.
#' @param max_tokens Maximum billed tokens (input, cached input and output
#'   summed over all calls).
#' @param max_cost Maximum cost in US dollars at list price. Token prices
#'   come from [model_registry()]. Provider-side tools such as Groq's
#'   `browser_search` may also charge per call; such fees are counted only
#'   when set with, for example, `options(researchAgentR.builtin_fees =
#'   c(browser_search = 0.005))` (US$ per call, from the provider's current
#'   price list; the number here is only an example).
#' @param max_seconds Maximum wall-clock time.
#' @param max_tool_errors Stop after this many failed tool calls in a row.
#' @param final_call When `max_steps` or `max_tokens` is reached by an agent
#'   with an `output_schema`, allow one more call in which the model can only
#'   call `final_answer`, with what it has so far (and one retry if the
#'   provider rejects that call). The episode then stops with
#'   `"max_steps_final_answer"` or `"max_tokens_final_answer"` (a forced,
#'   possibly partial answer: check it) instead of stopping with no answer.
#'   Useful for workers of a team.
#' @return An object of class `budget`.
#' @examples
#' budget(max_steps = 6, max_cost = 0.01)
#' @export
budget <- function(max_steps = 10, max_tokens = 60000, max_cost = 0.10, max_seconds = 300,
                   max_tool_errors = 4, final_call = FALSE) {
  structure(list(max_steps = max_steps, max_tokens = max_tokens, max_cost = max_cost,
                 max_seconds = max_seconds, max_tool_errors = max_tool_errors,
                 final_call = final_call),
            class = "budget")
}

#' @export
print.budget <- function(x, ...) {
  cat(sprintf("<budget> %d steps | %s tokens | $%.4f | %d s | %d tool errors in a row%s\n",
              x$max_steps, format(x$max_tokens, big.mark = ","), x$max_cost,
              x$max_seconds, x$max_tool_errors,
              if (isTRUE(x$final_call)) " | final-answer call when out of steps or tokens" else ""))
  invisible(x)
}

#' Run an agent on a task (one episode)
#'
#' The loop: send the conversation to the model; if it asks for tools, run
#' them and add the results; repeat until it gives a final answer or a budget
#' is exhausted. Every step (model call or tool call) is recorded with its
#' tokens, cost and latency.
#'
#' @param agent An [agent()].
#' @param task The task, as text.
#' @param budget Limits from [budget()].
#' @param verbose Print each step as it happens?
#' @return An object of class `episode`: the final answer, the stop reason,
#'   every step, and the totals. Print it, pass it to [view_episode()], or
#'   save it with [save_episode()].
#' @examples
#' \dontrun{
#' ep <- run(counter, "How many words are in 'agents use tools in a loop'?")
#' ep
#' ep$answer
#' }
#' @export
run <- function(agent, task, budget = researchAgentR::budget(), verbose = .verbose()) {
  tools <- .agent_tools(agent)
  ep <- .new_episode(agent, task, budget)
  messages <- list(list(role = "system", content = .system_prompt(agent)),
                   list(role = "user", content = task))
  repeat {
    reason <- .over_budget(ep, budget)
    if (!is.null(reason)) {
      if (reason %in% c("max_steps", "max_tokens") && !is.null(agent$output_schema) &&
          isTRUE(budget$final_call)) {
        return(.final_call(agent, ep, messages, tools, reason, verbose))
      }
      return(.finish(ep, messages, reason))
    }
    sent <- agent$context$prepare(messages, ep)
    ep <- .record_context(ep, sent, agent$context$name)   # e.g. the cost of a summary
    reply <- tryCatch(
      chat(sent, model = agent$model, tools = tools, max_tokens = agent$max_tokens,
           effort = agent$effort),
      error = function(e) e)
    if (inherits(reply, "error")) {
      if (!.invalid_tool_call(reply)) return(.finish(ep, messages, paste("api_error:", .plain(conditionMessage(reply)))))
      # The provider rejected a malformed tool call (for example a tool that
      # does not exist): count it as a tool error and let the model retry.
      ep <- .record_invalid_call(ep, reply, verbose)
      messages <- c(messages, list(list(role = "user", content = paste0(
        "Your last reply was rejected: ", .error_line(reply), " Call only these tools: ",
        paste(vapply(tools, `[[`, character(1), "name"), collapse = ", "), "."))))
      next
    }
    ep <- .record_model(ep, reply, sent, verbose)
    messages <- c(messages, list(.assistant_message(reply)))
    if (length(reply$tool_calls) == 0) {
      if (is.null(agent$output_schema)) return(.finish(ep, messages, "final_answer", reply$content))
      messages <- c(messages, list(list(role = "user", content =
        "Give your final answer by calling the final_answer tool, not as plain text.")))
      next
    }
    # final_answer counts only when called alone: called together with other
    # tools, it was written before their results were known.
    early <- length(reply$tool_calls) > 1
    for (tc in reply$tool_calls) {
      res <- if (early && tc$name == "final_answer") .early_final(tc) else call_tool(tc, tools)
      res$output <- .note_repeat(res$output, tc, ep)   # loops are a common failure
      ep <- .record_tool(ep, tc, res, verbose)
      messages <- c(messages, list(list(role = "tool", tool_call_id = tc$id, name = tc$name,
                                        content = res$output, is_error = res$is_error)))
      if (tc$name == "final_answer" && !res$is_error) {
        return(.finish(ep, messages, "final_answer", res$value))
      }
    }
  }
}

# The tools the model sees: the agent's tools plus final_answer when the
# answer is structured.
.agent_tools <- function(agent) {
  tools <- agent$tools
  if (!is.null(agent$output_schema)) tools <- c(tools, list(.final_answer_tool(agent$output_schema)))
  tools
}

.final_answer_tool <- function(fields) {
  if (!all(vapply(fields, inherits, logical(1), "tool_arg"))) {
    .stop("Describe output_schema fields with arg_string(), arg_array(), arg_object() and similar.")
  }
  structure(list(
    name = "final_answer",
    description = "Submit the final answer. Call this once, when the task is complete.",
    fn = function(...) list(...), args = fields,
    schema = c(list(type = "object"), .fields_schema(fields)),
    strict = .all_required(fields), approval = FALSE, timeout = 10,
    max_output_tokens = 200, cost_per_call = 0, untrusted = FALSE
  ), class = "agent_tool")
}

.system_prompt <- function(agent) {
  finish <- if (is.null(agent$output_schema)) {
    "When the task is complete, reply with the answer as plain text and do not call a tool."
  } else {
    "When the task is complete, call the final_answer tool with arguments that follow its schema."
  }
  paste(agent$instructions,
        # options(researchAgentR.today = "2026-12-01") pins the date, so a run
        # recorded on one day replays from the cache on another.
        paste0("Today's date is ", .opt("today", format(Sys.Date())), "."),
        .untrusted_rule, finish, sep = "\n\n")
}

.assistant_message <- function(reply) {
  list(role = "assistant", content = reply$content, tool_calls = reply$tool_calls,
       reasoning = reply$reasoning, native = reply$native, native_api = reply$native_api)
}

.over_budget <- function(ep, b) {
  t <- .totals(ep)
  if (t$model_calls >= b$max_steps) return("max_steps")
  if (t$tokens >= b$max_tokens) return("max_tokens")
  if (!is.na(t$cost) && t$cost >= b$max_cost) return("max_cost")
  if (t$seconds >= b$max_seconds) return("max_seconds")
  if (.errors_in_a_row(ep) >= b$max_tool_errors) return("max_tool_errors")
  NULL
}

.errors_in_a_row <- function(ep) {
  tools <- Filter(function(s) s$type == "tool", ep$steps)
  n <- 0
  for (s in rev(tools)) if (isTRUE(s$is_error)) n <- n + 1 else break
  n
}

#' Keep the whole conversation (the default context strategy)
#'
#' Sends every message to the model at every step. Simple and lossless, but
#' the context, and the cost of each call, grows with every step. See the
#' other `ctx_*()` strategies for alternatives.
#'
#' @return A context strategy (a list with a `name` and a `prepare` function).
#' @examples
#' ctx_full()
#' @export
ctx_full <- function() {
  structure(list(name = "full", prepare = function(messages, episode) messages),
            class = "context_strategy")
}

#' @export
print.context_strategy <- function(x, ...) {
  cat("<context_strategy>", x$name, "\n")
  invisible(x)
}

# Groq rejects a tool call that does not match the request's tools with HTTP 400
# ("Tool call validation failed", code tool_use_failed); gpt-oss sometimes
# calls a tool named "json" or writes broken arguments. Such errors are the
# model's mistakes, so the loop reports them back instead of stopping.
.invalid_tool_call <- function(e) {
  grepl("tool call validation failed|tool_use_failed|failed to call a function|failed_generation|failed to parse tool call",
        conditionMessage(e), ignore.case = TRUE)
}

.error_line <- function(e) {
  lines <- strsplit(.plain(conditionMessage(e)), "\n")[[1]]
  lines <- trimws(sub("^\\W+", "", lines, perl = TRUE))
  paste(.shorten(lines[nzchar(lines)][-1], 300), collapse = " ")
}

.record_invalid_call <- function(ep, e, verbose, label = "(invalid tool call)") {
  step <- list(step = length(ep$steps) + 1, type = "tool", time = .now(), tool = label,
               arguments = "", latency = 0, cost = 0, is_error = TRUE, truncated = FALSE,
               result = .error_line(e), result_chars = nchar(.error_line(e)),
               child_episode = NA_character_)
  ep$steps[[length(ep$steps) + 1]] <- step
  if (verbose) .print_step(step)
  ep
}

.early_final <- function(tc) {
  list(id = tc$id, name = tc$name, is_error = TRUE, value = NULL, truncated = FALSE, cost = 0,
       latency = 0, output = paste(
         "final_answer was not accepted: it was called in the same turn as other tools,",
         "before their results were known. Read those results, then call final_answer alone."))
}

# Error messages from httr2/cli may carry terminal colour codes; logs keep plain text.
.plain <- function(x) gsub("\033\\[[0-9;]*m", "", x)

# A small model may call the same tool with the same arguments again and
# again. The call still runs, but the result says so, which often breaks the loop.
.note_repeat <- function(output, tc, ep) {
  if (tc$name == "final_answer") return(output)
  same <- Filter(function(s) identical(s$type, "tool") && identical(s$tool, tc$name) &&
                   identical(s$arguments, tc$arguments_raw %||% ""), ep$steps)
  if (!length(same)) return(output)
  paste0(output, "\n[Note: this exact call was already made ", length(same), " time(s) ",
         "(first at step ", same[[1]]$step, ") and gave this same result. ",
         "Use it, change the arguments, or finish.]")
}

# Out of steps (or tokens) with a structured answer due: one last call in
# which the model can only call final_answer, with what it has found so far.
# Small models often keep searching; a partial, structured answer (marked as
# forced) is more useful to a lead agent than nothing. A rejected call (for
# example arguments that do not match the schema) gets one retry with the
# reason, and every failure is recorded as a step, so the trajectory shows it.
.final_call <- function(agent, ep, messages, tools, reason, verbose, attempts = 2) {
  final_tool <- Filter(function(t) identical(t$name, "final_answer"), tools)
  messages <- c(messages, list(list(role = "user", content = paste(
    "The budget is used up. Call final_answer now with what you have found so far;",
    "leave out anything you could not check."))))
  for (k in seq_len(attempts)) {
    sent <- agent$context$prepare(messages, ep)
    reply <- tryCatch(chat(sent, model = agent$model, tools = final_tool, max_tokens = agent$max_tokens,
                           effort = agent$effort), error = function(e) e)
    if (inherits(reply, "error")) {
      ep <- .record_invalid_call(ep, reply, verbose, label = "(final call rejected)")
      if (!.invalid_tool_call(reply)) break          # a network or account error: stop
      messages <- c(messages, list(list(role = "user", content = paste0(
        "Your last reply was rejected: ", .error_line(reply),
        " Call final_answer with arguments that follow its schema."))))
      next
    }
    ep <- .record_model(ep, reply, sent, verbose)
    messages <- c(messages, list(.assistant_message(reply)))
    if (!length(reply$tool_calls)) {
      messages <- c(messages, list(list(role = "user", content = "Call the final_answer tool.")))
      next
    }
    for (tc in reply$tool_calls) {
      res <- if (tc$name == "final_answer") call_tool(tc, final_tool) else
        list(id = tc$id, name = tc$name, is_error = TRUE, value = NULL, truncated = FALSE, cost = 0,
             latency = 0, output = "Only final_answer can be called now.")
      ep <- .record_tool(ep, tc, res, verbose)
      messages <- c(messages, list(list(role = "tool", tool_call_id = tc$id, name = tc$name,
                                        content = res$output, is_error = res$is_error)))
      if (tc$name == "final_answer" && !res$is_error) {
        return(.finish(ep, messages, paste0(reason, "_final_answer"), res$value))
      }
    }
  }
  .finish(ep, messages, reason)
}
