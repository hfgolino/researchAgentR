# One function, chat(), talks to every provider. It takes messages in the
# package's own format and returns a response in the package's own format, so
# the agent loop never needs to know which provider it is using.
#
# Messages (a list; every element is a list with a `role`):
#   list(role = "system",    content = "...")
#   list(role = "user",      content = "...")
#   list(role = "assistant", content = "...", tool_calls = list(...),
#        reasoning = "...", native = <provider blocks>, native_api = "chat")
#   list(role = "tool", tool_call_id = "...", name = "...", content = "...",
#        is_error = FALSE)
# A tool call is list(id = , name = , arguments = <named list>,
#                     arguments_raw = <JSON text>, parse_error = NULL or text).

#' Send messages to any model and get one reply
#'
#' `chat()` is the single doorway to every provider. It converts the
#' package's message format to the provider's wire format (Chat Completions,
#' Responses or Anthropic Messages), sends the request with retries, and
#' converts the reply back, with token counts, cost and latency.
#'
#' @param messages A character string (one user message) or a list of
#'   messages in the format described in Details.
#' @param model A `"provider:model"` string. See [parse_model()].
#' @param system Optional system prompt (instructions).
#' @param tools Optional list of tools from [define_tool()] or [builtin_tool()].
#' @param output_schema Optional JSON Schema (an R list) the reply must follow.
#'   Not combined with `tools`: agents use a `final_answer` tool instead.
#' @param max_tokens Maximum output tokens (reasoning tokens included).
#' @param effort Reasoning effort for reasoning models (`"low"`, `"medium"`,
#'   `"high"`); `NULL` leaves the provider default. For local models in Ollama,
#'   `"none"` asks Ollama to switch thinking off (`think = false`). Models
#'   that only exist as thinking models (for example the default `qwen3:4b`
#'   since its 2507 release) keep reasoning and write it into the answer.
#' @param temperature Sampling temperature; `NULL` leaves the provider default.
#'   Claude 5.x models reject it, so it is dropped for them and the drop is
#'   recorded in `$notes`.
#' @param ... Extra fields added to the request body as they are (for
#'   example `tool_choice = "required"`).
#' @details Messages are lists with a `role` (`"system"`, `"user"`,
#'   `"assistant"` or `"tool"`) and `content`. Assistant messages may carry
#'   `tool_calls`; tool messages carry `tool_call_id`. The agent loop builds
#'   these for you; most users only pass a string.
#' @return An object of class `llm_response`: a list with `content`,
#'   `tool_calls`, `reasoning`, `usage` (`input`, `cache_read`,
#'   `cache_write`, `output`), `cost` (US dollars at list price), `latency`
#'   (seconds), `model`, `stop_reason`, `cached` and `notes`.
#' @examples
#' \dontrun{
#' r <- chat("In one sentence, what is exploratory graph analysis?",
#'           model = "groq:openai/gpt-oss-20b")
#' r
#' r$usage
#' chat("Same question.", model = "anthropic:claude-haiku-5-5")
#' chat("Same question.", model = "ollama:qwen3:4b")
#' }
#' @export
chat <- function(messages, model = default_model("lead"), system = NULL, tools = NULL,
                 output_schema = NULL, max_tokens = 4096, effort = NULL,
                 temperature = NULL, ...) {
  m <- parse_model(model)
  info <- .provider_info(m$provider)
  messages <- .as_messages(messages, system)
  args <- list(m = m, info = info, messages = messages, tools = tools,
               output_schema = output_schema, max_tokens = max_tokens,
               effort = effort, temperature = temperature, extra = list(...))
  res <- switch(info$api,
    chat = do.call(.chat_completions, args),
    responses = do.call(.responses_api, args),
    anthropic = do.call(.anthropic_messages, args),
    ollama = do.call(.ollama_chat, args)
  )
  res$model <- m$id
  res$provider <- m$provider
  res$cost <- if (isTRUE(res$cached)) 0 else .cost(m, res$usage)
  # running total of model spending in this R session (unknown prices count
  # as unpriced calls), read with researchAgentR:::.session_spend()
  .pkg_env$spend <- (.pkg_env$spend %||% 0) + (if (is.na(res$cost)) 0 else res$cost)
  if (is.na(res$cost)) .pkg_env$unpriced <- (.pkg_env$unpriced %||% 0) + 1
  if (!isTRUE(res$cached)) .pkg_env$live_calls <- (.pkg_env$live_calls %||% 0) + 1
  class(res) <- "llm_response"
  res
}

#' Ask a model one question and get the text back
#'
#' A shortcut for `chat(prompt, ...)$content`, for quick questions.
#'
#' @inheritParams chat
#' @param prompt The question.
#' @return The reply text, with the full [chat()] response in the attribute
#'   `"response"`.
#' @examples
#' \dontrun{
#' ask("Name three threats to internal validity.", model = "groq:openai/gpt-oss-20b")
#' }
#' @export
ask <- function(prompt, model = default_model("lead"), system = NULL, ...) {
  r <- chat(prompt, model = model, system = system, ...)
  structure(r$content, response = r)
}

#' @export
print.llm_response <- function(x, ...) {
  cat("<llm_response>", x$model, "\n")
  if (length(x$reasoning) && nzchar(x$reasoning)) {
    cat("Reasoning:", .shorten(x$reasoning, 200), "\n")
  }
  if (nzchar(x$content %||% "")) cat(x$content, "\n")
  for (tc in x$tool_calls) {
    cat("Tool call:", tc$name, .shorten(tc$arguments_raw, 150), "\n")
  }
  u <- x$usage
  cat(sprintf("Tokens: %d in (%d cached), %d out | cost $%.6f | %.2f s%s\n",
              as.integer(u$input + u$cache_read + u$cache_write), as.integer(u$cache_read),
              as.integer(u$output), x$cost %||% NA_real_, x$latency %||% NA_real_,
              if (isTRUE(x$cached)) " | from cache" else ""))
  for (n in x$notes) cat("Note:", n, "\n")
  invisible(x)
}

.shorten <- function(x, n) {
  x <- gsub("\\s+", " ", paste(x, collapse = " "))
  if (nchar(x) > n) paste0(substr(x, 1, n), "...") else x
}

.as_messages <- function(messages, system = NULL) {
  if (is.character(messages)) messages <- list(list(role = "user", content = paste(messages, collapse = "\n")))
  if (!is.null(system)) {
    messages <- c(list(list(role = "system", content = system)),
                  Filter(function(x) x$role != "system", messages))
  }
  messages
}

.usage <- function(input = 0, cache_read = 0, cache_write = 0, output = 0, reasoning = 0) {
  n <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) 0 else as.numeric(x)
  list(input = max(0, n(input)), cache_read = n(cache_read), cache_write = n(cache_write),
       output = n(output), reasoning = n(reasoning))
}

# Parse tool-call arguments from JSON text. Malformed JSON is kept and
# reported (never silently replaced), so the loop can ask the model to retry.
.parse_arguments <- function(raw) {
  if (is.list(raw)) return(list(arguments = raw, raw = as.character(.to_json(raw)), error = NULL))
  raw <- raw %||% "{}"
  if (!nzchar(trimws(raw))) raw <- "{}"
  parsed <- tryCatch(.from_json(raw), error = function(e) e)
  if (inherits(parsed, "error")) {
    return(list(arguments = NULL, raw = raw,
                error = paste("The arguments were not valid JSON:", conditionMessage(parsed))))
  }
  if (!is.list(parsed)) parsed <- list()
  list(arguments = parsed, raw = raw, error = NULL)
}

.tool_call <- function(id, name, raw) {
  p <- .parse_arguments(raw)
  list(id = id, name = name, arguments = p$arguments, arguments_raw = p$raw,
       parse_error = p$error)
}

# An empty JSON object ({}), which jsonlite would otherwise write as [].
.empty_object <- function() stats::setNames(list(), character(0))

.json_args <- function(args) {
  if (length(args) == 0) return("{}")
  as.character(.to_json(args))
}

# Split tools into function tools (built by define_tool) and provider
# built-in tools (passed through as they are).
.split_tools <- function(tools) {
  is_builtin <- vapply(tools, inherits, logical(1), "builtin_tool")
  list(functions = tools[!is_builtin], builtins = tools[is_builtin])
}

# Per-provider details for the Chat Completions adapter.
.chat_quirks <- function(provider, base_url = .provider_info(provider)$base_url) {
  # OpenAI's reasoning models reject max_tokens; Groq, Cerebras and OpenRouter
  # accept the newer name. Decided by address, so a provider registered by the
  # user at the same address (for example "openai_chat") behaves the same.
  new_name <- grepl("api\\.openai\\.com|api\\.groq\\.com|api\\.cerebras\\.ai|openrouter\\.ai", base_url)
  list(
    max_tokens_field = if (new_name) "max_completion_tokens" else "max_tokens",
    send_reasoning = isTRUE(.opt(paste0("pass_reasoning.", provider), FALSE))
  )
}

.session_spend <- function() {
  list(cost = .pkg_env$spend %||% 0, unpriced_calls = .pkg_env$unpriced %||% 0,
       live_calls = .pkg_env$live_calls %||% 0)
}
