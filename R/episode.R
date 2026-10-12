# The episode ledger ("Lens"): every model call and tool call of a run, with
# tokens, cost and latency, so a run can be inspected, costed, compared and
# documented. Cost is counted per complete episode, failed attempts included.

.new_episode <- function(agent, task, budget) {
  structure(list(
    id = paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), "-", paste(sample(c(letters, 0:9), 4), collapse = "")),
    agent = agent$name, model = agent$model, task = task,
    instructions = agent$instructions,
    tools = vapply(.agent_tools(agent), `[[`, character(1), "name"),
    context = agent$context$name, budget = unclass(budget),
    started = .now(), t0 = Sys.time(), steps = list(),
    answer = NULL, stop_reason = "running", finished = "",
    package_version = as.character(utils::packageVersion("researchAgentR")),
    r_version = R.version.string
  ), class = "episode")
}

.record_model <- function(ep, reply, sent, verbose) {
  u <- reply$usage
  step <- list(
    step = length(ep$steps) + 1, type = "model", time = .now(), model = reply$model,
    latency = reply$latency %||% NA_real_, input = u$input, cache_read = u$cache_read,
    cache_write = u$cache_write, output = u$output, reasoning_tokens = u$reasoning,
    cost = reply$cost, cached = isTRUE(reply$cached),
    context_tokens = count_tokens(.to_json(lapply(sent, function(m) m[c("role", "content")]))),
    messages_sent = length(sent),
    content = reply$content %||% "", reasoning = .shorten(reply$reasoning %||% "", 2000),
    tool_calls = vapply(reply$tool_calls, function(tc) paste0(tc$name, "(", tc$arguments_raw, ")"), character(1)),
    executed_tools = vapply(reply$executed_tools, function(x) as.character(x$type %||% x$name %||% "builtin"), character(1)),
    # what provider-side tools (browser_search, web_search, remote MCP) searched
    # and returned, so a run can be checked for contamination afterwards
    builtin_outputs = vapply(reply$executed_tools, function(x) {
      txt <- paste(c(.as_text(x$arguments %||% x$action %||% ""), .as_text(x$output %||% "")), collapse = "\n")
      substr(txt, 1, .opt("log_chars", 4000))
    }, character(1)),
    stop_reason = reply$stop_reason %||% "", notes = reply$notes %||% character(0),
    rate_limits = if (length(reply$headers)) stats::setNames(lapply(unclass(reply$headers), as.character), names(reply$headers)) else list()
  )
  # Provider-side tools may carry a fee per call that the token price does not
  # include. Fees are counted only when set, in US$ per call, for example
  # options(researchAgentR.builtin_fees = c(browser_search = 0.005)), from the
  # provider's current price list.
  fees <- .opt("builtin_fees", NULL)
  if (length(step$executed_tools) && !is.null(fees)) {
    fee <- sum(fees[step$executed_tools], na.rm = TRUE)
    step$builtin_fee <- fee
    step$cost <- step$cost + fee
  }
  ep$steps[[length(ep$steps) + 1]] <- step
  if (verbose) .print_step(step)
  ep
}

.record_tool <- function(ep, tc, res, verbose) {
  step <- list(
    step = length(ep$steps) + 1, type = "tool", time = .now(), tool = tc$name,
    arguments = tc$arguments_raw %||% "", latency = res$latency, cost = res$cost %||% 0,
    is_error = isTRUE(res$is_error), truncated = isTRUE(res$truncated),
    result = substr(res$output, 1, .opt("log_chars", 4000)), result_chars = nchar(res$output),
    child_episode = res$child %||% NA_character_
  )
  ep$steps[[length(ep$steps) + 1]] <- step
  if (verbose) .print_step(step)
  ep
}

.finish <- function(ep, messages, reason, answer = NULL) {
  ep$answer <- answer
  ep$stop_reason <- reason
  ep$finished <- .now()
  ep$seconds <- as.numeric(difftime(Sys.time(), ep$t0, units = "secs"))
  ep$messages <- lapply(messages, function(m) m[intersect(names(m), c("role", "content", "tool_call_id", "name", "is_error"))])
  ep$t0 <- NULL
  if (.verbose()) cat(sprintf("Stopped: %s\n", reason))
  ep
}

.totals <- function(ep) {
  steps <- ep$steps
  of <- function(types) Filter(function(s) s$type %in% types, steps)
  total <- function(types, field) sum(vapply(of(types), function(s) as.numeric(s[[field]] %||% 0), numeric(1)), na.rm = TRUE)
  billed <- c("model", "context")   # context steps are model calls made by a context strategy
  costs <- vapply(steps, function(s) as.numeric(s$cost %||% 0), numeric(1))
  seconds <- ep$seconds %||% if (!is.null(ep$t0)) as.numeric(difftime(Sys.time(), ep$t0, units = "secs")) else NA_real_
  ctx <- vapply(of("model"), function(s) as.numeric(s$context_tokens %||% 0), numeric(1))
  list(
    model_calls = length(of("model")),
    tool_calls = length(of("tool")),
    tool_errors = sum(vapply(of("tool"), function(s) isTRUE(s$is_error), logical(1))),
    input = total(billed, "input"), cache_read = total(billed, "cache_read"),
    cache_write = total(billed, "cache_write"), output = total(billed, "output"),
    tokens = total(billed, "input") + total(billed, "cache_read") +
      total(billed, "cache_write") + total(billed, "output"),
    peak_context = if (length(ctx)) max(ctx) else 0,
    # one unpriced step makes the total unknown (NA) rather than too low
    cost = if (anyNA(costs)) NA_real_ else sum(costs),
    seconds = seconds
  )
}

.print_step <- function(s) {
  if (s$type == "model") {
    what <- if (length(s$tool_calls)) paste("->", paste(.shorten(s$tool_calls, 90), collapse = "; ")) else
      paste("->", .shorten(s$content, 90))
    cat(sprintf("[%d] model  %6s in %5s out  $%.5f  %4.1fs %s\n", s$step,
                format(s$input + s$cache_read + s$cache_write, big.mark = ","),
                format(s$output, big.mark = ","), s$cost %||% NA_real_, s$latency, what))
  } else {
    cat(sprintf("[%d] tool   %-22s %4.1fs %s%s\n", s$step, s$tool, s$latency,
                if (s$is_error) "ERROR " else "ok ", .shorten(s$result, 80)))
  }
}

#' @export
print.episode <- function(x, ...) {
  t <- .totals(x)
  cat("<episode>", x$id, "|", x$agent, "|", x$model, "\n")
  cat("Task:", .shorten(x$task, 150), "\n")
  cat(sprintf("Stop reason: %s | %d model calls, %d tool calls (%d errors)\n",
              x$stop_reason, t$model_calls, t$tool_calls, t$tool_errors))
  cat(sprintf("Tokens: %s billed (%s cached), peak context ~%s | cost $%.5f | %.1f s\n",
              format(t$tokens, big.mark = ","), format(t$cache_read, big.mark = ","),
              format(t$peak_context, big.mark = ","), t$cost, t$seconds))
  n_builtin <- sum(vapply(x$steps, function(s) length(s$executed_tools %||% character(0)), numeric(1)))
  if (n_builtin && is.null(.opt("builtin_fees", NULL))) {
    cat(sprintf("Provider-side tool calls: %d (any per-call fee is not in the cost; see ?budget)\n", n_builtin))
  }
  cat("Answer:", if (is.null(x$answer)) "(none)" else .shorten(.as_text(x$answer), 400), "\n")
  invisible(x)
}

#' Summaries of an episode or of many episodes
#'
#' `episode_steps()` gives one row per step of one episode; `episodes_df()`
#' gives one row per episode, for comparing runs.
#'
#' @param episode An episode from [run()].
#' @param episodes A list of episodes, or a folder of saved episode files.
#' @return A data frame.
#' @examples
#' \dontrun{
#' episode_steps(ep)
#' episodes_df(list(ep1, ep2))
#' }
#' @export
episode_steps <- function(episode) {
  rows <- lapply(episode$steps, function(s) data.frame(
    step = s$step, type = s$type, name = if (s$type == "model") s$model else s$tool,
    input = s$input %||% NA, cache_read = s$cache_read %||% NA, cache_write = s$cache_write %||% NA,
    output = if (s$type == "model") s$output else NA,
    context_tokens = s$context_tokens %||% NA, cost = s$cost %||% NA, latency = s$latency %||% NA,
    is_error = isTRUE(s$is_error), cached = isTRUE(s$cached),
    detail = .shorten(if (s$type == "model") c(unlist(s$tool_calls), s$content) else s$result, 120),
    stringsAsFactors = FALSE))
  if (!length(rows)) {
    return(data.frame(step = integer(0), type = character(0), name = character(0), input = numeric(0),
                      cache_read = numeric(0), cache_write = numeric(0), output = numeric(0),
                      context_tokens = numeric(0), cost = numeric(0), latency = numeric(0),
                      is_error = logical(0), cached = logical(0), detail = character(0)))
  }
  do.call(rbind, rows)
}

#' @rdname episode_steps
#' @export
episodes_df <- function(episodes) {
  if (is.character(episodes)) {
    files <- if (dir.exists(episodes)) list.files(episodes, "\\.json$", full.names = TRUE) else episodes
    episodes <- lapply(files, read_episode)
  }
  if (inherits(episodes, "episode")) episodes <- list(episodes)
  do.call(rbind, lapply(episodes, function(e) {
    t <- .totals(e)
    data.frame(id = e$id, agent = e$agent, model = e$model, task = .shorten(e$task, 60),
               stop_reason = e$stop_reason, model_calls = t$model_calls, tool_calls = t$tool_calls,
               tool_errors = t$tool_errors, tokens = t$tokens, cache_read = t$cache_read,
               peak_context = t$peak_context, cost = t$cost, seconds = t$seconds,
               started = e$started, stringsAsFactors = FALSE)
  }))
}

#' Save and read episodes
#'
#' Episodes are saved as JSON (readable by people and by other tools), one
#' file per episode.
#'
#' @param episode An episode from [run()].
#' @param file Path of the JSON file. Defaults to `<id>.json` in `dir`.
#' @param dir Folder for episodes.
#' @param children Also save the episodes of agents that ran as tools of this
#'   one ([team()], [as_tool()]), inside the same file. [read_episode()]
#'   makes them available again through [sub_episodes()].
#' @return `save_episode()` returns the file path, invisibly; `read_episode()`
#'   returns the episode.
#' @examples
#' \dontrun{
#' f <- save_episode(ep)
#' ep2 <- read_episode(f)
#' }
#' @export
save_episode <- function(episode, file = NULL, dir = "episodes", children = TRUE) {
  if (is.null(file)) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    file <- file.path(dir, paste0(episode$id, ".json"))
  }
  out <- unclass(episode)
  if (children) {
    # the episodes of agents that ran as tools, so a team run is saved whole
    kids <- .with_children(list(episode))[-1]
    if (length(kids)) out$sub_episodes <- lapply(kids, unclass)
  }
  write_text(jsonlite::prettify(.to_json(out)), file, encoding = "UTF-8")
  invisible(file)
}

#' @rdname save_episode
#' @export
read_episode <- function(file) {
  x <- .from_json(paste(read_text(file, encoding = "UTF-8"), collapse = "\n"))
  # sub-episodes saved with the lead become available to sub_episodes(),
  # view_episode(), cost_ledger() and ai_use_statement() again
  for (k in x$sub_episodes %||% list()) .register_sub_episode(.fix_episode(k), k$called_as_tool %||% NA_character_)
  x$sub_episodes <- NULL
  .fix_episode(x)
}

.fix_episode <- function(x) {
  for (f in c("stop_reason", "agent", "model", "task", "id")) {
    if (is.null(x[[f]]) || length(x[[f]]) == 0) x[[f]] <- NA_character_
  }
  if (is.null(x$seconds) || length(x$seconds) == 0) x$seconds <- NA_real_
  structure(unclass(x), class = "episode")
}
