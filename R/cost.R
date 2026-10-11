# Cost helpers: make every token count, and count every token.
#
# Cost is measured per complete task episode, not per call (Harvard CS2680,
# "Cost-effective agents", https://cs2680.seas.harvard.edu/notes/cost-effective-agents):
# a cascade that fails cheaply twice and then succeeds with a strong model
# costs all three attempts. Optimization work done before deployment (tuning
# prompts, running evaluations) belongs in a separate, offline ledger.

#' A model cascade: try cheap models first, escalate when a check fails
#'
#' Returns a solver for [run_eval()] (or for direct use) that runs the agent
#' with each model in turn, from the first (cheapest) to the last
#' (strongest), and stops at the first answer the verifier accepts. The
#' answer carries the full ledger of the attempts: `attr(answer, "cost")`
#' sums every attempt (failed ones included) plus the verifier's own cost,
#' and `attr(answer, "episodes")` keeps each attempt's episode.
#'
#' The verifier runs at answer time, so it must not use the answer key: it
#' checks what can be checked without it (the answer is not empty, follows
#' a format, cites a DOI that resolves, passes a schema). A verifier that
#' peeks at the key makes the cascade look better than it can ever be.
#'
#' @param models `"provider:model"` strings, cheapest first.
#' @param verifier A function `(answer, task)` returning `TRUE`/`FALSE` or a
#'   grader-style list with `pass` (and optionally `cost`). It receives the
#'   public task only (no answer key). Use [grader_code()] to write one.
#' @param agent A template [agent()] whose model is replaced at each step,
#'   or a function `(model)` that returns an agent. `NULL` uses a plain
#'   agent with no tools.
#' @param budget Limits for each attempt, from [budget()].
#' @return A function of one task (a list with `instruction`, or a string)
#'   that returns the answer with attributes `cost`, `tokens`, `model`,
#'   `verified`, `stop_reason`, `attempts` (a data frame) and `episodes`.
#' @examples
#' # A runtime verifier that needs no answer key: the answer must contain a DOI.
#' has_doi <- grader_code(function(answer, task)
#'   grepl("10\\.[0-9]{4,9}/", paste(answer, collapse = " ")), label = "has_doi")
#' solve <- cascade(c("groq:openai/gpt-oss-20b", "groq:openai/gpt-oss-120b"),
#'                  verifier = has_doi)
#' \dontrun{
#' ans <- solve("Return the DOI of 'Entropy Fit Indices' (Multivariate Behavioral Research, 2020).")
#' attr(ans, "attempts")
#' run_eval(solve, research_tasks, trials = 3, name = "cascade 20b -> 120b")
#' }
#' @export
cascade <- function(models, verifier, agent = NULL, budget = researchAgentR::budget()) {
  for (m in models) parse_model(m)
  if (!is.function(verifier)) .stop("`verifier` must be a function(answer, task).")
  make_agent <- .agent_maker(agent)
  function(task) {
    if (is.character(task)) task <- list(id = "task", instruction = task)
    task$answer <- NULL   # a runtime verifier never sees the key
    task$grader <- NULL
    episodes <- list()
    attempts <- list()
    verify_cost <- 0
    for (m in models) {
      ep <- run(make_agent(m), .task_prompt(task), budget, verbose = FALSE)
      v <- .as_check(verifier(ep$answer, task))
      verify_cost <- verify_cost + v$cost
      t <- .totals(ep)
      episodes[[length(episodes) + 1]] <- ep
      attempts[[length(attempts) + 1]] <- data.frame(
        model = m, stop_reason = ep$stop_reason, verified = v$pass, cost = t$cost,
        tokens = t$tokens, seconds = t$seconds, episode_id = ep$id, stringsAsFactors = FALSE)
      if (isTRUE(v$pass)) break
    }
    attempts <- do.call(rbind, attempts)
    .with_ledger(ep$answer, attempts, episodes, extra_cost = verify_cost,
                 stop_reason = if (isTRUE(v$pass)) ep$stop_reason else "unverified")
  }
}

# Agent factory: a template agent with its model swapped, a user function,
# or a bare agent with no tools.
.agent_maker <- function(agent) {
  if (is.function(agent)) return(agent)
  if (inherits(agent, "agent")) return(function(model) { a <- agent; a$model <- model; a })
  if (is.null(agent)) {
    return(function(model) researchAgentR::agent(
      "solver", "Complete the task. Reply with the answer only.", model = model))
  }
  .stop("`agent` must be an agent(), a function(model) returning one, or NULL.")
}

# A verifier result as list(pass, cost).
.as_check <- function(v) {
  if (is.list(v)) return(list(pass = isTRUE(v$pass), cost = as.numeric(v$cost %||% 0)))
  list(pass = isTRUE(v), cost = 0)
}

# Attach the episode ledger of several attempts to the final answer.
.with_ledger <- function(answer, attempts, episodes, extra_cost = 0, stop_reason) {
  if (is.null(answer)) answer <- NA_character_
  final <- attempts[nrow(attempts), ]
  structure(answer,
            cost = sum(attempts$cost) + extra_cost,   # NA if any attempt's cost is unknown
            tokens = sum(attempts$tokens),
            model = final$model, verified = isTRUE(final$verified),
            stop_reason = stop_reason, attempts = attempts, episodes = episodes)
}

#' Run the baselines every agent should beat
#'
#' A cost or quality claim means little without baselines on the same tasks
#' and budget. `baselines()` evaluates the same agent design three ways:
#' with a fixed cheap model, with a fixed strong model, and with the cheap
#' model plus retries (a retry happens when an attempt fails the runtime
#' verifier; by default, when it gives no answer). The results are combined
#' and ready for [eval_summary()], [paired_bootstrap()] and
#' [plot_frontier()].
#'
#' @param make_agent A function `(model)` that returns an [agent()], or a
#'   template agent whose model is replaced.
#' @param tasks A [task_set()].
#' @param models A named list with `cheap` and `strong` model strings.
#' @param trials Trials per task.
#' @param retries Extra attempts for the cheap-with-retries baseline.
#' @param verifier Runtime check that triggers a retry (no answer key; see
#'   [cascade()]). The default retries only when an attempt ends without a
#'   final answer or with an empty one.
#' @param budget Limits for each episode, from [budget()].
#' @param save_dir Optional folder for episodes and results (one subfolder
#'   per baseline).
#' @return An `eval_results` data frame with one `solver` per baseline.
#' @examples
#' \dontrun{
#' make <- function(model) agent("doi_finder", "Find the DOI. Reply with the DOI only.",
#'                               model = model, tools = list(tool_crossref()))
#' dev <- research_tasks[research_tasks$split == "dev", ]
#' base <- keep("baselines_doi", baselines(make, dev, trials = 3))
#' eval_summary(base)
#' plot_frontier(base)
#' }
#' @export
baselines <- function(make_agent, tasks,
                      models = list(cheap = default_model("worker"), strong = default_model("lead")),
                      trials = 3, retries = 1, verifier = NULL,
                      budget = researchAgentR::budget(), save_dir = NULL) {
  if (!all(c("cheap", "strong") %in% names(models))) .stop("`models` needs `cheap` and `strong`.")
  make <- .agent_maker(make_agent)
  verifier <- verifier %||% function(answer, task) {
    !is.null(answer) && !all(is.na(answer)) && nzchar(trimws(.answer_text(answer)))
  }
  sub <- function(x) if (is.null(save_dir)) NULL else file.path(save_dir, x)
  cheap <- run_eval(make(models$cheap), tasks, trials, budget, sub("cheap"),
                    name = paste("cheap:", models$cheap))
  strong <- run_eval(make(models$strong), tasks, trials, budget, sub("strong"),
                     name = paste("strong:", models$strong))
  retry <- run_eval(cascade(rep(models$cheap, retries + 1), verifier, make, budget), tasks,
                    trials, budget, sub("cheap_retries"),
                    name = sprintf("cheap + %d retr%s: %s", retries, if (retries == 1) "y" else "ies",
                                   models$cheap))
  out <- rbind(as.data.frame(cheap), as.data.frame(strong), as.data.frame(retry))
  .as_eval_results(out, solver = unique(out$solver), budget = budget, trials = trials)
}

#' Keep runtime and offline costs in separate ledgers
#'
#' Runtime cost is what the agent costs each time it does its job. Offline
#' cost is what it took to build it: prompt tuning, evaluation runs, judge
#' calls, baselines. Reporting them together hides how expensive tuning
#' was, and reporting only runtime cost hides it entirely. Build a ledger
#' for each kind and combine them with [rbind()]; printing shows the totals
#' by kind.
#'
#' @param episodes Episodes from [run()] (a list, or a folder of saved
#'   episodes), or an `eval_results` data frame from [run_eval()] (one row
#'   per trial; its grading cost is kept in its own column).
#' @param kind `"runtime"` or `"offline"`.
#' @param note A short note recorded in every row (for example "prompt v2
#'   tuning").
#' @return A data frame of class `cost_ledger` with `kind`, `note`, `id`,
#'   `model`, `tokens`, `cost`, `grading_cost`, `seconds`.
#' @examples
#' # Hand-made results (not model output) from a tuning session.
#' tuning <- data.frame(solver = "v2", task_id = c("a", "b"), trial = 1, pass = c(TRUE, FALSE),
#'                      cost = c(0.004, 0.006), grading_cost = c(0.001, 0.001),
#'                      tokens = c(5200, 7100), seconds = c(6, 9), model = "groq:openai/gpt-oss-20b",
#'                      episode_id = c("e1", "e2"))
#' cost_ledger(tuning, kind = "offline", note = "prompt v2 tuning")
#' @export
cost_ledger <- function(episodes, kind = c("runtime", "offline"), note = "") {
  kind <- match.arg(kind)
  if (is.data.frame(episodes) && all(c("cost", "task_id") %in% names(episodes))) {
    d <- data.frame(id = episodes$episode_id %||% NA_character_, model = episodes$model %||% NA_character_,
                    tokens = episodes$tokens %||% NA_real_, cost = episodes$cost,
                    grading_cost = episodes$grading_cost %||% 0,
                    seconds = episodes$seconds %||% NA_real_, stringsAsFactors = FALSE)
  } else {
    e <- episodes_df(episodes)
    d <- data.frame(id = e$id, model = e$model, tokens = e$tokens, cost = e$cost,
                    grading_cost = 0, seconds = e$seconds, stringsAsFactors = FALSE)
  }
  d <- cbind(kind = kind, note = note, d, stringsAsFactors = FALSE)
  class(d) <- c("cost_ledger", "data.frame")
  d
}

#' @export
print.cost_ledger <- function(x, ...) {
  cat("<cost_ledger>\n")
  for (k in c("runtime", "offline")) {
    d <- x[x$kind == k, , drop = FALSE]
    if (!nrow(d)) next
    cat(sprintf("%-8s %4d rows | model cost $%.5f | grading $%.5f | %s tokens%s\n", k, nrow(d),
                sum(d$cost), sum(d$grading_cost), format(sum(d$tokens), big.mark = ","),
                if (anyNA(d$cost)) " | some costs unknown (NA)" else ""))
  }
  cat("Report runtime and offline totals separately.\n")
  invisible(x)
}
