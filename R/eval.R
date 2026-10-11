# The evaluation harness, kept apart from the agent harness.
#
# Vocabulary (after Harvard CS2680, "Agent evaluation and benchmarks",
# https://cs2680.seas.harvard.edu/notes/agent-evaluation):
#   task        initial state + instruction + completion criteria (a grader)
#   trial       one run of a solver on a task
#   trajectory  the recorded steps of that run (the episode)
#   outcome     the graded answer or end state
#
# run_eval() works in two phases. Phase 1 runs every trial and never shows the
# solver the answer key or the grader. Phase 2 grades the finished answers, in
# random order, so graders never share state with the agent.

# ggplot2 aesthetics use the .data pronoun, which ggplot2 supplies at run time.
utils::globalVariables(".data")

# ---- Task sets -----------------------------------------------------------------

#' Define a set of evaluation tasks
#'
#' A task is an instruction, an optional initial state (data or materials
#' the solver starts from) and completion criteria in the form of a grader.
#' Tasks are split into a development set (`"dev"`, used while building the
#' agent) and a held-out set (`"heldout"`, used once, at the end). Held-out
#' answers should be stored as hashes from [hash_answer()], so the key never
#' appears in plain text.
#'
#' @param id Unique task identifiers, or a data frame with columns `id`,
#'   `instruction`, `grader` and optionally `answer`, `initial_state`,
#'   `split` and others.
#' @param instruction The instruction given to the solver, one per task.
#' @param grader A grader (see [grader_code()]) used for every task, or a
#'   list with one grader per task.
#' @param answer The answer key: `NULL`, a vector with one answer per task,
#'   or a list (one element per task; each element may hold several
#'   accepted answers or expected DOIs).
#' @param initial_state Optional materials per task (a list, one element per
#'   task, `NULL` for none). Agents receive them as text after the
#'   instruction; solver functions receive them as `task$initial_state`.
#' @param split `"dev"` or `"heldout"`, one value or one per task.
#' @param ... Further columns, such as `topic` or `source`.
#' @return A data frame of class `task_set` with list columns `answer`,
#'   `initial_state` and `grader`.
#' @examples
#' tasks <- task_set(
#'   id = c("doi_dynega", "doi_entropy"),
#'   instruction = c(
#'     "Return the DOI of the article titled 'Modeling Latent Topics in Social Media using Dynamic Exploratory Graph Analysis: The Case of the Right-wing and Left-wing Trolls in the 2016 US Elections' (Psychometrika, 2022).",
#'     "Return the DOI of the article titled 'Entropy Fit Indices: New Fit Measures for Assessing the Structure and Dimensionality of Multiple Latent Variables' (Multivariate Behavioral Research, 2020)."),
#'   grader = grader_doi(),
#'   answer = c("10.1007/s11336-021-09820-y", "10.1080/00273171.2020.1779642"))
#' tasks
#' tasks$grader[[1]]("doi:10.1007/S11336-021-09820-Y",
#'                   list(id = "doi_dynega", answer = tasks$answer[[1]]))$pass
#' @export
task_set <- function(id, instruction, grader, answer = NULL, initial_state = NULL,
                     split = "dev", ...) {
  if (is.data.frame(id)) {
    d <- id
    if (!all(c("id", "instruction", "grader") %in% names(d))) {
      .stop("A task data frame needs the columns id, instruction and grader.")
    }
    extra <- d[setdiff(names(d), c("id", "instruction", "grader", "answer", "initial_state", "split"))]
    return(do.call(task_set, c(list(id = d$id, instruction = d$instruction, grader = d$grader,
                                    answer = d$answer, initial_state = d$initial_state,
                                    split = d$split %||% "dev"), as.list(extra))))
  }
  n <- length(id)
  if (anyDuplicated(id)) .stop("Task ids must be unique; duplicated: ",
                               paste(unique(id[duplicated(id)]), collapse = ", "))
  if (length(instruction) != n) .stop("Give one instruction per task id.")
  per_task <- function(x, what) {
    if (is.null(x)) return(vector("list", n))
    if (is.function(x)) return(rep(list(x), n))
    x <- as.list(x)
    if (length(x) != n) .stop("Give one ", what, " per task (", n, "), or one for all.")
    x
  }
  graders <- per_task(grader, "grader")
  if (!all(vapply(graders, is.function, logical(1)))) .stop("Every grader must be a function; see grader_code().")
  split <- rep_len(split, n)
  if (!all(split %in% c("dev", "heldout"))) .stop("`split` must be \"dev\" or \"heldout\".")
  d <- data.frame(id = as.character(id), instruction = as.character(instruction),
                  split = split, stringsAsFactors = FALSE)
  d$answer <- per_task(answer, "answer")
  d$initial_state <- per_task(initial_state, "initial_state")
  d$grader <- graders
  extra <- list(...)
  for (nm in names(extra)) d[[nm]] <- rep_len(extra[[nm]], n)
  # Plain answers in a held-out task defeat its purpose: warn, do not block.
  plain <- d$split == "heldout" & lengths(d$answer) > 0 &
    !vapply(d$answer, function(a) .is_hashed(as.character(unlist(a))), logical(1))
  if (any(plain)) {
    .warn("Held-out task(s) with plain-text answers: ", paste(d$id[plain], collapse = ", "),
          ". Store them with hash_answer().")
  }
  class(d) <- c("task_set", "data.frame")
  d
}

#' @export
print.task_set <- function(x, ...) {
  n_dev <- sum(x$split == "dev")
  cat(sprintf("<task_set> %d tasks (%d dev, %d heldout)\n", nrow(x), n_dev, nrow(x) - n_dev))
  show <- data.frame(
    id = x$id, split = x$split,
    grader = vapply(x$grader, function(g) attr(g, "label") %||% "function", character(1)),
    answer = vapply(x$answer, function(a) {
      a <- as.character(unlist(a))
      if (!length(a)) "(none)" else if (.is_hashed(a)) "(hashed)" else .shorten(paste(a, collapse = "; "), 24)
    }, character(1)),
    instruction = vapply(x$instruction, .shorten, character(1), 40, USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
  print(show, row.names = FALSE, right = FALSE)
  invisible(x)
}

# One task as a list. `public = TRUE` drops the answer key and the grader:
# that is all a solver (or a runtime verifier) may see.
.task_row <- function(tasks, i, public = FALSE) {
  t <- list(id = tasks$id[i], instruction = tasks$instruction[i], split = tasks$split[i],
            initial_state = tasks$initial_state[[i]])
  if (!public) {
    t$answer <- tasks$answer[[i]]
    t$grader <- tasks$grader[[i]]
  }
  t
}

# The text an agent receives: the instruction, then any initial state.
.task_prompt <- function(task) {
  if (is.null(task$initial_state)) return(task$instruction)
  paste0(task$instruction, "\n\nMaterials for this task:\n", .as_text(task$initial_state))
}

# ---- Running an evaluation -----------------------------------------------------

#' Run an evaluation: every task, several trials, graded separately
#'
#' Runs a solver on every task `trials` times, then grades the finished
#' answers. The solver never sees the answer key or the grader; graders
#' never see the solver's messages. Grading happens after all trials, in a
#' random order, so a judge that drifts during a long session does not
#' favor the tasks it saw first.
#'
#' Nothing is dropped: a trial that throws an error becomes a row with
#' `pass = FALSE` and `stop_reason = "error: ..."`; a trial that runs out of
#' time is kept with its real duration and `timed_out = TRUE`; a grader that
#' cannot decide gives `pass = NA` (ungraded, reported by [eval_summary()]).
#'
#' @param solver An [agent()] (each trial is `run(agent, instruction,
#'   budget)`), or a function of one task (a list with `id`, `instruction`,
#'   `initial_state`) that returns an episode or an answer. An answer may
#'   carry attributes `cost`, `tokens`, `model`, `stop_reason` and
#'   `episodes` (as [cascade()] answers do).
#' @param tasks A [task_set()].
#' @param trials Trials per task (at least `k` for pass@k and pass^k).
#' @param budget Limits for each agent episode, from [budget()].
#' @param save_dir Optional folder: every episode is saved there as JSON and
#'   the results as `eval_results.rds` and `.csv`.
#' @param name A name for the solver in results and plots. Defaults to the
#'   agent's name and model.
#' @return A data frame of class `eval_results`, one row per trial:
#'   `solver`, `task_id`, `split`, `trial`, `answer` (as text), `pass`,
#'   `score`, `reason`, `grader`, `cost` (runtime, US dollars),
#'   `grading_cost`, `tokens`, `seconds`, `stop_reason`, `timed_out`,
#'   `model`, `episode_id`. Attributes: `solver`, `date`, `budget`,
#'   `trials`.
#' @examples
#' \dontrun{
#' data(research_tasks)
#' finder <- agent("doi_finder", "Find the DOI the user asks for. Reply with the DOI only.",
#'                 model = "groq:openai/gpt-oss-20b", tools = list(tool_crossref()))
#' res <- keep("eval_finder", run_eval(finder, research_tasks[research_tasks$split == "dev", ],
#'                                     trials = 3, budget = budget(max_steps = 6)))
#' eval_summary(res, k = c(1, 3))
#' }
#' @export
run_eval <- function(solver, tasks, trials = 3, budget = researchAgentR::budget(),
                     save_dir = NULL, name = NULL) {
  if (!inherits(tasks, "task_set")) .stop("`tasks` must be a task_set(); see ?task_set.")
  if (!inherits(solver, "agent") && !is.function(solver)) {
    .stop("`solver` must be an agent() or a function(task).")
  }
  name <- name %||% if (inherits(solver, "agent")) paste0(solver$name, " (", solver$model, ")") else "solver"
  if (!is.null(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

  # Phase 1: run every trial. The solver gets the public task only.
  plan <- expand.grid(trial = seq_len(trials), i = seq_len(nrow(tasks)))
  outcomes <- vector("list", nrow(plan))
  for (r in seq_len(nrow(plan))) {
    i <- plan$i[r]
    .msg(sprintf("[%d/%d] %s, trial %d", r, nrow(plan), tasks$id[i], plan$trial[r]))
    # Each trial gets its own cache "salt", so with use_cache() on, trial 2 is a
    # new request (recorded and replayed separately), not a replay of trial 1.
    old_salt <- .pkg_env$cache_salt
    .pkg_env$cache_salt <- paste("eval", tasks$id[i], plan$trial[r], sep = ":")
    outcomes[[r]] <- .run_trial(solver, .task_row(tasks, i, public = TRUE), budget, save_dir)
    .pkg_env$cache_salt <- old_salt
  }

  # Phase 2: grade, in random order, with the full task (answer key included).
  grades <- vector("list", nrow(plan))
  for (r in sample(seq_len(nrow(plan)))) {
    o <- outcomes[[r]]
    task <- .task_row(tasks, plan$i[r])
    grades[[r]] <- if (o$error) {
      .verdict(FALSE, 0, "solver error: not graded")
    } else {
      tryCatch(task$grader(o$answer, task),
               error = function(e) .verdict(NA, NA_real_, paste("grader error:", conditionMessage(e))))
    }
  }

  res <- data.frame(
    solver = name, task_id = tasks$id[plan$i], split = tasks$split[plan$i], trial = plan$trial,
    answer = vapply(outcomes, function(o) if (o$error) NA_character_ else .answer_text(o$answer), character(1)),
    pass = vapply(grades, function(g) if (is.na(g$pass)) NA else isTRUE(g$pass), logical(1)),
    score = vapply(grades, function(g) as.numeric(g$score %||% NA), numeric(1)),
    reason = vapply(grades, function(g) as.character(g$reason %||% ""), character(1)),
    grader = vapply(plan$i, function(i) attr(tasks$grader[[i]], "label") %||% "function", character(1)),
    cost = vapply(outcomes, `[[`, numeric(1), "cost"),
    grading_cost = vapply(grades, function(g) as.numeric(g$cost %||% 0), numeric(1)),
    tokens = vapply(outcomes, `[[`, numeric(1), "tokens"),
    seconds = vapply(outcomes, `[[`, numeric(1), "seconds"),
    stop_reason = vapply(outcomes, `[[`, character(1), "stop_reason"),
    model = vapply(outcomes, `[[`, character(1), "model"),
    episode_id = vapply(outcomes, `[[`, character(1), "episode_id"),
    stringsAsFactors = FALSE)
  res$timed_out <- res$stop_reason == "max_seconds"
  res <- res[, c(setdiff(names(res), c("model", "episode_id")), "model", "episode_id")]
  res <- .as_eval_results(res, solver = name, budget = budget, trials = trials)
  if (!is.null(save_dir)) {
    saveRDS(res, file.path(save_dir, "eval_results.rds"))
    utils::write.csv(as.data.frame(res), file.path(save_dir, "eval_results.csv"), row.names = FALSE)
  }
  res
}

# One trial. Returns the answer and the runtime ledger numbers; an error is
# caught and reported, never allowed to end the evaluation.
.run_trial <- function(solver, task, budget, save_dir) {
  t0 <- Sys.time()
  out <- tryCatch({
    if (inherits(solver, "agent")) run(solver, .task_prompt(task), budget, verbose = FALSE)
    else solver(task)
  }, error = function(e) e)
  seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (inherits(out, "error")) {
    return(list(error = TRUE, answer = NULL, cost = NA_real_, tokens = NA_real_, seconds = seconds,
                stop_reason = paste("error:", conditionMessage(out)), model = NA_character_,
                episode_id = NA_character_))
  }
  if (inherits(out, "episode")) {
    t <- .totals(out)
    if (!is.null(save_dir)) save_episode(out, dir = save_dir)
    return(list(error = FALSE, answer = out$answer, cost = t$cost, tokens = t$tokens,
                seconds = out$seconds %||% seconds, stop_reason = out$stop_reason,
                model = out$model, episode_id = out$id))
  }
  # A plain answer, possibly carrying its own ledger (see cascade()).
  eps <- attr(out, "episodes")
  if (!is.null(save_dir)) for (e in eps) save_episode(e, dir = save_dir)
  list(error = FALSE, answer = .strip_ledger(out),
       cost = as.numeric(attr(out, "cost") %||% NA_real_),
       tokens = as.numeric(attr(out, "tokens") %||% NA_real_),
       seconds = seconds,
       stop_reason = as.character(attr(out, "stop_reason") %||% "final_answer"),
       model = as.character(attr(out, "model") %||% NA_character_),
       episode_id = if (length(eps)) paste(vapply(eps, `[[`, character(1), "id"), collapse = ";") else NA_character_)
}

# The answer without its ledger attributes (graders see the answer only).
.strip_ledger <- function(x) {
  for (a in c("cost", "tokens", "model", "stop_reason", "episodes", "attempts", "verified")) attr(x, a) <- NULL
  x
}

.as_eval_results <- function(d, solver, budget = NULL, trials = NA) {
  rownames(d) <- NULL
  structure(d, class = c("eval_results", "data.frame"), solver = solver, date = .now(),
            budget = if (is.null(budget)) NULL else unclass(budget), trials = trials,
            package_version = as.character(utils::packageVersion("researchAgentR")))
}

#' @export
print.eval_results <- function(x, ...) {
  solvers <- unique(x$solver)
  cat(sprintf("<eval_results> %d trials of %d tasks | solver(s): %s\n", nrow(x),
              length(unique(x$task_id)), paste(solvers, collapse = ", ")))
  cat(sprintf("Passed %d, failed %d, ungraded %d | errors %d | timeouts %d\n",
              sum(x$pass %in% TRUE), sum(x$pass %in% FALSE), sum(is.na(x$pass)),
              sum(grepl("^error", x$stop_reason)), sum(x$timed_out %in% TRUE)))
  cols <- intersect(c("solver", "task_id", "trial", "pass", "score", "cost", "seconds", "stop_reason"), names(x))
  print(utils::head(as.data.frame(x)[, cols], 10), row.names = FALSE)
  if (nrow(x) > 10) cat("... use eval_summary() for the totals\n")
  invisible(x)
}

# ---- Metrics -------------------------------------------------------------------

#' pass@k and pass^k: how often an agent works at least once, or every time
#'
#' From \eqn{n} trials of one task with \eqn{c} successes, the unbiased
#' estimators are
#' \deqn{\mathrm{pass@}k = 1 - \binom{n-c}{k} / \binom{n}{k}}
#' (the chance that at least one of \eqn{k} trials succeeds) and
#' \deqn{\mathrm{pass}^k = \binom{c}{k} / \binom{n}{k}}
#' (the chance that all \eqn{k} trials succeed). pass@k suits tasks where a
#' person picks the best of several tries; pass^k tells a researcher how
#' often the agent works every time, which is what a pipeline needs. Compute
#' them per task and average over tasks ([eval_summary()] does this).
#'
#' Worked example: \eqn{n = 5}, \eqn{c = 3}, \eqn{k = 2}. There are
#' \eqn{\binom{5}{2} = 10} pairs of trials; \eqn{\binom{2}{2} = 1} pair has
#' two failures and \eqn{\binom{3}{2} = 3} pairs have two successes, so
#' pass@2 = 1 - 1/10 = 0.9 and pass^2 = 3/10 = 0.3.
#'
#' @param n Number of trials (vector).
#' @param c Number of successful trials (vector).
#' @param k Number of tries considered (vector). The result is `NA` when
#'   `k > n`.
#' @return A numeric vector.
#' @examples
#' pass_at_k(n = 5, c = 3, k = 2)    # 0.9
#' pass_hat_k(n = 5, c = 3, k = 2)   # 0.3
#' pass_hat_k(n = 5, c = 3, k = 1:5) # falls as k grows
#' @export
pass_at_k <- function(n, c, k) {
  .check_nck(n, c, k)
  out <- 1 - choose(n - c, k) / choose(n, k)
  out[k > n] <- NA_real_
  out
}

#' @rdname pass_at_k
#' @export
pass_hat_k <- function(n, c, k) {
  .check_nck(n, c, k)
  out <- choose(c, k) / choose(n, k)
  out[k > n] <- NA_real_
  out
}

.check_nck <- function(n, c, k) {
  if (any(c > n, na.rm = TRUE) || any(c < 0 | k < 1 | n < 0, na.rm = TRUE)) {
    .stop("Need 0 <= c <= n and k >= 1.")
  }
}

#' Summarize an evaluation: success, reliability, cost and latency
#'
#' One row per solver with the numbers to report:
#' * `success_rate`: share of graded trials that passed;
#' * `pass@k` and `pass^k` for each `k`, computed per task with the unbiased
#'   estimators of [pass_at_k()] and averaged over tasks (`NA` when a task
#'   has fewer than `k` graded trials);
#' * `cost_per_attempt`: total runtime cost over all trials;
#' * `cost_per_success`: total runtime cost (failures and retries included)
#'   divided by the number of successful trials (`NA` when none passed);
#' * `p50_seconds`, `p95_seconds`: median and 95th percentile time to an
#'   answer, over all trials, timeouts included at their real duration;
#' * `errors`, `timeouts`, `ungraded`: counted, not hidden;
#' * `grading_cost`: what the graders cost, kept apart from runtime cost.
#'
#' When any trial has an unknown cost (a model with no price in
#' [model_registry()]), the cost columns are `NA` rather than an
#' underestimate.
#'
#' @param results An `eval_results` data frame from [run_eval()] (one or
#'   several solvers, for example combined with [rbind()]), or a list of
#'   them.
#' @param k The values of k for pass@k and pass^k.
#' @return A data frame, one row per solver.
#' @examples
#' # A hand-made results table (not model output): 2 tasks x 3 trials.
#' res <- data.frame(
#'   solver = "demo", task_id = rep(c("a", "b"), each = 3), trial = rep(1:3, 2),
#'   pass = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE),
#'   cost = rep(0.002, 6), grading_cost = 0,
#'   seconds = c(4, 5, 6, 8, 30, 7), stop_reason = "final_answer")
#' eval_summary(res, k = c(1, 3))
#' @export
eval_summary <- function(results, k = c(1, 3)) {
  results <- .bind_results(results)
  if (!"solver" %in% names(results)) results$solver <- attr(results, "solver") %||% "solver"
  rows <- lapply(split(results, factor(results$solver, unique(results$solver))), function(d) {
    graded <- d[!is.na(d$pass), , drop = FALSE]
    # Trials and successes per task: the n and c of the pass@k estimators.
    n_t <- as.vector(table(graded$task_id))
    c_t <- as.vector(tapply(as.logical(graded$pass), factor(graded$task_id), sum))
    out <- data.frame(solver = d$solver[1], tasks = length(unique(d$task_id)), trials = nrow(d),
                      success_rate = if (nrow(graded)) mean(graded$pass) else NA_real_,
                      check.names = FALSE, stringsAsFactors = FALSE)
    for (kk in k) {
      out[[paste0("pass@", kk)]] <- if (length(n_t)) mean(pass_at_k(n_t, c_t, kk)) else NA_real_
      out[[paste0("pass^", kk)]] <- if (length(n_t)) mean(pass_hat_k(n_t, c_t, kk)) else NA_real_
    }
    total <- sum(d$cost)  # NA when any cost is unknown: never underestimate
    successes <- sum(d$pass %in% TRUE)
    out$cost_per_attempt <- total / nrow(d)
    out$cost_per_success <- if (successes > 0) total / successes else NA_real_
    out$p50_seconds <- stats::quantile(d$seconds, 0.50, names = FALSE, na.rm = TRUE)
    out$p95_seconds <- stats::quantile(d$seconds, 0.95, names = FALSE, na.rm = TRUE)
    out$errors <- sum(grepl("^error", d$stop_reason))
    out$timeouts <- sum(d$stop_reason %in% "max_seconds")
    out$ungraded <- sum(is.na(d$pass))
    out$grading_cost <- sum(d$grading_cost %||% 0)
    out
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (anyNA(results$cost)) .msg("Some trials have an unknown cost; their solver's cost columns are NA.")
  out
}

# Accept one eval_results, several in a list, or a plain data frame.
.bind_results <- function(results) {
  if (is.data.frame(results)) return(results)
  if (is.list(results)) {
    common <- Reduce(intersect, lapply(results, names))
    return(do.call(rbind, lapply(results, function(r) as.data.frame(r)[, common])))
  }
  .stop("Give an eval_results data frame or a list of them.")
}

# ---- Paired comparisons --------------------------------------------------------

#' Compare two solvers on the same tasks
#'
#' Two agents evaluated on the same tasks should be compared task by task,
#' not by their overall averages: tasks differ much more in difficulty than
#' agents do, and pairing removes that variation. `task_compare()` lists
#' the per-task difference (b minus a, each averaged over its trials) and
#' marks gains and regressions. `paired_bootstrap()` resamples TASKS with
#' replacement (each task keeps all its trials together) and reports the
#' mean difference with a percentile 95% confidence interval.
#'
#' @param a,b `eval_results` data frames for the baseline (`a`) and the
#'   candidate (`b`). Only tasks present in both are compared.
#' @param metric A column to compare: `"pass"` (default), `"score"`,
#'   `"cost"`, `"seconds"` or `"tokens"`.
#' @param R Number of bootstrap resamples.
#' @param seed Random seed, so the interval can be reproduced. The global
#'   random state is restored afterwards.
#' @return `task_compare()`: a data frame with `task_id`, `a`, `b`, `diff`,
#'   `change` (`"gain"`, `"regression"`, `"same"`). `paired_bootstrap()`: a
#'   list of class `paired_bootstrap` with `estimate`, `lower`, `upper`,
#'   `n_tasks`, `R`, `metric`, `a_mean`, `b_mean` and `per_task`.
#' @examples
#' # Hand-made results (not model output): 4 tasks x 2 trials per solver.
#' make <- function(solver, pass) data.frame(
#'   solver = solver, task_id = rep(c("t1", "t2", "t3", "t4"), each = 2),
#'   trial = rep(1:2, 4), pass = pass, cost = 0.001, seconds = 5)
#' a <- make("baseline", c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
#' b <- make("candidate", c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
#' task_compare(a, b)
#' paired_bootstrap(a, b, R = 500)
#' @export
paired_bootstrap <- function(a, b, metric = "pass", R = 2000, seed = 2026) {
  tc <- task_compare(a, b, metric)
  d <- tc$diff
  n <- length(d)
  if (n < 2) .stop("Need at least two shared tasks for a bootstrap.")
  # Use the seed without disturbing the user's random stream.
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else
    assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  set.seed(seed)
  boots <- vapply(seq_len(R), function(i) mean(d[sample.int(n, n, replace = TRUE)]), numeric(1))
  ci <- stats::quantile(boots, c(0.025, 0.975), names = FALSE, na.rm = TRUE)
  structure(list(metric = metric, estimate = mean(d), lower = ci[1], upper = ci[2],
                 n_tasks = n, R = R, seed = seed, a_mean = mean(tc$a), b_mean = mean(tc$b),
                 a_name = .solver_name(a), b_name = .solver_name(b), per_task = tc),
            class = "paired_bootstrap")
}

#' @rdname paired_bootstrap
#' @export
task_compare <- function(a, b, metric = "pass") {
  if (!metric %in% names(a) || !metric %in% names(b)) .stop("Column '", metric, "' is missing.")
  per_task <- function(x) {
    v <- as.numeric(x[[metric]])
    m <- tapply(v, x$task_id, mean, na.rm = TRUE)
    m[is.nan(m)] <- NA_real_
    m
  }
  ma <- per_task(a)
  mb <- per_task(b)
  shared <- intersect(names(ma), names(mb))
  dropped <- setdiff(union(names(ma), names(mb)), shared)
  if (length(dropped)) .msg(length(dropped), " task(s) not in both results were left out: ",
                            paste(utils::head(dropped, 5), collapse = ", "))
  out <- data.frame(task_id = shared, a = unname(ma[shared]), b = unname(mb[shared]),
                    stringsAsFactors = FALSE)
  out <- out[!is.na(out$a) & !is.na(out$b), , drop = FALSE]
  out$diff <- out$b - out$a
  # For cost, time and tokens, lower is better, so a decrease is the gain.
  better <- if (metric %in% c("cost", "seconds", "tokens", "grading_cost")) -out$diff else out$diff
  out$change <- ifelse(better > 0, "gain", ifelse(better < 0, "regression", "same"))
  rownames(out) <- NULL
  out
}

.solver_name <- function(x) attr(x, "solver") %||% (if ("solver" %in% names(x)) x$solver[1]) %||% "?"

#' @export
print.paired_bootstrap <- function(x, ...) {
  cat(sprintf("<paired_bootstrap> %s: %s (b) minus %s (a) over %d shared tasks\n",
              x$metric, x$b_name, x$a_name, x$n_tasks))
  cat(sprintf("Mean a = %.4g, mean b = %.4g, difference = %.4g, 95%% CI [%.4g, %.4g] (%d resamples of tasks)\n",
              x$a_mean, x$b_mean, x$estimate, x$lower, x$upper, x$R))
  ch <- table(factor(x$per_task$change, c("gain", "same", "regression")))
  cat(sprintf("Per task: %d gains, %d same, %d regressions (see $per_task)\n",
              ch[["gain"]], ch[["same"]], ch[["regression"]]))
  invisible(x)
}

# ---- Quality-cost frontier -----------------------------------------------------

# TRUE for runs no other run beats: no run is at least as cheap AND at least
# as successful while strictly better on one of the two.
.pareto_front <- function(cost, success) {
  vapply(seq_along(cost), function(i) {
    if (is.na(cost[i]) || is.na(success[i])) return(NA)
    dominated <- cost <= cost[i] & success >= success[i] & (cost < cost[i] | success > success[i])
    !any(dominated, na.rm = TRUE)
  }, logical(1))
}

#' Plot the quality-cost frontier
#'
#' Each run (solver, model or configuration) is a point: success rate
#' against cost per attempted task. Runs on the Pareto front are those that
#' no other run beats on both axes at once; every other run is dominated (a
#' cheaper run does at least as well). Uses ggplot2 when it is installed and
#' base graphics otherwise.
#'
#' @param ... `eval_results` data frames (one or several solvers each), or
#'   one data frame from [eval_summary()].
#' @param log_cost Put cost on a log scale (useful when costs span orders of
#'   magnitude)?
#' @return A ggplot object (or, with base graphics, the summary invisibly),
#'   with the summary and a logical column `pareto`.
#' @examples
#' # Hand-made summary (not model output) of four configurations.
#' s <- data.frame(solver = c("cheap", "cheap + retries", "strong", "cascade"),
#'                 success_rate = c(0.55, 0.70, 0.85, 0.83),
#'                 cost_per_attempt = c(0.0008, 0.0016, 0.0090, 0.0030))
#' plot_frontier(s)
#' @export
plot_frontier <- function(..., log_cost = FALSE) {
  inputs <- list(...)
  s <- if (length(inputs) == 1 && is.data.frame(inputs[[1]]) &&
           all(c("success_rate", "cost_per_attempt") %in% names(inputs[[1]]))) {
    inputs[[1]]
  } else {
    eval_summary(.bind_results(inputs))
  }
  s$pareto <- .pareto_front(s$cost_per_attempt, s$success_rate)
  if (any(is.na(s$pareto))) .msg("Runs with unknown cost or success are not plotted.")
  s <- s[!is.na(s$pareto), , drop = FALSE]
  front <- s[s$pareto, , drop = FALSE]
  front <- front[order(front$cost_per_attempt), , drop = FALSE]
  # Okabe-Ito blue and vermillion, checked for color-vision deficiency;
  # shape and the legend repeat the distinction, so color is never alone.
  cols <- c(`Pareto front` = "#0072B2", Dominated = "#D55E00")
  s$status <- ifelse(s$pareto, "Pareto front", "Dominated")
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(s, ggplot2::aes(x = .data$cost_per_attempt, y = .data$success_rate)) +
      ggplot2::geom_line(data = front, colour = cols[["Pareto front"]], linewidth = 0.6) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$status, shape = .data$status), size = 3) +
      ggplot2::geom_text(ggplot2::aes(label = .data$solver), hjust = 0, vjust = 0.5,
                         size = 3.3, colour = "grey20",
                         position = ggplot2::position_nudge(x = .label_gap(s$cost_per_attempt, log_cost))) +
      ggplot2::scale_colour_manual(values = cols, name = NULL) +
      ggplot2::scale_shape_manual(values = c(`Pareto front` = 16, Dominated = 17), name = NULL) +
      ggplot2::scale_y_continuous(labels = function(v) paste0(round(100 * v), "%"),
                                  expand = ggplot2::expansion(mult = c(0.08, 0.08))) +
      ggplot2::labs(x = "Cost per attempted task (USD)", y = "Success rate",
                    title = "Quality-cost frontier") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank())
    p <- p + if (log_cost) ggplot2::scale_x_log10(expand = ggplot2::expansion(mult = c(0.05, 0.35))) else
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.35)))
    attr(p, "summary") <- s
    return(p)
  }
  graphics::plot(s$cost_per_attempt, s$success_rate, col = cols[s$status],
                 pch = ifelse(s$pareto, 16, 17), log = if (log_cost) "x" else "",
                 xlab = "Cost per attempted task (USD)", ylab = "Success rate",
                 main = "Quality-cost frontier", ylim = c(0, 1))
  graphics::lines(front$cost_per_attempt, front$success_rate, col = cols[["Pareto front"]])
  graphics::text(s$cost_per_attempt, s$success_rate, s$solver, pos = 4, cex = 0.8)
  graphics::legend("bottomright", names(cols), col = cols, pch = c(16, 17), bty = "n")
  invisible(s)
}

# A fixed gap between a point and its label: 2.5% of the x range (in log10
# units on a log scale, where ggplot2 applies nudges).
.label_gap <- function(x, log_scale) {
  if (log_scale) x <- log10(x[x > 0])
  r <- diff(range(x))
  0.025 * if (is.finite(r) && r > 0) r else max(abs(x), 1e-6)
}

# ---- Contamination -------------------------------------------------------------

#' Check an episode for answer-key contamination
#'
#' A browsing agent can "solve" a benchmark by finding its answer key: the
#' benchmark's repository, a page that lists the answers, or the exact
#' expected answer in a search result. This function searches every tool
#' call (arguments) and tool result in an episode for such strings. A hit is
#' a reason to read the trajectory, not automatic proof of cheating (a DOI
#' task's answer will legitimately appear in a Crossref result).
#'
#' @param episode An episode from [run()] or [read_episode()], or a list of
#'   them.
#' @param patterns Strings to look for (fixed text, case-insensitive): parts
#'   of the answer key, the URL of the benchmark repository, file names.
#' @param task Optional task (a list or a one-row [task_set()]) whose plain
#'   answers are added to `patterns`. Hashed answers are matched by hashing
#'   every DOI found in the tool outputs.
#' @return A data frame with one row per hit: `episode_id`, `step`, `where`
#'   (`"tool_call"` or `"tool_result"`), `tool`, `pattern`, `context` (the
#'   text around the hit). Zero rows means no hit.
#' @examples
#' # A hand-built episode-like list (not a real run).
#' ep <- list(id = "demo", steps = list(
#'   list(step = 1, type = "model", content = ""),
#'   list(step = 2, type = "tool", tool = "web_search",
#'        arguments = '{"query": "researchAgentR research_tasks answers"}',
#'        result = "github.com/hfgolino/researchAgentR/blob/main/data-raw/research_tasks.R")))
#' contamination_check(ep, patterns = c("data-raw/research_tasks", "10.1080/00273171.2020.1779642"))
#' @export
contamination_check <- function(episode, patterns = character(0), task = NULL) {
  if (inherits(episode, "episode") || !is.null(episode$steps)) episode <- list(episode)
  hashed <- character(0)
  if (!is.null(task)) {
    if (is.data.frame(task)) task <- .task_row(task, 1)
    a <- as.character(unlist(task$answer))
    if (.is_hashed(a)) hashed <- a else patterns <- c(patterns, a)
  }
  patterns <- unique(patterns[nzchar(patterns)])
  hits <- list()
  for (ep in episode) {
    for (s in Filter(function(s) identical(s$type, "tool"), ep$steps)) {
      for (where in c("tool_call", "tool_result")) {
        text <- as.character(if (where == "tool_call") s$arguments %||% "" else s$result %||% "")
        if (!length(text) || !nzchar(text)) next
        for (p in patterns) {
          pos <- regexpr(tolower(p), tolower(text), fixed = TRUE)
          if (pos > 0) hits[[length(hits) + 1]] <- .hit(ep, s, where, p, text, pos)
        }
        if (length(hashed)) {
          dois <- .extract_dois(text)
          for (doi in dois[hash_answer(dois, "doi") %in% hashed]) {
            pos <- regexpr(doi, tolower(text), fixed = TRUE)
            hits[[length(hits) + 1]] <- .hit(ep, s, where, "(hashed answer)", text, max(pos, 1))
          }
        }
      }
    }
  }
  if (!length(hits)) {
    return(data.frame(episode_id = character(0), step = integer(0), where = character(0),
                      tool = character(0), pattern = character(0), context = character(0)))
  }
  do.call(rbind, hits)
}

.hit <- function(ep, s, where, pattern, text, pos) {
  data.frame(episode_id = as.character(ep$id %||% NA), step = as.integer(s$step %||% NA),
             where = where, tool = as.character(s$tool %||% NA), pattern = pattern,
             context = .shorten(substr(text, max(1, pos - 60), pos + 100), 180),
             stringsAsFactors = FALSE)
}
