# Evaluation metrics and harness logic. Every results table below is built
# by hand in the test, to check arithmetic; none of it is model output.

test_that("pass@k and pass^k match the worked example", {
  # n = 5 trials, c = 3 successes, k = 2: 10 pairs, 1 pair of failures,
  # 3 pairs of successes.
  expect_equal(pass_at_k(5, 3, 2), 0.9)
  expect_equal(pass_hat_k(5, 3, 2), 0.3)
})

test_that("pass@k and pass^k handle edge cases and vectors", {
  expect_equal(pass_at_k(5, 0, 2), 0)
  expect_equal(pass_hat_k(5, 0, 2), 0)
  expect_equal(pass_at_k(5, 5, 3), 1)
  expect_equal(pass_hat_k(5, 5, 3), 1)
  expect_equal(pass_at_k(4, 1, 1), 0.25)            # k = 1 is the success rate
  expect_equal(pass_hat_k(4, 1, 1), 0.25)
  expect_equal(pass_at_k(5, 4, 2), 1)               # fewer than k failures
  expect_true(is.na(pass_at_k(3, 2, 4)))            # k > n
  expect_true(is.na(pass_hat_k(3, 2, 4)))
  expect_equal(pass_hat_k(5, 3, 1:4), c(0.6, 0.3, 0.1, 0))
  expect_equal(pass_at_k(c(5, 5), c(3, 1), 2), c(0.9, 1 - choose(4, 2) / choose(5, 2)))
  expect_error(pass_at_k(3, 4, 1), "c <= n")
})

hand_results <- function() {
  # 2 tasks x 3 trials, one solver; built by hand for the arithmetic.
  data.frame(solver = "demo", task_id = rep(c("a", "b"), each = 3), trial = rep(1:3, 2),
             pass = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE),
             cost = rep(0.002, 6), grading_cost = c(0, 0, 0, 0.001, 0, 0),
             seconds = c(4, 5, 6, 8, 30, 7),
             stop_reason = c("final_answer", "final_answer", "final_answer",
                             "max_seconds", "error: boom", "final_answer"),
             stringsAsFactors = FALSE)
}

test_that("eval_summary computes success, pass@k, pass^k, costs and latency", {
  s <- eval_summary(hand_results(), k = c(1, 3))
  expect_equal(nrow(s), 1)
  expect_equal(s$tasks, 2)
  expect_equal(s$trials, 6)
  expect_equal(s$success_rate, 0.5)
  expect_equal(s[["pass@1"]], 0.5)           # mean(2/3, 1/3)
  expect_equal(s[["pass@3"]], 1)             # each task passed at least once
  expect_equal(s[["pass^3"]], 0)             # no task passed every time
  expect_equal(s$cost_per_attempt, 0.002)
  expect_equal(s$cost_per_success, 0.012 / 3)  # failures included in the total
  expect_equal(s$p50_seconds, 6.5)
  expect_equal(s$p95_seconds, 8 + 0.75 * 22)   # type-7 quantile, timeout kept
  expect_equal(s$timeouts, 1)
  expect_equal(s$errors, 1)
  expect_equal(s$grading_cost, 0.001)
})

test_that("eval_summary keeps unknown costs and ungraded trials visible", {
  r <- hand_results()
  r$cost[2] <- NA
  r$pass[6] <- NA
  expect_message(s <- eval_summary(r, k = 1), "unknown cost")
  expect_true(is.na(s$cost_per_success))
  expect_true(is.na(s$cost_per_attempt))
  expect_equal(s$ungraded, 1)
  expect_equal(s$success_rate, 2 / 5)
  none <- hand_results()
  none$pass <- FALSE
  expect_true(is.na(eval_summary(none)$cost_per_success))
  expect_true(is.na(eval_summary(hand_results(), k = 4)[["pass@4"]]))
  two <- rbind(hand_results(), transform(hand_results(), solver = "other", cost = 0.01))
  expect_equal(eval_summary(two)$solver, c("demo", "other"))
})

make_pair <- function() {
  # 4 tasks x 2 trials per solver, built by hand.
  make <- function(solver, pass) data.frame(
    solver = solver, task_id = rep(c("t1", "t2", "t3", "t4"), each = 2),
    trial = rep(1:2, 4), pass = pass, cost = 0.001, seconds = 5, stringsAsFactors = FALSE)
  list(a = make("baseline", c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)),
       b = make("candidate", c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)))
}

test_that("task_compare lists per-task gains and regressions", {
  p <- make_pair()
  tc <- task_compare(p$a, p$b)
  expect_equal(tc$diff, c(0.5, 0.5, 0, 0.5))
  expect_equal(tc$change, c("gain", "gain", "same", "gain"))
  # For cost, a decrease is the gain.
  p$b$cost[p$b$task_id == "t1"] <- 0.0005
  tcc <- task_compare(p$a, p$b, "cost")
  expect_equal(tcc$change[tcc$task_id == "t1"], "gain")
  expect_message(task_compare(p$a, p$b[p$b$task_id != "t4", ]), "left out")
})

test_that("paired_bootstrap resamples tasks, is reproducible and leaves the RNG alone", {
  p <- make_pair()
  set.seed(1)
  before <- .Random.seed
  b1 <- paired_bootstrap(p$a, p$b, R = 500, seed = 7)
  expect_identical(.Random.seed, before)
  b2 <- paired_bootstrap(p$a, p$b, R = 500, seed = 7)
  expect_equal(b1$estimate, 0.375)
  expect_equal(c(b1$lower, b1$upper), c(b2$lower, b2$upper))
  # Each resample averages 4 task differences drawn from {0.5, 0.5, 0, 0.5}.
  expect_gte(b1$lower, 0)
  expect_lte(b1$upper, 0.5)
  expect_equal(b1$n_tasks, 4)
  expect_output(print(b1), "3 gains, 1 same, 0 regressions")
  # The header names each side from its own rows: both subsets of one set of
  # results keep the attribute of the whole set.
  both <- rbind(p$a, p$b); attr(both, "solver") <- "baseline"
  b3 <- paired_bootstrap(both[both$solver == "baseline", ], both[both$solver == "candidate", ], R = 100)
  expect_equal(c(b3$a_name, b3$b_name), c("baseline", "candidate"))
  expect_output(print(b3), "candidate (b) minus baseline (a)", fixed = TRUE)
  expect_equal(researchAgentR:::.solver_name(structure(data.frame(pass = TRUE), solver = "named")), "named")
})

test_that("the Pareto front keeps only non-dominated runs", {
  front <- researchAgentR:::.pareto_front
  # run 4 (cost 2.5, success 0.6) is beaten by run 2 (cost 2, success 0.7)
  expect_equal(front(c(1, 2, 3, 2.5), c(0.5, 0.7, 0.9, 0.6)), c(TRUE, TRUE, TRUE, FALSE))
  # same success at a higher cost is dominated; identical runs are both kept
  expect_equal(front(c(1, 2), c(0.8, 0.8)), c(TRUE, FALSE))
  expect_equal(front(c(1, 1), c(0.8, 0.8)), c(TRUE, TRUE))
  expect_equal(front(c(1, NA), c(0.8, 0.9)), c(TRUE, NA))
})

test_that("plot_frontier returns a plot with the front marked", {
  skip_if_not_installed("ggplot2")
  s <- data.frame(solver = c("cheap", "cheap + retries", "strong", "cascade"),
                  success_rate = c(0.55, 0.70, 0.85, 0.83),
                  cost_per_attempt = c(0.0008, 0.0016, 0.0090, 0.0030))
  p <- plot_frontier(s)
  expect_s3_class(p, "ggplot")
  expect_equal(attr(p, "summary")$pareto, c(TRUE, TRUE, TRUE, TRUE))
  p2 <- plot_frontier(hand_results(), transform(hand_results(), solver = "pricier", cost = 0.01))
  expect_equal(attr(p2, "summary")$pareto, c(TRUE, FALSE))
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  expect_silent(print(p2))     # a front of one solver: no line, so no ggplot2 message
})

test_that("task_set validates input and builds from a data frame", {
  ts <- task_set(c("a", "b"), c("Do A.", "Do B."), grader = grader_exact(), answer = c("x", "y"))
  expect_s3_class(ts, "task_set")
  expect_equal(ts$answer, list("x", "y"))
  expect_equal(length(ts$grader), 2)
  expect_error(task_set(c("a", "a"), c("1", "2"), grader_exact()), "unique")
  expect_error(task_set("a", "1", grader_exact(), split = "test"), "split")
  expect_warning(task_set("a", "1", grader_exact(), answer = "plain", split = "heldout"), "hash_answer")
  df <- data.frame(id = c("a", "b"), instruction = c("Do A.", "Do B."), topic = c("x", "y"))
  df$grader <- list(grader_exact(), grader_contains("b"))
  ts2 <- task_set(df)
  expect_equal(ts2$topic, c("x", "y"))
  expect_output(print(ts2), "2 tasks")
})

test_that("run_eval keeps errors, hides the key from the solver, and grades", {
  ts <- task_set(c("ok", "fails", "boom"), c("Say x.", "Say y.", "Crash."),
                 grader = grader_exact(), answer = c("x", "y", "z"))
  seen <- new.env()
  # A deterministic R solver (no model): it answers "x" and records what it
  # was shown, so the test can check that the key never reaches it.
  solver <- function(task) {
    seen[[task$id]] <- names(task)
    if (task$id == "boom") stop("tool exploded")
    structure("x", cost = 0.001, tokens = 10, model = "none")
  }
  res <- suppressMessages(run_eval(solver, ts, trials = 2, name = "fixed"))
  expect_s3_class(res, "eval_results")
  expect_equal(nrow(res), 6)
  expect_false(any(c("answer", "grader") %in% seen[["ok"]]))
  expect_equal(res$pass[res$task_id == "ok"], c(TRUE, TRUE))
  expect_equal(res$pass[res$task_id == "fails"], c(FALSE, FALSE))
  boom <- res[res$task_id == "boom", ]
  expect_equal(boom$pass, c(FALSE, FALSE))
  expect_match(boom$stop_reason, "^error: tool exploded")
  expect_equal(res$cost[res$task_id == "ok"], c(0.001, 0.001))
  expect_equal(attr(res, "solver"), "fixed")
  expect_message(s <- eval_summary(res), "unknown cost")
  expect_equal(s$errors, 2)
  expect_true(is.na(s$cost_per_success))   # the error trials have unknown cost
})

test_that("a grader error makes a trial ungraded, not failed", {
  ts <- task_set("t", "Anything.", grader = grader_code(function(answer, task) stop("API down")))
  res <- suppressMessages(run_eval(function(task) "answer", ts, trials = 1))
  expect_true(is.na(res$pass))
  expect_match(res$reason, "grader error: API down")
})

test_that("run_eval saves results when asked", {
  d <- tempfile()
  ts <- task_set("t", "Say x.", grader = grader_exact(), answer = "x")
  res <- suppressMessages(run_eval(function(task) "x", ts, trials = 1, save_dir = d))
  expect_true(file.exists(file.path(d, "eval_results.rds")))
  expect_true(file.exists(file.path(d, "eval_results.csv")))
})

test_that("contamination_check finds answer-key strings in tool calls and results", {
  # A hand-built episode-like list (not a real run).
  ep <- list(id = "demo", steps = list(
    list(step = 1, type = "model", content = "10.1080/00273171.2020.1779642"),
    list(step = 2, type = "tool", tool = "web_search",
         arguments = '{"query": "researchAgentR research_tasks answers"}',
         result = "github.com/hfgolino/researchAgentR/blob/main/data-raw/research_tasks.R"),
    list(step = 3, type = "tool", tool = "crossref",
         arguments = '{"title": "Entropy Fit Indices"}',
         result = "DOI: 10.1080/00273171.2020.1779642")))
  hits <- contamination_check(ep, patterns = c("data-raw/research_tasks", "research_tasks answers"))
  expect_equal(nrow(hits), 2)
  expect_setequal(hits$where, c("tool_call", "tool_result"))
  # The plain answer of a task is added to the patterns; model text is not searched.
  task <- list(answer = "10.1080/00273171.2020.1779642")
  h2 <- contamination_check(ep, task = task)
  expect_equal(h2$step, 3)
  # Hashed answers are matched through the DOIs found in tool output.
  h3 <- contamination_check(ep, task = list(answer = hash_answer("10.1080/00273171.2020.1779642", "doi")))
  expect_equal(h3$pattern, "(hashed answer)")
  expect_equal(nrow(contamination_check(ep, patterns = "nothing like this")), 0)
})

test_that("the research_tasks benchmark is well formed", {
  data("research_tasks", package = "researchAgentR", envir = environment())
  expect_s3_class(research_tasks, "task_set")
  expect_equal(nrow(research_tasks), 7)
  held <- research_tasks[research_tasks$split == "heldout" & lengths(research_tasks$answer) > 0, ]
  expect_true(all(vapply(held$answer, function(a) all(startsWith(a, "md5:")), logical(1))))
  i <- which(research_tasks$id == "doi_entropy")
  task <- list(id = "doi_entropy", answer = research_tasks$answer[[i]])
  expect_true(research_tasks$grader[[i]]("https://doi.org/10.1080/00273171.2020.1779642", task)$pass)
  expect_equal(researchAgentR:::.nih_project_numbers("Try 5R01MH000001-02 or r21ag000002."),
               c("R01MH000001", "R21AG000002"))
})

test_that("the NIH grader looks each project up by its number (the search itself is replaced here)", {
  # The grader's logic only: which lookup it makes and how it reads the rows.
  # The record is made up (serial 000001); the live check is in tools/live_smoke.R.
  asked <- list()
  record <- data.frame(project_num = "5R01MH000001-02", title = "Ecological momentary assessment of mood",
                       abstract = "Daily surveys.", fiscal_year = 2025L, stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(search_nih_projects = function(..., project_nums = NULL, fiscal_years = NULL, n = 25) {
    asked[[length(asked) + 1]] <<- list(nums = project_nums, fy = fiscal_years)
    if (any(grepl("R01MH000001", project_nums)) && 2025 %in% fiscal_years) record else record[0, ]
  }, .package = "researchAgentR")
  grade <- researchAgentR:::.grader_nih_fy(2025, "ecological momentary assessment")
  out <- grade("The project 5R01MH000001-02 fits.", list(id = "t"))
  expect_true(out$pass)
  # by number, not as free text: the nine type digits, "*" only at the end
  expect_equal(asked[[1]], list(nums = paste0(1:9, "R01MH000001*"), fy = 2025))
  expect_length(asked, 1)
  expect_true(grade("R01MH000001", list(id = "t"))$pass)
  expect_false(grade("R01MH000001 and R21AG000002", list(id = "t"))$pass)  # one number cannot be verified
  expect_false(researchAgentR:::.grader_nih_fy(2024, "ecological momentary assessment")("R01MH000001", list(id = "t"))$pass)
  expect_false(researchAgentR:::.grader_nih_fy(2025, "item response theory")("R01MH000001", list(id = "t"))$pass)
  expect_false(grade("No project number here.", list(id = "t"))$pass)
})

test_that("run_eval runs a live agent on the dev DOI tasks (live)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")), "GROQ_API_KEY not set")
  data("research_tasks", package = "researchAgentR", envir = environment())
  dev <- research_tasks[research_tasks$id %in% c("doi_dynega", "doi_entropy"), ]
  a <- agent("doi_finder", "Reply with the DOI of the article the user describes, and nothing else.",
             model = "groq:openai/gpt-oss-20b")
  res <- run_eval(a, dev, trials = 1, budget = budget(max_steps = 2))
  expect_equal(nrow(res), 2)
  expect_true(all(is.finite(res$cost)))
})
