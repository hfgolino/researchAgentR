# Solutions: Activity 09, The Evaluation Harness
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Exercises 1 to 4 run offline (no model, no key). Exercise 5 needs the
# objects of Activity 09 (finder_v1, finder_v2, B, fresh_trials, EP_DIR) and
# a live run.

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")
data(research_tasks)
dev <- research_tasks[research_tasks$split == "dev", ]


# ==============================================================================
# EXERCISE 1: pass@3 and pass^3 for n = 10, c = 7
# ==============================================================================

n <- 10; c_ok <- 7; k <- 3
choose(n, k)                                  # 120 sets of 3 trials
choose(n - c_ok, k)                           # 1 set with 3 failures
choose(c_ok, k)                               # 35 sets with 3 successes
1 - choose(n - c_ok, k) / choose(n, k)        # pass@3 = 119/120 = 0.9917
choose(c_ok, k) / choose(n, k)                # pass^3 = 35/120  = 0.2917
pass_at_k(n = 10, c = 7, k = 3)
pass_hat_k(n = 10, c = 7, k = 3)
# A 70% success rate looks good, but three runs in a row all succeed less
# than 30% of the time.


# ==============================================================================
# EXERCISE 2: a DOI grader that cannot be gamed by listing many DOIs
# ==============================================================================

strict_doi <- grader_doi(min_recall = 1, min_precision = 1)
task1 <- list(id = dev$id[1], answer = dev$answer[[1]])

# Hand-written answers (not model output)
answers <- c(right   = "10.1007/s11336-021-09820-y",
             shotgun = "10.1007/s11336-021-09820-y; 10.1080/00273171.2020.1779642",
             none    = "I could not find the DOI.")
data.frame(
  answer  = names(answers),
  default = vapply(answers, function(a) dev$grader[[1]](a, task1)$pass, logical(1)),
  strict  = vapply(answers, function(a) strict_doi(a, task1)$pass, logical(1)),
  reason  = vapply(answers, function(a) strict_doi(a, task1)$reason, character(1)))
# default: TRUE, TRUE, FALSE; strict: TRUE, FALSE, FALSE.

# The same rule as a custom code grader (exactly one DOI, and it is the key)
one_right_doi <- grader_code(function(answer, task) {
  found <- regmatches(answer, gregexpr("10\\.[0-9]{4,9}/[^[:space:];,]+", answer))[[1]]
  found <- tolower(sub("[.]$", "", found))
  length(found) == 1 && found == tolower(unlist(task$answer))
}, label = "exactly one DOI, and correct")
vapply(answers, function(a) one_right_doi(a, task1)$pass, logical(1))


# ==============================================================================
# EXERCISE 3: a judge that always says "pass"
# ==============================================================================

human  <- c(rep(TRUE, 9), FALSE)       # the human passes 9 of 10 answers
always <- rep(TRUE, 10)                # the judge passes everything
p_o <- mean(always == human)                                          # 0.9
p_e <- mean(always) * mean(human) + mean(!always) * mean(!human)      # 0.9
(p_o - p_e) / (1 - p_e)                                               # 0
calibrate_judge(always, human)
# 90% agreement, kappa = 0: the judge adds nothing beyond the base rate. On a
# task set where most answers pass, agreement is cheap; kappa corrects for it.
# The single answer that matters (the failure) is the one the judge missed.


# ==============================================================================
# EXERCISE 4: paired vs unpaired comparison (CONSTRUCTED PRACTICE DATA)
# ==============================================================================

# These results are CONSTRUCTED with rbinom() for practice. They are not the
# output of any model or agent.
set.seed(2026)
n_tasks <- 12
difficulty <- seq(0.15, 0.95, length.out = n_tasks)   # tasks differ a lot
make_results <- function(solver, p_task, cost_per_trial) {
  d <- data.frame(solver = solver,
                  task_id = rep(sprintf("task%02d", 1:n_tasks), each = 3),
                  trial = rep(1:3, n_tasks),
                  pass = rbinom(3 * n_tasks, 1, rep(p_task, each = 3)) == 1,
                  cost = round(rnorm(3 * n_tasks, cost_per_trial, cost_per_trial / 10), 5),
                  grading_cost = 0,
                  seconds = round(rexp(3 * n_tasks, 1 / 8), 1),
                  stop_reason = "final_answer", stringsAsFactors = FALSE)
  d
}
practice_a <- make_results("version A (constructed)", difficulty, 0.0010)
practice_b <- make_results("version B (constructed)", pmin(difficulty + 0.10, 1), 0.0014)

eval_summary(list(practice_a, practice_b), k = c(1, 3))
task_compare(practice_a, practice_b)
paired <- paired_bootstrap(practice_a, practice_b, R = 2000, seed = 2026)
paired
# Version B was CONSTRUCTED with a success probability 0.10 higher on every
# task, yet with 12 tasks x 3 trials the estimated difference can be close to
# zero or even negative: small task sets have little power to show a real gain.

paired_cost <- paired_bootstrap(practice_a, practice_b, metric = "cost", R = 2000, seed = 2026)
paired_cost
# The printout rounds to 3 decimals, which hides differences of a fraction of
# a cent; read the numbers directly:
signif(unlist(paired_cost[c("a_mean", "b_mean", "estimate", "lower", "upper")]), 3)

# An UNPAIRED bootstrap: resample each version's tasks independently
per_task_a <- tapply(practice_a$pass, practice_a$task_id, mean)
per_task_b <- tapply(practice_b$pass, practice_b$task_id, mean)
set.seed(2026)
unpaired <- replicate(2000, mean(sample(per_task_b, replace = TRUE)) -
                            mean(sample(per_task_a, replace = TRUE)))
data.frame(
  method = c("paired (tasks resampled together)", "unpaired (resampled separately)"),
  lower = round(c(paired$lower, quantile(unpaired, 0.025)), 3),
  upper = round(c(paired$upper, quantile(unpaired, 0.975)), 3))
# The paired interval is narrower: pairing removes the (large) differences in
# task difficulty, which affect both versions in the same way.


# ==============================================================================
# EXERCISE 5: add your own dev tasks (LIVE RUN NEEDED)
# ==============================================================================

# Use a paper you know. Check the DOI yourself on https://doi.org/ before
# using it as an answer key: a wrong key makes every correct agent "fail".
MY_TITLE   <- "REPLACE with the exact title of a paper you know"
MY_YEAR    <- "REPLACE with its year"
MY_JOURNAL <- "REPLACE with its journal"
MY_DOI     <- "REPLACE with its DOI, checked on doi.org"

if (any(grepl("^REPLACE", c(MY_TITLE, MY_YEAR, MY_JOURNAL, MY_DOI)))) {
  message("Fill in MY_TITLE, MY_YEAR, MY_JOURNAL and MY_DOI first.")
} else {
  mine <- task_set(
    id = c("doi_mine", "journal_mine"),
    instruction = c(
      sprintf("Return the DOI of the article titled \"%s\" (%s, %s). Reply with the DOI only.",
              MY_TITLE, MY_JOURNAL, MY_YEAR),
      sprintf("Which journal published the %s article titled \"%s\"? Reply with the journal name only.",
              MY_YEAR, MY_TITLE)),
    grader = list(grader_doi(min_precision = 1), grader_exact()),
    answer = c(MY_DOI, MY_JOURNAL))
  dev_plus <- rbind(as.data.frame(dev)[, names(as.data.frame(mine))], as.data.frame(mine))
  dev_plus <- task_set(dev_plus)
  dev_plus

  # Needs finder_v1, finder_v2, B, fresh_trials() and EP_DIR from Activity 09
  r1 <- keep("eval09_ex5_v1", fresh_trials(run_eval(finder_v1, dev_plus, trials = 3, budget = B,
                                                    save_dir = file.path(EP_DIR, "ex5_v1"))))
  r2 <- keep("eval09_ex5_v2", fresh_trials(run_eval(finder_v2, dev_plus, trials = 3, budget = B,
                                                    save_dir = file.path(EP_DIR, "ex5_v2"))))
  print(task_compare(r1, r2))
  print(paired_bootstrap(r1, r2))
}
