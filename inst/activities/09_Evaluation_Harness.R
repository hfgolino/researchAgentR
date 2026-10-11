# Activity 09: The Evaluation Harness (Eval)
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: Evaluate an agent the way a measurement instrument is evaluated:
# repeated trials on a fixed task set, graders kept apart from the agent,
# reliability (pass^k), cost per success, and paired comparisons between
# versions, with the trajectories read and not only the scores.
#
# OVERVIEW: An agent that answered correctly once has not shown that it works.
# The evaluation harness is a SEPARATE piece of software from the agent
# harness (the loop of Activity 02): it runs the agent, collects its answers,
# and grades them afterwards with graders the agent never sees. The vocabulary:
#
#   task        initial state + instruction + completion criteria (a grader)
#   trial       one run of an agent on a task
#   trajectory  the recorded steps of that trial (the episode)
#   outcome     the graded answer (pass or fail, a score, and why)
#
# The activity uses the shipped benchmark research_tasks (4 dev tasks for
# building, 3 held-out tasks used ONCE at the very end), a code grader
# for everything code can decide, an LLM judge with a frozen rubric for what
# it cannot, and the reliability metrics pass@k and pass^k.
#
# The vocabulary and the metrics follow Harvard CS2680 (Juncheng Yang, Fall
# 2026), "Agent evaluation and benchmarks":
# https://cs2680.seas.harvard.edu/notes/agent-evaluation
#
# researchAgentR functions used here:
#   research_tasks, task_set()          the benchmark and how to define tasks
#   grader_doi(), grader_exact(), grader_code()   code graders
#   grader_judge(), calibrate_judge()   LLM judge with a frozen rubric, and its
#                                       agreement with human labels (kappa)
#   run_eval(), eval_summary()          repeated trials, then the numbers to report
#   pass_at_k(), pass_hat_k()           "works at least once" vs "works every time"
#   task_compare(), paired_bootstrap()  compare two agent versions task by task
#   contamination_check()               did a browsing agent find the answer key?
#   plot_frontier()                     quality-cost frontier
#   view_episode(), read_episode()      read a failed trajectory
#
# TIME: about 75 minutes.  COST: free tier (Groq, Crossref, OpenAlex, NIH
# RePORTER). Step 10 optionally uses Tavily web search (free plan, key needed).
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

use_cache("auto")      # every API response of the first live run is saved
set_groq_key()         # reads GROQ_API_KEY from ~/.Renviron (never type keys here)
if (nzchar(Sys.getenv("OPENALEX_API_KEY"))) set_openalex_key()   # optional free key

dir.create("results", showWarnings = FALSE)
EP_DIR <- file.path("episodes", "activity09")     # every trajectory is saved here

# IMPORTANT: repeated trials must be INDEPENDENT. With use_cache("auto"), the
# second trial of a task sends exactly the same first request as the first
# trial (same instructions, same task, same date), so it would REPLAY the
# first trial's response: the same answer at cost 0. pass^k and cost per
# success would then be meaningless. fresh_trials() switches the response
# cache off while an evaluation runs; keep() saves the finished results, so
# later runs of this script (and the class demo) load them offline.
fresh_trials <- function(expr) {
  old <- use_cache("off")
  on.exit(use_cache(old))
  expr                    # evaluated here, after the cache was switched off
}

TRIALS <- 5            # n trials per task (pass^k needs n >= k); use 3 if rate limited

# Set the budget BEFORE looking at any result, and keep it fixed for every
# version compared below (a comparison with different budgets is not fair).
B <- budget(max_steps = 6, max_tokens = 30000, max_cost = 0.01, max_seconds = 120)
B
# EXPECTED OUTPUT: <budget> 6 steps | 30,000 tokens | $0.0100 | 120 s | 4 tool errors in a row


# ==============================================================================
# STEP 1: TASKS, TRIALS, TRAJECTORIES, OUTCOMES
# ==============================================================================

data(research_tasks)
research_tasks
# EXPECTED OUTPUT: <task_set> 7 tasks (4 dev, 3 heldout), one row per task with
# its id, split, grader label, answer ("(hashed)" for held-out tasks, "(none)"
# for the NIH tasks, which are graded live) and the start of the instruction.

dev     <- research_tasks[research_tasks$split == "dev", ]
heldout <- research_tasks[research_tasks$split == "heldout", ]   # do not touch until STEP 11

# One task, as the AGENT sees it (instruction only) ...
cat(dev$instruction[1], "\n")
# ... and what only the GRADER sees (the answer key and the grader itself)
dev$answer[[1]]
dev$grader[[1]]
# EXPECTED OUTPUT: the instruction asks for the DOI of a Psychometrika (2022)
# article; the answer key is one DOI; the grader is "<grader> doi (recall >= 1)".

# Held-out answers are stored as hashes: an agent that browses the package
# source (or a student) cannot read the key.
heldout$answer[[1]]
# EXPECTED OUTPUT: a string that starts with "md5:".


# ==============================================================================
# STEP 2: CODE GRADERS FIRST (NO MODEL, NO COST)
# ==============================================================================

# A grader is a function(answer, task) returning pass, score, reason and cost.
# Prefer code wherever code can decide: it is free, fast and deterministic.
# The answers below are typed by hand to test the grader (not model output).

task1 <- list(id = dev$id[1], answer = dev$answer[[1]])
g_doi <- dev$grader[[1]]

g_doi("doi:10.1007/S11336-021-09820-Y", task1)$pass           # TRUE: case and prefix ignored
g_doi("The DOI is https://doi.org/10.1007/s11336-021-09820-y.", task1)$pass   # TRUE
g_doi("10.1007/s11336-021-09820", task1)$reason               # a truncated DOI fails
# EXPECTED OUTPUT: TRUE, TRUE, then "recall 0.00 (0 of 1 expected DOIs),
# precision 0.00 (1 DOIs returned)".

# Grader gaming: an answer that lists MANY DOIs passes a recall-only grader.
shotgun <- "10.1007/s11336-021-09820-y or 10.1080/00273171.2020.1779642"
g_doi(shotgun, task1)$pass                                    # TRUE (recall 1, precision 0.5)
grader_doi(min_recall = 1, min_precision = 1)(shotgun, task1)$pass   # FALSE
# EXPECTED OUTPUT: TRUE, then FALSE. Graders are part of the measurement:
# decide what a correct answer is BEFORE running the agent.

# The journal task uses an exact-match grader (after normalization)
task3 <- list(id = dev$id[3], answer = dev$answer[[3]])
dev$grader[[3]]("multivariate behavioral research.", task3)$pass        # TRUE
dev$grader[[3]]("Multivariate Behavioral Research (MBR)", task3)$reason # fails
# EXPECTED OUTPUT: TRUE, then "no exact match; got 'Multivariate Behavioral Research (MBR)'".

# The NIH task has no fixed answer: its grader checks the project number
# against the live NIH RePORTER API at grading time (pass = NA when offline).
dev$grader[[4]]


# ==============================================================================
# STEP 3: THE AGENT UNDER TEST (VERSION 1)
# ==============================================================================

finder_v1 <- agent(
  name = "finder_v1",
  instructions = paste(
    "You answer bibliographic and research-funding questions.",
    "Use the tools to look up the answer. Reply with the answer only."),
  model = "groq:openai/gpt-oss-20b",
  tools = list(tool_crossref(n = 5), tool_openalex(n = 5),
               tool_nih_reporter(n = 5, fiscal_years = 2025)),
  description = "Finds DOIs, journals and NIH project numbers.")
finder_v1


# ==============================================================================
# STEP 4: RUN THE EVALUATION (REPEATED TRIALS, GRADED AFTERWARDS)
# ==============================================================================

# run_eval() works in two phases:
#   Phase 1: every trial runs; the agent receives the instruction only.
#   Phase 2: the finished answers are graded, in random order, by graders that
#            never shared state with the agent.
# 4 dev tasks x 5 trials = 20 episodes (several minutes on the free tier).

res_v1 <- keep("eval09_finder_v1", fresh_trials(
  run_eval(finder_v1, dev, trials = TRIALS, budget = B,
           save_dir = file.path(EP_DIR, "v1"), name = "finder v1")))
res_v1
# EXPECTED OUTPUT: <eval_results> 20 trials of 4 tasks, a line with the counts
# of passed, failed and ungraded trials, errors and timeouts, then the first
# 10 rows (solver, task_id, trial, pass, score, cost, seconds, stop_reason).

table(res_v1$task_id, res_v1$pass, useNA = "ifany")   # successes per task
table(res_v1$stop_reason)                              # how did the trials end?
# EXPECTED OUTPUT: a tasks x pass table (NA = ungraded), and a count of stop
# reasons (final_answer, max_steps, api_error: ..., and so on).


# ==============================================================================
# STEP 5: WHAT TO REPORT: eval_summary()
# ==============================================================================

summary_v1 <- eval_summary(res_v1, k = c(1, 3, 5))
summary_v1
# EXPECTED OUTPUT: one row with tasks, trials, success_rate, pass@1, pass^1,
# pass@3, pass^3, pass@5, pass^5, cost_per_attempt, cost_per_success,
# p50_seconds, p95_seconds, errors, timeouts, ungraded and grading_cost.
#
# Read it as a researcher:
#   success_rate      share of graded trials that passed
#   pass^k            chance that k trials in a row ALL succeed (pipelines need this)
#   cost_per_success  total runtime cost (failures included) / successful trials
#   p95_seconds       the slow tail: 1 trial in 20 takes longer than this
#   ungraded          trials the grader could not decide (never counted as failures)


# ==============================================================================
# STEP 6: pass@k VERSUS pass^k (WORKED EXAMPLE)
# ==============================================================================

# One task, n = 5 trials, c = 3 successes, k = 2.
#   pass@k = 1 - C(n - c, k) / C(n, k)   at least one of k trials succeeds
#   pass^k = C(c, k) / C(n, k)           all k trials succeed
n <- 5; c_ok <- 3; k <- 2
choose(n, k)                       # 10 possible pairs of trials
choose(n - c_ok, k)                # 1 pair with two failures
choose(c_ok, k)                    # 3 pairs with two successes
1 - choose(n - c_ok, k) / choose(n, k)   # 0.9
choose(c_ok, k) / choose(n, k)           # 0.3
pass_at_k(n = 5, c = 3, k = 2)            # 0.9
pass_hat_k(n = 5, c = 3, k = 2)           # 0.3
# EXPECTED OUTPUT: 10, 1, 3, 0.9, 0.3, 0.9, 0.3.

# The same by brute force: list all 10 pairs of the 5 trials (S S S F F)
trial_outcomes <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
pairs <- combn(5, 2)
mean(apply(pairs, 2, function(ix) any(trial_outcomes[ix])))   # 0.9
mean(apply(pairs, 2, function(ix) all(trial_outcomes[ix])))   # 0.3

# As k grows, pass@k rises and pass^k falls:
data.frame(k = 1:5, pass_at_k = pass_at_k(5, 3, 1:5), pass_hat_k = pass_hat_k(5, 3, 1:5))
# EXPECTED OUTPUT: pass@k = 0.6, 0.9, 1, 1, 1 and pass^k = 0.6, 0.3, 0.1, 0, 0.

# The intuition with INDEPENDENT trials that each succeed with p = 0.8, k = 3:
p <- 0.8
1 - (1 - p)^3      # 0.992: almost surely right at least once in 3 tries
p^3                # 0.512: right all 3 times only about half the time
# EXPECTED OUTPUT: 0.992 and 0.512. A researcher running an agent over 500
# abstracts needs it to work EVERY time: report pass^k, not only pass@k.


# ==============================================================================
# STEP 7: INSPECT TRAJECTORIES, NOT ONLY SCORES
# ==============================================================================

# A score says THAT a trial failed; the trajectory says WHY (wrong search
# query, tool error, a correct record that was ignored, a format problem).
failed <- as.data.frame(res_v1)[res_v1$pass %in% FALSE, c("task_id", "trial", "answer", "reason",
                                            "stop_reason", "episode_id")]
failed
# EXPECTED OUTPUT: one row per failed trial with the grader's reason. Zero rows
# means every graded trial passed (then pick a trial with the highest cost).

if (nrow(failed) > 0) {
  ep_file <- file.path(EP_DIR, "v1", paste0(failed$episode_id[1], ".json"))
  ep <- read_episode(ep_file)
  print(episode_steps(ep)[, c("step", "type", "name", "is_error", "detail")])
  view_episode(ep_file)      # opens the self-contained HTML trajectory viewer
}
# EXPECTED OUTPUT: the steps of the failed trial (model calls and tool calls,
# with errors marked) and an HTML page in the browser. Write down, in one
# sentence, where the trial went wrong.


# ==============================================================================
# STEP 8: AN LLM JUDGE WITH A FROZEN RUBRIC, CALIBRATED AGAINST HUMANS
# ==============================================================================

# Some answers cannot be graded by code (is this a real threat to validity
# for this design?). An LLM judge can grade them, under three rules:
#   1. the rubric is FROZEN before grading (its hash is stored in every row);
#   2. the judge's cost is GRADING cost, kept apart from the agent's cost;
#   3. the judge is trusted only as far as it agrees with human graders.

RUBRIC <- paste(
  "Pass if the answer gives exactly what the instruction asks for (the number of",
  "items requested), every item is correct for the design described in the task,",
  "and each item is explained in one sentence. Fail if an item is missing, does not",
  "apply to the design described, or contains a factual error about research methods.")

judge <- grader_judge(RUBRIC, model = "groq:openai/gpt-oss-120b")
judge
# EXPECTED OUTPUT: <grader> judge groq:openai/gpt-oss-120b rubric <12-character
# hash>, then the model, the hash and the rubric text.

# Tasks written for this activity (instructions only; no fixed answer key)
design_tasks <- task_set(
  id = c("threats_one_group", "threats_two_schools", "measure_loneliness"),
  instruction = c(
    paste("A study measures anxiety in 40 students before and after a 6-week mindfulness",
          "course, with no control group. Name two threats to internal validity, one",
          "sentence each."),
    paste("Two schools are compared at the end of the year: one adopted a new reading",
          "program and the other did not. Name two threats to internal validity, one",
          "sentence each."),
    paste("Propose two different ways to measure loneliness in adults aged 65 or older,",
          "and name one weakness of each, in at most four sentences.")),
  grader = judge)

methods_tutor <- agent("methods_tutor",
                       "You answer research methods questions concisely and precisely.",
                       model = "groq:openai/gpt-oss-20b")

res_judge <- keep("eval09_judge", fresh_trials(
  run_eval(methods_tutor, design_tasks, trials = 3, budget = budget(max_steps = 2),
           save_dir = file.path(EP_DIR, "judge"), name = "methods tutor")))
as.data.frame(res_judge)[, c("task_id", "trial", "pass", "reason", "cost", "grading_cost")]
eval_summary(res_judge, k = 1)[, c("success_rate", "cost_per_attempt", "grading_cost")]
# EXPECTED OUTPUT: 9 rows (3 tasks x 3 trials) with the judge's verdict and
# reason, the agent's runtime cost and the judge's grading cost in separate
# columns; the summary keeps grading_cost apart from cost_per_attempt.

# --- Calibration: a judge is only as good as its agreement with humans ------
# Cohen's kappa = (p_o - p_e) / (1 - p_e), where p_o is the observed agreement
# and p_e the agreement expected by chance from each rater's pass rates.
# A worked example with 10 verdicts (typed in, not model output):
example_judge <- c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
example_human <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
p_o <- mean(example_judge == example_human)                       # 0.8
p_e <- mean(example_judge) * mean(example_human) +
  mean(!example_judge) * mean(!example_human)                     # 0.58
(p_o - p_e) / (1 - p_e)                                           # 0.524
calibrate_judge(example_judge, example_human)
# EXPECTED OUTPUT: 0.5238 (with p_o = 0.8 and p_e = 0.58); then "<judge_calibration> 10 pairs |
# agreement 80.0% | Cohen's kappa 0.524", the 2 x 2 table and a note about
# 2 disagreements.

# --- YOUR labels: read the 9 answers and grade them yourself with the rubric --
for (i in seq_len(nrow(res_judge))) {
  cat(sprintf("\n[%d] %s, trial %d\n%s\n", i, res_judge$task_id[i], res_judge$trial[i],
              res_judge$answer[i]))
}
# Type TRUE (pass) or FALSE (fail) for each answer, in the order printed above,
# WITHOUT looking at the judge's verdicts first.
HUMAN_LABELS <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA)

if (all(is.na(HUMAN_LABELS))) {
  message("Fill in HUMAN_LABELS (one TRUE/FALSE per answer) to calibrate the judge.")
} else {
  human <- data.frame(task_id = res_judge$task_id, trial = res_judge$trial,
                      pass = HUMAN_LABELS)
  cal <- calibrate_judge(res_judge, human)
  print(cal)
  cal$disagreements        # read these answers again: who was right?
}
# EXPECTED OUTPUT: <judge_calibration> 9 pairs, the percent agreement, Cohen's
# kappa, the judge x human table and the disagreements. With 9 answers the
# kappa is very imprecise: a real calibration needs a few dozen.


# ==============================================================================
# STEP 9: COMPARE TWO VERSIONS ON THE SAME TASKS (PAIRED)
# ==============================================================================

# Version 2 changes ONE thing: the instructions (same model, tools, budget).
finder_v2 <- agent(
  name = "finder_v2",
  instructions = paste(
    "You answer bibliographic and research-funding questions.",
    "For a DOI or a journal: search Crossref with the exact title, then check that the",
    "record's title matches the requested title word for word and that the year and",
    "journal match before you answer.",
    "For an NIH project: use the NIH RePORTER tool and return the core project number",
    "(for example R01 followed by the institute code and six digits).",
    "Reply with the answer only: no URL prefix, no explanation."),
  model = "groq:openai/gpt-oss-20b",
  tools = list(tool_crossref(n = 5), tool_openalex(n = 5),
               tool_nih_reporter(n = 5, fiscal_years = 2025)),
  description = "Finds DOIs, journals and NIH project numbers.")

res_v2 <- keep("eval09_finder_v2", fresh_trials(
  run_eval(finder_v2, dev, trials = TRIALS, budget = B,
           save_dir = file.path(EP_DIR, "v2"), name = "finder v2")))

eval_summary(list(res_v1, res_v2), k = c(1, 3))
# EXPECTED OUTPUT: two rows (finder v1, finder v2) with the columns of STEP 5.

# Tasks differ much more in difficulty than versions do, so compare TASK BY
# TASK (each task's success rate under v2 minus under v1):
task_compare(res_v1, res_v2)
# EXPECTED OUTPUT: task_id, a (v1), b (v2), diff and change ("gain",
# "regression" or "same") for each of the 4 tasks.

paired_bootstrap(res_v1, res_v2, R = 2000)
pb_cost <- paired_bootstrap(res_v1, res_v2, metric = "cost", R = 2000)
pb_cost
signif(unlist(pb_cost[c("a_mean", "b_mean", "estimate", "lower", "upper")]), 3)
# (the printout uses 3 decimals, too few for costs of a fraction of a cent)
# EXPECTED OUTPUT: for each metric, the mean of v1 and v2, the difference
# (v2 minus v1), a 95% percentile interval from resampling TASKS, and the
# number of gains, ties and regressions. With only 4 dev tasks the interval
# is wide: 4 tasks cannot show much. Real comparisons need more tasks
# (Exercise 5), and a regression on any task deserves a look at its trajectory.


# ==============================================================================
# STEP 10: CONTAMINATION: DID A BROWSING AGENT FIND THE ANSWER KEY?
# ==============================================================================

# Three contamination risks (CS2680): the model saw the answers in training,
# the agent RETRIEVES published answers while browsing (the benchmark's own
# source code, a page listing the answers), and the agent games the grader.
# contamination_check() searches every tool call and tool result of an
# episode for strings from the answer key and the benchmark's location.

# OPTIONAL (needs a Tavily key: TAVILY_API_KEY in ~/.Renviron, free plan).
if (!nzchar(Sys.getenv("TAVILY_API_KEY"))) {
  message("No TAVILY_API_KEY in ~/.Renviron: read this step, skip the run.")
} else {
  set_tavily_key()
  browser_agent <- agent(
    name = "browser",
    instructions = "Answer the question. Search the web and read pages as needed. Reply with the answer only.",
    model = "groq:openai/gpt-oss-120b",
    tools = list(tool_web_search(engine = "tavily", n = 5), tool_read_page(max_chars = 6000)))

  doi_tasks <- dev[dev$id %in% c("doi_dynega", "doi_entropy"), ]
  res_browse <- keep("eval09_browser", fresh_trials(
    run_eval(browser_agent, doi_tasks, trials = 2, budget = B,
             save_dir = file.path(EP_DIR, "browser"), name = "browser")))

  # Strings that point to the answer key's home (not to legitimate sources)
  KEY_PATTERNS <- c("researchAgentR", "research_tasks", "data-raw")

  hits <- do.call(rbind, lapply(seq_len(nrow(res_browse)), function(r) {
    f <- file.path(EP_DIR, "browser", paste0(res_browse$episode_id[r], ".json"))
    if (!file.exists(f)) return(NULL)            # an errored trial has no episode
    contamination_check(read_episode(f), patterns = KEY_PATTERNS,
                        task = doi_tasks[doi_tasks$id == res_browse$task_id[r], ])
  }))
  print(hits)
}
# EXPECTED OUTPUT: a data frame with episode_id, step, where ("tool_call" or
# "tool_result"), tool, pattern and context. Expect the expected DOI itself
# to appear in search results (that is how a DOI is found: not cheating).
# A hit on "researchAgentR" or "research_tasks" is a red flag: open that
# trajectory with view_episode() and read what the agent did with it.
# Zero rows means no hit.


# ==============================================================================
# STEP 11: THE QUALITY-COST FRONTIER, THEN THE HELD-OUT TASKS (ONCE)
# ==============================================================================

plot_frontier(res_v1, res_v2)
# EXPECTED OUTPUT: a scatter plot of success rate against cost per attempted
# task, one point per version, with the Pareto front (versions no other
# version beats on both axes) in blue and dominated versions in vermillion.

# Choose the final version on the DEV results, write the choice down, and
# only then run the held-out tasks, ONCE. Tuning after seeing held-out
# results turns them into development tasks.
FINAL <- finder_v2       # replace with the version your dev results support

res_heldout <- keep("eval09_heldout_final", fresh_trials(
  run_eval(FINAL, heldout, trials = TRIALS, budget = B,
           save_dir = file.path(EP_DIR, "heldout"), name = paste(FINAL$name, "(held-out)"))))
eval_summary(res_heldout, k = c(1, 3, 5))
# EXPECTED OUTPUT: one row for the held-out run, with the same columns as STEP 5.
# Graders compare HASHES of the agent's answer with the hashed key.

saveRDS(list(v1 = res_v1, v2 = res_v2, judge = res_judge, heldout = res_heldout,
             rubric = RUBRIC, date = Sys.Date()),
        "results/activity09_eval.rds")


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Re-grade res_v1 with a stricter DOI grader (min_precision = 1): build a
#    copy of dev with that grader and grade the saved answers by hand with
#    vapply(); does any pass become a fail?
# 2. Change ONE word of RUBRIC and print grader_judge(RUBRIC2): the hash
#    changes, so results graded with it are not comparable to res_judge.
# 3. Run eval_summary(res_v1, k = 1:5) and plot pass^k against k.


# ==============================================================================
# EXERCISES (solutions in solutions/09_solutions.R)
# ==============================================================================

# 1. A task was run n = 10 times with c = 7 successes. Compute pass@3 and
#    pass^3 by hand with choose(), then check with pass_at_k() and pass_hat_k().
# 2. Write a DOI grader that cannot be gamed by listing many DOIs, and test it
#    on three hand-written answers: the right DOI, the right DOI plus a wrong
#    one, and no DOI.
# 3. A judge that says "pass" to everything agrees with a human 90% of the
#    time when the human passes 9 of 10 answers. Compute its Cohen's kappa by
#    hand and with calibrate_judge(). What does the result tell you?
# 4. Practise the paired comparison on CONSTRUCTED results (12 tasks x 3
#    trials for two versions; build them with data.frame()). Run
#    task_compare() and paired_bootstrap(), then compare the paired interval
#    with an UNPAIRED bootstrap (resampling each version's tasks separately).
# 5. Add two dev tasks of your own with task_set() (a DOI you checked
#    yourself on doi.org, and its journal), combine them with dev, and
#    re-run the comparison of v1 and v2.


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - All trials of a task are identical and cost 0: the response cache replayed
#   trial 1. Run evaluations inside fresh_trials() (cache off), wrapped in keep().
# - "429 Rate limit" or very slow trials: the free tier allows 8K tokens per
#   minute for gpt-oss on Groq. Lower TRIALS to 3, give the agent fewer and
#   smaller tools (Activity 10), or run the versions on different days.
# - The NIH task shows pass = NA: its grader checks NIH RePORTER live at
#   grading time; NA means ungraded (network or API down), not a failure.
# - cost_per_success is NA: no trial passed, or a model has no price in
#   model_registry() (add one with registry_set()).
# - calibrate_judge() complains about the length: HUMAN_LABELS needs exactly
#   one TRUE/FALSE per answer, in the printed order.
# - contamination_check() finds nothing for an agent that used Groq's built-in
#   browser_search: built-in tools run on Groq's servers and their pages are not
#   tool steps in the episode. Evaluate with custom search tools, whose calls and
#   results are recorded and can be checked.
# - The held-out results disappoint and you want to tweak the agent: you can,
#   but then the held-out tasks become development tasks. Report that honestly
#   and build new held-out tasks.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. Why must the evaluation harness be separate from the agent harness?
#    (An agent that can see its grader or answer key is not being measured.)
# 2. pass@k vs pass^k: which one belongs in the methods section of a paper
#    that used an agent to code 2,000 open-ended responses? Why?
# 3. A judge with 85% agreement and kappa = 0.2: would you use it? What does
#    a low kappa with high agreement reveal about the task set?
# 4. Paired vs unpaired: why does pairing by task shrink the interval?
#    (Psychometric analogy: a within-subject design removes person variance.)
# 5. The browsing agent found the benchmark's source code. Is a correct
#    answer then a success? How would you design tasks that resist this?
# 6. Evaluation is measurement: which parts of this harness map onto
#    reliability, validity and standardization of administration?
