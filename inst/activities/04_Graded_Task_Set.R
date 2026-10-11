# Activity 04: A Graded Task Set (Before Tuning Anything)
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: Before changing a single prompt, write down what success means: a small
# set of research tasks with code graders, run the literature scout on them
# several times, and report how often it works, how often it works EVERY time
# (pass^k), and what one success really costs.
#
# OVERVIEW: An agent that worked once in a demo is an anecdote. Evaluation
# turns it into a measurement. The vocabulary below follows Harvard CS2680
# (Juncheng Yang), "Agent evaluation and benchmarks",
# https://cs2680.seas.harvard.edu/notes/agent-evaluation (credited as an
# inspiration; no text or figures were copied):
#   task        an initial state + an instruction + completion criteria (a grader)
#   trial       one run of the agent on one task
#   trajectory  everything the agent did in that trial (the episode)
#   outcome     the graded result
# The evaluation harness is SEPARATE from the agent harness: the agent never
# sees the answer key or the grader, and graders never see the agent's
# messages. Development tasks ("dev") are used while building; held-out tasks
# ("heldout") are used once, at the end (Activity 09).
#
# researchAgentR functions used here:
#   research_tasks         a small benchmark of research tasks shipped with the package
#   hash_answer()          store an answer key as a hash (for held-out tasks)
#   grader_exact()         the answer equals the key (case, spaces, final period ignored)
#   grader_contains()      the answer contains every pattern
#   grader_doi()           the DOIs in the answer match the expected DOIs
#   grader_code()          any R check you write (here: a live check of DOIs)
#   task_set()             define tasks: id, instruction, grader, answer, split
#   run_eval()             run every task several times, then grade separately
#   eval_summary()         success rate, pass@k, pass^k, cost per attempt and per success
#   pass_at_k(), pass_hat_k()  the unbiased estimators, from n trials with c successes
#
# TIME: about 45 minutes (the live evaluation itself takes longer on the free
# tier; it is recorded once with keep()).  COST: free tier (Groq, OpenAlex,
# Crossref). The list-price "shadow cost" of the evaluation is reported below.
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

use_cache("auto")      # see the note in Step 6 about caching and repeated trials
set_groq_key()         # GROQ_API_KEY in ~/.Renviron
if (nzchar(Sys.getenv("OPENALEX_API_KEY"))) set_openalex_key()


# ==============================================================================
# STEP 1: THE SHIPPED TASK SET
# ==============================================================================

data(research_tasks)
research_tasks
table(research_tasks$split)
research_tasks$instruction[1]
# EXPECTED OUTPUT: <task_set> 7 tasks (4 dev, 3 heldout), one row per task
# with its id, split, grader and answer. Dev answers are shown (shortened);
# held-out answers are shown as "(hashed)"; the NIH tasks have no fixed
# answer ("(none)") because a live grader checks them.

# During development, use ONLY the dev split. Looking at held-out results
# while tuning turns them into development data.
dev <- research_tasks[research_tasks$split == "dev", ]
# as.data.frame() first: printing a task_set needs all of its columns.
as.data.frame(dev)[, c("id", "type", "source")]
dev$answer
# EXPECTED OUTPUT: four dev tasks: doi_dynega and doi_entropy (DOI tasks),
# journal_entropy (journal name, exact match), nih_ema_2025 (an NIH project
# of fiscal year 2025, checked live in NIH RePORTER at grading time).
# Every fixed answer was read from the published article, not from a model.


# ==============================================================================
# STEP 2: WHY HELD-OUT ANSWERS ARE HASHES (offline)
# ==============================================================================

# A browsing agent can find an answer key that is written in plain text
# anywhere it can search (a package's source on GitHub, for example). So
# held-out answers are stored as MD5 hashes: the grader hashes the agent's
# answer the same way and compares hashes.
research_tasks$answer[research_tasks$split == "heldout"]
# EXPECTED OUTPUT: strings that start with "md5:" (and NULL for the NIH task).

# hash_answer() normalizes first, so equivalent forms give the same hash:
hash_answer(c("10.1080/00273171.2020.1779642",
              "https://doi.org/10.1080/00273171.2020.1779642"), type = "doi")
hash_answer("Multivariate Behavioral Research")
# EXPECTED OUTPUT: two identical "md5:..." strings for the two forms of the
# DOI, then one "md5:..." string for the journal name.

# A grader works with a hashed key exactly as with a plain one (demonstrated
# with a DEV answer; do not try to recover held-out answers).
hashed_task <- list(id = "demo", answer = hash_answer(dev$answer[[2]], type = "doi"))
grader_doi()("The DOI is doi:10.1080/00273171.2020.1779642.", hashed_task)$pass
# EXPECTED OUTPUT: [1] TRUE
# A hash hides the key from accidental exposure; short answers can still be
# guessed, so treat hashed keys as hidden, not secret.


# ==============================================================================
# STEP 3: GRADERS ON GIVEN ANSWERS (offline)
# ==============================================================================

# Every answer below was typed by hand to test the graders. No model is
# involved, so the results are exact. A grader returns pass, score (0 to 1),
# reason and cost.
entropy_task <- list(id = "doi_entropy", answer = dev$answer[[2]])
g_doi <- grader_doi()     # the grader the shipped DOI tasks use

g_doi("https://doi.org/10.1080/00273171.2020.1779642", entropy_task)[c("pass", "score", "reason")]
g_doi("I could not find this article.", entropy_task)[c("pass", "reason")]
# EXPECTED OUTPUT: pass TRUE, score 1, "recall 1.00 (1 of 1 expected DOIs),
# precision 1.00 (1 DOIs returned)"; then pass FALSE with recall 0.00.

# --- Graders can be gamed: an answer that lists MANY DOIs -----------------------
shotgun <- paste("10.1080/00273171.2020.1779642", "10.1007/s11336-021-09820-y",
                 "10.1234/not.a.real.doi", sep = "\n")
g_doi(shotgun, entropy_task)[c("pass", "reason")]
grader_doi(min_precision = 1)(shotgun, entropy_task)[c("pass", "reason")]
# EXPECTED OUTPUT: the default grader PASSES (recall 1.00, precision 0.33,
# 3 DOIs returned); the strict grader (min_precision = 1) FAILS the same
# answer. "Return THE DOI" is only measured by a grader that also checks
# precision. Decide this before running the agent, not after seeing scores.

# --- The answer FORMAT is part of the task ----------------------------------------
# The scout returns a structured answer (a list of records). Graders read it
# as JSON text. A DOI grader still finds the DOI inside; an exact-match
# grader does not.
structured <- list(records = list(list(
  title = "Entropy Fit Indices", year = 2020L, doi = "10.1080/00273171.2020.1779642",
  why_relevant = "Typed by hand for this test.")))
g_doi(structured, entropy_task)$pass
g_exact <- grader_exact()
g_exact(" multivariate behavioral research. ", list(answer = dev$answer[[3]]))$pass
g_exact(structured, list(answer = dev$answer[[3]]))$reason
# EXPECTED OUTPUT: TRUE (the DOI is inside the JSON); TRUE (case, spaces and
# the final period are ignored); then "no exact match; got '{ "records": ...'":
# a structured answer can never pass an exact-match task, whatever it contains.

# --- Contains: several patterns, partial credit in the score ---------------------
g_contains <- grader_contains(c("exploratory graph analysis", "2017"))
g_contains("Golino & Epskamp (2017). Exploratory graph analysis: ...", list())[c("pass", "score")]
g_contains("Exploratory Graph Analysis (2016)", list())[c("pass", "score", "reason")]
# EXPECTED OUTPUT: pass TRUE, score 1; then pass FALSE, score 0.5,
# reason "missing: 2017".


# ==============================================================================
# STEP 4: YOUR TASK SET (dev tasks + two tasks of your own)
# ==============================================================================

# Task A: graded with grader_contains(). The 2017 article that introduced EGA
# (Golino and Epskamp, PLoS ONE) has "Exploratory graph analysis" in its title.
# Task B: graded with a LIVE code grader. The answer must contain at least
# three DOIs that exist, whose registered title contains "exploratory graph
# analysis" and whose registered year is between 2018 and 2023. The grader
# calls Crossref at grading time; if the check itself fails (network, rate
# limit), the trial is UNGRADED (pass = NA), never counted as an agent failure.
dois_in <- function(answer) {
  text <- if (is.character(answer)) paste(answer, collapse = "\n") else
    as.character(jsonlite::toJSON(answer, auto_unbox = TRUE))
  hits <- regmatches(text, gregexpr("10\\.[0-9]{4,9}/[^[:space:]\"',;<>]+", text))[[1]]
  unique(stats::na.omit(normalize_doi(hits)))
}

grader_ega_dois <- grader_code(function(answer, task) {
  dois <- dois_in(answer)
  if (length(dois) < 3) {
    return(list(pass = FALSE, score = length(dois) / 3,
                reason = paste("only", length(dois), "DOI(s) in the answer")))
  }
  chk <- tryCatch(verify_citations(dois, check_retraction = FALSE), error = function(e) NULL)
  if (is.null(chk) || any(chk$status == "error")) {
    return(list(pass = NA, reason = "the DOIs could not be checked (network or rate limit)"))
  }
  ok <- chk$found %in% TRUE &
    grepl("exploratory graph analysis", chk$matched_title, ignore.case = TRUE) &
    chk$matched_year %in% 2018:2023
  list(pass = sum(ok) >= 3, score = min(1, sum(ok) / 3),
       reason = sprintf("%d of %d DOIs exist with a matching title and a year in 2018-2023",
                        sum(ok), length(dois)))
}, label = "live EGA DOI check")

# The format checks of the live grader run without the network:
grader_ega_dois("No DOIs here.", list())[c("pass", "reason")]
# EXPECTED OUTPUT: pass FALSE, reason "only 0 DOI(s) in the answer".

tasks <- task_set(
  id = c(dev$id, "ega_origin", "ega_three_articles"),
  instruction = c(
    dev$instruction,
    "Find the 2017 article by Golino and Epskamp that introduced exploratory graph analysis for estimating the number of dimensions. Return its title, year and DOI.",
    "Find three peer-reviewed journal articles published between 2018 and 2023 whose titles contain the words 'exploratory graph analysis'. Return their titles, years and DOIs."),
  grader = c(dev$grader, list(grader_contains(c("exploratory graph analysis", "2017")),
                              grader_ega_dois)),
  answer = c(dev$answer, list(NULL, NULL)),
  split = "dev",
  type = c(dev$type, "title_year", "live_doi_check"))
tasks
# EXPECTED OUTPUT: <task_set> 6 tasks (6 dev, 0 heldout) with their graders:
# "doi (recall >= 1)" (twice), "exact", "live NIH RePORTER check (FY2025)",
# "contains" and "live EGA DOI check".

# Success criteria and budget are now FIXED. Write the date in your notes.
eval_budget <- budget(max_steps = 8, max_tokens = 40000, max_cost = 0.05,
                      max_seconds = 240, max_tool_errors = 3)


# ==============================================================================
# STEP 5: THE SCOUT FROM ACTIVITY 03 (rebuilt, so this script stands alone)
# ==============================================================================

scout <- agent(
  name = "literature_scout",
  description = "Finds and verifies key literature on a research topic.",
  instructions = paste(
    "You are a careful literature scout for a researcher in psychology.",
    "1. Search OpenAlex first, then Crossref with different keywords.",
    "   Use Europe PMC only for health topics and arXiv only for methods preprints.",
    "2. Snowball (references or citations) from the one or two most relevant papers.",
    "3. Only report works that appeared in a tool result. Never write a DOI from memory.",
    "4. Before the final answer, call verify_citations on every DOI you plan to report,",
    "   with its title, first author and year. Drop every work that is not VERIFIED.",
    "5. Prefer peer-reviewed articles; say so in why_relevant when a work is a preprint."),
  model = "groq:openai/gpt-oss-120b",
  tools = list(tool_openalex(n = 5), tool_crossref(n = 5),
               tool_europepmc(n = 5), tool_arxiv(n = 5), tool_snowball(n = 5),
               tool_unpaywall(), tool_verify_citations()),
  output_schema = list(
    records = arg_array(
      "The relevant works found, most relevant first (at most 8).",
      items = arg_object("One work.", fields = list(
        title        = arg_string("Title exactly as in the search results."),
        year         = arg_integer("Publication year."),
        doi          = arg_string("DOI exactly as in the search results, in the form 10.xxxx/yyyy."),
        why_relevant = arg_string("One sentence: why this work matters for the topic.")))),
    search_summary = arg_string("Which databases and queries were used, and how many results each gave.")),
  effort = "low")
scout


# ==============================================================================
# STEP 6: RUN THE EVALUATION (3 trials per task)
# ==============================================================================

# CACHING AND REPEATED TRIALS: with use_cache("auto"), trial 2 sends exactly
# the same first request as trial 1, so it would REPLAY trial 1 from the cache
# (same answer, cost 0). That would make every trial identical and pass^k
# meaningless. So the cache is switched off while the trials run, and the
# whole result is kept with keep(): the live evaluation runs once, and every
# later run (and the class demo) loads the recorded results.
res <- keep("act04_eval_scout_dev", {
  old_mode <- use_cache("off")
  out <- run_eval(scout, tasks, trials = 3, budget = eval_budget,
                  save_dir = "act04_episodes")
  use_cache(old_mode)
  out
})
res
# EXPECTED OUTPUT: <eval_results> 18 trials of 6 tasks, the counts of passed,
# failed and ungraded trials, errors and timeouts, and the first 10 rows
# (task_id, trial, pass, score, cost, seconds, stop_reason).

summary_scout <- eval_summary(res, k = c(1, 3))
summary_scout
# EXPECTED OUTPUT: one row for the scout with tasks (6), trials (18),
# success_rate, pass@1, pass^1, pass@3, pass^3, cost_per_attempt,
# cost_per_success, p50_seconds, p95_seconds, errors, timeouts, ungraded and
# grading_cost.

# Scores are not enough: read WHY trials failed. (as.data.frame() first: the
# print method of eval_results shows a fixed set of columns, without `reason`.)
as.data.frame(res)[!(res$pass %in% TRUE), c("task_id", "trial", "pass", "reason", "stop_reason")]
# EXPECTED OUTPUT: the failed and ungraded trials with the grader's reason.
# The journal task fails in every trial by construction (a structured answer
# cannot match exactly, Step 3), and the NIH task is expected to fail (the
# scout has no NIH tool) or to be ungraded (if NIH RePORTER cannot be reached
# at grading time): failures of task-agent FIT, not of search skill.

# The trajectories (every tool call) of all trials, on one HTML page:
view_episodes("act04_episodes")


# ==============================================================================
# STEP 7: pass@k AND pass^k
# ==============================================================================

# From n trials of a task with c successes:
#   pass@k = 1 - C(n - c, k) / C(n, k)   at least one of k tries succeeds
#   pass^k = C(c, k) / C(n, k)           ALL k tries succeed
# pass@k suits a person who looks at k tries and keeps the best one.
# pass^k tells a researcher how often the agent works EVERY time, which is what
# a pipeline that runs unattended (screening 500 abstracts, say) depends on.

# Worked example: n = 5 trials, c = 3 successes, k = 2.
n <- 5; c_ok <- 3; k <- 2
choose(n, k)                                   # 10 possible pairs of trials
choose(n - c_ok, k)                            # 1 pair with two failures
choose(c_ok, k)                                # 3 pairs with two successes
1 - choose(n - c_ok, k) / choose(n, k)         # pass@2 = 1 - 1/10
choose(c_ok, k) / choose(n, k)                 # pass^2 = 3/10
pass_at_k(n = 5, c = 3, k = 2)
pass_hat_k(n = 5, c = 3, k = 2)
# EXPECTED OUTPUT: 10, 1, 3, then 0.9, 0.3, and again [1] 0.9 and [1] 0.3.
# The same agent "works 90% of the time" (pass@2) and "works twice in a row
# 30% of the time" (pass^2). Both are true; they answer different questions.

round(data.frame(k = 1:5, pass_at_k = pass_at_k(5, 3, 1:5), pass_hat_k = pass_hat_k(5, 3, 1:5)), 2)
# EXPECTED OUTPUT: at k = 1 both equal 0.6 (c/n); as k grows, pass@k rises to
# 1 and pass^k falls to 0 (0.6, 0.9, 1, 1, 1 versus 0.6, 0.3, 0.1, 0, 0).

# The same numbers per task, from the evaluation (graded trials only):
graded <- res[!is.na(res$pass), ]
per_task <- data.frame(n = as.vector(table(graded$task_id)),
                       c = as.vector(tapply(graded$pass, graded$task_id, sum)))
per_task$task_id <- names(table(graded$task_id))
per_task$pass_at_3 <- pass_at_k(per_task$n, per_task$c, 3)
per_task$pass_hat_3 <- pass_hat_k(per_task$n, per_task$c, 3)
per_task
# EXPECTED OUTPUT: one row per task with n (graded trials), c (successes),
# pass@3 and pass^3. A task with c = n has pass^3 = 1; a task that passed in
# some trials but not all has pass@3 = 1 and pass^3 = 0: the agent CAN do it
# but does not do it reliably. eval_summary() averages these over tasks.


# ==============================================================================
# STEP 8: COST PER SUCCESS VERSUS COST PER ATTEMPT
# ==============================================================================

# Cost is measured per complete task episode, and failed attempts count.
#   cost per attempt = total cost / number of trials
#   cost per success = total cost / number of SUCCESSFUL trials
# Worked example with made-up numbers (not model output): 12 trials at $0.002
# each cost $0.024. With 6 successes, the cost per attempt is $0.002 but each
# success cost $0.004, because the 6 failures were paid for too.
total_cost <- 12 * 0.002
c(per_attempt = total_cost / 12, per_success = total_cost / 6)

# The same arithmetic on the real results:
c(trials = nrow(res), successes = sum(res$pass %in% TRUE), total_cost = sum(res$cost),
  per_attempt = sum(res$cost) / nrow(res),
  per_success = sum(res$cost) / sum(res$pass %in% TRUE))
summary_scout[, c("cost_per_attempt", "cost_per_success", "grading_cost")]
# EXPECTED OUTPUT: your hand computation equals eval_summary()'s columns.
# cost_per_success is always at least cost_per_attempt; the gap grows as the
# success rate falls. Tasks the agent never solves (here the journal and NIH
# tasks) only add cost. grading_cost (the live Crossref checks are free; an
# LLM judge would not be) is reported apart from the agent's runtime cost.
# Costs are list prices ("shadow cost") even on the free tier, so runs can be
# compared across providers.

# Cost per task: where does the money go?
aggregate(cbind(cost, seconds) ~ task_id, data = res, FUN = mean)


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Regrade WITHOUT rerunning the agent: graders are separate, so stored
#    answers can be graded again with a stricter grader (see Exercise 1).
# 2. Change k: eval_summary(res, k = 1:3). Which tasks drive pass^3 down?
# 3. Open one failed trajectory in act04_episodes/ with view_episode() and
#    find the step where the trial went wrong.


# ==============================================================================
# EXERCISES (solutions in solutions/04_solutions.R)
# ==============================================================================

# Exercise 1 (no new model calls). Regrade the stored answers of the two DOI
#   tasks with grader_doi(min_precision = 1). How many trials that passed
#   before now fail?
#
# Exercise 2. Build `scout_text`, the same scout WITHOUT output_schema and
#   with the instruction "Answer in exactly the format the task asks for".
#   Evaluate it on the same tasks (3 trials, same budget, cache off inside
#   keep()) and compare eval_summary() of both agents. Which tasks changed?
#
# Exercise 3 (offline). Add a task of your own to `tasks` whose answer is
#   stored with hash_answer() (use a DOI you checked yourself in the article),
#   and show that its grader passes a correct answer and fails a wrong one.
#
# Exercise 4 (offline). For n = 3 trials, tabulate pass@k and pass^k for
#   k = 1, 2, 3 and c = 0, 1, 2, 3. At which c does pass^3 first exceed 0?


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - Identical trials: with use_cache("auto") ON during run_eval(), trials 2
#   and 3 replay trial 1 (same answers, cost 0), so pass^k equals pass@k and
#   cost per success is too low. Switch the cache off inside keep() (Step 6).
# - Deleting the kept results and rerunning with the cache on gives cost 0
#   for every replayed call: never report costs from a replay.
# - Ungraded trials (pass = NA): the live graders (NIH RePORTER, Crossref)
#   could not reach their API at grading time. They are reported in
#   `ungraded`, not as failures; regrade them later.
# - The format trap: a correct answer in the wrong format fails an exact-match
#   grader (Step 3). Decide whether format is part of the task, and write the
#   instruction and the grader to agree.
# - Grader gaming: a lenient grader (DOI recall only) rewards listing many
#   DOIs. Look for answers with many DOIs in res$answer.
# - Contamination: the answer key reaches the model. The scout's schema
#   deliberately describes a DOI only by its form ("10.xxxx/yyyy"), because an
#   example DOI in a tool description is read by the model on every step. A
#   browsing agent can also find a key published online.
# - Free-tier limits: 18 episodes with several model calls each can hit the
#   Groq per-minute and per-day token limits (429). The client retries; if
#   trials end with "api_error: ..." in stop_reason, they count as failures
#   with their real cost. Run the evaluation outside class time, once.
# - OpenAlex 429 when the daily budget is spent (every scout search costs
#   $0.001; 18 episodes with several searches each are still well under $1).
# - Small samples: 6 tasks x 3 trials is a smoke test, not a benchmark. The
#   uncertainty of a difference between agents needs paired comparisons
#   (paired_bootstrap(), Activity 09).


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. Which number would you report for an agent that screens abstracts
#    unattended: pass@3 or pass^3? And for an agent whose three drafts a
#    person reads before choosing one?
# 2. The success criteria and the budget were fixed BEFORE the first run.
#    What changes if you tune the agent until the dev score looks good and
#    then report that score?
# 3. Two dev tasks fail by design (journal format, NIH task without an NIH
#    tool). Should they stay in the task set? What do they tell you that the
#    passing tasks do not?
# 4. Cost per success versus cost per attempt: a cheap agent that rarely
#    succeeds can be more expensive per success than a strong agent. Where is
#    the break-even point for your own tasks?
# 5. Held-out answers are hashed, but you already met some of these papers in
#    Activity 03. Is a human developer a contamination risk too?
# 6. Inspect trajectories, not only scores: did any trial pass for the wrong
#    reason (for example, a DOI that appears in the answer without ever
#    appearing in a tool result)?
