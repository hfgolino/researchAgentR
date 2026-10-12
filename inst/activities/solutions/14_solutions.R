# Solutions: Activity 14, Capstone Team Brief
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Exercise 1 runs offline. Exercises 2 to 5 need the objects of Activity 14
# (all_eps, worker_eps, lead_ep, capstone_team, funding_scout, the four
# workers, TEAM_TASK, TEAM_BUDGET, WORKER_BUDGET, statement, team_total).

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")


# ==============================================================================
# EXERCISE 1: check_claims() step 1 (quote in source?) needs no model
# ==============================================================================

# The source: the capstone rubric that ships with the package (real text)
rubric_text <- paste(read_text(course_template("capstone_rubric")), collapse = " ")
rubric_claims <- data.frame(
  claim = c("The capstone rubric has a maximum of 24 points.",
            "The capstone rubric has a maximum of 30 points.",
            "Each criterion is scored on a four-point scale from 0 to 3."),
  quote = c("Maximum: 24 points",
            "Maximum: 30 points",                                   # changed number
            "Score each criterion 0 to 3. ... 3 | Complete, verified and reproducible"),
  source = "rubric")
check_claims(rubric_claims, sources = c(rubric = rubric_text))
# Row 1: quote_found. Row 2: quote_not_found (the number was changed). Row 3:
# quote_found (each fragment around "..." appears, in order). Without a model,
# a found quote is only "quote_found": whether it SUPPORTS the claim is step 2
# (model = "groq:openai/gpt-oss-20b"), which costs one short call per claim.


# ==============================================================================
# EXERCISE 2: the facts the verification checklist asks for (needs the run)
# ==============================================================================

checklist_facts <- do.call(rbind, lapply(all_eps, function(e) {
  tool_steps <- Filter(function(s) identical(s$type, "tool"), e$steps)
  data.frame(
    agent = e$agent,
    stop_reason = e$stop_reason,
    tool_calls = length(tool_steps),
    tool_errors = sum(vapply(tool_steps, function(s) isTRUE(s$is_error), logical(1))),
    truncated_outputs = sum(vapply(tool_steps, function(s) isTRUE(s$truncated), logical(1))),
    max_steps = e$budget$max_steps, max_cost = e$budget$max_cost,
    stringsAsFactors = FALSE)
}))
checklist_facts
# Any stop reason other than final_answer, any tool error the answer relies
# on, and any truncated output must be read in the trajectory (STEP 5) and
# noted in the checklist.


# ==============================================================================
# EXERCISE 3: the team against an equal-budget single agent (LIVE)
# ==============================================================================

solo <- agent(
  "solo_brief",
  instructions = paste(capstone_team$instructions,
                       "You have no workers: use the tools yourself."),
  model = "groq:openai/gpt-oss-120b",
  tools = c(literature_scout$tools, measurement_finder$tools, design_advisor$tools,
            funding_scout$tools),
  output_schema = capstone_team$output_schema)
cmp <- keep("capstone14_ex3_single_vs_team",
            compare_single_vs_team(solo, capstone_team, TEAM_TASK, TEAM_BUDGET))
cmp[, c("arm", "stop_reason", "model_calls", "tool_calls", "tokens", "peak_context", "cost", "seconds")]
# The team's row includes its workers' costs. Score both briefs with the
# rubric before deciding: a cheaper brief with unverifiable claims is not
# a better brief. (With 10 tools in one schema, the solo agent's every call
# is large: watch peak_context against the free tier's tokens per minute.)


# ==============================================================================
# EXERCISE 4: evaluate the funding scout on the NIH tasks (LIVE)
# ==============================================================================

data(research_tasks)
nih_dev <- research_tasks[research_tasks$id == "nih_ema_2025", ]
nih_heldout <- research_tasks[research_tasks$id == "nih_irt_2025", ]

eval_once <- function(name, tasks, trials) keep(name, (function() {
  old <- use_cache("off")                  # independent trials (see Activity 09)
  on.exit(use_cache(old))
  run_eval(funding_scout, tasks, trials = trials, budget = WORKER_BUDGET,
           save_dir = file.path("capstone", "episodes", name), name = "funding scout")
})())
res_dev <- eval_once("capstone14_ex4_dev", nih_dev, trials = 3)
eval_summary(res_dev, k = c(1, 3))
# Decide whether the scout is good enough BEFORE the held-out run, then run it once.
res_heldout <- eval_once("capstone14_ex4_heldout", nih_heldout, trials = 3)
eval_summary(res_heldout, k = c(1, 3))
# The grader reads the project numbers from the scout's structured answer
# (as JSON text) and checks them live in NIH RePORTER; pass = NA means the
# grader could not reach the API, not that the scout failed.


# ==============================================================================
# EXERCISE 5: the corrected cost sentence (needs the run)
# ==============================================================================

corrected <- sub("The estimated cost at list price was [^ ]+ US dollars",
                 sprintf("The estimated cost at list price was %.5f US dollars", team_total),
                 unclass(statement))
cat(strwrap(corrected, width = 78), sep = "\n")
c(statement_said = attr(statement, "facts")$cost, ledger_says = team_total)
# The difference is the workers' cost, counted twice by the draft. Write the
# corrected statement into brief.md (STEP 11) and into capstone/appendix_ai_use.md.
