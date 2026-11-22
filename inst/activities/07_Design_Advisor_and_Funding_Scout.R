# Activity 07: Design Advisor and Funding Scout
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: Build two research agents that compute and look things up instead of
# guessing: a research design advisor that runs its power analysis in R, and a
# funding scout that searches NIH RePORTER, NSF awards and Grants.gov.
#
# OVERVIEW: Part A starts with the sandboxed R-code runner on its own (what it
# protects and what it does NOT protect), turns it into a tool, and builds a
# design advisor that (1) asks clarifying questions first, (2) proposes
# designs, (3) runs a power analysis with the run_r tool, (4) lists threats to
# validity and (5) drafts a preregistration outline as a STRUCTURED answer.
# Part B calls the three funding APIs directly, then gives them to a funding
# scout that matches open opportunities to a researcher profile and finds
# funded projects similar to a draft aim. Every ID the agent reports is then
# checked in code against what the tools actually returned.
#
# researchAgentR functions used here:
#   run_r_code()              run R code in a separate, time-limited R process
#   tool_run_r()              the same, as a tool an agent can call
#   search_nih_projects()     funded NIH projects (NIH RePORTER API v2, no key)
#   search_nsf_awards()       funded NSF awards (NSF Award Search API, no key)
#   search_grants_gov()       open funding opportunities (Grants.gov, no key)
#   grants_gov_opportunity()  the full synopsis of one Grants.gov opportunity
#   tool_nih_reporter(), tool_nsf_awards(), tool_grants_gov()   agent tools
#   (also: agent(), run(), budget(), call_tool(), tool_schema(), arg_*(),
#    episode_steps(), view_episode(), keep())
# TIME: about 75 minutes.  COST: free tier (Groq); the funding APIs are free.
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

# Keys live in ~/.Renviron (see Activity 00), never in scripts.
# NIH RePORTER, the NSF Award Search API and Grants.gov need no key.
set_groq_key()

# Save every API response the first time; later runs (and the class demo)
# replay them offline at no cost.
use_cache("auto")

# Tool results are stored in the episode log up to this many characters.
# The funding tools return long lists, and Part B checks IDs against the log.
options(researchAgentR.log_chars = 20000)

# Is the pwr package installed? (Optional. If not, the agent uses
# stats::power.t.test(), which ships with R and gives the same answer for a
# two-group t-test.)
has_pwr <- requireNamespace("pwr", quietly = TRUE)
cat("pwr installed:", has_pwr, "\n")


# ==============================================================================
# PART A, STEP 1: RUN R CODE IN A SEPARATE PROCESS
# ==============================================================================

# An agent that "computes" a sample size in its head is guessing. An agent
# that writes R code and RUNS it is computing. But code written by a model
# must never run inside your own R session: one rm(list = ls()) or an
# infinite loop and your work is gone. run_r_code() starts a NEW R process
# (with the callr package), runs the code there with a time limit and a
# temporary working folder, and returns what it printed.

r <- run_r_code("x <- rnorm(100, mean = 10); mean(x) > 9")
r
# EXPECTED OUTPUT: "<r_code_result> ok | ... s", then "Output:" and the printed
# value ([1] TRUE).

str(unclass(r), max.level = 1)
# EXPECTED OUTPUT: a list with code, output, warnings, messages, error (NA
# when the code ran), value (a str() summary of the last value), files, dir,
# seconds and timed_out.

# A power analysis, exactly as an agent would run it:
power_code <- if (has_pwr) {
  "pwr::pwr.t.test(d = 0.4, power = 0.80, sig.level = 0.05, type = 'two.sample')"
} else {
  "power.t.test(delta = 0.4, sd = 1, power = 0.80, sig.level = 0.05)"
}
r_power <- run_r_code(power_code)
r_power
# EXPECTED OUTPUT: a power calculation for a two-sample t-test with n (the
# sample size PER GROUP), d or delta, sig.level and power: n = 99.08032 with
# pwr, n = 99.08057 with power.t.test().

# Verify: the same calculation in your own session gives the same n up to
# rounding (pwr and power.t.test() differ in the fourth decimal).
power.t.test(delta = 0.4, sd = 1, power = 0.80, sig.level = 0.05)$n


# ==============================================================================
# PART A, STEP 2: WHAT THE SANDBOX PROTECTS, AND WHAT IT DOES NOT
# ==============================================================================

# (a) Time limit: an infinite loop ends with the child process.
r_slow <- run_r_code("Sys.sleep(10)", timeout = 2)
r_slow$timed_out
r_slow$error
# EXPECTED OUTPUT: TRUE, and "the code did not finish within 2 seconds".

# (b) Errors and warnings are captured, never thrown into your session.
r_err <- run_r_code("y <- log(-1)\nstop('a deliberate error')")
r_err
# EXPECTED OUTPUT: status "error", a "Warnings:" block (NaNs produced) and
# "Error: a deliberate error".

# (c) Keys are hidden: variables whose names contain KEY, TOKEN, SECRET,
# PASSWORD or CREDENTIAL are emptied in the child, and ~/.Renviron is not read.
Sys.setenv(DEMO_FAKE_API_KEY = "not-a-real-key")
run_r_code("nchar(Sys.getenv('DEMO_FAKE_API_KEY'))")$output
Sys.unsetenv("DEMO_FAKE_API_KEY")
# EXPECTED OUTPUT: "[1] 0" (the child sees an empty value).

# (d) No network (soft): proxy variables point to an address where nothing
# listens, so ordinary web requests fail.
run_r_code("readLines('https://cran.r-project.org')")$error
# EXPECTED OUTPUT: an error message about a failed connection or proxy, for
# example "cannot open the connection to 'https://cran.r-project.org'".

# (e) THE LIMIT: this is NOT a security boundary. The child runs as YOUR user
# account, so it can still see (and read) your files, including ~/.Renviron.
run_r_code("file.exists(path.expand('~/.Renviron'))")$output
# EXPECTED OUTPUT: "[1] TRUE" if you keep your keys in ~/.Renviron. The code
# above only checks that the file exists; it does not print it. Code that runs
# system() or opens raw sockets can also escape the proxy trick. Read
# model-written code before it runs when the stakes are high (approval = TRUE
# below), never give a code-running agent confidential data, and use a
# container or virtual machine for untrusted code.

# (f) Files written by the code stay in its working folder.
r_plot <- run_r_code("png('hist.png'); hist(rnorm(500)); dev.off()")
basename(r_plot$files)
# EXPECTED OUTPUT: "hist.png".


# ==============================================================================
# PART A, STEP 3: THE SAME RUNNER AS AN AGENT TOOL
# ==============================================================================

r_tool <- tool_run_r(packages = if (has_pwr) "pwr", timeout = 60)
r_tool
tool_schema(r_tool)
# EXPECTED OUTPUT: a tool named run_r with ONE argument, code (string). The
# description tells the model to print every result, that objects do not
# persist between calls but files do, and that there is no internet access.

# Call the tool by hand, exactly as the agent loop will:
res <- call_tool("run_r", list(r_tool), arguments = list(code = power_code))
cat(res$output)
res$is_error
# EXPECTED OUTPUT: the same power calculation as in STEP 1, and FALSE.

# A failing call is an OBSERVATION for the model, not a crash:
res_bad <- call_tool("run_r", list(r_tool), arguments = list(code = "pwr.t.test(d = )"))
cat(res_bad$output)
res_bad$is_error
# EXPECTED OUTPUT: "Error while running run_r: the code failed." followed by
# the R error, and TRUE. The model reads this and can fix its code.


# ==============================================================================
# PART A, STEP 4: A DESIGN ADVISOR WITH A STRUCTURED ANSWER
# ==============================================================================

# The final answer follows a schema (checked in R), so the result is data you
# can print, save and verify, not a wall of text. Every field is required;
# fields that do not apply yet are left empty ("" or []).
design_schema <- list(
  status = arg_enum("'needs_clarification' if key information is missing, otherwise 'complete'",
                    values = c("needs_clarification", "complete")),
  clarifying_questions = arg_array("At most 4 questions for the researcher (empty when complete)",
                                   items = arg_string("One question")),
  candidate_designs = arg_array("Two or three candidate designs (empty while clarifying)",
    items = arg_object("One design", fields = list(
      name = arg_string("Short name, for example 'two-arm randomized experiment'"),
      description = arg_string("Two or three sentences"),
      main_tradeoff = arg_string("The main advantage and the main cost")))),
  recommended_design = arg_string("Name of the recommended design (empty while clarifying)"),
  power_analysis = arg_object("The power analysis actually run with the run_r tool", fields = list(
    r_code = arg_string("The exact R code that was run (empty while clarifying)"),
    result = arg_string("The sample size copied from the run_r output, saying per group or total"),
    assumptions = arg_string("Test, effect size and its source, alpha, power"))),
  threats_to_validity = arg_array("Threats to the validity of the recommended design",
    items = arg_object("One threat", fields = list(
      threat = arg_string("The threat, in one sentence"),
      type = arg_enum("Kind of validity", values = c("internal", "external", "construct",
                                                     "statistical conclusion")),
      mitigation = arg_string("How the design reduces it")))),
  preregistration = arg_object("A preregistration outline (OSF-style headings)", fields = list(
    hypotheses = arg_array("One testable sentence per hypothesis", items = arg_string("A hypothesis")),
    design = arg_string("Design, conditions and assignment"),
    sampling_plan = arg_string("Planned sample size, its justification and the stopping rule"),
    variables = arg_string("Manipulated and measured variables"),
    analysis_plan = arg_string("The statistical model for each hypothesis"),
    inference_criteria = arg_string("Alpha, one- or two-sided tests, corrections"),
    exclusions = arg_string("Data exclusion rules")))
)

advisor <- agent(
  name = "design_advisor",
  model = "groq:openai/gpt-oss-120b",
  description = "Proposes study designs, runs power analyses in R and drafts preregistrations.",
  instructions = paste(
    "You are a research design advisor for social and behavioral scientists.",
    "FIRST decide whether you have enough information: the research question, the outcome",
    "and how it is measured, the population, practical constraints (time, budget, access),",
    "and an expected effect size or a smallest effect size of interest. If any of these is",
    "missing, set status to 'needs_clarification', ask at most 4 short questions, leave the",
    "other fields empty, and do not call any tool.",
    "When you have enough information: propose two or three designs, recommend one, and run",
    "the power analysis with the run_r tool. NEVER compute a sample size in your head: copy",
    "it from the run_r output. Use the pwr package if it is attached, otherwise",
    "stats::power.t.test() or stats::power.prop.test(). Then list threats to validity and",
    "draft the preregistration outline. Never invent citations; if an effect size comes",
    "from the researcher, say so."),
  tools = list(r_tool),
  output_schema = design_schema
)
advisor

# --- Run 1: the advisor asks questions ----------------------------------------
# run() executes ONE episode and ends with a final answer: there is no
# "wait for the user" inside the loop. So the pattern is two runs: the first
# run returns questions (as data), the researcher answers, and the second run
# receives the original request plus the questions and answers. Each run is
# logged separately, which keeps the whole exchange documented.

request <- paste(
  "I want to test whether a brief self-compassion writing exercise reduces exam anxiety",
  "in undergraduate students.")

ep1 <- run(advisor, request, budget = budget(max_steps = 4, max_cost = 0.02))
ep1
ep1$answer$status
ep1$answer$clarifying_questions
# EXPECTED OUTPUT: stop reason final_answer, status "needs_clarification",
# and two to four questions (for example about the outcome measure, the
# comparison condition, the expected effect size or the sample available).
# Recorded run (November 22, 2026): final_answer after 4 model calls and 4
# tool calls, 3 of them "(invalid tool call)" errors before the model called
# final_answer correctly; 4 questions; cost $0.00153.

# --- Run 2: the researcher answers --------------------------------------------
# Answers come from your notes, not from the model. Edit them for your study.
my_answers <- c(
  "Outcome: the Westside Test Anxiety Scale, completed one day before the final exam.",
  "Comparison: a neutral writing exercise of the same length (active control).",
  "Population: undergraduates in introductory psychology at one university; about 400 available per semester.",
  "Effect size: plan for a smallest effect size of interest of d = 0.30.",
  "Alpha 0.05, two-sided; power 0.80. One semester of data collection.")

questions <- unlist(ep1$answer$clarifying_questions)
task2 <- paste0(
  "Original request:\n", request,
  "\n\nYour clarifying questions:\n", paste0("- ", questions, collapse = "\n"),
  "\n\nThe researcher's answers:\n", paste0("- ", my_answers, collapse = "\n"),
  "\n\nNow complete the design advice. Do not ask further questions.")

ep2 <- run(advisor, task2, budget = budget(max_steps = 8, max_cost = 0.05))
ep2
# EXPECTED OUTPUT: stop reason final_answer after one or more run_r calls;
# status "complete".
# Recorded run (November 22, 2026): final_answer after 3 model calls and 3
# tool calls (run_r, one invalid tool call, final_answer); cost $0.00224.

advice <- ep2$answer
cat("Recommended design:", advice$recommended_design, "\n")
str(advice$power_analysis)
# EXPECTED OUTPUT: the R code the agent ran, the sample size it copied from
# the output, and its assumptions (d = 0.30, alpha = .05, power = .80).
# Recorded run (November 22, 2026): "two-arm randomized experiment"; the code
# was a pwr.t.test() call with d = 0.30, and the result field said 176
# participants per group (352 in total).

# Structured fields become data frames in one line each:
designs <- do.call(rbind, lapply(advice$candidate_designs, as.data.frame))
threats <- do.call(rbind, lapply(advice$threats_to_validity, as.data.frame))
designs
threats
# EXPECTED OUTPUT: one row per design (name, description, main_tradeoff) and
# one row per threat (threat, type, mitigation).
# Recorded run (November 22, 2026): 3 designs (two-arm randomized, crossover,
# three-arm randomized) and 4 threats, one of each type.

str(advice$preregistration, nchar.max = 120)
# EXPECTED OUTPUT: hypotheses (a list of sentences) and six text fields.
# Recorded run (November 22, 2026): List of 7; hypotheses held one entry
# ("H1: ...") and str() showed it as chr, like the six text fields.

# --- Verify the computation yourself --------------------------------------------
# The trajectory shows every run_r call and whether it failed:
steps <- episode_steps(ep2)
steps[steps$type == "tool", c("step", "name", "is_error", "detail")]
# EXPECTED OUTPUT: one row per tool step: every run_r call with the start of
# its output, any "(invalid tool call)" step (is_error TRUE) and the
# final_answer step.
# Recorded run (November 22, 2026): 3 rows: run_r (output with n = 175.3847),
# one invalid tool call (the model called a tool named 'answer', which is not
# one of its tools) and final_answer.

# Rerun the agent's own code in a fresh sandbox and compare with its claim:
check <- run_r_code(advice$power_analysis$r_code, packages = if (has_pwr) "pwr")
check
cat("Agent's claim:", advice$power_analysis$result, "\n")
# EXPECTED OUTPUT: the same n that the agent reported. If they differ, the
# agent misread its own output: that is an error to report, not to fix by hand.
# Recorded run (November 22, 2026): the rerun printed n = 175.3847; the
# agent's claim was 176 participants per group, the same n rounded up.

# Open the full trajectory (every message, tool call, token and cost):
# view_episode(ep2)


# ==============================================================================
# PART B, STEP 5: THE FUNDING APIS, CALLED DIRECTLY
# ==============================================================================

# NIH RePORTER etiquette (https://api.reporter.nih.gov/): no more than ONE
# request per second (the package enforces it), large jobs only on weekends
# or between 9 PM and 5 AM Eastern time, at most 500 records per request and
# offsets up to 14,999. NIH may block addresses that ignore these rules. With
# 30 students in one class, keep n small and rely on the cache.

nih <- search_nih_projects("ecological momentary assessment loneliness",
                           fiscal_years = 2024:2025, n = 10)
attr(nih, "total")
nih[, c("project_num", "fiscal_year", "agency", "award_amount", "title")]
# EXPECTED OUTPUT: the total number of matching projects, then up to 10 rows
# with project number, fiscal year, administering institute (for example
# NIA or NIMH), award amount in US dollars and title.
# Recorded run (November 22, 2026): total 58; 10 rows, all of fiscal year
# 2025, from NIMH, NIA, NIDCD and NIAAA.

nsf <- search_nsf_awards("psychometrics machine learning", n = 10, date_start = "2024-01-01")
attr(nsf, "total")
nsf[, c("id", "start_date", "amount", "title")]
# EXPECTED OUTPUT: up to 10 NSF awards that started on or after 2024-01-01,
# with award id, start date, funds obligated and title.
# Recorded run (November 22, 2026): total 10000; the 10 rows had start dates
# from 03/01/2027 to 04/01/2027.

grants <- search_grants_gov("mental health measurement", n = 10)
attr(grants, "total")
grants[, c("id", "number", "agency_code", "close_date", "title")]
# EXPECTED OUTPUT: currently posted opportunities with Grants.gov id, funding
# opportunity number, agency code, closing date (NA when not given) and title.
# Recorded run (November 22, 2026): total 397; 10 rows, 8 of them with agency
# code HHS-NIH11; every row had a closing date.

# The full synopsis of the first one (eligibility, award size, description):
if (nrow(grants)) {
  opp <- grants_gov_opportunity(grants$id[1])
  print(opp[, c("number", "agency", "posting_date", "response_date", "award_floor", "award_ceiling")])
  cat(substr(opp$description, 1, 800), "\n")
}
# EXPECTED OUTPUT: a one-row data frame and the first 800 characters of the
# synopsis in plain text.
# Recorded run (November 22, 2026): PAR-28-091, National Institutes of
# Health, response_date NA, award_floor NA, award_ceiling 5e+05.


# ==============================================================================
# PART B, STEP 6: A FUNDING SCOUT AGENT
# ==============================================================================

# The tools show the model a short numbered list (n records per call) and
# keep the full data frame for you. Small n keeps the context, the cost and
# the free-tier token rate small.
funding_tools <- list(
  tool_nih_reporter(n = 5, fiscal_years = 2023:2025),
  tool_nsf_awards(n = 5),
  tool_grants_gov(n = 5)
)
funding_tools[[1]]
tool_schema(funding_tools[[3]])
# EXPECTED OUTPUT: nih_reporter with text (required) and optional years,
# institutes and pi_name; the grants_gov schema with two optional arguments,
# keyword and opportunity_id.

funding_schema <- list(
  open_opportunities = arg_array("Open or forecasted Grants.gov opportunities that fit the profile",
    items = arg_object("One opportunity", fields = list(
      opportunity_id = arg_string("The Grants.gov id, copied from a tool result"),
      number = arg_string("The funding opportunity number, copied from a tool result"),
      title = arg_string("Title"),
      close_date = arg_string("Closing date as given by Grants.gov, or 'not given'"),
      fit = arg_string("One sentence: why it fits the researcher profile")))),
  similar_funded_projects = arg_array("Funded projects similar to the draft aim",
    items = arg_object("One funded project", fields = list(
      source = arg_enum("Database", values = c("NIH", "NSF")),
      project_id = arg_string("NIH project number or NSF award id, copied from a tool result"),
      title = arg_string("Title"),
      year = arg_string("NIH fiscal year or NSF start date"),
      similarity = arg_string("One sentence: what it shares with the draft aim")))),
  searches = arg_array("Every search you ran, with the exact words used",
                       items = arg_string("One search"))
)

scout <- agent(
  name = "funding_scout",
  model = "groq:openai/gpt-oss-120b",
  description = "Finds funding opportunities and similar funded projects for a researcher.",
  instructions = paste(
    "You help a researcher find funding. Use nih_reporter and nsf_awards to find FUNDED",
    "projects similar to the draft aim, and grants_gov to find OPEN opportunities.",
    "Search with few, specific words (NIH requires ALL words to appear); try two or three",
    "phrasings before giving up. Read the synopsis (grants_gov with opportunity_id) of an",
    "opportunity before saying it fits. Copy every id and number exactly from a tool",
    "result; never write an id you did not see. Report at most 5 opportunities and 5",
    "projects. If nothing fits, return empty lists and say so in the searches field."),
  tools = funding_tools,
  output_schema = funding_schema
)

profile <- paste(
  "Researcher profile: quantitative psychologist; methods expertise in psychometrics,",
  "network psychometrics and large language models; substantive interest in loneliness",
  "and well-being in older adults; US university; early-career eligible: no.")
draft_aim <- paste(
  "Draft aim: develop and validate a short adaptive measure of loneliness for older adults,",
  "using AI-assisted item generation and ecological momentary assessment.")

# final_call = TRUE: a scout that is still searching after 10 model calls gets
# one more call to hand in what it has (stop reason "max_steps_final_answer");
# without it, such a run ends with no answer and nothing to verify below.
ep_fund <- run(scout, paste(profile, draft_aim, sep = "\n\n"),
               budget = budget(max_steps = 10, max_cost = 0.05, max_seconds = 300,
                               final_call = TRUE))
ep_fund
# EXPECTED OUTPUT: several tool calls (nih_reporter, nsf_awards, grants_gov),
# then stop reason final_answer, or max_steps_final_answer when the 10 model
# calls of the budget were used up and the scout handed in what it had.

opps <- do.call(rbind, lapply(ep_fund$answer$open_opportunities, as.data.frame))
projects <- do.call(rbind, lapply(ep_fund$answer$similar_funded_projects, as.data.frame))
opps
projects
unlist(ep_fund$answer$searches)
# EXPECTED OUTPUT: up to 5 opportunities and up to 5 funded projects, each
# with a one-sentence justification, and the list of searches the agent ran
# (NULL three times if the episode ended without an answer).

# --- Verify in code: did every id come from a tool result? ---------------------
# The model only saw what the tools returned. An id that does not appear in
# any tool output was made up (or copied wrongly). Count those; do not fix them.
tool_steps <- Filter(function(s) s$type == "tool", ep_fund$steps)
tool_text <- paste(vapply(tool_steps, function(s) s$result, character(1)), collapse = "\n")

if (!is.null(projects)) {
  projects$seen_in_tools <- vapply(projects$project_id, function(id)
    grepl(id, tool_text, fixed = TRUE), logical(1))
  print(projects[, c("source", "project_id", "seen_in_tools")])
}
if (!is.null(opps)) {
  opps$seen_in_tools <- vapply(opps$opportunity_id, function(id)
    grepl(id, tool_text, fixed = TRUE), logical(1))
  print(opps[, c("opportunity_id", "number", "seen_in_tools")])
}
# EXPECTED OUTPUT: seen_in_tools = TRUE for every row. Any FALSE is a
# fabricated or mistyped id: report it in your AI-use log.

# Open a project page to read the abstract yourself (NIH and NSF links):
# browseURL(paste0("https://reporter.nih.gov/project-details/", ...))
# Save the episode for the capstone and your AI-use statement:
# save_episode(ep_fund)


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Change request to a question from your own research and run both advisor
#    runs again. Do the clarifying questions target what is really missing?
# 2. Run the advisor with budget(max_steps = 2) in run 2. Which stop reason do
#    you get, and what is in ep$answer?
# 3. Give the scout tool_nih_reporter(n = 5, agencies = "NIA") instead. Does the
#    fixed filter change what it finds?
# 4. Run search_nih_projects() with pi_names = "your surname" to see your own
#    (or a colleague's) funding history.


# ==============================================================================
# EXERCISES (solutions in solutions/07_solutions.R)
# ==============================================================================

# Exercise 1. Use run_r_code() to compute the total sample size for a
#   two-proportion comparison (30% versus 20%, alpha = .05, power = .80) with
#   stats::power.prop.test(). Then show that the result is identical to the
#   same call in your own session.
# Exercise 2. Write R code that tries to write a file OUTSIDE the sandbox's
#   working folder (for example in tempdir() of the child) and print where it
#   went. What does this tell you about the limits of the sandbox?
# Exercise 3. Make a version of r_tool that needs your approval before every
#   call (approval = TRUE), and approve calls automatically in a script with
#   options(researchAgentR.approve = ...) only when the code contains no
#   "system(", "unlink(" or "file.remove(". Test it with call_tool().
# Exercise 4. From ep_fund, build a data frame with one row per tool call:
#   the tool name, its arguments and whether it failed. Which search words
#   returned nothing?
# Exercise 5. For the NIH projects in `projects`, fetch the official record
#   again with search_nih_projects(project_nums = list(<id>)) and compare the
#   title the agent reported with the title in RePORTER.


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - "run_r_code() needs the callr package": install.packages("callr").
# - pwr is not installed: the advisor falls back to stats::power.t.test();
#   install.packages("pwr") to use pwr in the sandbox.
# - The advisor skips the questions and answers at once: the instructions are
#   a request, not a guarantee. Check ep1$answer$status in code before using
#   the answer, and tighten the instructions if needed.
# - The advisor reports a sample size that differs from the run_r output:
#   the number in the answer was typed by the model. Always rerun the code
#   (STEP 4) and trust the R output, not the sentence.
# - status = "complete" but power_analysis$r_code is empty: the run had no
#   run_r call. Look at episode_steps(ep2) and rerun with a clearer task.
# - Free-tier limits: Groq lists 8K tokens per minute for openai/gpt-oss-120b
#   on its rate-limit page (check console.groq.com/settings/limits). A long
#   schema plus long tool outputs reaches it quickly; the client retries 429
#   errors with backoff. Use n = 5 in the funding tools and the cache.
# - NIH RePORTER returns 0 projects: the text search requires ALL words; use
#   fewer words. Grants.gov returns many closed calls: status defaults to
#   posted (search) and posted plus forecasted (tool).
# - An API changes or is down (US federal APIs sometimes are): the tool
#   returns an error observation and the agent continues without it; the
#   episode records the failure.
# - seen_in_tools is FALSE for an id that is really in the results: the tool
#   output was longer than researchAgentR.log_chars (set in SETUP).


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. Compute, do not guess: the power analysis is only as good as the code
#    that produced it, and the code is in the log. A reviewer can rerun it.
# 2. Clarifying questions as data: two logged runs instead of a chat. Which
#    answers should come from the researcher, and which may the agent assume?
# 3. The sandbox is soft: separate process, time limit, no keys, no ordinary
#    network, but the same user account. What data would you never let a
#    code-running agent near?
# 4. Funding data are public, but your draft aim is not: sending an unpublished
#    idea to a cloud model is a disclosure. When would you use a local model?
# 5. A fabricated grant number in a proposal is a research-integrity problem.
#    The seen_in_tools check is cheap; where else in your workflow would a
#    check in code catch what a model makes up?
# 6. NIH RePORTER etiquette (one request per second, large jobs at night or on
#    weekends) is part of responsible use. How would 30 students in one class
#    stay within it?
