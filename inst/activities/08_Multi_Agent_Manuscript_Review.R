# Activity 08: Multi-Agent Manuscript Review
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: Learn the multi-agent patterns (agents as tools, teams, parallel
# fan-out, routers, critics, human checkpoints) and test whether a team of
# reviewers earns its cost against one reviewer with the same budget, on a
# manuscript with six known, planted errors.
#
# OVERVIEW: The test manuscript is an excerpt of a published, openly licensed
# article (Russell-Lasalandra, Christensen and Golino, 2026, Behavior Research
# Methods, CC BY 4.0) with exactly six planted errors and an answer key, plus
# an unmodified copy of the same excerpt. Steps 1 to 6 build every
# multi-agent pattern on this manuscript. Steps 7 to 10 are the experiment:
# free checks in code, a model reviewer with a verifier, and a team versus an
# equal-budget single agent, each GRADED IN CODE against the answer key
# (recall and false alarms).
#
# researchAgentR functions used here:
#   as_tool()                 wrap an agent so another agent can call it
#   team()                    a lead agent whose tools are worker agents
#   sub_episodes()            the episodes of agents that ran as tools
#   fan_out()                 one agent, many independent tasks, in parallel
#   route()                   send each input to the best route (method "llm")
#   critique_loop()           writer and critic (evaluator-optimizer)
#   checkpoint(), checkpoint_log()   ask a person before data leave the computer
#   review_checks()           checks in code only: free, no model
#   review_manuscript()       checks + one reviewer call per section + verifier
#   review_report()           a self-contained HTML report of a review
#   default_style_rules()     the author's style rules, as regular expressions
#   compare_single_vs_team()  same tasks, same budget, both designs
#   (also: read_documents(), define_tool(), agent(), run(), budget(),
#    episodes_df(), call_tool())
# TIME: about 90 minutes.  COST: free tier (Groq); the checks in code are free.
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

set_groq_key()          # reads GROQ_API_KEY from ~/.Renviron
use_cache("auto")       # first live run is recorded; later runs replay it

lead_model   <- "groq:openai/gpt-oss-120b"
worker_model <- "groq:openai/gpt-oss-20b"

pe_dir     <- system.file("extdata", "planted_errors", package = "researchAgentR")
f_planted  <- file.path(pe_dir, "aigenie_excerpt_with_errors.md")
f_original <- file.path(pe_dir, "aigenie_excerpt_original.md")
key_file   <- file.path(pe_dir, "answer_key.csv")
# Do NOT open answer_key.csv until STEP 8. Read the manuscript first and
# write down the problems you find yourself: that is the human baseline.
# file.edit(f_planted)


# ==============================================================================
# STEP 1: WHEN DOES A TEAM OF AGENTS EARN ITS COST?
# ==============================================================================

# Plain-English explainer. Every agent re-reads its own instructions, tools
# and inputs on every call, and the lead re-reads every worker's result. So a
# team multiplies tokens. Anthropic reports that its multi-agent research
# system used about 15 times the tokens of a chat
# (https://www.anthropic.com/engineering/multi-agent-research-system).
# Cognition's "Don't build multi-agents"
# (https://cognition.com/blog/dont-build-multi-agents) argues the other side:
# workers that do not share context make conflicting decisions. Harvard
# CS2680 assigns both as readings on multi-agent architecture
# (https://cs2680.seas.harvard.edu/readings/sep-23.html).
#
# Rule of this course: add an agent only when it beats an EQUAL-BUDGET single
# agent on the same graded tasks. Parallel workers help most on decomposable,
# READ-ONLY subtasks (reviewing separate sections is one), and structured
# hand-offs (a schema for what each worker returns) beat free-form chat.

# A worked example of the episode ledger. HYPOTHETICAL numbers chosen only to
# show the arithmetic; the real numbers come from your runs in STEP 10.
single_tokens <- 6 * 3000                      # 6 calls of about 3,000 tokens
team_tokens   <- 4 * 4000 + 3 * (5 * 3500)     # lead: 4 calls; 3 workers: 5 calls each
c(single = single_tokens, team = team_tokens, ratio = team_tokens / single_tokens)
# Cost per SUCCESSFUL task = cost per attempt / success rate. If the single
# agent succeeds on half of the tasks and the team on 80%:
c(single = single_tokens / 0.5, team = team_tokens / 0.8)
# EXPECTED OUTPUT: 18000, 68500 and a ratio of about 3.8; then 36000 and
# 85625 tokens per success. In this example the team is better, but it costs
# more per success, so it earns its cost only if the extra successes matter.


# ==============================================================================
# STEP 2: THE MANUSCRIPT AND A TOOL TO READ ONE SECTION
# ==============================================================================

docs <- read_documents(f_planted)
table(docs$section)
# EXPECTED OUTPUT: paragraphs per section: the title block (with the license
# notice), Abstract, Introduction (excerpt), Methods (excerpt), Empirical
# validation, Participants, Materials, Analyses, Results and References.

# Reviewers read sections on demand instead of receiving the whole paper in
# their prompt: just-in-time context, and the lead never holds the full text.
review_sections <- setdiff(unique(docs$section), docs$section[1])   # drop the title block
review_sections

read_section <- function(section) {
  txt <- docs$text[docs$section %in% section]
  if (!length(txt)) stop("no section called '", section, "'")
  paste(txt, collapse = "\n\n")
}
section_tool <- define_tool(
  read_section, name = "read_section",
  description = "Return the full text of one section of the manuscript under review.",
  args = list(section = arg_enum("The exact section title", values = review_sections)))
section_tool
cat(substr(call_tool("read_section", list(section_tool),
                     arguments = list(section = "Abstract"))$output, 1, 400))
# EXPECTED OUTPUT: the tool with one enum argument, then the start of the
# abstract wrapped in <tool_output ... trust="untrusted">: the manuscript is
# data for the model, never instructions.


# ==============================================================================
# STEP 3: A HUMAN CHECKPOINT BEFORE THE TEXT LEAVES YOUR COMPUTER
# ==============================================================================

# This excerpt is published and openly licensed, so sending it to a cloud
# model is fine. A manuscript you received AS A REVIEWER is confidential:
# never send it to a cloud model (NIH prohibits generative AI in the peer
# review of applications, NOT-OD-23-149). The checkpoint makes the decision
# explicit and logs it.

sections_df <- as.data.frame(table(section = docs$section))
go <- checkpoint("Send the excerpt (a published, CC BY 4.0 article) to a cloud model (Groq)?",
                 data = sections_df, default = FALSE)
go
checkpoint_log()
# EXPECTED OUTPUT: in RStudio, the console asks "Continue? [y/N]". In a
# non-interactive run (Rscript, knitting) nobody can answer: the default
# FALSE is used, a message says so, and the log shows source = "default".

# To run the whole script unattended, state the decision IN CODE (it is
# logged with source = "option"), then remove the rule:
# options(researchAgentR.approve = function(tool, arguments) TRUE)
# go <- checkpoint("Send the excerpt to a cloud model (Groq)?", default = FALSE)
# options(researchAgentR.approve = NULL)
if (!isTRUE(go)) stop("Stopped at the checkpoint: nothing was sent to a model.")


# ==============================================================================
# STEP 4: STRUCTURED HAND-OFFS AND as_tool()
# ==============================================================================

# Every reviewer returns the SAME artifact: a list of issues that follows a
# schema checked in R. The lead (or your grading code) then works with data,
# not with paragraphs of prose.
issue_schema <- list(issues = arg_array("Problems found; an empty list if none",
  items = arg_object("One problem", fields = list(
    section = arg_string("Section title where the quote is"),
    quote = arg_string("An exact span copied from the manuscript, one sentence or less"),
    category = arg_enum("Kind of problem", values = c("citation", "numbers", "statistics",
                                                     "claim", "style", "other")),
    severity = arg_enum("Severity", values = c("high", "medium", "low")),
    explanation = arg_string("What is wrong and how to fix it, in one or two sentences")))))

style_rules_text <- "The author's style rules: no em dashes, and never the word 'delve'."

section_reviewer <- agent(
  name = "section_reviewer",
  model = worker_model,
  description = "Reviews ONE named section of the manuscript and returns a list of issues.",
  instructions = paste(
    "You review one section of a scientific manuscript. Read it with read_section.",
    "To check a citation, also read the References; to check a number, you may read at",
    "most two other sections. Report concrete problems only: citations without a",
    "reference entry, numbers that cannot be right or disagree with another section,",
    "test statistics whose p-values do not match, claims stronger than the evidence,",
    "and style problems.", style_rules_text,
    "Copy each quote exactly. Do not report problems you cannot quote."),
  tools = list(section_tool),
  output_schema = issue_schema)

reviewer_tool <- as_tool(section_reviewer, budget = budget(max_steps = 4, max_cost = 0.01))
reviewer_tool
tool_schema(reviewer_tool)
# EXPECTED OUTPUT: a tool named section_reviewer with ONE argument, task. Its
# description says the worker cannot see the caller's conversation: a
# delegated task must be self-contained (goal, what to return, limits).


# ==============================================================================
# STEP 5: A TEAM (ORCHESTRATOR AND WORKERS) AND ITS SUB-EPISODES
# ==============================================================================

consistency_checker <- agent(
  name = "consistency_checker",
  model = worker_model,
  description = "Compares numbers and claims ACROSS named sections and returns inconsistencies.",
  instructions = paste(
    "Read the sections named in the task with read_section and compare them: sample",
    "sizes, counts, totals, model names and claims must agree across sections, and",
    "totals must be possible given the parts. Report only inconsistencies, with exact",
    "quotes from each side."),
  tools = list(section_tool),
  output_schema = issue_schema)

review_team <- team(
  name = "review_lead",
  model = lead_model,
  instructions = paste(
    "You lead the review of a manuscript excerpt. Send every section except References",
    "to section_reviewer (one self-contained task per section, naming the section).",
    "Send consistency_checker one task comparing the Abstract with Participants and",
    "Results. Then merge the issues: drop duplicates, keep the exact quotes, keep the",
    "worker's section, and do not invent problems no worker reported."),
  workers = list(section_reviewer, consistency_checker),
  worker_budget = budget(max_steps = 4, max_cost = 0.01, max_seconds = 120),
  output_schema = issue_schema)
review_team

task_review <- paste(
  "Review the manuscript excerpt 'Generative psychometrics via AI-GENIE'. Its sections are:",
  paste(review_sections, collapse = "; "), ". Report every concrete problem with an exact",
  "quote: citations and references, numbers that disagree across sections, statistical",
  "reporting, claims stronger than the evidence, and style.", style_rules_text)

ep_team <- run(review_team, task_review,
               budget = budget(max_steps = 14, max_cost = 0.08, max_seconds = 900))
ep_team
# EXPECTED OUTPUT: one tool call per delegated task (lines such as
# "[section_reviewer] final_answer | ... model calls | $..."), then the lead's
# final answer. The episode cost INCLUDES the workers' costs.

team_issues <- do.call(rbind, lapply(ep_team$answer$issues, as.data.frame))
head(team_issues[, c("section", "category", "severity", "quote")], 10)

# Every worker run is kept as its own episode:
workers_df <- sub_episodes()
workers_df[, c("agent", "called_as_tool", "stop_reason", "model_calls", "tokens", "cost")]
c(lead_episode_total = episodes_df(ep_team)$cost, sum_of_workers = sum(workers_df$cost))
# EXPECTED OUTPUT: one row per worker call; the lead episode's total is the
# workers' sum plus the lead's own model calls.

# Open one worker's trajectory from the parent's tool step:
child_ids <- unlist(lapply(ep_team$steps, function(s) if (s$type == "tool") s$child_episode))
child_ids <- child_ids[!is.na(child_ids)]
if (length(child_ids)) episode_steps(sub_episodes(child_ids[1]))[, c("step", "type", "name", "detail")]


# ==============================================================================
# STEP 6: FAN-OUT, A ROUTER AND A CRITIC
# ==============================================================================

# --- fan_out(): the same agent on independent, read-only tasks ---------------
section_tasks <- paste0("Review the section '", setdiff(review_sections, "References"),
                        "'. Return the issues you find in it.")
fan <- fan_out(section_reviewer, section_tasks,
               budget = budget(max_steps = 4, max_cost = 0.01, max_seconds = 120))
fan
summary(fan)
# EXPECTED OUTPUT: one row per section (stop_reason, model_calls, tokens,
# cost, seconds), then the total cost, the summed time and the wall-clock
# time. With the mirai package installed, wall-clock time is below the summed
# time; the COST is the same either way.

fan_issues <- do.call(rbind, lapply(fan, function(e)
  if (length(e$answer$issues)) do.call(rbind, lapply(e$answer$issues, as.data.frame))))
nrow(fan_issues)

# --- route(method = "llm"): a small model for a stable, narrow job ------------
routes <- c(
  references = "Citations, reference list entries and DOIs",
  statistics = "Reported numbers, sample sizes, test statistics and p-values",
  claims     = "Claims stronger than the evidence supports",
  style      = "Wording, register, banned words and punctuation")
to_route <- head(fan_issues$explanation, 8)
routed <- route(to_route, routes, method = "llm", model = worker_model)
routed[, c("route", "reason", "cost")]
table(router = routed$route, reviewer = head(fan_issues$category, 8), useNA = "ifany")
# EXPECTED OUTPUT: one route per input (NA for an input that could not be
# routed, counted in a message), a short reason, and the cost of each call;
# the table compares the router's choice with the reviewer's own category.

# --- critique_loop(): writer and critic ---------------------------------------
# A critic is worth its cost where an error is expensive and checking is
# cheaper than redoing. A critic on the same model shares the writer's blind
# spots, so the critic here uses the larger model.
writer <- agent("plain_writer", model = worker_model,
  instructions = "Write plain-English explanations for a general audience, using ONLY the source text given.")
critic <- agent("accuracy_critic", model = lead_model,
  instructions = paste("Check the explanation against the source text: every statement must be",
                       "supported by it, jargon must be explained, and it must have at most 80 words."))
glossary_task <- paste0(
  "Using only the source text below, explain in at most 80 words what 'item stability' ",
  "means in this study and why the value 0.75 matters.\n\nSource text:\n", read_section("Analyses"))
crit <- critique_loop(writer, critic, glossary_task, max_rounds = 3,
                      budget = budget(max_steps = 3, max_cost = 0.01))
crit
c(rounds = crit$rounds, approved = crit$approved, cost = crit$cost)
# EXPECTED OUTPUT: "<critique_loop> N round(s) | approved (or max_rounds) |
# cost $...", the critic's feedback for each round and the final answer.


# ==============================================================================
# STEP 7: THE EXPERIMENT, PART 1: CHECKS IN CODE (FREE, NO MODEL)
# ==============================================================================

# The style rules are regular expressions. The published article uses "we"
# (journal convention), so the first-person rule would flag dozens of
# sentences that are not errors in this paper. Turn it off for this text.
default_style_rules()
rules <- default_style_rules(first_person_plural = FALSE)
rules$rule
# EXPECTED OUTPUT: three rules (em_dash, first_person_plural, banned_word)
# with pattern, suggestion and severity; then "em_dash" and "banned_word".

rv_checks   <- review_checks(f_planted,  rules = rules, run_code = FALSE)
rv_original <- review_checks(f_original, rules = rules, run_code = FALSE)
print(rv_checks, n = Inf)
# EXPECTED OUTPUT: "checks in code only", cost $0, and notes on what was
# checked (DOIs online; statistics only if the statcheck package is
# installed). Rows: citations without a reference entry, reference entries
# not cited, DOIs not found, p-values that do not match (statcheck), and
# style-rule hits with the exact sentence quoted.

review_report(rv_checks, "review_checks.html")
# browseURL("review_checks.html")


# ==============================================================================
# STEP 8: GRADE IN CODE AGAINST THE ANSWER KEY (NO FUNCTIONS, STEP BY STEP)
# ==============================================================================

# Recipe:
#  (a) A flag that is ALSO raised on the unmodified excerpt is pre-existing
#      (a problem of the article or a parser limit), not about a planted error.
#  (b) A remaining flag catches planted error i when its quote or suggestion
#      matches the key's match_pattern for i.
#  (c) Recall = planted errors caught / 6. False alarm = a remaining flag that
#      matches no planted error.
key <- read.csv(key_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
key[, c("id", "type", "location")]

sig_planted  <- paste(rv_checks$quote, rv_checks$suggestion)
sig_original <- paste(rv_original$quote, rv_original$suggestion)
rv_checks$pre_existing <- sig_planted %in% sig_original

rv_checks$error_id <- NA_integer_
for (i in seq_len(nrow(key))) {
  hit <- !rv_checks$pre_existing & is.na(rv_checks$error_id) &
    grepl(key$match_pattern[i], sig_planted, perl = TRUE)
  rv_checks$error_id[hit] <- key$id[i]
}
key$caught_by_checks <- key$id %in% rv_checks$error_id

grades <- data.frame(
  method       = "review_checks (code only)",
  caught       = sum(key$caught_by_checks),
  recall       = mean(key$caught_by_checks),
  false_alarms = sum(!rv_checks$pre_existing & is.na(rv_checks$error_id)),
  pre_existing = sum(rv_checks$pre_existing),
  cost         = attr(rv_checks, "cost"))
grades
key[, c("id", "type", "caught_by_checks")]
# EXPECTED OUTPUT: which of the six errors the free checks caught. Without
# statcheck and without network access, only the missing reference (1) and
# the em dash and "delve" (5) are caught; with statcheck installed and the
# network on, the p-value (3) and the invented DOI (6) are caught too. The
# sample size (2) and the overclaim (4) need reading, by a person or a model.
# Offline, every DOI lookup fails; read the notes of the review before
# trusting any DOI row (see WHAT CAN GO WRONG).

# Look at the pre-existing flags: some are real problems of the published
# text (in-text "Deepseek" versus the entry "DeepSeek-AI"), some are limits
# of the citation parser (entries whose author list contains ". . .").
# (as.data.frame() drops the review class, whose print method expects all columns)
as.data.frame(rv_checks)[rv_checks$pre_existing, c("category", "quote", "suggestion")]


# ==============================================================================
# STEP 9: THE EXPERIMENT, PART 2: A MODEL REVIEWER WITH A VERIFIER
# ==============================================================================

# Stage 1: the same checks in code. Stage 2: one reviewer call per section,
# returning rows that must quote the text. Stage 3: a verifier IN CODE drops
# suggestions whose quote is not in the text, merges near-duplicates, ranks.
rv_model <- review_manuscript(f_planted, rules = rules, run_code = FALSE, model = lead_model)
print(rv_model, n = 20)
attr(rv_model, "counts")
attr(rv_model, "cost")
# EXPECTED OUTPUT: rows from "check" and from "model"; the counts show how
# many model suggestions were made, how many were DROPPED because their quote
# is not in the text, how many were merged, and failed calls.

head(attr(rv_model, "dropped")[, c("section", "quote", "suggestion")])
review_report(rv_model, "review_model.html")
# browseURL("review_model.html")

# Grade with the same recipe. Check rows can be pre-existing; model rows
# that match no planted error are "other suggestions": read some of them,
# because several will be fair comments on the published article itself.
sig_model <- paste(rv_model$quote, rv_model$suggestion)
rv_model$pre_existing <- rv_model$source == "check" & sig_model %in% sig_original
rv_model$error_id <- NA_integer_
for (i in seq_len(nrow(key))) {
  hit <- !rv_model$pre_existing & is.na(rv_model$error_id) &
    grepl(key$match_pattern[i], sig_model, perl = TRUE)
  rv_model$error_id[hit] <- key$id[i]
}
key$caught_by_model_review <- key$id %in% rv_model$error_id
grades <- rbind(grades, data.frame(
  method       = "review_manuscript (checks + model)",
  caught       = sum(key$caught_by_model_review),
  recall       = mean(key$caught_by_model_review),
  false_alarms = sum(rv_model$source == "check" & !rv_model$pre_existing & is.na(rv_model$error_id)),
  pre_existing = sum(rv_model$pre_existing),
  cost         = attr(rv_model, "cost")))
grades
table(source = rv_model$source, matched_planted_error = !is.na(rv_model$error_id))
# EXPECTED OUTPUT: a second row in `grades` (false_alarms counts check rows
# only) and a table of matched versus other rows by source.


# ==============================================================================
# STEP 10: THE EXPERIMENT, PART 3: TEAM VERSUS AN EQUAL-BUDGET SINGLE AGENT
# ==============================================================================

single_reviewer <- agent(
  name = "single_reviewer",
  model = lead_model,
  description = "Reviews every section of the manuscript alone.",
  instructions = paste(
    "You review a scientific manuscript excerpt alone. Read every section with",
    "read_section (including References, to check citations). Report concrete problems:",
    "citations without a reference entry, numbers that disagree across sections, test",
    "statistics whose p-values do not match, claims stronger than the evidence, and style",
    "problems.", style_rules_text, "Copy each quote exactly."),
  tools = list(section_tool),
  output_schema = issue_schema)

# Same tasks, same budget, both arms. Two trials each show the variability;
# the budget caps the WHOLE team (worker costs are part of the lead's ledger).
equal_budget <- budget(max_steps = 14, max_cost = 0.08, max_seconds = 900)
cmp <- compare_single_vs_team(single_reviewer, review_team,
                              tasks = rep(task_review, 2), budget = equal_budget)
cmp[, c("arm", "stop_reason", "model_calls", "tool_calls", "tokens", "peak_context", "cost", "seconds")]
# EXPECTED OUTPUT: four rows (single, single, team, team) with the stop
# reason and the ledger of each episode.

# Grade each episode's answer with the same patterns (no functions):
eps <- c(attr(cmp, "episodes")$single, attr(cmp, "episodes")$team)
cmp$caught <- 0L
cmp$n_issues <- 0L
for (e in seq_along(eps)) {
  issues <- do.call(rbind, lapply(eps[[e]]$answer$issues, as.data.frame))
  if (is.null(issues)) next              # no answer: 0 caught (counted, not dropped)
  txt <- paste(issues$quote, issues$explanation)
  for (i in seq_len(nrow(key))) {
    cmp$caught[e] <- cmp$caught[e] + any(grepl(key$match_pattern[i], txt, perl = TRUE))
  }
  cmp$n_issues[e] <- nrow(issues)
}
cmp[, c("arm", "caught", "n_issues", "cost", "tokens")]
aggregate(cbind(caught, n_issues, cost, tokens) ~ arm, data = cmp, FUN = mean)
# EXPECTED OUTPUT: planted errors caught (0 to 6), issues reported, cost and
# tokens per episode, then the mean of each arm. Decide with these numbers,
# not with impressions: does the team catch more, and at what price per
# planted error caught?

# Put all methods side by side (cost per planted error caught):
arm_means <- aggregate(cbind(caught, cost) ~ arm, data = cmp, FUN = mean)
summary_table <- rbind(grades[, c("method", "caught", "cost")],
                       data.frame(method = paste("agent:", arm_means$arm),
                                  caught = arm_means$caught, cost = arm_means$cost))
summary_table$cost_per_error_caught <- ifelse(summary_table$caught > 0,
                                              summary_table$cost / summary_table$caught, NA)
summary_table


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Give the team's workers lead_model instead of worker_model. Does recall
#    change? Does the cost per planted error caught?
# 2. Turn the first-person rule back on (default_style_rules()) and count how
#    many new flags appear. Are they errors in this paper?
# 3. Add a rule of your own to `rules` (for example "(?i)\\bproves?\\b" with
#    suggestion "Hedge: data support, they do not prove.") and rerun STEP 7.
# 4. Run review_checks(f_planted, rules = rules, confidential = TRUE). What
#    changes in the notes, and why would you use it?


# ==============================================================================
# EXERCISES (solutions in solutions/08_solutions.R)
# ==============================================================================

# Exercise 1. Using rv_checks only: which planted errors did code catch, and
#   for each one, which check (citation, DOI, statistics, style rule)?
# Exercise 2. Add a style rule that flags "prove", "proves" and "proven" and
#   rerun the grading of STEP 8. Does recall go up? Does the rule raise false
#   alarms on the ORIGINAL excerpt?
# Exercise 3. Write a code check for planted error 2: extract every "N = ..."
#   and "final Ns ranged from A to B" from the text, and test whether the total
#   in the abstract is possible for five samples. (Code catches what a rule
#   can describe.)
# Exercise 4. From cmp, compute the cost per planted error caught for each
#   arm and say which design you would use for your own papers.
# Exercise 5. Take 10 model rows of rv_model that matched no planted error and
#   label each by hand: valid comment on the article, or false alarm. What is
#   the model's precision on your sample?


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - "Stopped at the checkpoint": you answered no, or the script ran
#   non-interactively. See STEP 3 for approving in code.
# - 429 errors and long waits: free-tier limits (Groq lists 8K tokens per
#   minute for openai/gpt-oss-120b; check console.groq.com/settings/limits).
#   A team multiplies tokens and fan_out() sends requests at the same time.
#   Use worker_model for workers, lower max_steps, stagger runs, and replay
#   from the cache in class.
# - A worker stops with max_steps or max_cost: its tool result says it
#   stopped without an answer; the lead continues without it. That is a
#   counted failure, visible in sub_episodes().
# - "4 reference entries could not be parsed" and flags such as "No reference
#   list entry matches Team (2024)": the citation parser splits APA entries
#   whose author list contains ". . .". These flags are pre-existing (also on
#   the original) and are not counted as false alarms of the planted set.
# - DOI rows: verify_citations() needs the network. Offline (or when Crossref
#   and OpenAlex are down) the lookups fail, and the review currently reports
#   EVERY DOI as "not found". Check the message "verify_citations(): ...
#   error = N" and the notes; a failed lookup is not evidence of a bad DOI.
#   Use confidential = TRUE to skip the online check when offline.
# - "Statistics not checked": install.packages("statcheck").
# - A model quote that differs from the text by one character is DROPPED by
#   the verifier (counted in attr(x, "counts")): strictness is the point.
# - Results differ between runs: models are not deterministic. That is why
#   STEP 10 runs two trials per arm and grades in code.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. Which planted errors did code catch for free, and which needed reading?
#    Put deterministic checks in code first; send to a model only what code
#    cannot decide.
# 2. Did the team beat the equal-budget single agent on recall? On cost per
#    planted error caught? Anthropic's 15x and Cognition's counterpoint: which
#    one describes your result?
# 3. Structured hand-offs: every worker returned the same schema. What would
#    the lead have had to do with free-text reports?
# 4. The verifier drops model suggestions whose quote is not in the text. Why
#    is "the quote must exist" such an effective and cheap check?
# 5. Six planted errors are a small benchmark. What would make it a fair test
#    of a reviewing agent (more manuscripts, held-out errors, several trials)?
# 6. Confidentiality: use these tools on your OWN manuscripts. A manuscript
#    you review for a journal or a grant panel must never go to a cloud model.
