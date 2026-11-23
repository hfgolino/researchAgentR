# Activity 14: Capstone, a Research Team that Writes a Verified Proposal Brief
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: Coordinate four specialist agents under a lead agent to produce a short
# proposal brief whose references are verified, whose claims carry quoted
# evidence, whose AI use is documented from the logs, and whose full cost is
# known: a brief that can be checked, not only read.
#
# OVERVIEW: The team (orchestrator-workers, Activity 08):
#
#   lead agent  plans, delegates self-contained tasks, assembles the brief
#     |-- literature_scout    papers with DOIs and verbatim quotes from abstracts
#     |-- measurement_finder  instruments with items, reliability and a citation
#     |-- design_advisor      a design and a power analysis run in the R sandbox
#     |-- funding_scout       funded projects and open calls (NIH, NSF, Grants.gov)
#
# Every worker returns an ARTIFACT that follows a schema (a structured
# hand-off), not free text. Then the brief goes through the checks that turn
# agent output into research material:
#
#   references   verify_citations()   does each DOI exist and match? retracted?
#   claims       check_claims()       is each quote really in its source, and
#                                     does it support the claim?
#   documentation ai_use_statement()  a disclosure drafted from the logs
#   cost         cost_ledger(), episodes_df()   the whole team's episode ledger
#   trace        view_episodes()      every episode of the team on one page
#
# The capstone is graded with the rubric from course_template("capstone_rubric").
# Teams set their success criteria and budgets FIRST (STEP 1).
#
# researchAgentR functions used here:
#   agent(output_schema), team(), as_tool()   the team and its structured hand-offs
#   tool_get_work()                one work with its complete abstract (for verbatim quotes)
#   sub_episodes()                 the workers' own episodes
#   verify_citations(), check_claims()        verification
#   ai_use_statement()             short statement and appendix from the logs
#   cost_ledger(), episodes_df()   the episode ledger
#   view_episodes(), save_episode()           trajectories, saved as JSON
#   course_template()              AI-use template, verification checklist, rubric
#
# TIME: about 90 minutes (self-study; also the guide for the Day 3 team run).
# COST: free tier (Groq, OpenAlex, Crossref, NIH RePORTER, NSF, Grants.gov).
# Web search for the measurement finder is optional (Tavily free plan).
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

use_cache("auto")      # every API response of the first run is saved and replayed
set_groq_key()         # reads GROQ_API_KEY from ~/.Renviron
if (nzchar(Sys.getenv("OPENALEX_API_KEY"))) set_openalex_key()   # optional free key
HAS_SEARCH <- nzchar(Sys.getenv("TAVILY_API_KEY"))               # optional web search
if (HAS_SEARCH) set_tavily_key()

OUT_DIR <- "capstone"
EP_DIR  <- file.path(OUT_DIR, "episodes")
dir.create(EP_DIR, recursive = TRUE, showWarnings = FALSE)

LEAD   <- "groq:openai/gpt-oss-120b"
WORKER <- "groq:openai/gpt-oss-20b"

# The team's research topic: one sentence, specific enough to search.
TOPIC <- "Loneliness in adults aged 65 or older, measured with ecological momentary assessment"


# ==============================================================================
# STEP 1: SUCCESS CRITERIA AND BUDGETS, BEFORE THE FIRST RUN
# ==============================================================================

# What counts as a successful brief? Decide now; check it in STEP 9.
SUCCESS <- list(
  min_verified_references = 5,   # references with status "verified"
  max_unsupported_claims  = 0,   # claims whose quote is missing or does not support them
  min_instruments         = 2,   # instruments with reliability evidence and a citation
  power_analysis_in_code  = TRUE,
  max_team_cost_usd       = 0.05)

# Each worker call runs with its own budget; the team budget caps the lead.
# max_cost for the team INCLUDES the workers (their cost is recorded on the
# lead's tool steps); max_steps and max_tokens count the lead's own calls.
# final_call = TRUE: a worker that runs out of steps or tokens gets one last
# call to hand in what it has (stop reason "max_steps_final_answer" or
# "max_tokens_final_answer") instead of nothing. That call has no tools: the
# worker writes its answer from what it has found. Check such answers: they
# may be partial.
# max_cost counts web searches at list price (Tavily: $0.008 each), even on
# the free plan.
WORKER_BUDGET <- budget(max_steps = 8, max_tokens = 40000, max_cost = 0.05, max_seconds = 240,
                        final_call = TRUE)
HAS_PWR <- requireNamespace("pwr", quietly = TRUE)   # install.packages("pwr") (Activity 00)
TEAM_BUDGET   <- budget(max_steps = 10, max_tokens = 80000,
                        max_cost = SUCCESS$max_team_cost_usd, max_seconds = 1200)
WORKER_BUDGET
TEAM_BUDGET
# EXPECTED OUTPUT: two <budget> lines with the limits above.


# ==============================================================================
# STEP 2: THE WORKERS, EACH WITH A STRUCTURED HAND-OFF
# ==============================================================================

# Keep the artifacts SHORT: the lead re-reads every worker's answer at every
# step, and Groq's free tier allows about 8,000 tokens per minute.

literature_scout <- agent(
  name = "literature_scout",
  description = "Finds published papers on a topic and returns DOIs with verbatim quotes from their abstracts.",
  instructions = paste(
    "Find 3 to 5 relevant, published papers on the topic you are given. Search with",
    "search_openalex (search results show only the start of each abstract), then call",
    "get_work with the DOI of each paper you keep to read its complete abstract. For each",
    "paper give one key finding and a VERBATIM quote (one sentence, copied exactly) from",
    "that abstract. Never write a quote from memory; skip papers without an abstract.",
    "Stop searching once you have enough papers and call final_answer."),
  model = WORKER,
  tools = list(tool_openalex(n = 8), tool_get_work()),
  output_schema = list(papers = arg_array("The papers found", arg_object("One paper", fields = list(
    doi = arg_string("The DOI, for example 10.1037/xxxx"),
    title = arg_string("The title"),
    first_author = arg_string("Surname of the first author"),
    year = arg_integer("Publication year"),
    key_finding = arg_string("One sentence: what the paper found"),
    quote = arg_string("A verbatim sentence from the abstract that supports the key finding"))))))

measurement_finder <- agent(
  name = "measurement_finder",
  description = "Finds validated instruments for a construct, with reliability evidence and a citation.",
  instructions = paste(
    "Find at most 3 validated instruments for the construct and population you are given.",
    "For each: the number of items, the response format, one reliability or validity result",
    "with the DOI of the paper that reports it, the population studied, and the license or",
    "availability. Report only what a source states; write 'not reported' otherwise.",
    "On a long page, set read_page's find argument to a word such as 'alpha' or 'reliability'",
    "instead of requesting the page again."),
  model = WORKER,
  tools = c(list(tool_openalex(n = 5), tool_crossref(n = 5)),
            if (HAS_SEARCH) list(tool_web_search(engine = "tavily", n = 5),
                                 tool_read_page(max_chars = 6000))),
  output_schema = list(instruments = arg_array("The instruments", arg_object("One instrument", fields = list(
    name = arg_string("Name and abbreviation"),
    n_items = arg_string("Number of items, or 'not reported'"),
    response_format = arg_string("Response format, or 'not reported'"),
    evidence = arg_string("One reliability or validity result, with numbers as reported"),
    evidence_doi = arg_string("DOI of the paper reporting that evidence"),
    population = arg_string("Population in which the evidence was obtained"),
    license = arg_string("License or availability, or 'not reported'"))))))

design_advisor <- agent(
  name = "design_advisor",
  description = "Proposes a study design and runs a power analysis in R.",
  instructions = paste(
    "Propose a study design for the aim you are given. Run the power analysis with the",
    "run_r tool", if (HAS_PWR) "(the pwr package is attached)" else "(use base R: power.t.test or power.prop.test)",
    "and report the code you ran and the sample size it gave. List the main threats to validity."),
  model = WORKER,
  tools = list(tool_run_r(packages = if (HAS_PWR) "pwr", timeout = 60)),
  output_schema = list(
    design = arg_string("The design in at most 80 words"),
    power_code = arg_string("The exact R code that was run for the power analysis"),
    n_required = arg_string("The sample size the code returned, as printed"),
    threats = arg_array("Main threats to validity", arg_string("One threat, one sentence"))))

funding_scout <- agent(
  name = "funding_scout",
  description = "Finds funded projects and open funding opportunities related to a research aim.",
  instructions = paste(
    "Find at most 3 NIH or NSF projects funded since 2024 that are close to the aim you are",
    "given, and at most 2 open or forecasted opportunities on Grants.gov. Report the",
    "identifiers exactly as the tools return them."),
  model = WORKER,
  tools = list(tool_nih_reporter(n = 5, fiscal_years = 2024:2026), tool_nsf_awards(n = 5),
               tool_grants_gov(n = 5)),
  output_schema = list(leads = arg_array("Funding leads", arg_object("One lead", fields = list(
    id = arg_string("Project number, award ID or opportunity number, exactly as returned"),
    title = arg_string("Title"),
    agency = arg_string("Agency or institute"),
    kind = arg_enum("What it is", values = c("funded_project", "open_opportunity")),
    why = arg_string("One sentence on the match with the aim"))))))

measurement_finder
# EXPECTED OUTPUT: <agent> measurement_finder | groq:openai/gpt-oss-20b, its
# description, instructions, tools and "Final answer: structured ( instruments )".


# ==============================================================================
# STEP 3: THE LEAD AND THE BRIEF'S SCHEMA
# ==============================================================================

# team() wraps each worker with as_tool(): the worker becomes a tool with one
# argument, `task`, runs in a FRESH context (it sees only that task text), and
# reports its answer, its cost and its episode id back to the lead.
as_tool(literature_scout, budget = WORKER_BUDGET)
# EXPECTED OUTPUT: <agent_tool> literature_scout, the scout's description and
# one argument, task (string).

BRIEF_SCHEMA <- list(
  title = arg_string("A working title"),
  aims = arg_array("Two or three specific aims", arg_string("One aim")),
  background = arg_string("150 to 250 words; every factual sentence appears as a claim below"),
  claims = arg_array("Every factual claim in the background, with its evidence",
                     arg_object("One claim", fields = list(
                       claim = arg_string("The claim, as written in the background"),
                       quote = arg_string("The verbatim quote returned by the literature scout"),
                       source_doi = arg_string("DOI of the paper the quote comes from")))),
  references = arg_array("Every paper cited", arg_object("One reference", fields = list(
    doi = arg_string("DOI"), title = arg_string("Title"),
    first_author = arg_string("Surname of the first author"),
    year = arg_integer("Publication year")))),
  instruments = arg_array("Instruments proposed, as returned by the measurement finder",
                          arg_string("Name, items, evidence and DOI")),
  design = arg_string("Design, power analysis result and main threats, at most 120 words"),
  funding = arg_array("Funding leads, as returned by the funding scout",
                      arg_string("Identifier, agency and title")))

capstone_team <- team(
  name = "capstone_team",
  instructions = paste(
    "Write a short proposal brief on the topic you are given. Send the literature scout",
    "the topic; send the measurement finder the main construct and population; send the",
    "design advisor one specific aim; send the funding scout that aim. Call each worker",
    "at most once: a worker that returned nothing will not do better with a reworded task.",
    "Then write the brief",
    "from their artifacts only: every claim must use a quote and DOI returned by the",
    "literature scout, and every reference must be a paper a worker returned. Never add a",
    "reference, number or quote from memory. Say so when a worker returned nothing."),
  workers = list(literature_scout, measurement_finder, design_advisor, funding_scout),
  model = LEAD, worker_budget = WORKER_BUDGET, output_schema = BRIEF_SCHEMA)
capstone_team
# EXPECTED OUTPUT: <agent> capstone_team | groq:openai/gpt-oss-120b, the
# description "A team led by 'capstone_team' with workers: literature_scout,
# measurement_finder, design_advisor, funding_scout.", and the brief fields.


# ==============================================================================
# STEP 4: RUN THE TEAM ONCE, AND KEEP EVERY EPISODE
# ==============================================================================

# The workers' episodes live in this R session only (sub_episodes()), so the
# run returns them together with the lead's episode, and keep() saves all.
TEAM_TASK <- paste("Topic:", TOPIC)

team_run <- keep("capstone14_team_run", (function() {
  sub_episodes(clear = TRUE)                 # forget workers from earlier runs
  lead_ep <- run(capstone_team, TEAM_TASK, TEAM_BUDGET)
  list(lead = lead_ep, workers = lapply(sub_episodes()$id, sub_episodes))
})())

lead_ep <- team_run$lead
worker_eps <- team_run$workers
all_eps <- c(list(lead_ep), worker_eps)
for (e in all_eps) save_episode(e, dir = EP_DIR)     # the logs, as JSON

lead_ep$stop_reason
episodes_df(all_eps)[, c("agent", "model", "stop_reason", "model_calls", "tool_calls",
                         "tool_errors", "tokens", "cost", "seconds")]
# EXPECTED OUTPUT: the lead's stop reason ("final_answer" when the brief was
# delivered; anything else means the brief is incomplete), then one row per
# episode (the lead and each worker call) with calls, errors, tokens, cost
# and time.

if (is.null(lead_ep$answer)) stop("The team stopped without a brief (", lead_ep$stop_reason,
                                  "). Read the trajectory in STEP 5 before re-running.")
brief <- lead_ep$answer
str(brief, max.level = 1)
# EXPECTED OUTPUT: a list with title, aims, background, claims, references,
# instruments, design and funding.

# A brief without references or claims cannot be verified: that happens when
# the literature scout returned nothing (read its episode in STEP 5).
if (!length(brief$references) || !length(brief$claims)) {
  stop("The brief has ", length(brief$references), " references and ", length(brief$claims),
       " claims, so there is nothing to verify. Read the workers' episodes in STEP 5.")
}


# ==============================================================================
# STEP 5: READ THE WHOLE TEAM'S TRAJECTORY
# ==============================================================================

view_episodes(all_eps, file = file.path(OUT_DIR, "team_episodes.html"))
# EXPECTED OUTPUT: an HTML page with one row per episode (lead and workers),
# linked from the lead's tool calls to each worker's episode. Find what each
# worker was asked (the task text the lead wrote) and what it returned.


# ==============================================================================
# STEP 6: VERIFY EVERY REFERENCE
# ==============================================================================

field <- function(x, f) if (is.null(x[[f]]) || !length(x[[f]])) NA else x[[f]]
refs <- do.call(rbind, lapply(brief$references, function(r) data.frame(
  doi = as.character(field(r, "doi")), title = as.character(field(r, "title")),
  first_author = as.character(field(r, "first_author")), year = as.integer(field(r, "year")),
  stringsAsFactors = FALSE)))

ref_check <- keep("capstone14_verify_refs", verify_citations(refs))
ref_check[, c("doi", "status", "title_similarity", "author_match", "year_match", "detail")]
table(ref_check$status)
# EXPECTED OUTPUT: one row per reference with its status ("verified",
# "mismatch", "not_found", "retracted" or "error") and what differed; then the
# counts per status. Every reference that is not "verified" is removed or
# corrected BY HAND before the brief is used.


# ==============================================================================
# STEP 7: THE CLAIM-EVIDENCE TABLE
# ==============================================================================

claims_df <- do.call(rbind, lapply(brief$claims, function(cl) data.frame(
  claim = as.character(field(cl, "claim")), quote = as.character(field(cl, "quote")),
  source = normalize_doi(as.character(field(cl, "source_doi"))), stringsAsFactors = FALSE)))

# The sources are the abstracts of the cited papers, fetched again here from
# OpenAlex (single-record lookups are free): the quote must be in the REAL
# abstract, not only in what an agent said the abstract was.
cited <- unique(stats::na.omit(claims_df$source))
abstracts <- keep("capstone14_abstracts", vapply(cited, function(d)
  tryCatch(as.character(get_work(d)$abstract), error = function(e) NA_character_),
  character(1)))
sources <- abstracts[!is.na(abstracts)]       # a claim whose source has no abstract is flagged below

claim_check <- keep("capstone14_check_claims", check_claims(claims_df, sources, model = WORKER))
evidence_table <- data.frame(
  claim = claims_df$claim, source_doi = claims_df$source, quote = claims_df$quote,
  quote_found = claim_check$quote_found, status = claim_check$status,
  reference_status = ref_check$status[match(claims_df$source, normalize_doi(ref_check$doi),
                                          incomparables = NA)],
  reason = claim_check$reason, stringsAsFactors = FALSE)
evidence_table[, c("claim", "status", "reference_status")]
table(evidence_table$status)
attr(claim_check, "cost")
# EXPECTED OUTPUT: one row per claim with its status ("supported",
# "not_supported", "unclear", or "quote_not_found" when the quote is not in
# the abstract or the abstract was unavailable) and the status of its
# reference; the counts per status; and the cost of the support checks.
save_table(evidence_table, file.path(OUT_DIR, "claim_evidence_table.csv"))


# ==============================================================================
# STEP 8: THE EPISODE COST LEDGER
# ==============================================================================

# A worker's cost appears TWICE in the raw logs: in its own episode, and on
# the lead's tool step that called it. The team total is therefore the
# LEAD's episode cost (which already includes the workers); adding the
# workers' episodes on top would count them twice.
per_episode <- episodes_df(all_eps)
team_total <- per_episode$cost[1]                           # the lead's episode, workers included
workers_total <- sum(per_episode$cost[-1])
c(team_total = team_total, workers = workers_total, lead_own_calls = team_total - workers_total,
  verification = attr(claim_check, "cost"))
aggregate(cbind(cost, tokens) ~ agent, data = per_episode[-1, ], FUN = sum)   # per worker role

runtime_ledger <- cost_ledger(worker_eps, kind = "runtime", note = "workers")
runtime_ledger
# EXPECTED OUTPUT: the team total, the workers' share, the lead's own model
# calls and the cost of the claim checks (US dollars at list price); a table
# of cost and tokens per worker role; and the ledger of the workers' episodes.
# Earlier test runs of the team belong in an OFFLINE ledger (Activity 10).

c(within_budget = isTRUE(team_total <= SUCCESS$max_team_cost_usd))


# ==============================================================================
# STEP 9: DID THE BRIEF MEET THE SUCCESS CRITERIA?
# ==============================================================================

# Did the design advisor really run pwr? Look at the code it sent to run_r.
tool_args <- unlist(lapply(worker_eps, function(e)
  vapply(e$steps, function(s) paste(s$arguments, collapse = " "), character(1))))
power_in_code <- any(grepl("pwr", tool_args))

data.frame(
  criterion = c("verified references", "unsupported claims", "instruments",
                "power analysis in code", "team cost (USD)"),
  target = c(SUCCESS$min_verified_references, SUCCESS$max_unsupported_claims,
             SUCCESS$min_instruments, SUCCESS$power_analysis_in_code, SUCCESS$max_team_cost_usd),
  observed = c(sum(ref_check$status == "verified"),
               sum(evidence_table$status != "supported"),
               length(brief$instruments),
               power_in_code,
               round(team_total, 5)))
# EXPECTED OUTPUT: one row per criterion with the target set in STEP 1 and
# what this run achieved. "observed" for instruments counts what the lead
# listed: check by hand that each has evidence and a verified citation.


# ==============================================================================
# STEP 10: DOCUMENT THE AI USE FROM THE LOGS
# ==============================================================================

PURPOSE <- paste("drafting a short research proposal brief (literature search, instrument",
                 "search, power analysis and funding search) with a team of agents")

statement <- ai_use_statement(all_eps, purpose = PURPOSE, style = "short")
statement
ai_use_statement(all_eps, purpose = PURPOSE, style = "appendix",
                 file = file.path(OUT_DIR, "appendix_ai_use.md"))
# EXPECTED OUTPUT: one paragraph drafted from the logs (package and R
# versions, dates, episodes, model calls per model, tool calls per tool, the
# services the tool names indicate, tokens, cost at list price, cache
# replays, verification calls, stop reasons) with [placeholders] that only
# the team can complete; then the appendix, written to capstone/.
#
# CHECK BEFORE USE (researchAgentR 0.1.0): the cost sentence adds the lead's
# episode, which already includes its workers, to the workers' own
# episodes, so it counts the workers twice. Replace it with team_total from
# STEP 8. The statement also cannot know about verify_citations() and
# check_claims() run in STEPS 6 and 7 (outside the agents): add one sentence
# for each, with the counts from those steps.


# ==============================================================================
# STEP 11: THE TEMPLATES, AND THE BRIEF AS A DOCUMENT
# ==============================================================================

course_template()
# EXPECTED OUTPUT: "agent_verification_checklist" "ai_use_statement" "capstone_rubric"

copy_template <- function(name) {
  target <- file.path(OUT_DIR, basename(course_template(name)))
  if (!file.exists(target)) course_template(name, dest = OUT_DIR)   # never overwrite your edits
  target
}
checklist <- copy_template("agent_verification_checklist")
template  <- copy_template("ai_use_statement")
rubric    <- copy_template("capstone_rubric")
cat(read_text(rubric), sep = "\n")
# EXPECTED OUTPUT: the capstone rubric (8 criteria scored 0 to 3, maximum 24
# points). Fill in the checklist and the rubric in capstone/ as a team.

verified <- ref_check$status == "verified"
brief_md <- c(
  paste("#", brief$title), "",
  "## Aims", "", paste0(seq_along(brief$aims), ". ", unlist(brief$aims)), "",
  "## Background", "", brief$background, "",
  "## Claim-evidence table", "",
  "| Claim | Source DOI | Check | Reference |", "|---|---|---|---|",
  sprintf("| %s | %s | %s | %s |", gsub("\\|", "/", evidence_table$claim),
          evidence_table$source_doi, evidence_table$status, evidence_table$reference_status), "",
  "## References (verified)", "",
  sprintf("- %s (%s). %s. https://doi.org/%s", refs$first_author[verified], refs$year[verified],
          refs$title[verified], normalize_doi(refs$doi[verified])), "",
  "## References that FAILED verification (remove or correct by hand)", "",
  if (any(!verified)) sprintf("- %s: %s (%s)", refs$doi[!verified], ref_check$status[!verified],
                              ref_check$detail[!verified]) else "- none", "",
  "## Instruments", "", paste("-", unlist(brief$instruments)), "",
  "## Design", "", brief$design, "",
  "## Funding leads", "", paste("-", unlist(brief$funding)), "",
  "## AI-use statement (draft; complete the placeholders)", "", unclass(statement), "",
  "## Episode cost ledger", "",
  sprintf("Team total (lead episode, workers included): US$%.5f at list price.", team_total),
  sprintf("Claim checks (outside the team): US$%.5f.", attr(claim_check, "cost")))
write_text(brief_md, file.path(OUT_DIR, "brief.md"))
list.files(OUT_DIR)
# EXPECTED OUTPUT: the files of the capstone folder: brief.md,
# claim_evidence_table.csv, appendix_ai_use.md, team_episodes.html, the
# three templates, and the episodes/ folder with one JSON file per episode.


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Remove the literature scout's instruction to quote verbatim and re-run
#    (new keep() name): how many claims fail check_claims() now?
# 2. Give the funding scout WORKER_BUDGET with max_steps = 2: what does the
#    lead do when a worker stops without an answer?
# 3. Change TOPIC to your own research question.
# 4. The rubric's team also lists a screener: add the screener agent of
#    Activity 12 as a fifth worker that screens the scout's papers against
#    written inclusion criteria before the lead cites them.


# ==============================================================================
# EXERCISES (solutions in solutions/14_solutions.R)
# ==============================================================================

# 1. Step 1 of check_claims() needs no model. Test it on the capstone rubric
#    text as the source: one real quote, one quote with a changed number,
#    and one quote with an ellipsis.
# 2. Collect the facts the agent verification checklist asks for, from the
#    logs: stop reasons, tool errors, truncated tool outputs and budgets of
#    every episode of the team.
# 3. Compare the team with an equal-budget single agent that has all the
#    workers' tools (compare_single_vs_team()). Which one is cheaper? Which
#    brief is better on the rubric?
# 4. Evaluate one component with the evaluation harness (rubric criterion 7):
#    run the funding scout on the NIH task of research_tasks (dev) with 3
#    trials, then once on the held-out NIH task.
# 5. Write the corrected cost sentence for the AI-use statement from the
#    ledger (STEP 8), and replace it in the short statement text.


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - "413 Request too large" or long 429 waits: the lead's context (instructions,
#   four tool schemas and four worker artifacts) is too large for the free
#   tier's tokens per minute. Ask workers for fewer items, shorten their
#   fields, or lower max_output_tokens by keeping artifacts small.
# - stop_reason "max_cost" or "max_steps": the brief is incomplete. Read the
#   trajectory (STEP 5) before raising the budget.
# - A worker returns "stopped without an answer": its own budget ran out; the
#   lead receives that message and should say the part is missing.
# - Claims "quote_not_found": the quote was paraphrased, or OpenAlex has no
#   abstract for that DOI (the claim then has no source to check). Both mean
#   the claim is not supported by checkable evidence.
# - References "mismatch": often a real DOI attached to the wrong title (the
#   classic hallucinated citation). Never keep it without checking by hand.
# - The AI-use statement's cost is too high: see the note in STEP 10.
# - keep() returns an old run after you changed the team: give the new run a
#   new keep() name (or refresh = TRUE), and record the old one as offline cost.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. Which parts of the brief could a single agent with all the tools have
#    produced as well, and at what cost? When does the team earn its cost?
# 2. Why must the claim checks fetch the abstracts again instead of trusting
#    the scout's quotes?
# 3. Which statements in the AI-use statement can only the authors make?
# 4. Who is responsible for a wrong reference that passed verification
#    because the DOI was real but irrelevant?
# 5. What would a reviewer of your grant need to see to trust an agent-assisted
#    brief: the brief, the logs, the ledger, the evaluation, or all of them?
