# Activity 18: Review Your Own Paper
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: review your OWN manuscript before submission in three stages (checks
# in code, a model reviewer, a verifier in code), score the review against a
# known answer key, and apply only the suggestions you accept to a COPY.
# OVERVIEW: review_manuscript() combines
#   1. checks in code: in-text citations against the reference list (both
#      directions), DOIs (online), reported p-values (recomputed in base R),
#      your style rules, figure and table labels, and whether R code runs;
#   2. one reviewer model call per section, with a structured output in which
#      every suggestion must QUOTE the text it refers to;
#   3. a verifier in code that drops suggestions whose quote is not found
#      verbatim, merges near-duplicates and ranks the rest by severity.
# The default example is a teaching manuscript with PLANTED errors and an
# answer key: an excerpt adapted from Russell-Lasalandra, Christensen and
# Golino (2026), Behavior Research Methods, https://doi.org/10.3758/s13428-026-03082-1,
# CC BY 4.0, with six deliberate errors. Do not cite that version. Then run
# the same code on a manuscript of your own.
#
# researchAgentR functions used here:
#   review_checks()          stage 1 only: free, offline, no model
#   default_style_rules()    style rules as regular expressions (add your own)
#   review_manuscript()      all three stages (model calls)
#   review_report()          a self-contained HTML report
#   apply_revisions()        accepted suggestions -> a revised COPY
# TIME: about 50 minutes.  COST: free tier (Groq). Steps 1 to 4, 7 and 8a run
# offline (the online DOI check in Step 1 needs the network); Steps 5, 6b and
# 8b call a model.
# ==============================================================================


# ==============================================================================
# FIRST: WHOSE TEXT IS THIS?
# ==============================================================================

# These tools are for YOUR OWN work (or work whose authors asked you to help).
# - Never upload a manuscript or grant you received as a REVIEWER to a cloud
#   model: it breaks the confidentiality of peer review.
# - NIH prohibits reviewers from using generative AI to analyze or write
#   critiques of applications and R&D contract proposals: NOT-OD-23-149,
#   https://grants.nih.gov/grants/guide/notice-files/NOT-OD-23-149.html
#   Many journals and funders have similar rules; read them.
# - Unpublished work of your own is confidential too. Use confidential = TRUE
#   with a local model (Step 6) when the text must not leave your computer.


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

use_cache("auto")      # first run calls the APIs and saves; later runs replay
set_groq_key()         # reads GROQ_API_KEY from ~/.Renviron (see Activity 00)

# The manuscript to review. Default: the planted-error teaching manuscript.
# For your own paper: manuscript <- "my_paper/manuscript.qmd" (a .md, .Rmd,
# .qmd, .tex or .txt file, or a folder with the manuscript and its R code).
manuscript <- system.file("extdata", "planted_errors", "aigenie_excerpt_with_errors.md",
                          package = "researchAgentR")
if (!nzchar(manuscript)) {    # older installations: a small synthetic draft
  manuscript <- system.file("extdata", "synthetic_methods_draft.md", package = "researchAgentR")
}
answer_key_file <- system.file("extdata", "planted_errors", "answer_key.csv",
                               package = "researchAgentR")

# Work on a COPY in a folder of your own. The original is never edited.
dir.create("review_work", showWarnings = FALSE)
my_copy <- file.path("review_work", basename(manuscript))
file.copy(manuscript, my_copy, overwrite = TRUE)
md5_before <- unname(tools::md5sum(my_copy))


# ==============================================================================
# STEP 1: CHECKS IN CODE ONLY (OFFLINE, FREE)
# ==============================================================================

# confidential = TRUE skips the only online check (DOIs), so this runs with
# no network at all. run_code = FALSE: this manuscript has no R code.
checks <- review_checks(my_copy, confidential = TRUE, run_code = FALSE)
checks
print(checks, n = Inf)
table(checks$category, checks$severity)
attr(checks, "notes")

# EXPECTED OUTPUT (computed offline on the planted-error manuscript):
# "22 suggestions (22 from checks in code, 0 from the model): 1 high, 2
# medium, 19 low"; 1 row of category "references", 1 of "reporting" and 20
# of "style". Notes: DOIs not checked because confidential = TRUE, and "1 test
# result(s) checked in base R". The high row is the in-text citation with no
# matching reference entry, "No reference list entry matches Clark (2016)";
# the reporting row is the p-value that does not match t(63) = 2.31
# (recomputed p = 0.0242).
#
# Read the rows: checks in code are proxies. The 19 low rows are style-rule
# hits (em dashes and "we"/"our"), each with the exact sentence quoted; they
# follow YOUR rules, so a rule you do not hold should be removed (Step 2).
# Every row says how it was found, in the rationale column.
# Selecting rows keeps the review view; selecting columns gives a plain data frame.
checks[checks$severity == "high", ]
checks[checks$severity == "high", c("id", "quote", "suggestion")]

# The online DOI check (needs the network; verify_citations() from the
# verification module looks up every DOI written in the text):
checks_online <- review_checks(my_copy, run_code = FALSE)
as.data.frame(checks_online)[grepl("DOI", checks_online$suggestion), c("severity", "quote", "suggestion")]
attr(checks_online, "notes")

# EXPECTED OUTPUT: a note "N DOI(s) checked." and a high-severity row for
# every DOI that was not found or is marked as retracted.

# A reported statistic can be recomputed in one line. review_checks() does
# this for t, F, r, z and chi-square results; statcheck (Nuijten and Polanin,
# 2020), when installed, covers more formats. By hand, for t(63) = 2.31:
2 * pt(-2.31, df = 63)

# EXPECTED OUTPUT (computed offline): 0.02417681, a two-tailed p of about .024.


# ==============================================================================
# STEP 2: YOUR STYLE RULES (OFFLINE)
# ==============================================================================

default_style_rules()

# Rules are Perl regular expressions. Add your own as rows:
my_rules <- rbind(
  default_style_rules(),
  data.frame(rule = "overclaim", pattern = "(?i)\\bprov(e|es|ed|en|ing)\\b",
             suggestion = "Empirical results do not prove a claim; use show, indicate or suggest.",
             severity = "medium"),
  data.frame(rule = "utilize", pattern = "(?i)\\butiliz\\w*",
             suggestion = "Use 'use'.", severity = "low"))
my_rules[, c("rule", "severity")]

checks2 <- review_checks(my_copy, rules = my_rules, confidential = TRUE, run_code = FALSE)
as.data.frame(checks2)[grepl("overclaim", checks2$suggestion), c("id", "severity", "quote")]

# EXPECTED OUTPUT: three default rules (em_dash, first_person_plural,
# banned_word) and the two new ones. Computed offline: one overclaim row,
# with the quote "The findings prove that AI-GENIE streamlines the scale".


# ==============================================================================
# STEP 3: SCORE A REVIEW AGAINST THE ANSWER KEY (OFFLINE)
# ==============================================================================

# The teaching manuscript has an answer key: one row per planted error, with
# a regular expression that a review row must match to count as "found".
key <- read.csv(answer_key_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
key[, c("id", "type", "location")]

recall_table <- function(review, key) {
  txt <- paste(review$quote, review$suggestion)
  data.frame(id = key$id, type = key$type,
             found = vapply(key$match_pattern, function(p) any(grepl(p, txt, perl = TRUE)),
                            logical(1)),
             row.names = NULL)
}
recall_table(checks, key)
recall_table(checks2, key)

# EXPECTED OUTPUT (computed offline): six planted errors. With the default
# rules, code finds 3 of 6 (the citation without a reference, the wrong
# p-value and the "delve"/em dash insertion); with my_rules, 4 of 6 (the
# overclaim too). The sample-size mismatch needs arithmetic across sections,
# and the invalid DOI needs the online check: Step 5 asks
# whether a model reviewer finds what code missed.


# ==============================================================================
# STEP 4: GUIDELINES FOR THE REVIEWER (OFFLINE)
# ==============================================================================

# `guidelines` is text (or a file) the reviewer checks the manuscript
# against: a journal's instructions, or the reporting standards of your
# field. For psychology, the APA Journal Article Reporting Standards (JARS):
# https://apastyle.apa.org/jars . Paste the items that apply to YOUR study
# into a text file of your own for your own use; do not redistribute the
# JARS tables. A short checklist in your own words also works:
my_guidelines <- paste(
  "- Every sample size reported in the abstract matches the method and results.",
  "- Every primary result reports an effect size with a confidence interval.",
  "- Every p-value is consistent with its test statistic and degrees of freedom.",
  "- Claims about generality are no stronger than the design allows.",
  "- The method says how the sample size was determined.",
  sep = "\n")
# my_guidelines <- "my_reporting_checklist.txt"     # a file works too
cat(my_guidelines)


# ==============================================================================
# STEP 5: THE FULL REVIEW WITH A MODEL (MODEL CALLS)
# ==============================================================================

# A learned style from Activity 17, if it was saved there (otherwise NULL).
# For your own paper, use the style learned from YOUR papers.
style_file <- file.path(cache_dir(), "kept", "style_dynega_train.rds")
my_style <- if (file.exists(style_file)) readRDS(style_file) else NULL

review <- keep("review_planted_manuscript", review_manuscript(
  my_copy,
  focus = c("argument", "methods", "reporting", "references", "style"),
  guidelines = my_guidelines,
  style = my_style,
  rules = my_rules,
  model = "groq:openai/gpt-oss-120b",
  run_code = FALSE))
review
attr(review, "counts")
attr(review, "dropped")[, c("section", "quote", "suggestion")]
as.data.frame(review)[review$source == "model", c("id", "severity", "category", "quote", "suggestion")]

# EXPECTED OUTPUT: "<manuscript_review> ... | groq:openai/gpt-oss-120b | <date>",
# then "N suggestions (C from checks in code, M from the model): ... high,
# ... medium, ... low" and "Model quotes not found and dropped: D of S". The
# counts list has checks, model_suggestions, quotes_dropped, merged and
# failed_calls. The dropped rows are suggestions whose quote is NOT in the
# manuscript: the verifier removed them, and they are kept for inspection.

recall_table(review, key)
sum(recall_table(review, key)$found)

# EXPECTED OUTPUT: the six planted errors with found TRUE or FALSE. Compare
# with Step 3: which errors did the model add (for example the sample-size
# mismatch, or the overclaim), and which did it miss? A single run is one
# trial; Activity 09 explains why several trials are needed before saying
# "the reviewer finds X".


# ==============================================================================
# STEP 6: CONFIDENTIAL MANUSCRIPTS STAY ON YOUR COMPUTER
# ==============================================================================

# 6a. confidential = TRUE refuses any model that is not local (offline check):
refused <- tryCatch(
  review_manuscript(my_copy, model = "groq:openai/gpt-oss-120b", confidential = TRUE),
  error = function(e) conditionMessage(e))
refused

# EXPECTED OUTPUT (computed offline): "confidential = TRUE accepts only
# models that run on this computer (providers() with local = TRUE, such as
# "ollama:qwen3:8b"). 'groq:openai/gpt-oss-120b' sends the text to a remote
# provider." Nothing was sent.

# 6b. With a local model (Ollama running, model pulled; see Activity 11).
#     Slower, and usually less thorough than a large cloud model: compare
#     its recall with Step 5.
if (inherits(try(ollama_status(), silent = TRUE), "data.frame")) {
  review_local <- keep("review_planted_local", review_manuscript(
    my_copy, focus = c("argument", "reporting"), guidelines = my_guidelines,
    rules = my_rules, model = "ollama:qwen3:8b", confidential = TRUE, run_code = FALSE))
  print(review_local)
  print(recall_table(review_local, key))
}

# EXPECTED OUTPUT (only with Ollama): the same structure as Step 5, the note
# "DOIs not checked online because confidential = TRUE.", and cost $0.


# ==============================================================================
# STEP 7: A REPORT TO READ AND SHARE WITH CO-AUTHORS (OFFLINE)
# ==============================================================================

review_report(checks2, "review_work/review_checks.html")
review_report(review, "review_work/review.html")
# browseURL("review_work/review.html")

# EXPECTED OUTPUT: "Report written to review_work/..." twice. Each file is one
# self-contained HTML page (no external files): counts, notes, model, date
# and cost at the top, then one row per suggestion grouped by severity, with
# the quote, the suggestion, the rationale and any proposed revision.


# ==============================================================================
# STEP 8: APPLY ONLY WHAT YOU ACCEPT, TO A COPY
# ==============================================================================

# 8a. Offline: two rows from the checks, with revisions YOU write.
k_delve <- grep("banned_word", checks2$suggestion)
k_prove <- grep("overclaim", checks2$suggestion)
checks2$quote[c(k_delve, k_prove)]

checks2$revision[k_delve] <- sub("\u2014which can delve into almost any topic\u2014", ", ",
                                 checks2$quote[k_delve], fixed = TRUE)
checks2$revision[k_prove] <- sub("prove that AI-GENIE streamlines",
                                 "suggest that AI-GENIE can streamline",
                                 checks2$quote[k_prove], fixed = TRUE)
fixed <- apply_revisions(checks2, my_copy, accept = checks2$id[c(k_delve, k_prove)],
                         out = "review_work/manuscript_revised.md", overwrite = TRUE)
fixed
fixed$changes[, c("id", "status", "line")]

# The original copy is unchanged; only the revised file differs.
identical(unname(tools::md5sum(my_copy)), md5_before)
sum(grepl("delve", read_text(my_copy)))
sum(grepl("delve", read_text("review_work/manuscript_revised.md")))

# EXPECTED OUTPUT (computed offline): the two quotes; then "2 of 2 accepted
# suggestion(s) applied; original unchanged." and "2 applied, 0 skipped",
# each change printed with "-" (before) and "+" (after) and its line (9 for
# the overclaim in the abstract, 17 for "delve"); TRUE (the copy in
# review_work is untouched); 1 and 0 lines with "delve".

# 8b. The model's suggestions: read each one, then accept by id. Never accept
#     rows you have not read, and never a row whose "revision" adds a fact,
#     number or citation you cannot verify.
candidates <- as.data.frame(review)[nzchar(review$revision), c("id", "severity", "category", "quote", "revision")]
candidates
accept_ids <- integer(0)          # for example accept_ids <- c(3, 7), after reading
if (length(accept_ids)) {
  revised <- apply_revisions(review, my_copy, accept = accept_ids,
                             out = "review_work/manuscript_revised_model.md", overwrite = TRUE)
  revised
}

# EXPECTED OUTPUT: the rows with a proposed revision. After you fill
# accept_ids, a revision_result: "applied" rows, and skipped rows with a
# reason: "no_revision", "not_found" (the quote is no longer in the text)
# or "ambiguous" (the quote occurs more than once).


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Run Steps 1 to 5 on a manuscript of your own (set `manuscript` in SETUP).
#    For an R Markdown or Quarto project, give the folder and run_code = TRUE.
# 2. Run Step 5 with focus = "reporting" only. Is the review cheaper? Does
#    recall drop?
# 3. Add a rule for a habit of your own (for example "very unique",
#    "data is", or a word your advisor always deletes).


# ==============================================================================
# EXERCISES (solutions in solutions/18_solutions.R)
# ==============================================================================

# Exercise 1: The sample-size error (answer key row 2) can be found in code.
# Extract every "N = <number>" from the manuscript and every final sample
# size in the Participants section, and test whether the total in the
# abstract is possible. (Offline.)
#
# Exercise 2: Run review_manuscript() three times (three keep() names) and
# report, for each planted error, in how many of the three runs it was found.
# Which errors are found every time (pass^3 in the language of Activity 09)?
#
# Exercise 3: How many model suggestions in Step 5 were dropped by the
# verifier, and why? Read attr(review, "dropped") and classify each dropped
# quote as paraphrased, merged from two places, or invented.
#
# Exercise 4: Write a rule that flags reported p-values written as "p = .000"
# (which should be "p < .001") and test it on a two-paragraph file you write.
# (Offline.)


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - "No reference list found": the reference section must have a heading
#   such as "References" or "Bibliography". Numbered citation styles ([1])
#   and LaTeX \cite{} keys are not cross-checked.
# - The citation check compares the first author and the year only, so it
#   can still raise false alarms: organizations written in several words
#   ("R Core Team" cited as "(R Core Team, 2024)") or two authors with the
#   same surname and year. Read the rows before acting.
# - A .txt manuscript, or text before the first heading, has no section name
#   (NA). In researchAgentR 0.1.0, review_manuscript() then sends those
#   paragraphs to the reviewer as "NA" (reported to the package author). Work
#   around it by giving the paragraphs a section name:
#     docs <- read_documents("draft.txt")
#     docs$section[is.na(docs$section)] <- "Text"
#     review <- review_manuscript(docs, ...)
# - With run_code = TRUE, researchAgentR 0.1.0 reports code that RAN as
#   "did not run in a clean session: NA" (a reported bug in the check; a real
#   failure shows an error message after the colon).
# - apply_revisions() reports "not_found" or "ambiguous": the quote changed
#   (an earlier revision touched it) or appears twice. Edit those by hand.
# - apply_revisions() stops with "The file ... exists": pass overwrite = TRUE
#   or another `out`. It never overwrites the file it reads.
# - PDF manuscripts can be reviewed (read_documents() reads them) but not
#   revised: apply_revisions() edits plain-text formats only.
# - Reviewer calls fail with 429 (rate limit) on long manuscripts: each
#   section is one call. Review one section file at a time, or wait a minute.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# - Code found some planted errors with certainty and for free; the model
#   found others with uncertainty and for a cost. Which checks in your field
#   could move from "a reviewer should notice" to "code checks it"?
# - The verifier drops suggestions whose quote is not in the text. What kinds
#   of bad advice can still pass that filter?
# - Recall against a known answer key is measurable here. On your own
#   manuscript there is no key: how would you decide whether the review was
#   useful, and how would you report its use in the AI-use statement?
# - Where is the line between "AI helped me check my own paper" and "AI
#   reviewed a paper", and why does NIH draw it for peer review?
