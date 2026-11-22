# Activity 03: Literature Scout and Verification
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: Search five scholarly databases from R, combine and deduplicate the
# results with PRISMA-style counts, build a literature scout agent that returns
# a structured list of papers, and verify its references and claims IN CODE.
#
# OVERVIEW: An agent is only as good as its tools and as trustworthy as the
# checks around it. This activity first calls every search function directly,
# so you see exactly what the agent will see. Then the same functions become
# tools of a "literature scout" agent with a structured final answer and a
# budget. Finally, nothing the agent wrote is trusted until code has checked
# it: every DOI against the DOI registry (Crossref), and every quoted piece of
# evidence against the source text.
#
# Topic used throughout: network psychometrics, exploratory graph analysis
# (EGA) and dimensionality.
#
# researchAgentR functions used here:
#   normalize_doi()            one standard form for DOIs ("10.xxxx/...", lower case)
#   search_openalex()          OpenAlex: free index of scholarly works (key: $1/day budget)
#   search_crossref()          Crossref: the DOI registry (polite pool with your email)
#   search_semantic_scholar()  Semantic Scholar (optional: only with a key, which is
#                              approved by hand and can take weeks)
#   search_europepmc()         Europe PMC: PubMed, PMC and preprints (health sciences)
#   search_arxiv()             arXiv: preprints in statistics, CS, mathematics
#   get_work()                 one work from OpenAlex by DOI (free lookup)
#   get_references()           backward snowballing (works a paper cites)
#   get_citations()            forward snowballing (works that cite a paper)
#   find_open_access()         a free, legal full text (Unpaywall, or OpenAlex)
#   dedupe_records()           remove duplicates across databases, keep provenance
#   prisma_counts()            PRISMA 2020 flow-diagram numbers, computed from data
#   tool_openalex(), tool_crossref(), tool_europepmc(),
#   tool_arxiv(), tool_unpaywall(), tool_snowball()   the same searches as agent tools
#   verify_citations()         does the DOI exist? do title, author, year match? retracted?
#   tool_verify_citations()    the same check as an agent tool
#   check_claims()             is the quoted evidence really in the source?
#
# TIME: about 60 minutes.  COST: free tier (Groq, OpenAlex, Crossref,
# Europe PMC, arXiv; Semantic Scholar only with a key). The OpenAlex searches below use well
# under one cent of the free daily budget.
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

# Save every API response on disk: the first (live) run records them, and every
# later run, including the class demo, replays them offline and for free.
use_cache("auto")

# Keys live in ~/.Renviron, never in scripts (see Activity 00):
#   GROQ_API_KEY=...            required for the agent (free tier)
#   OPENALEX_API_KEY=...        recommended (free key, $1/day budget)
#   RESEARCHAGENTR_EMAIL=...    recommended (Crossref "polite pool", Unpaywall)
set_groq_key()
if (nzchar(Sys.getenv("OPENALEX_API_KEY"))) set_openalex_key()

# OpenAlex daily budget (developers.openalex.org, accessed 2026-10-11):
#   without a key: $0.10 per day;  with a FREE key: $1 per day
#   a search costs $1 per 1,000 calls ($0.001 each); a list with filters $0.10
#   per 1,000; a single record by DOI or ID is free.
# With 30 students sharing one key, the budget is shared too: everyone needs a key.
if (!nzchar(Sys.getenv("OPENALEX_API_KEY"))) {
  message("No OPENALEX_API_KEY: OpenAlex allows only $0.10 per day without a key.")
}

# Crossref has two pools (limits from 1 December 2025): public access allows 1
# list request per second; the "polite" pool (requests that include an email)
# allows 3. The package waits between requests to stay below the limit.
if (!nzchar(Sys.getenv("RESEARCHAGENTR_EMAIL"))) {
  message("No RESEARCHAGENTR_EMAIL: Crossref will use the slower public pool. ",
          "Add RESEARCHAGENTR_EMAIL=you@university.edu to ~/.Renviron.")
}
# Alternative for this session only: options(researchAgentR.email = "you@university.edu")

topic <- "network psychometrics exploratory graph analysis dimensionality"

# Two articles used to test the verification code (DOIs read from the PDFs):
dynega_doi  <- "10.1007/s11336-021-09820-y"     # Golino et al. (2022), Psychometrika
entropy_doi <- "10.1080/00273171.2020.1779642"  # Golino et al. (2020), Multivariate Behavioral Research
# NOTE: the two DOIs of the held-out tasks in data(research_tasks) are
# deliberately NOT written in this script (see Activity 04).


# ==============================================================================
# STEP 1: DOIs ARE IDENTIFIERS, SO NORMALIZE THEM FIRST (offline)
# ==============================================================================

# The same DOI appears in many forms: with https://doi.org/, with "doi:", in
# upper case, with a period copied from a reference list. DOIs are
# case-insensitive, so normalize_doi() puts them all in one form. Anything that
# is not a DOI becomes NA: reported, never guessed.
normalize_doi(c("https://doi.org/10.1007/S11336-021-09820-Y",
                "doi: 10.1080/00273171.2020.1779642.",
                "DOI:10.1007/s11336-021-09820-y;",
                "http://dx.doi.org/10.1080%2F00273171.2020.1779642",
                "Golino et al. (2022)",
                NA))
# EXPECTED OUTPUT: a character vector of length 6. The first four elements are
# the two DOIs in lower case, starting with "10." (each one twice); the last
# two are NA (a citation is not a DOI, and NA stays NA).


# ==============================================================================
# STEP 2: SEARCH FIVE DATABASES DIRECTLY
# ==============================================================================

# Every search function returns the SAME columns: title, year, doi, authors,
# source, abstract, oa_url, id, database (plus a few extra columns per
# database). That common format is what lets you stack and deduplicate them.

# --- OpenAlex: broad, free, every field ---------------------------------------
oa <- search_openalex(topic, n = 25, year_from = 2017)
dim(oa)
head(oa[, c("title", "year", "doi", "cited_by_count")])
attr(oa, "total_found")         # matches in OpenAlex (you received only n of them)
attr(oa, "openalex_cost_usd")   # list price of this call against your daily budget
# EXPECTED OUTPUT: a data frame with up to 25 rows and the common columns plus
# cited_by_count, type and is_retracted; total_found is the number of matches
# in OpenAlex; openalex_cost_usd is the price OpenAlex reports for the call.

# --- Crossref: the DOI registry (few abstracts, no open-access links) ----------
cr <- search_crossref(topic, n = 25, filter = "from-pub-date:2017-01-01")
head(cr[, c("title", "year", "doi", "source")])
mean(is.na(cr$abstract))        # share of records WITHOUT an abstract
# EXPECTED OUTPUT: up to 25 rows; many abstracts are NA, because publishers
# do not always deposit abstracts with Crossref.

# --- Semantic Scholar (OPTIONAL: only with a key) -----------------------------
# Without a key, requests go to one pool shared by every unauthenticated user
# in the world and are often refused (HTTP 429). Keys are free but approved by
# hand (https://www.semanticscholar.org/product/api), so the course does not
# depend on them. With SEMANTIC_SCHOLAR_API_KEY in ~/.Renviron, this runs:
if (nzchar(Sys.getenv("SEMANTIC_SCHOLAR_API_KEY"))) {
  s2 <- search_semantic_scholar(topic, n = 25, year = "2017-2026")
  head(s2[, c("title", "year", "doi", "cited_by_count")])
} else {
  s2 <- oa[0, ]   # no key: an empty table with the same columns
}
# EXPECTED OUTPUT: with a key, up to 25 rows; without one, nothing prints.
# The Semantic Scholar API license asks you to credit Semantic Scholar.

# --- Europe PMC: biomedical and health literature -----------------------------
# Europe PMC syntax: quotes make a phrase.
epmc <- search_europepmc('"exploratory graph analysis"', n = 25)
head(epmc[, c("title", "year", "doi", "pmid")])
# EXPECTED OUTPUT: up to 25 rows with pmid and pmcid columns. Psychometric
# methods papers in health journals appear here; methods papers in purely
# psychological journals may not.

# --- arXiv: preprints ---------------------------------------------------------
# Plain words become all:word1 AND all:word2 ..., so keep arXiv queries short:
# a six-word AND query often finds nothing.
ax <- search_arxiv("exploratory graph analysis", n = 10)
head(ax[, c("title", "year", "doi", "oa_url")])
# EXPECTED OUTPUT: up to 10 rows; oa_url is the PDF link (every arXiv preprint
# is open access); doi is NA unless the authors added the published DOI.

# How many records per database?
sapply(list(OpenAlex = oa, Crossref = cr, `Semantic Scholar` = s2,
            `Europe PMC` = epmc, arXiv = ax), nrow)
# EXPECTED OUTPUT: a named integer vector with one count per database.


# ==============================================================================
# STEP 3: ONE WORK AND SNOWBALLING (REFERENCES AND CITATIONS)
# ==============================================================================

# get_work() is a single-record lookup: FREE in OpenAlex.
dynega <- get_work(dynega_doi)
dynega[, c("title", "year", "source", "cited_by_count", "is_retracted")]
head(attr(dynega, "referenced_works"))   # OpenAlex IDs of the works it cites
# EXPECTED OUTPUT: one row for the DynEGA article (Psychometrika); the
# attribute holds OpenAlex work IDs such as "W" followed by digits.

# Backward snowballing: the works this paper cites (older work).
refs <- get_references(dynega_doi, n = 25)
nrow(refs); attr(refs, "total_found")
# Forward snowballing: the works that cite it (newer work, most cited first).
cites <- get_citations(dynega_doi, n = 25)
head(cites[, c("title", "year", "cited_by_count")])
# EXPECTED OUTPUT: two data frames with the common columns. total_found for
# refs is the length of the reference list in OpenAlex. Snowballing finds
# studies that keyword searches miss, because it follows the authors'
# own citations instead of your search terms.


# ==============================================================================
# STEP 4: OPEN ACCESS
# ==============================================================================

# Where can the full text be read for free, legally? With RESEARCHAGENTR_EMAIL
# set, Unpaywall answers; without it, OpenAlex serves the same Unpaywall data.
find_open_access(c(dynega_doi, entropy_doi))
# EXPECTED OUTPUT: two rows with is_oa (TRUE/FALSE), oa_status, oa_url,
# license, version and checked_with ("Unpaywall" or "OpenAlex"). A DOI that
# could not be checked keeps its row, with the reason in `detail`.


# ==============================================================================
# STEP 5: DEDUPLICATE ACROSS DATABASES
# ==============================================================================

# --- 5a. First on a small hand-made table (offline), so the rule is clear -----
# (These rows were typed by hand to show the rule; they are not search results.)
toy <- data.frame(
  title = c("Modeling latent topics in social media using dynamic exploratory graph analysis",
            "Entropy fit indices: New fit measures for assessing the structure and dimensionality of multiple latent variables",
            "A toy record without a DOI",
            "Modeling Latent Topics in Social Media using Dynamic Exploratory Graph Analysis",
            "A toy record without a DOI.",
            "Entropy Fit Indices: New Fit Measures for Assessing the Structure and Dimensionality of Multiple Latent Variables"),
  year = c(2022, 2020, 2019, 2022, 2019, 2020),
  doi = c(dynega_doi, entropy_doi, NA, "https://doi.org/10.1007/S11336-021-09820-Y", NA,
          "DOI: 10.1080/00273171.2020.1779642"),
  abstract = c(NA, NA, "A made-up abstract.", "Another made-up abstract.", NA, NA),
  database = c("OpenAlex", "OpenAlex", "OpenAlex", "Crossref", "Crossref", "Semantic Scholar"))

toy_dedup <- dedupe_records(toy)
toy_dedup[, c("title", "doi", "database", "n_found")]
attr(toy_dedup, "dedup")
# EXPECTED OUTPUT (computed from the toy table, so the numbers are exact):
#   dedupe_records(): 6 records in, 3 duplicates removed (2 by DOI, 1 by title and year), 3 out.
#   Three rows remain. The DynEGA row lists "OpenAlex; Crossref" and kept the
#   abstract that only Crossref had; the Entropy row lists "OpenAlex; Semantic
#   Scholar"; the toy record without a DOI was matched by title and year.

# --- 5b. Now the real results --------------------------------------------------
# rbind() needs the same columns in every data frame, and each database adds
# its own extra columns (cited_by_count, pmid, ...). Keep the common ones.
common <- c("title", "year", "doi", "authors", "source", "abstract", "oa_url", "id", "database")
snowball <- rbind(refs[, common], cites[, common])
snowball$database <- "Citation searching"   # PRISMA reports these as "other methods"

all_records <- do.call(rbind, lapply(list(oa, cr, s2, epmc, ax, snowball),
                                     function(d) d[, common]))
nrow(all_records)

deduped <- dedupe_records(all_records)
attr(deduped, "dedup")
table(deduped$n_found)                    # how many databases found each record
head(deduped[order(-deduped$n_found), c("title", "year", "database", "n_found")])
# EXPECTED OUTPUT: a message with records in, duplicates removed by DOI and by
# title and year, and records out; a table of n_found (most records are found
# once; the central papers are found by several databases).


# ==============================================================================
# STEP 6: PRISMA-STYLE COUNTS, COMPUTED FROM THE DATA
# ==============================================================================

# Screening here is a transparent CODE rule (title or abstract mentions EGA or
# network psychometrics); records without an abstract are left for a human
# (NA). A real screening workflow with two screeners is in Activity 12.
text <- paste(deduped$title, ifelse(is.na(deduped$abstract), "", deduped$abstract))
screen <- grepl("exploratory graph analysis|network psychometric", text, ignore.case = TRUE)
screen[!screen & is.na(deduped$abstract)] <- NA   # cannot exclude without an abstract
table(screen, useNA = "ifany")

prisma <- prisma_counts(all_records, deduped, screening = screen)
prisma
# EXPECTED OUTPUT: a data frame with columns stage, database and n: one
# "Records identified" row per source (OpenAlex, Crossref, Europe PMC, arXiv,
# Citation searching, and Semantic Scholar when it was searched), the total, duplicates removed,
# records after removing duplicates, records screened, excluded, awaiting a
# human decision, and retained. Every number comes from R objects, so an
# agent cannot report counts that the code did not produce.


# ==============================================================================
# STEP 7: THE LITERATURE SCOUT AGENT
# ==============================================================================

# The same search functions, wrapped as tools. The model sees a compact list
# (title, year, authors, source, DOI, OA link, first 60 words of the
# abstract); the full data frame stays in R. Small n keeps each tool output
# short, which matters on the Groq free tier (8K tokens per minute for
# gpt-oss-120b; check console.groq.com/settings/limits).
scout_tools <- list(
  tool_openalex(n = 5),
  tool_crossref(n = 5),
  tool_europepmc(n = 5),
  tool_arxiv(n = 5),
  tool_snowball(n = 5),
  tool_unpaywall(),
  tool_verify_citations()
)
# With a Semantic Scholar key (optional), give the scout one more database:
if (nzchar(Sys.getenv("SEMANTIC_SCHOLAR_API_KEY"))) {
  scout_tools <- c(scout_tools, list(tool_semantic_scholar(n = 5)))
}
tool_openalex(n = 5)                       # what the model reads about the tool
cat(tool_schema(tool_snowball()), "\n")    # the JSON Schema the model receives
# EXPECTED OUTPUT: the tool's name, description and arguments (query, n,
# year_from, year_to), flagged "output untrusted"; then the JSON Schema of the
# snowball tool with "direction" as an enum ("references", "citations").

# A STRUCTURED final answer: the agent must finish by calling final_answer
# with an array of records. R checks the arguments; an invalid answer goes
# back to the model as an error to fix (counted, never silently repaired).
scout_schema <- list(
  records = arg_array(
    "The relevant works found, most relevant first (at most 8).",
    items = arg_object("One work.", fields = list(
      title        = arg_string("Title exactly as in the search results."),
      year         = arg_integer("Publication year."),
      # The example shows the FORM of a DOI only: an example DOI from a real
      # paper would put that answer in front of the model (see Activity 04).
      doi          = arg_string("DOI exactly as in the search results, in the form 10.xxxx/yyyy."),
      why_relevant = arg_string("One sentence: why this work matters for the topic.")))),
  search_summary = arg_string("Which databases and queries were used, and how many results each gave.")
)

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
  tools = scout_tools,
  output_schema = scout_schema,
  effort = "low"          # fewer reasoning tokens per step (free-tier friendly)
)
scout
# EXPECTED OUTPUT: <agent> literature_scout | groq:openai/gpt-oss-120b, the
# instructions (shortened), the 8 tool names, "Final answer: structured
# ( records, search_summary )" and "Context: full".

# Set the budget BEFORE running (and before tuning anything).
# final_call = TRUE: a scout that is still searching at step 12 gets one more
# call to hand in what it has (stop reason "max_steps_final_answer"); without
# it, a run that reaches a limit ends with no answer (see the exercises).
scout_budget <- budget(max_steps = 12, max_tokens = 80000, max_cost = 0.05,
                       max_seconds = 400, max_tool_errors = 3, final_call = TRUE)
scout_budget

scout_task <- paste(
  "Find the key peer-reviewed literature on exploratory graph analysis (EGA) as a",
  "network psychometrics method for estimating the number of dimensions, published",
  "since 2017. Return up to 8 works.")

# keep(): the live run happens once; later runs load the saved episode.
ep_scout <- keep("act03_scout", run(scout, scout_task, budget = scout_budget))
ep_scout
# EXPECTED OUTPUT: <episode> with the agent name and model, the stop reason
# (final_answer if it finished; max_steps, max_tokens, max_cost or
# max_seconds if a budget ran out), the number of model and tool calls (and
# tool errors), billed tokens, peak context, cost and seconds, and the start
# of the answer.

episode_steps(ep_scout)[, c("step", "type", "name", "input", "output", "cost", "is_error")]
view_episode(ep_scout)     # the full trajectory as an HTML page (Lens, Activity 01)
# EXPECTED OUTPUT: one row per step, alternating model calls and tool calls;
# the last model call calls final_answer when the run succeeded.

# Turn the structured answer (a list of records) into a data frame.
answer_table <- function(items) {
  if (length(items) == 0) return(data.frame())
  fields <- unique(unlist(lapply(items, names)))
  cols <- lapply(fields, function(f) vapply(items, function(r) {
    if (is.null(r[[f]])) NA_character_ else as.character(r[[f]])
  }, character(1)))
  stats::setNames(as.data.frame(cols, stringsAsFactors = FALSE), fields)
}

if (is.null(ep_scout$answer)) {
  message("The scout stopped with '", ep_scout$stop_reason, "' and gave no final answer. ",
          "Read the trajectory (view_episode) and see WHAT CAN GO WRONG.")
}
scout_records <- answer_table(ep_scout$answer$records)
if (nrow(scout_records)) {
  scout_records$year <- as.integer(scout_records$year)
  print(scout_records[, c("title", "year", "doi")])
  cat(ep_scout$answer$search_summary, "\n")
}
# EXPECTED OUTPUT: a data frame with title, year, doi and why_relevant, one row
# per reported work (at most 8), and the agent's own summary of its searches.


# ==============================================================================
# STEP 8: VERIFY THE REFERENCES IN CODE
# ==============================================================================

# The agent was TOLD to verify its citations. Did it? Never rely on that:
# check again in code. Add three test citations with known status:
#   - a correct citation (DynEGA, 2022)
#   - a real DOI with a WRONG year (Entropy Fit Indices cited as 2015)
#   - a DOI that was INVENTED for this exercise, in a real journal's prefix, the
#     way a language model invents DOIs (plausible but not registered)
test_citations <- data.frame(
  doi = c(dynega_doi, entropy_doi, "10.1080/00273171.2099.1234567"),
  title = c("Modeling Latent Topics in Social Media using Dynamic Exploratory Graph Analysis: The Case of the Right-wing and Left-wing Trolls in the 2016 US Elections",
            "Entropy Fit Indices: New Fit Measures for Assessing the Structure and Dimensionality of Multiple Latent Variables",
            "Network psychometrics for intensive longitudinal data: A tutorial"),
  first_author = c("Golino", "Golino", "Golino"),
  year = c(2022, 2015, 2023))

scout_rows <- if (nrow(scout_records)) {
  data.frame(doi = scout_records$doi, title = scout_records$title,
             first_author = NA_character_, year = scout_records$year)
}
to_check <- rbind(scout_rows, test_citations)   # rbind() skips NULL

checked <- verify_citations(to_check)
checked[, c("doi", "status", "title_similarity", "year_match", "retracted", "detail")]
attr(checked, "counts")
# EXPECTED OUTPUT: one row per citation with a status:
#   "verified"   the DOI exists and title and year match (the DynEGA test row);
#   "mismatch"   the DOI exists but something differs; for the Entropy test
#                row, detail says "year is ..." with the registered year;
#   "not_found"  the DOI is not registered (the invented test row);
#   "retracted"  a retraction notice exists (rare, but check every time);
#   "error"      the check itself failed (network, rate limit): never "verified".
# The scout's own rows should all be "verified" if it followed its
# instructions; any other status is a finding about the agent.

# Keep only the scout's records that passed (the test rows come last).
if (nrow(scout_records)) {
  scout_records$status <- checked$status[seq_len(nrow(scout_records))]
  verified_records <- scout_records[scout_records$status == "verified", ]
  cat(nrow(verified_records), "of", nrow(scout_records), "scout records verified\n")
}


# ==============================================================================
# STEP 9: CHECK CLAIMS AGAINST QUOTED EVIDENCE
# ==============================================================================

# A claim is acceptable only with a VERBATIM quote that is really in the
# source. Step 1 of check_claims() is pure code (free); step 2, optional,
# asks a model whether the quote supports the claim.

# Sources: the abstracts of the two test articles (OpenAlex lookups are free).
abstract_of <- function(doi) {
  w <- get_work(doi)
  a <- w$abstract
  if (is.na(a)) {   # some publishers do not share abstracts with OpenAlex
    hit <- search_europepmc(paste0('DOI:"', normalize_doi(doi), '"'), n = 1)
    if (nrow(hit) && identical(normalize_doi(hit$doi[1]), normalize_doi(doi))) a <- hit$abstract[1]
  }
  a
}
abstracts <- c(abstract_of(dynega_doi), abstract_of(entropy_doi))
names(abstracts) <- c(dynega_doi, entropy_doi)
nchar(abstracts)          # NA means no abstract was found for that DOI
if (anyNA(abstracts)) {
  stop("No abstract found for ", paste(names(abstracts)[is.na(abstracts)], collapse = ", "),
       ". Use two other DOIs from `deduped` that have an abstract.")
}

# One sentence of each abstract, chosen by code (the first sentence that
# mentions a keyword), so the quote is genuinely verbatim.
sentence_with <- function(text, keyword) {
  s <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))
  hit <- grep(keyword, s, ignore.case = TRUE, value = TRUE)
  if (length(hit)) hit[1] else s[1]
}
quote_real <- sentence_with(abstracts[[1]], "troll|topic")
quote_entropy <- sentence_with(abstracts[[2]], "entropy")
# The second quote is ALTERED: one word ("always") is inserted after the
# third word, the way a model "quotes" from memory. It is no longer in the source.
quote_altered <- sub("^((\\S+\\s+){3})", "\\1always ", quote_entropy)
stopifnot(quote_altered != quote_entropy)

claims <- data.frame(
  claim = c("Golino et al. (2022) used dynamic exploratory graph analysis to model latent topics in social media posts by trolls during the 2016 US elections.",
            "Golino et al. (2020) proposed entropy-based fit indices for assessing the dimensionality of multiple latent variables."),
  quote = c(quote_real, quote_altered),
  source = c(dynega_doi, entropy_doi))

claim_check <- check_claims(claims, abstracts)
claim_check[, c("quote_found", "status", "reason")]
# EXPECTED OUTPUT: two rows. The first quote is found ("quote_found": support
# was not judged because no model was given); the altered quote gives
# "quote_not_found" with the reason "the quote does not appear in the source".
# A one-word change is enough to fail: that is the point.

# Step 2 (optional, one short model call per found quote): does the quote
# SUPPORT the claim? A real quote can be used for a claim it does not support.
claim_judged <- keep("act03_claims_judged",
                     check_claims(claims, abstracts, model = "groq:openai/gpt-oss-20b"))
claim_judged[, c("status", "supported", "reason")]
attr(claim_judged, "cost")
# EXPECTED OUTPUT: the altered quote stays "quote_not_found" (no model call is
# spent on it); the found quote gets "supported", "not_supported" or "unclear"
# with a one-sentence reason. The cost attribute is the price of the judge calls.


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Run the scout with a tighter budget and compare the stop reasons:
#    ep_small <- keep("act03_scout_small", run(scout, scout_task, budget = budget(max_steps = 4)))
#    ep_small$stop_reason
# 2. Change the year filter of the direct OpenAlex search (year_from = 2021) and
#    look at how total_found and the PRISMA counts change.
# 3. Search Crossref with a full reference string instead of keywords:
#    search_crossref(paste("Entropy Fit Indices: New Fit Measures for Assessing the Structure",
#                          "and Dimensionality of Multiple Latent Variables 2020"), n = 3)
# 4. Sort OpenAlex by citations: search_openalex(topic, n = 10, sort = "cited_by_count:desc")


# ==============================================================================
# EXERCISES (solutions in solutions/03_solutions.R)
# ==============================================================================

# Exercise 1 (no new API calls). Count how many DOIs in all_records needed cleaning,
#   that is, how many differ from their normalize_doi() form (ignore NAs).
#
# Exercise 2. Run the same pipeline for YOUR topic with only OpenAlex and
#   Crossref (n = 20 each, a year filter), deduplicate, and report
#   prisma_counts() with a keyword screening rule of your own.
#
# Exercise 3. Add an `evidence_quote` field to the scout's output schema ("a
#   verbatim sentence from the abstract that supports why_relevant"), run the
#   scout again, and check every why_relevant with check_claims() against the
#   abstracts in `deduped`. How many quotes are really in the abstracts?
#
# Exercise 4. Write a function `flag_citations(checked)` that returns, for a
#   verify_citations() result, the rows a researcher must read by hand (every
#   status other than "verified", plus verified rows with title_similarity
#   below 0.9). Test it on `checked`.


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - OpenAlex answers 429 ("Rate limit or quota reached") when the daily budget
#   is spent: $0.10 without a key, $1 with a free key, reset daily. Searches
#   cost $0.001 each; lookups by DOI are free. Fix: add OPENALEX_API_KEY, use
#   keep() and use_cache() so nothing runs twice, and search less broadly.
# - Crossref rate limits: without an email the public pool allows 1 list
#   request per second; a whole class on one network can still be throttled.
#   verify_citations() makes one or two Crossref requests per DOI, so 30
#   citations take about a minute. Set RESEARCHAGENTR_EMAIL.
# - Semantic Scholar 429 without a key: the unauthenticated pool is shared by
#   everyone and often refuses requests. That is why it is optional here: the
#   course runs on OpenAlex, Crossref, Europe PMC and arXiv.
# - arXiv returns nothing for long queries (every word is ANDed), and its
#   terms ask for one request every three seconds (the package waits).
# - Europe PMC covers the life and health sciences: a methods topic can give
#   few or no hits there. Zero rows is an answer, not an error.
# - rbind() fails with "numbers of columns of arguments do not match":
#   different databases add different extra columns; keep the common ones.
# - Deduplication misses: a preprint and its published version have different
#   DOIs and slightly different titles or years, so both survive. Sort by
#   title and read the near-duplicates.
# - A DOI that resolves to a DIFFERENT paper: the model copied a DOI from one
#   result next to the title of another. verify_citations() reports
#   "mismatch" with a low title_similarity; the registered title is in
#   matched_title.
# - Retracted papers: status "retracted" (from Crossref retraction notices,
#   including Retraction Watch data); OpenAlex also flags is_retracted in
#   search results. A retracted paper can still be highly cited.
# - The model invents a DOI: status "not_found". The scout's instructions
#   forbid DOIs from memory, but instructions are not guarantees, which is why
#   Step 8 runs in code.
# - "Request too large" or 429 from Groq: on the free tier, gpt-oss-120b has a
#   limit of 8K tokens per minute, and every step resends the whole context.
#   Use smaller n in the tools, a smaller budget, or a context strategy
#   (Activity 05).
# - stop_reason is max_steps or max_tokens and ep_scout$answer is NULL: the
#   budget ran out before final_answer. Read the trajectory before raising
#   the budget: often the agent repeats the same search.
# - The cache: a replayed episode reports cost 0 (nothing was billed). To
#   measure cost again, run once with use_cache("off") or ("record").
# - No abstract: some publishers do not share abstracts with OpenAlex or
#   Crossref, so check_claims() has no source text (abstracts is NA).


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. Who did the counting? prisma_counts() derives every number from R
#    objects. An agent asked to "report PRISMA counts" writes numbers; code
#    computes them. Which belongs in a methods section?
# 2. The agent had tool_verify_citations() and was told to use it. Did it,
#    on every DOI (look at the trajectory)? Why verify again in code anyway?
# 3. Recall versus precision: five databases plus snowballing found far more
#    records than the scout reported. What did the scout's 8 records miss, and
#    is a scout a search strategy or a starting point for one?
# 4. Reproducibility: databases change daily. Record the search date, the
#    queries, the filters and the package version (the episode does), and keep
#    the raw results (keep()).
# 5. check_claims() step 1 is free and catches invented or altered quotes;
#    step 2 costs a model call and catches real quotes used for the wrong
#    claim. Where in your workflow is each one worth its cost?
# 6. Terms of use: Semantic Scholar asks for attribution; arXiv asks for slow,
#    cached access; OpenAlex data are CC0. Read the terms of every API an
#    agent uses on your behalf.
