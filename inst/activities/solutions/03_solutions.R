# Solutions: Activity 03, Literature Scout and Verification
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Run 03_Literature_Scout_and_Verification.R first: these solutions use its
# objects (all_records, deduped, scout, scout_schema, scout_budget, checked,
# answer_table). With use_cache("auto") and keep(), rerunning that script
# replays the recorded responses without new API calls.

library(researchAgentR)
use_cache("auto")


# ==============================================================================
# Exercise 1: how many DOIs needed cleaning? (no new API calls)
# ==============================================================================

raw <- all_records$doi
clean <- normalize_doi(raw)
has_doi <- !is.na(raw) & !is.na(clean)
needed_cleaning <- has_doi & raw != clean
sum(needed_cleaning)
table(database = all_records$database[needed_cleaning])
# Which forms appeared? (prefix, upper case, trailing punctuation)
head(unique(raw[needed_cleaning]))
# Raw values that are not DOIs at all (they became NA):
head(unique(raw[!is.na(raw) & is.na(clean)]))


# ==============================================================================
# Exercise 2: the pipeline for your own topic (OpenAlex + Crossref)
# ==============================================================================

my_topic <- "loneliness intervention older adults"   # replace with your topic
my_oa <- search_openalex(my_topic, n = 20, year_from = 2020)
my_cr <- search_crossref(my_topic, n = 20, filter = "from-pub-date:2020-01-01")

common <- c("title", "year", "doi", "authors", "source", "abstract", "oa_url", "id", "database")
my_all <- rbind(my_oa[, common], my_cr[, common])
my_dedup <- dedupe_records(my_all)

# A screening rule of your own, written as code (and therefore reportable):
my_text <- paste(my_dedup$title, ifelse(is.na(my_dedup$abstract), "", my_dedup$abstract))
my_screen <- grepl("loneli", my_text, ignore.case = TRUE) &
  grepl("random|trial|intervention", my_text, ignore.case = TRUE)
my_screen[!my_screen & is.na(my_dedup$abstract)] <- NA   # a human decides
prisma_counts(list(OpenAlex = my_oa, Crossref = my_cr), my_dedup, screening = my_screen)


# ==============================================================================
# Exercise 3: evidence quotes for every why_relevant, checked in code
# ==============================================================================

schema_q <- scout_schema
schema_q$records <- arg_array(
  "The relevant works found, most relevant first (at most 8).",
  items = arg_object("One work.", fields = list(
    title          = arg_string("Title exactly as in the search results."),
    year           = arg_integer("Publication year."),
    doi            = arg_string("DOI exactly as in the search results, in the form 10.xxxx/yyyy."),
    why_relevant   = arg_string("One sentence: why this work matters for the topic."),
    evidence_quote = arg_string("A verbatim sentence copied from the abstract shown in the search results that supports why_relevant."))))

scout_q <- agent(
  name = "literature_scout_quotes",
  instructions = paste(scout$instructions,
    "6. For every work, copy one sentence of its abstract WORD FOR WORD into evidence_quote."),
  model = scout$model, tools = scout$tools, output_schema = schema_q, effort = "low")

ep_q <- keep("act03_ex3_scout_quotes", run(scout_q, scout_task, budget = scout_budget))
ep_q$stop_reason
rec_q <- answer_table(ep_q$answer$records)

# Sources: the abstract of each reported DOI (OpenAlex lookups by DOI are free).
abstract_or_na <- function(doi) {
  tryCatch(get_work(doi)$abstract, error = function(e) NA_character_)
}
src <- vapply(rec_q$doi, abstract_or_na, character(1))
names(src) <- rec_q$doi
sum(is.na(src))   # works without an abstract cannot be checked

claims_q <- data.frame(claim = rec_q$why_relevant, quote = rec_q$evidence_quote,
                       source = rec_q$doi)
quote_check <- check_claims(claims_q, src[!is.na(src)])
table(quote_check$status)
quote_check[quote_check$status != "quote_found", c("claim", "reason")]
# Records whose DOI had no abstract are reported as "source ... is not among
# the sources provided": counted, never treated as found.


# ==============================================================================
# Exercise 4: which citations must a person read by hand?
# ==============================================================================

flag_citations <- function(checked, min_similarity = 0.9) {
  weak_title <- checked$status == "verified" & !is.na(checked$title_similarity) &
    checked$title_similarity < min_similarity
  flag <- checked$status != "verified" | weak_title
  out <- checked[flag, c("doi", "status", "title_similarity", "detail", "matched_title")]
  out$why <- ifelse(out$status != "verified", paste("status:", out$status),
                    "verified, but the title is not a close match")
  rownames(out) <- NULL
  out
}
flag_citations(checked)
# The wrong-year test row and the invented DOI are always flagged; any scout
# row that appears here is a finding about the agent.
