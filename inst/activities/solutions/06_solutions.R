# Solutions: Activity 06, Measurement Finder
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Run 06_Measurement_Finder.R first: these solutions use its objects
# (finder, finder_budget, instrument_schema, allowed_domains, answer_table,
# instruments, sources, page_or_na).

library(researchAgentR)
use_cache("auto")


# ==============================================================================
# Exercise 1: another construct
# ==============================================================================

task_ai <- paste(
  "Find up to 3 validated self-report instruments that measure AI anxiety",
  "(university students). For each, report the fields of the final answer.")
ep_ai <- keep("act06_ex1_finder_ai_anxiety", run(finder, task_ai, budget = finder_budget))
ep_ai$stop_reason
ai <- answer_table(ep_ai$answer$instruments)
ai[, c("instrument", "population", "doi")]

# The same verification as Part 3, written as one function so it can be reused.
verify_instruments <- function(tab, topic_pattern) {
  has_doi <- !is.na(normalize_doi(tab$doi))
  tab$doi_status <- "no DOI"
  tab$doi_on_topic <- NA
  if (any(has_doi)) {
    chk <- verify_citations(tab$doi[has_doi])
    tab$doi_status[has_doi] <- chk$status
    tab$doi_on_topic[has_doi] <- grepl(topic_pattern, chk$matched_title, ignore.case = TRUE)
  }
  src <- vapply(seq_len(nrow(tab)), function(i) {
    parts <- c(page_or_na(tab$source_url[i]),
               if (has_doi[i]) tryCatch(get_work(tab$doi[i])$abstract, error = function(e) NA_character_))
    parts <- parts[!is.na(parts)]
    if (length(parts)) paste(parts, collapse = "\n\n") else NA_character_
  }, character(1))
  names(src) <- paste0("entry_", seq_len(nrow(tab)))
  claims <- data.frame(claim = tab$reliability, quote = tab$reliability_quote, source = names(src))
  judged <- check_claims(claims, src[!is.na(src)], model = "groq:openai/gpt-oss-20b")
  tab$quote_status <- judged$status
  tab$verified <- judged$status == "supported" & tab$doi_status %in% c("verified", "no DOI") &
    !(tab$doi_on_topic %in% FALSE)
  tab
}
ai_checked <- keep("act06_ex1_verified", verify_instruments(ai, "anxiety|artificial intelligence|\\bAI\\b"))
ai_checked[, c("instrument", "doi_status", "quote_status", "verified")]
sum(ai_checked$verified)


# ==============================================================================
# Exercise 2: restrict READING to the allow list (offline test)
# ==============================================================================

read_page_allowed <- function(url) {
  inside <- filter_domains(data.frame(url = url), include_domains = allowed_domains)
  if (nrow(inside) == 0) {
    stop("this address is outside the allowed sites (", paste(allowed_domains, collapse = ", "),
         "). Choose a result from web_search instead.")
  }
  p <- read_page(url, max_chars = 4000)
  paste0("Title: ", p$title, "\nURL: ", p$url, "\n\n", p$text)
}
tool_read_allowed <- define_tool(
  read_page_allowed, name = "read_page",
  description = paste("Read one web page from the allowed sites and return its text.",
                      "Pages outside the allowed sites are refused."),
  args = list(url = arg_string("The full address, starting with https://")),
  max_output_tokens = 1200, untrusted = TRUE)

# The refusal happens before any download, so this test needs no network:
call_tool("read_page", list(tool_read_allowed),
          arguments = list(url = "https://www.example-scales-shop.com/loneliness"))$output
# "Error while running read_page: this address is outside the allowed sites (...)"
# The model reads this as an observation and can choose another page.

# Use it in place of tool_read_page():
finder_strict <- agent(
  name = "measurement_finder_strict", instructions = finder$instructions,
  model = finder$model,
  tools = list(tool_openalex(n = 5), tool_web_search(n = 5, include_domains = allowed_domains),
               tool_read_allowed, tool_verify_citations()),
  output_schema = instrument_schema, effort = "low")
finder_strict


# ==============================================================================
# Exercise 3: which fields does the agent leave empty? (offline)
# ==============================================================================

missing_report <- function(tab) {
  empty <- vapply(tab, function(x) {
    x <- as.character(x)
    sum(is.na(x) | !nzchar(trimws(x)) | grepl("^not stated$", trimws(x), ignore.case = TRUE))
  }, numeric(1))
  out <- data.frame(field = names(empty), empty = as.integer(empty), of = nrow(tab))
  out[order(-out$empty), ]
}
missing_report(instruments[, names(instrument_schema$instruments$items$fields)])


# ==============================================================================
# Exercise 4: does n_items agree with the source text?
# ==============================================================================

items_stated <- function(text) {
  if (is.na(text)) return(NA_integer_)
  m <- regmatches(text, gregexpr("\\b[0-9]{1,3}(-| )items?\\b", text, ignore.case = TRUE))[[1]]
  if (!length(m)) return(NA_integer_)
  as.integer(sub("^([0-9]+).*$", "\\1", m[1]))   # the first number stated
}
check_items <- data.frame(
  instrument = instruments$instrument,
  agent_n_items = instruments$n_items,
  source_n_items = vapply(sources, items_stated, integer(1)))
check_items$agree <- check_items$agent_n_items == check_items$source_n_items
check_items[instruments$verified, ]
# NA in source_n_items means the text states no "<number> items" (or the
# source could not be read); a disagreement can also mean the page describes
# a short form and a long form: read the source.
