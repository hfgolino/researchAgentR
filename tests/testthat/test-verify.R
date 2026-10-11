# Offline tests cover the comparisons done in code. Live tests (Crossref,
# a Groq model) are skipped without internet or a key.

test_that("title similarity is a token-set Jaccard index after normalization", {
  expect_equal(.title_similarity("Exploratory Graph Analysis", "exploratory graph analysis."), 1)
  expect_equal(.title_similarity("graph analysis", "exploratory graph analysis"), 2 / 3)
  expect_equal(.title_similarity("Psychom\u00e9trie", "psychometrie"), 1)   # accents removed
  expect_equal(.title_similarity("abc", "xyz"), 0)
  expect_true(is.na(.title_similarity(NA, "x")))
  expect_length(.title_similarity(c("a b", "c"), c("a b", "d")), 2)
})

test_that("first-author surnames are extracted and matched", {
  expect_equal(.first_surname(c("Golino, H. F.; Epskamp, S.", "Hudson Golino", NA)),
               c("Golino", "Golino", NA))
  expect_true(.surname_match("golino", "Golino"))
  expect_true(.surname_match("Garrido", "Garrido Rodr\u00edguez"))
  expect_false(.surname_match("Smith", "Golino"))
  expect_true(is.na(.surname_match(NA, "Golino")))
})

test_that("citation input accepts DOIs or a data frame", {
  x <- .citation_input(c("10.1/a", "10.1/b"))
  expect_equal(names(x), c("doi", "title", "first_author", "year"))
  y <- .citation_input(data.frame(doi = "10.1/a", authors = "Golino, H.; Epskamp, S.", year = "2020"))
  expect_equal(y$first_author, "Golino")
  expect_equal(y$year, 2020L)
  expect_error(.citation_input(data.frame(title = "x")), "doi")
})

test_that("invalid DOIs are reported as not_found without any request", {
  v <- verify_citations(c("not a doi", "doi:"))
  expect_equal(v$status, c("not_found", "not_found"))
  expect_equal(v$found, c(FALSE, FALSE))
  expect_equal(as.integer(attr(v, "counts")["not_found"]), 2L)
  expect_true(all(c("doi", "found", "title_similarity", "author_match", "year_match",
                    "retracted", "status", "detail") %in% names(v)))
})

test_that("retraction notices are read from update-to entries", {
  # Shape from https://www.crossref.org/blog/retraction-watch-retractions-now-in-the-crossref-api/
  # (a notice whose update-to lists the retracted work, one entry per source).
  items <- list(list(DOI = "10.1000/notice", `update-to` = list(
    list(DOI = "10.1000/ORIGINAL", type = "retraction", source = "publisher", label = "Retraction"),
    list(DOI = "10.1000/original", type = "retraction", source = "retraction-watch",
         label = "Retraction", `record-id` = 123L),
    list(DOI = "10.1000/other", type = "correction", source = "publisher"))))
  u <- .update_entries(items, "10.1000/original")
  expect_length(u, 2)
  expect_equal(u[[1]]$notice, "10.1000/notice")
  expect_equal(vapply(u, `[[`, character(1), "source"), c("publisher", "retraction-watch"))
  expect_true(grepl(.retraction_pattern, "retraction"))
  expect_false(grepl(.retraction_pattern, "correction"))
})

test_that("quotes are matched verbatim, ignoring typography and spacing", {
  src <- "Exploratory graph analysis (EGA) estimated the number of\n  dimensions \u2014 more accurately."
  expect_true(.quote_in_source("EGA) estimated the number of dimensions - more accurately", src))
  expect_true(.quote_in_source("\u201cexploratory graph analysis ... more accurately\u201d", src))
  expect_true(.quote_in_source("Exploratory graph analysis [...] dimensions", src))
  expect_false(.quote_in_source("more accurately ... exploratory graph analysis", src))  # order
  expect_false(.quote_in_source("EGA always recovers the structure", src))
  expect_false(.quote_in_source("", src))
  expect_false(.quote_in_source(NA, src))
  expect_equal(.normalize_quote("It\u2019s \u201cX\u201d \u2013 Y"), "it's \"x\" - y")
})

test_that("check_claims() without a model checks quotes only", {
  src <- c(p1 = "EGA estimated the number of dimensions more accurately than parallel analysis.")
  claims <- data.frame(
    claim = c("EGA beat parallel analysis.", "EGA is perfect.", "Unknown source."),
    quote = c("more accurately than parallel analysis", "EGA is always right", "anything"),
    source = c("p1", "p1", "p9"))
  out <- check_claims(claims, src)
  expect_equal(out$status, c("quote_found", "quote_not_found", "quote_not_found"))
  expect_equal(out$quote_found, c(TRUE, FALSE, FALSE))
  expect_true(all(is.na(out$supported)))
  expect_match(out$reason[3], "not among the sources")
  expect_equal(attr(out, "cost"), 0)
  expect_error(check_claims(data.frame(claim = "x"), src), "columns")
  expect_error(check_claims(claims, "unnamed text"), "named")
})

test_that("the claim-judging schema is a valid strict JSON Schema", {
  s <- .claim_schema
  expect_equal(unlist(s$required), names(s$properties))
  expect_false(s$additionalProperties)
  expect_equal(unlist(s$properties$verdict$enum), c("supported", "not_supported", "unclear"))
})

test_that("tool_verify_citations() has a valid schema", {
  t <- tool_verify_citations()
  expect_s3_class(t, "agent_tool")
  s <- jsonlite::fromJSON(tool_schema(t), simplifyVector = FALSE)
  expect_equal(s$name, "verify_citations")
  item <- s$parameters$properties$citations$items
  expect_equal(item$type, "object")
  expect_equal(unlist(item$required), "doi")
  # An invalid DOI is handled without a request, through the tool interface.
  out <- call_tool("verify_citations", list(t),
                   arguments = list(citations = list(list(doi = "nonsense"))))
  expect_false(out$is_error)
  expect_match(out$output, "NOT_FOUND")
  expect_s3_class(out$value, "data.frame")
})

# ---- Live tests ------------------------------------------------------------------

test_that("verify_citations() checks real and invented DOIs live", {
  skip_on_cran()
  skip_if_offline("api.crossref.org")
  refs <- data.frame(
    doi = c("10.1037/met0000255", "10.9999/this.doi.does.not.exist.2024"),
    title = c("Investigating the performance of exploratory graph analysis and traditional techniques to identify the number of latent factors: A simulation and tutorial",
              "A made-up paper"),
    first_author = c("Golino", "Smith"), year = c(2020, 2024))
  v <- verify_citations(refs)
  expect_equal(v$status[2], "not_found")
  expect_true(v$found[1])
  expect_true(v$author_match[1])
  expect_gt(v$title_similarity[1], 0.75)
  expect_false(v$retracted[1])
})

test_that("check_claims() asks a model whether quotes support claims", {
  skip_on_cran()
  skip_if_offline("api.groq.com")
  skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")), "GROQ_API_KEY not set")
  src <- c(p1 = "In most simulated conditions, EGA estimated the number of dimensions more accurately than parallel analysis.")
  claims <- data.frame(
    claim = c("EGA was more accurate than parallel analysis in most simulated conditions.",
              "EGA is always more accurate than every other method."),
    quote = c("In most simulated conditions, EGA estimated the number of dimensions more accurately than parallel analysis",
              "EGA estimated the number of dimensions more accurately than parallel analysis"),
    source = "p1")
  out <- check_claims(claims, src, model = "groq:openai/gpt-oss-20b")
  expect_equal(out$status[1], "supported")
  expect_true(out$status[2] %in% c("not_supported", "unclear"))
  expect_gte(attr(out, "cost"), 0)
})

test_that("title similarity also compares main titles without subtitles", {
  s <- researchAgentR:::.title_similarity(
    c("Entropy fit indices", "A different paper"),
    c("Entropy Fit Indices: New Fit Measures for Assessing Dimensionality", "Entropy Fit Indices: New Fit Measures"))
  expect_equal(s[1], 1)
  expect_lt(s[2], 0.5)
})
