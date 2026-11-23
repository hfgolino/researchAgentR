# Offline tests: request building, input checks, parsers and tool schemas.
# Parsers are tested on small fragments whose field names come from the
# official documentation (URLs in each test). Live tests query the real APIs
# (no keys needed) and are skipped without a network.

test_that("NIH criteria follow the RePORTER v2 field names", {
  # https://api.reporter.nih.gov/ (Project search parameters)
  cr <- .nih_criteria("loneliness aging", fiscal_years = 2024:2025, agencies = c("nia", "nimh"),
                      pi_names = "Golino")
  expect_equal(cr$advanced_text_search$operator, "and")
  expect_equal(cr$advanced_text_search$search_text, "loneliness aging")
  expect_equal(cr$fiscal_years, list(2024L, 2025L))
  expect_equal(cr$agencies, list("NIA", "NIMH"))
  expect_equal(cr$pi_names, list(list(any_name = "Golino")))
  json <- as.character(.to_json(list(criteria = .nih_criteria(fiscal_years = 2025))))
  expect_equal(json, "{\"criteria\":{\"fiscal_years\":[2025]}}")   # a one-year array, not a scalar
  extra <- .nih_criteria("x", org_names = list("UNIVERSITY OF VIRGINIA"))
  expect_equal(extra$org_names, list("UNIVERSITY OF VIRGINIA"))
  expect_length(.nih_criteria(), 0)
  # project_nums is an array even for one number; "*" is the API's wildcard
  expect_equal(.nih_criteria(project_nums = "*R01MH000001*")$project_nums, list("*R01MH000001*"))
  expect_equal(as.character(.to_json(.nih_criteria(project_nums = list("5R01MH000001-02")))),
               "{\"project_nums\":[\"5R01MH000001-02\"]}")
})

test_that("NIH searches check their inputs before any request", {
  expect_error(search_nih_projects(), "at least one")
  expect_error(search_nih_projects("x", n = 501), "between 1 and 500")
  expect_error(search_nih_projects("x", offset = 15000), "14,999")
})

test_that("an NIH project record becomes one row", {
  # Field names from "Data Elements for RePORTER Project API V2":
  # https://api.reporter.nih.gov/documents/Data%20Elements%20for%20RePORTER%20Project%20API_V2.pdf
  p <- list(appl_id = 1, project_num = "R01AG000001-01", project_title = "A title",
            contact_pi_name = "DOE, JANE", organization = list(org_name = "SOME UNIVERSITY"),
            fiscal_year = 2025, award_amount = 456789,
            agency_ic_admin = list(abbreviation = "NIA", name = "National Institute on Aging"),
            project_start_date = "2025-09-01T00:00:00Z", project_end_date = "2030-08-31T00:00:00Z",
            abstract_text = "An abstract.")
  r <- .nih_row(p)
  expect_equal(r$project_num, "R01AG000001-01")
  expect_equal(r$organization, "SOME UNIVERSITY")
  expect_equal(r$agency, "NIA")
  expect_equal(r$start_date, "2025-09-01")
  expect_equal(r$award_amount, 456789)
  expect_equal(r$url, "https://reporter.nih.gov/project-details/1")
  # Missing fields become NA, never an error.
  empty <- .nih_row(list())
  expect_true(all(is.na(empty$title), is.na(empty$award_amount), is.na(empty$url)))
  expect_equal(names(empty), names(.empty_nih()))
})

test_that("NSF dates are converted to mm/dd/yyyy", {
  expect_equal(.nsf_date("2025-01-31"), "01/31/2025")
  expect_equal(.nsf_date(as.Date("2024-12-01")), "12/01/2024")
  expect_equal(.nsf_date("03/15/2023"), "03/15/2023")
  expect_null(.nsf_date(NULL))
  expect_error(.nsf_date("last year"), "Dates must look like")
  expect_error(search_nsf_awards(""), "non-empty")
})

test_that("an NSF award becomes one row", {
  # printFields names from https://resources.research.gov/common/webapi/awardapisearch-v1.htm
  a <- list(id = "2400001", title = "A title", pdPIName = "Jane Doe", awardeeName = "Some University",
            agency = "NSF", startDate = "09/01/2024", expDate = "08/31/2027",
            fundsObligatedAmt = "350000")
  r <- .nsf_row(a)
  expect_equal(r$amount, 350000)
  expect_equal(r$url, "https://www.nsf.gov/awardsearch/showAward?AWD_ID=2400001")
  expect_true(is.na(r$abstract))
})

test_that("Grants.gov errors are reported, successes pass", {
  # Response envelope from https://www.grants.gov/api/common/search2
  expect_silent(.grants_check(list(errorcode = 0, msg = "Webservice Succeeds")))
  expect_error(.grants_check(list(errorcode = 1, msg = "Bad request")), "Bad request")
  expect_error(search_grants_gov("x", status = "open"), "should be one of")
})

test_that("funding tools have the expected schemas", {
  nih <- tool_nih_reporter(n = 5, agencies = "NIA")
  expect_equal(nih$name, "nih_reporter")
  expect_equal(nih$schema$required, list("text"))
  expect_equal(nih$schema$properties$years$items$type, "integer")
  expect_true(nih$untrusted)
  nsf <- tool_nsf_awards()
  expect_equal(nsf$schema$required, list("keyword"))
  gg <- tool_grants_gov()
  expect_length(gg$schema$required, 0)
  out <- call_tool("grants_gov", list(gg), list())
  expect_true(out$is_error)
  expect_match(out$output, "keyword or an opportunity_id")
})

test_that("tool text is short and the data frame is kept", {
  res <- .empty_nih()
  attr(res, "total") <- 0
  v <- .format_nih(res)
  expect_match(v, "No matching projects")
  expect_s3_class(attr(v, "data"), "data.frame")
})

# ---- Live tests ------------------------------------------------------------

test_that("NIH RePORTER search works (live)", {
  skip_on_cran()
  skip_if_offline("api.reporter.nih.gov")
  res <- search_nih_projects("exploratory graph analysis", n = 5)
  expect_true(all(c("project_num", "title", "award_amount", "url") %in% names(res)))
  expect_lte(nrow(res), 5)
  expect_false(is.na(attr(res, "total")))
})

test_that("NSF award search works (live)", {
  skip_on_cran()
  skip_if_offline("api.nsf.gov")
  res <- search_nsf_awards("psychometrics", n = 3)
  expect_lte(nrow(res), 3)
  expect_true(all(c("id", "title", "amount") %in% names(res)))
})

test_that("Grants.gov search and fetch work (live)", {
  skip_on_cran()
  skip_if_offline("api.grants.gov")
  res <- search_grants_gov("health", n = 3)
  expect_lte(nrow(res), 3)
  skip_if(nrow(res) == 0)
  d <- grants_gov_opportunity(res$id[1])
  expect_equal(nrow(d), 1)
  expect_false(is.na(d$title))
})

test_that("an NSF search of several words keeps the awards that contain every word", {
  # NSF returns awards with ANY of the words. The API itself is replaced here
  # (made-up awards in the documented shape) to check the two steps: ask for
  # the rarest word, then keep the awards that contain the others.
  award <- function(id, title, abstract) list(id = id, title = title, abstractText = abstract)
  pool <- list(
    award("1", "Psychometric models with machine learning", "A study of test scores."),
    award("2", "Psychometrics of reading tests", "No learning algorithms here."),
    award("3", "Machine learning for geology", "Rocks."),
    award("4", "Deep learning and measurement", "Machine-scored essays; psychometric validity."))
  asked <- character(0)
  testthat::local_mocked_bindings(.nsf_page = function(query, size, offset) {
    asked <<- c(asked, query$keyword)
    hits <- Filter(function(a) grepl(tolower(query$keyword), tolower(paste(a$title, a$abstractText)), fixed = TRUE), pool)
    total <- c(psychometric = 3, machine = 3000, learning = 9000)[[query$keyword]]
    list(awards = if (offset > length(hits)) list() else utils::head(hits[offset:length(hits)], size), total = total)
  })
  res <- suppressMessages(search_nsf_awards("psychometric machine learning", n = 5))
  expect_equal(asked[1:3], c("psychometric", "machine", "learning"))        # one count per word
  expect_true(all(asked[-(1:3)] == "psychometric"))                         # then the rarest word only
  expect_equal(res$id, c("1", "4"))                                         # every word, in title or abstract
  expect_equal(attr(res, "match"), "all"); expect_equal(attr(res, "scanned"), 3)
  expect_message(search_nsf_awards("psychometric machine learning", n = 5), "2 of the 3 awards read")
  expect_equal(.nsf_words("theory of mind in  children"), c("theory", "mind", "children"))
  expect_true(.has_all_words("Psychometrics and Machine Learning", c("psychometric", "machine")))
  expect_false(.has_all_words(NA, "x"))
})

test_that("an NSF search of one word, a quoted phrase or match = 'any' is sent as it is", {
  sent <- list()
  testthat::local_mocked_bindings(.nsf_page = function(query, size, offset) {
    sent[[length(sent) + 1]] <<- c(query["keyword"], size = size)
    list(awards = list(), total = 0)
  })
  search_nsf_awards("psychometrics", n = 3)
  search_nsf_awards("\"measurement invariance\"", n = 3)
  search_nsf_awards("measurement invariance", n = 3, match = "any")
  search_nsf_awards("measurement invariance", n = 3, match = "phrase")
  expect_equal(vapply(sent, `[[`, "", "keyword"),
               c("psychometrics", "\"measurement invariance\"", "measurement invariance", "\"measurement invariance\""))
  expect_true(all(vapply(sent, `[[`, 0, "size") == 3))                      # one request each, of n awards
})

test_that("Grants.gov dates are read as yyyy-mm-dd, from either form the API gives", {
  # Field names and formats as returned by fetchOpportunity
  # (https://www.grants.gov/api/common/fetchopportunity): each date comes as
  # text and, with "Str" added to the name, in a sortable form.
  s <- list(responseDate = "Oct 15, 2026 12:00:00 AM EDT", responseDateStr = "2026-10-15-00-00-00",
            postingDate = "May 28, 2026 12:00:00 AM EDT")
  expect_equal(.grants_date(s, "responseDate"), "2026-10-15")
  expect_equal(.grants_date(s, "postingDate"), "2026-05-28")          # from the text form
  expect_true(is.na(.grants_date(s, "archiveDate")))
  expect_true(is.na(.grants_date(list(responseDate = "to be announced"), "responseDate")))
})
