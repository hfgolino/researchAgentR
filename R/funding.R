# Funding tools: NIH RePORTER (funded NIH projects), the NSF Award Search API
# (funded NSF awards) and Grants.gov (open funding opportunities). All three
# are free and need no key. Each direct function returns a data frame; each
# tool_*() gives the model a short numbered list and keeps the data frame.

# ---- NIH RePORTER ----------------------------------------------------------------

# Build the "criteria" object of a RePORTER project search. Kept separate from
# the request so it can be read, printed and tested without the network.
# Field names follow the RePORTER API v2 documentation:
# https://api.reporter.nih.gov/ (Project search parameters).
.nih_criteria <- function(text = NULL, fiscal_years = NULL, agencies = NULL,
                          pi_names = NULL, ...) {
  criteria <- list()
  if (length(text) && nzchar(text)) {
    # Search the title, the scientific terms and the abstract, requiring
    # every word ("and"), as the RePORTER web site does by default.
    criteria$advanced_text_search <- list(operator = "and",
                                          search_field = "projecttitle,terms,abstracttext",
                                          search_text = text)
  }
  if (length(fiscal_years)) criteria$fiscal_years <- as.list(as.integer(fiscal_years))
  if (length(agencies)) criteria$agencies <- as.list(toupper(agencies))
  if (length(pi_names)) {
    # Name fields accept partial names (wildcards are on by default).
    criteria$pi_names <- lapply(pi_names, function(p) list(any_name = p))
  }
  c(criteria, list(...))
}

# The fields requested from RePORTER (names from "Data Elements for RePORTER
# Project API V2", https://api.reporter.nih.gov/documents/Data%20Elements%20for%20RePORTER%20Project%20API_V2.pdf).
.nih_fields <- c("ApplId", "ProjectNum", "ProjectTitle", "ContactPiName",
                 "PrincipalInvestigators", "Organization", "FiscalYear", "AwardAmount",
                 "AgencyIcAdmin", "ProjectStartDate", "ProjectEndDate", "AbstractText",
                 "ProjectDetailUrl")

.empty_nih <- function() {
  data.frame(project_num = character(0), title = character(0), pi = character(0),
             organization = character(0), fiscal_year = integer(0),
             award_amount = numeric(0), agency = character(0), start_date = character(0),
             end_date = character(0), abstract = character(0), url = character(0),
             appl_id = character(0), stringsAsFactors = FALSE)
}

.nih_row <- function(p) {
  pis <- p$principal_investigators %||% list()
  pi_all <- vapply(pis, .field, character(1), "full_name", default = NA_character_)
  appl_id <- as.character(.field(p, "appl_id", default = NA_character_))
  url <- .field(p, "project_detail_url", default = NA_character_)
  if (is.na(url) && !is.na(appl_id)) url <- paste0("https://reporter.nih.gov/project-details/", appl_id)
  data.frame(
    project_num = as.character(.field(p, "project_num", default = NA_character_)),
    title = as.character(.field(p, "project_title", default = NA_character_)),
    pi = as.character(.field(p, "contact_pi_name",
                             default = if (length(pi_all)) paste(pi_all, collapse = "; ") else NA_character_)),
    organization = as.character(.field(p, "organization", "org_name", default = NA_character_)),
    fiscal_year = as.integer(.field(p, "fiscal_year", default = NA_integer_)),
    award_amount = as.numeric(.field(p, "award_amount", default = NA_real_)),
    agency = as.character(.field(p, "agency_ic_admin", "abbreviation", default = NA_character_)),
    start_date = substr(as.character(.field(p, "project_start_date", default = NA_character_)), 1, 10),
    end_date = substr(as.character(.field(p, "project_end_date", default = NA_character_)), 1, 10),
    abstract = as.character(.field(p, "abstract_text", default = NA_character_)),
    url = as.character(url), appl_id = appl_id, stringsAsFactors = FALSE)
}

#' Search funded NIH projects (NIH RePORTER)
#'
#' Queries the NIH RePORTER API v2 (`POST
#' https://api.reporter.nih.gov/v2/projects/search`), which needs no key.
#' The text search covers project titles, scientific terms and abstracts and
#' requires every word. Use it to find funded projects similar to a draft
#' aim, the funding history of an investigator, or what an institute funds.
#'
#' Rules of use (<https://api.reporter.nih.gov/>): no more than one request
#' per second (enforced here), large jobs only on weekends or between 9 PM
#' and 5 AM Eastern time, at most 500 records per request and offsets up to
#' 14,999. NIH may block addresses that ignore these rules.
#'
#' @param text Words to search for in titles, terms and abstracts.
#' @param fiscal_years Optional integer vector, for example `2024:2025`.
#' @param agencies Optional vector of NIH institute or center abbreviations,
#'   for example `c("NIA", "NIMH")`.
#' @param pi_names Optional vector of investigator names (partial names
#'   match).
#' @param n Number of projects to return (at most 500 per request).
#' @param offset Number of records to skip, for paging (at most 14,999).
#' @param ... Other RePORTER criteria passed as they are, for example
#'   `org_names = list("UNIVERSITY OF VIRGINIA")`.
#' @return A data frame with `project_num`, `title`, `pi` (contact PI),
#'   `organization`, `fiscal_year`, `award_amount` (US dollars), `agency`
#'   (administering institute), `start_date`, `end_date`, `abstract`, `url`
#'   and `appl_id`, with attributes `total` (matching projects), `criteria`
#'   and `retrieved_at`.
#' @examples
#' \dontrun{
#' nih <- search_nih_projects("ecological momentary assessment loneliness",
#'                            fiscal_years = 2025, agencies = "NIA", n = 10)
#' nih[, c("project_num", "title", "award_amount")]
#' attr(nih, "total")
#' }
#' @export
search_nih_projects <- function(text = NULL, fiscal_years = NULL, agencies = NULL,
                                pi_names = NULL, n = 25, offset = 0, ...) {
  criteria <- .nih_criteria(text, fiscal_years, agencies, pi_names, ...)
  if (!length(criteria)) {
    .stop("Give at least one of `text`, `fiscal_years`, `agencies` or `pi_names`: ",
          "an empty search would ask RePORTER for every project.")
  }
  n <- as.integer(n)
  if (n < 1 || n > 500) .stop("`n` must be between 1 and 500 (the RePORTER limit per request).")
  if (offset > 14999) .stop("RePORTER does not accept offsets above 14,999.")
  body <- list(criteria = criteria, include_fields = as.list(.nih_fields),
               offset = as.integer(offset), limit = n)
  req <- .request("https://api.reporter.nih.gov/v2/projects/search", body = body,
                  auth = "none", realm = "nih_reporter", rpm = 60)
  r <- .perform(req)
  hits <- r$body$results %||% list()
  out <- if (length(hits)) do.call(rbind, lapply(hits, .nih_row)) else .empty_nih()
  attr(out, "total") <- as.numeric(.field(r$body, "meta", "total", default = NA_real_))
  attr(out, "criteria") <- criteria
  attr(out, "retrieved_at") <- .now()
  out
}

# ---- NSF Award Search ----------------------------------------------------------

# Base URL. The official page lists http://api.nsf.gov/services/v1/; https is
# used here and can be changed with options(researchAgentR.nsf_url = ...).
.nsf_url <- function() .opt("nsf_url", "https://api.nsf.gov/services/v1/awards.json")

# Output fields (printFields) listed in NSF's Award Search API examples.
.nsf_fields <- c("id", "title", "awardeeName", "pdPIName", "agency", "date", "startDate",
                 "expDate", "fundsObligatedAmt", "abstractText")

# NSF dates are written mm/dd/yyyy. Accept Date objects or "yyyy-mm-dd".
.nsf_date <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x) && grepl("^\\d{2}/\\d{2}/\\d{4}$", x)) return(x)
  d <- tryCatch(as.Date(x), error = function(e) NA)
  if (is.na(d)) .stop("Dates must look like \"2025-01-31\". Got: ", x)
  format(d, "%m/%d/%Y")
}

.nsf_row <- function(a) {
  id <- as.character(.field(a, "id", default = NA_character_))
  data.frame(
    id = id,
    title = as.character(.field(a, "title", default = NA_character_)),
    pi = as.character(.field(a, "pdPIName", default = NA_character_)),
    organization = as.character(.field(a, "awardeeName", default = NA_character_)),
    agency = as.character(.field(a, "agency", default = NA_character_)),
    award_date = as.character(.field(a, "date", default = NA_character_)),
    start_date = as.character(.field(a, "startDate", default = NA_character_)),
    end_date = as.character(.field(a, "expDate", default = NA_character_)),
    amount = suppressWarnings(as.numeric(.field(a, "fundsObligatedAmt", default = NA_real_))),
    abstract = as.character(.field(a, "abstractText", default = NA_character_)),
    url = if (is.na(id)) NA_character_ else paste0("https://www.nsf.gov/awardsearch/showAward?AWD_ID=", id),
    stringsAsFactors = FALSE)
}

#' Search funded NSF awards
#'
#' Queries the NSF Award Search API (`awards.json`), which needs no key. The
#' API returns at most 25 awards per request, so larger `n` values are
#' fetched page by page. Documentation:
#' <https://resources.research.gov/common/webapi/awardapisearch-v1.htm>.
#'
#' @param keyword Words to search for in award titles and abstracts.
#' @param n Number of awards to return.
#' @param date_start,date_end Optional range for the award start date, as
#'   `"yyyy-mm-dd"` or `Date` (sent as NSF's `startDateStart` and
#'   `startDateEnd`, in mm/dd/yyyy).
#' @return A data frame with `id`, `title`, `pi`, `organization`, `agency`,
#'   `award_date`, `start_date`, `end_date`, `amount` (funds obligated, US
#'   dollars), `abstract` and `url`, with attributes `total` (when the API
#'   reports it), `query` and `retrieved_at`.
#' @examples
#' \dontrun{
#' nsf <- search_nsf_awards("psychometrics machine learning", n = 10,
#'                          date_start = "2024-01-01")
#' nsf[, c("id", "title", "amount")]
#' }
#' @export
search_nsf_awards <- function(keyword, n = 25, date_start = NULL, date_end = NULL) {
  if (!is.character(keyword) || length(keyword) != 1 || !nzchar(keyword)) {
    .stop("`keyword` must be one non-empty string.")
  }
  rpp <- 25L                                  # the API's maximum per page
  query <- list(keyword = keyword, printFields = paste(.nsf_fields, collapse = ","),
                startDateStart = .nsf_date(date_start), startDateEnd = .nsf_date(date_end))
  query <- Filter(Negate(is.null), query)
  rows <- list()
  total <- NA_real_
  # NSF numbers records from 1: offset = 1 is the first award.
  for (page in seq_len(ceiling(n / rpp))) {
    q <- c(query, list(rpp = min(rpp, n - length(rows)), offset = (page - 1) * rpp + 1))
    req <- .request(.nsf_url(), auth = "none", query = q, realm = "nsf_awards", rpm = 60)
    r <- .perform(req)
    resp <- r$body$response %||% list()
    note <- resp$serviceNotification
    if (length(note)) .warn("NSF API: ", .as_text(note))
    total <- as.numeric(.field(resp, "metadata", "totalCount", default = total))
    awards <- resp$award %||% list()
    rows <- c(rows, lapply(awards, .nsf_row))
    if (length(awards) < q$rpp || length(rows) >= n) break
  }
  out <- if (length(rows)) do.call(rbind, rows) else .nsf_row(list())[0, ]
  rownames(out) <- NULL
  attr(out, "total") <- total
  attr(out, "query") <- query
  attr(out, "retrieved_at") <- .now()
  out
}

# ---- Grants.gov ------------------------------------------------------------------

.grants_check <- function(body) {
  code <- .field(body, "errorcode", default = 0)
  if (!identical(as.numeric(code), 0)) {
    .stop("Grants.gov returned error ", code, ": ", .field(body, "msg", default = "no message"))
  }
}

#' Search funding opportunities on Grants.gov
#'
#' `search_grants_gov()` searches open (or forecasted, closed, archived)
#' federal funding opportunities with the Grants.gov `search2` API;
#' `grants_gov_opportunity()` reads the full synopsis of one opportunity with
#' `fetchOpportunity`. Neither needs a key. Documentation:
#' <https://grants.gov/api/api-guide>,
#' <https://www.grants.gov/api/common/search2> and
#' <https://www.grants.gov/api/common/fetchopportunity>.
#'
#' @param keyword Words to search for.
#' @param n Number of opportunities to return.
#' @param status One or more of `"posted"`, `"forecasted"`, `"closed"`,
#'   `"archived"`.
#' @param agencies Optional agency code, for example `"HHS-NIH11"` or `"NSF"`.
#' @return `search_grants_gov()`: a data frame with `id`, `number`, `title`,
#'   `agency`, `agency_code`, `open_date`, `close_date`, `status`,
#'   `aln` (Assistance Listing numbers) and `url`, with attributes `total`
#'   and `retrieved_at`. `grants_gov_opportunity()`: a one-row data frame
#'   with `id`, `number`, `title`, `agency`, `posting_date`, `response_date`,
#'   `award_ceiling`, `award_floor`, `description` (plain text) and `url`.
#' @examples
#' \dontrun{
#' g <- search_grants_gov("mental health measurement", n = 10)
#' g[, c("number", "title", "close_date")]
#' grants_gov_opportunity(g$id[1])
#' }
#' @export
search_grants_gov <- function(keyword, n = 25, status = "posted", agencies = NULL) {
  status <- match.arg(status, c("posted", "forecasted", "closed", "archived"), several.ok = TRUE)
  body <- list(keyword = keyword, rows = as.integer(n),
               oppStatuses = paste(status, collapse = "|"))
  if (length(agencies)) body$agencies <- paste(agencies, collapse = "|")
  req <- .request("https://api.grants.gov/v1/api/search2", body = body, auth = "none",
                  realm = "grants_gov", rpm = 60)
  r <- .perform(req)
  .grants_check(r$body)
  hits <- r$body$data$oppHits %||% list()
  out <- if (length(hits)) {
    data.frame(
      id = vapply(hits, function(h) as.character(.field(h, "id")), character(1)),
      number = vapply(hits, .field, character(1), "number", default = NA_character_),
      title = vapply(hits, .field, character(1), "title", default = NA_character_),
      agency = vapply(hits, .field, character(1), "agencyName", default = NA_character_),
      agency_code = vapply(hits, .field, character(1), "agencyCode", default = NA_character_),
      open_date = vapply(hits, .field, character(1), "openDate", default = NA_character_),
      close_date = vapply(hits, .field, character(1), "closeDate", default = NA_character_),
      status = vapply(hits, .field, character(1), "oppStatus", default = NA_character_),
      aln = vapply(hits, .field, character(1), "alnist", default = NA_character_),
      stringsAsFactors = FALSE)
  } else {
    data.frame(id = character(0), number = character(0), title = character(0),
               agency = character(0), agency_code = character(0), open_date = character(0),
               close_date = character(0), status = character(0), aln = character(0))
  }
  out$close_date[!is.na(out$close_date) & out$close_date == ""] <- NA_character_
  out$url <- if (nrow(out)) paste0("https://www.grants.gov/search-results-detail/", out$id) else character(0)
  attr(out, "total") <- as.numeric(.field(r$body, "data", "hitCount", default = NA_real_))
  attr(out, "retrieved_at") <- .now()
  out
}

#' @rdname search_grants_gov
#' @param id The opportunity id (the `id` column of `search_grants_gov()`).
#' @export
grants_gov_opportunity <- function(id) {
  req <- .request("https://api.grants.gov/v1/api/fetchOpportunity",
                  body = list(opportunityId = as.integer(id)), auth = "none",
                  realm = "grants_gov", rpm = 60)
  r <- .perform(req)
  .grants_check(r$body)
  d <- r$body$data %||% list()
  s <- d$synopsis %||% list()
  desc <- .field(s, "synopsisDesc", default = NA_character_)
  # The synopsis is HTML; the model and the researcher both want plain text.
  if (!is.na(desc)) desc <- as.character(html_to_text(paste0("<body>", desc, "</body>")))
  data.frame(
    id = as.character(.field(d, "id", default = id)),
    number = as.character(.field(d, "opportunityNumber", default = NA_character_)),
    title = as.character(.field(d, "opportunityTitle", default = NA_character_)),
    agency = as.character(.field(s, "agencyName", default = NA_character_)),
    posting_date = as.character(.field(s, "postingDate", default = NA_character_)),
    response_date = as.character(.field(s, "responseDateDesc", default = NA_character_)),
    award_ceiling = suppressWarnings(as.numeric(.field(s, "awardCeiling", default = NA_real_))),
    award_floor = suppressWarnings(as.numeric(.field(s, "awardFloor", default = NA_real_))),
    description = desc,
    url = paste0("https://www.grants.gov/search-results-detail/", id),
    stringsAsFactors = FALSE)
}

# ---- Tools ---------------------------------------------------------------------

.money <- function(x) ifelse(is.na(x), "amount n/a", paste0("$", format(round(x), big.mark = ",", trim = TRUE)))

#' Funding tools for agents
#'
#' Tools for a funding scout: `tool_nih_reporter()` searches funded NIH
#' projects ([search_nih_projects()]), `tool_nsf_awards()` funded NSF awards
#' ([search_nsf_awards()]) and `tool_grants_gov()` open opportunities on
#' Grants.gov ([search_grants_gov()], [grants_gov_opportunity()]). The model
#' sees a short numbered list; the full data frame is kept in
#' `attr(value, "data")` of the tool result. All three services are free and
#' need no key.
#'
#' @param n Number of records the model sees per call. Small values keep the
#'   context (and the cost) small.
#' @param fiscal_years,agencies For `tool_nih_reporter()`, fixed filters set
#'   by the researcher (the model can still add its own years and
#'   institutes).
#' @param status For `tool_grants_gov()`, the opportunity statuses searched.
#' @return An [define_tool()] object.
#' @examples
#' tool_nih_reporter()
#' tool_nsf_awards()
#' tool_grants_gov()
#' @name funding_tools
NULL

#' @rdname funding_tools
#' @export
tool_nih_reporter <- function(n = 10, fiscal_years = NULL, agencies = NULL) {
  # The researcher's filters (fiscal_years, agencies) are fixed when the tool
  # is made; the model can add its own years and institutes on each call.
  fixed_years <- fiscal_years
  fixed_agencies <- agencies
  fn <- function(text, years = NULL, institutes = NULL, pi_name = NULL) {
    res <- search_nih_projects(text, fiscal_years = c(fixed_years, years),
                               agencies = c(fixed_agencies, institutes),
                               pi_names = pi_name, n = n)
    .format_nih(res)
  }
  define_tool(fn, name = "nih_reporter",
    description = paste(
      "Search projects funded by the US National Institutes of Health (NIH RePORTER).",
      "Finds funded grants whose title, terms or abstract contain ALL the words given.",
      "Use few, specific words. Returns project number, title, fiscal year, institute,",
      "award amount, PI, organization and the start of the abstract."),
    args = list(
      text = arg_string("Words that must all appear, for example 'loneliness older adults intervention'"),
      years = arg_array("Optional fiscal years, for example [2024, 2025]",
                        items = arg_integer("A year"), required = FALSE),
      institutes = arg_array("Optional NIH institute abbreviations, for example ['NIA', 'NIMH']",
                             items = arg_string("An abbreviation"), required = FALSE),
      pi_name = arg_string("Optional investigator name (partial names match)", required = FALSE)),
    max_output_tokens = 3000, untrusted = TRUE)
}

.format_nih <- function(res) {
  lines <- if (nrow(res)) vapply(seq_len(nrow(res)), function(i) {
    r <- res[i, ]
    sprintf("%d. [%s] %s\n   FY%s | %s | %s | PI: %s | %s\n   %s to %s | %s\n   Abstract: %s",
            i, r$project_num, r$title, r$fiscal_year, r$agency, .money(r$award_amount),
            r$pi, r$organization, r$start_date, r$end_date, r$url,
            .shorten(r$abstract %|na|% "", 300))
  }, character(1)) else "No matching projects."
  head <- sprintf("NIH RePORTER: %s matching projects; showing %d.",
                  format(attr(res, "total"), big.mark = ","), nrow(res))
  .tool_text(c(head, lines), data = res)
}

#' @rdname funding_tools
#' @export
tool_nsf_awards <- function(n = 10) {
  fn <- function(keyword, start_after = NULL, start_before = NULL) {
    res <- search_nsf_awards(keyword, n = n, date_start = start_after, date_end = start_before)
    lines <- if (nrow(res)) vapply(seq_len(nrow(res)), function(i) {
      r <- res[i, ]
      sprintf("%d. [%s] %s\n   %s | PI: %s | %s | %s to %s | %s\n   Abstract: %s",
              i, r$id, r$title, .money(r$amount), r$pi, r$organization, r$start_date,
              r$end_date, r$url, .shorten(r$abstract %|na|% "", 300))
    }, character(1)) else "No matching awards."
    head <- sprintf("NSF awards: %s matching; showing %d.",
                    format(attr(res, "total"), big.mark = ","), nrow(res))
    .tool_text(c(head, lines), data = res)
  }
  define_tool(fn, name = "nsf_awards",
    description = paste(
      "Search awards funded by the US National Science Foundation (NSF Award Search).",
      "Returns award id, title, amount, PI, organization, dates and the start of the abstract."),
    args = list(
      keyword = arg_string("Search words, for example 'measurement invariance'"),
      start_after = arg_string("Optional: only awards starting on or after this date (YYYY-MM-DD)",
                               required = FALSE),
      start_before = arg_string("Optional: only awards starting on or before this date (YYYY-MM-DD)",
                                required = FALSE)),
    max_output_tokens = 3000, untrusted = TRUE)
}

#' @rdname funding_tools
#' @export
tool_grants_gov <- function(n = 10, status = c("posted", "forecasted")) {
  fn <- function(keyword = NULL, opportunity_id = NULL) {
    if (length(opportunity_id)) {
      d <- grants_gov_opportunity(opportunity_id)
      text <- sprintf("[%s] %s\nAgency: %s | Posted: %s | Closes: %s | Award: %s to %s\n%s\n\n%s",
                      d$number, d$title, d$agency, d$posting_date, d$response_date,
                      .money(d$award_floor), .money(d$award_ceiling), d$url,
                      .shorten(d$description %|na|% "", 3000))
      return(.tool_text(text, data = d))
    }
    if (!length(keyword) || !nzchar(keyword)) .stop("give a keyword or an opportunity_id")
    res <- search_grants_gov(keyword, n = n, status = status)
    lines <- if (nrow(res)) vapply(seq_len(nrow(res)), function(i) {
      r <- res[i, ]
      sprintf("%d. id %s [%s] %s\n   %s | %s | opens %s | closes %s",
              i, r$id, r$number, r$title, r$agency, r$status, r$open_date,
              r$close_date %|na|% "not given")
    }, character(1)) else "No matching opportunities."
    head <- sprintf("Grants.gov: %s matching opportunities; showing %d.",
                    format(attr(res, "total"), big.mark = ","), nrow(res))
    .tool_text(c(head, lines), data = res)
  }
  define_tool(fn, name = "grants_gov",
    description = paste(
      "Search US federal funding opportunities on Grants.gov (", paste(status, collapse = " and "),
      "opportunities), or read the full synopsis of one opportunity. Search first with a",
      "keyword; then pass an id from the results as opportunity_id to read eligibility,",
      "dates, award size and description."),
    args = list(
      keyword = arg_string("Search words, for example 'substance use prevention'", required = FALSE),
      opportunity_id = arg_string("Optional: the id of one opportunity from a previous search",
                                  required = FALSE)),
    max_output_tokens = 3000, untrusted = TRUE)
}
