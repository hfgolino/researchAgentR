# Literature search: one small client per database (OpenAlex, Crossref,
# Semantic Scholar, Europe PMC, arXiv, Unpaywall), all returning the same
# columns, so their results can be stacked with rbind(), deduplicated with
# dedupe_records() and counted with prisma_counts(). Each tool_*() wraps a
# search function for an agent and shows the model a compact summary, while
# the full data frame stays available in R (the `value` of call_tool()).
#
# Every request goes through .request() and .perform() (R/http.R), so
# retries, the response cache (use_cache()) and readable errors come for free.
# On top of that, .lit_perform() keeps a minimum pause between live requests to
# the same database, because each database publishes its own etiquette.

# ---- Shared helpers ------------------------------------------------------------

# The columns every search function returns (see inst/DEVELOPERS.md).
.lit_columns <- c("title", "year", "doi", "authors", "source", "abstract",
                  "oa_url", "id", "database")

# First element of a JSON field as a string; NULL, empty and "" become NA.
.lit_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- x[[1]]
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_character_)
  x <- as.character(x)
  if (!nzchar(trimws(x))) NA_character_ else x
}

.lit_int <- function(x) {
  x <- suppressWarnings(as.integer(.lit_chr(x)))
  if (length(x) == 0) NA_integer_ else x
}

# Build the common data frame. Extra named columns (vectors of the same
# length) are added after the common ones.
.lit_records <- function(title = character(0), year = integer(0), doi = character(0),
                         authors = character(0), source = character(0),
                         abstract = character(0), oa_url = character(0),
                         id = character(0), database = character(0), ...) {
  df <- data.frame(title = as.character(title), year = as.integer(year),
                   doi = as.character(doi), authors = as.character(authors),
                   source = as.character(source), abstract = as.character(abstract),
                   oa_url = as.character(oa_url), id = as.character(id),
                   database = as.character(database), stringsAsFactors = FALSE)
  extra <- list(...)
  for (nm in names(extra)) df[[nm]] <- extra[[nm]]
  df
}

.lit_bind <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(.lit_records())
  do.call(rbind, rows)
}

# Remove HTML or JATS XML tags (Crossref and Europe PMC abstracts carry them)
# and squeeze whitespace.
.lit_strip_tags <- function(x) {
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&lt;", "<", gsub("&gt;", ">", gsub("&amp;", "&", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  x <- trimws(gsub("\\s+", " ", x))
  x <- gsub(" ([.,;:!?)])", "\\1", x)   # "word ." left by a removed tag
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}

# Contact email for the "polite" access of Crossref and for Unpaywall:
# options(researchAgentR.email = "you@university.edu") or the environment
# variable RESEARCHAGENTR_EMAIL in ~/.Renviron. Returns "" when not set.
.lit_email <- function() {
  e <- .opt("email") %||% Sys.getenv("RESEARCHAGENTR_EMAIL")
  if (is.null(e) || is.na(e)) "" else as.character(e)
}

# Perform a request while keeping at least `min_interval` seconds between live
# requests to the same database. Answers read from the cache do not count,
# so a replayed demo never waits. Errors are rethrown with the database name.
.lit_perform <- function(req, realm, min_interval, service = realm) {
  last <- .pkg_env$lit_last_call[[realm]]
  if (!is.null(last)) {
    wait <- min_interval - (as.numeric(Sys.time()) - last)
    if (wait > 0) Sys.sleep(wait)
  }
  res <- tryCatch(.perform(req), error = function(e) {
    .stop(service, " request failed: ", conditionMessage(e))
  })
  if (!isTRUE(res$cached)) {
    if (is.null(.pkg_env$lit_last_call)) .pkg_env$lit_last_call <- list()
    .pkg_env$lit_last_call[[realm]] <- as.numeric(Sys.time())
  }
  res
}

# Encode a DOI for use inside a URL path ("/" stays, everything else that is
# not safe is percent-encoded).
.lit_encode_doi <- function(doi) {
  gsub("%2F", "/", utils::URLencode(doi, reserved = TRUE), fixed = TRUE)
}

#' Normalize DOIs
#'
#' Turns the many ways a DOI is written (`https://doi.org/10.1037/...`,
#' `doi:10.1037/...`, upper case, trailing punctuation copied from a
#' reference list) into one form: lower case, starting with `10.`. DOIs are
#' case-insensitive, so lower case is safe for comparing them. Anything that
#' does not look like a DOI becomes `NA` (and is reported, never guessed).
#'
#' @param x A character vector.
#' @return A character vector of the same length: normalized DOIs or `NA`.
#' @examples
#' normalize_doi(c("https://doi.org/10.1037/MET0000255",
#'                 "doi: 10.1007/s11336-021-09820-y.",
#'                 "not a doi", NA))
#' @export
normalize_doi <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:\\s*", "", x)
  # DOIs inside URLs are sometimes percent-encoded (10.1000%2Fabc).
  enc <- !is.na(x) & grepl("%[0-9a-f]{2}", x)
  x[enc] <- vapply(x[enc], utils::URLdecode, character(1))
  x <- sub("[.,;]+$", "", x)
  x[!is.na(x) & !grepl("^10\\.[0-9]{4,9}/\\S+$", x)] <- NA_character_
  unname(x)
}

# "Hudson F. Golino" -> "Golino, H. F."; names that already contain a comma
# are kept. Lower-case particles (van, de, da ...) stay with the surname.
.lit_author_name <- function(name) {
  name <- trimws(gsub("\\s+", " ", name))
  if (is.na(name) || !nzchar(name)) return(NA_character_)
  if (grepl(",", name)) return(name)
  parts <- strsplit(name, " ", fixed = TRUE)[[1]]
  if (length(parts) == 1) return(parts)
  particles <- c("van", "von", "de", "der", "den", "da", "das", "do", "dos", "del",
                 "della", "di", "du", "la", "le", "ter", "ten")
  first_sur <- length(parts)
  while (first_sur > 2 && tolower(parts[first_sur - 1]) %in% particles) first_sur <- first_sur - 1
  surname <- paste(parts[first_sur:length(parts)], collapse = " ")
  paste0(surname, ", ", .lit_initials(paste(parts[seq_len(first_sur - 1)], collapse = " ")))
}

# "Hudson Fernandes" -> "H. F."
.lit_initials <- function(given) {
  if (is.null(given) || is.na(given) || !nzchar(trimws(given))) return("")
  g <- strsplit(trimws(given), "[ .]+")[[1]]
  g <- g[nzchar(g)]
  paste0(substr(g, 1, 1), ".", collapse = " ")
}

.lit_join_authors <- function(names) {
  names <- names[!is.na(names) & nzchar(names)]
  if (length(names) == 0) NA_character_ else paste(names, collapse = "; ")
}

# ---- OpenAlex ------------------------------------------------------------------

# Rebuild an abstract from OpenAlex's inverted index: a list that maps each
# word to the positions where it occurs, for example
# list(Despite = list(0), growing = list(1), interest = list(2)).
# Source: https://docs.openalex.org/api-entities/works/work-object
.lit_reconstruct_abstract <- function(inverted_index) {
  if (is.null(inverted_index) || length(inverted_index) == 0) return(NA_character_)
  positions <- lapply(inverted_index, function(p) as.integer(unlist(p)))
  words <- rep(names(positions), lengths(positions))
  paste(words[order(unlist(positions))], collapse = " ")
}

# Fields requested from OpenAlex (select= keeps responses small). All names are
# fields of the Work schema in https://developers.openalex.org/api-reference/openapi.json
.openalex_select <- paste("id", "doi", "display_name", "publication_year", "type",
                          "cited_by_count", "authorships", "primary_location",
                          "best_oa_location", "open_access", "abstract_inverted_index",
                          "is_retracted", sep = ",")

# One OpenAlex Work (parsed JSON) -> one row of the common data frame.
.lit_parse_openalex <- function(w) {
  authors <- vapply(w$authorships %||% list(), function(a) {
    .lit_author_name(.lit_chr(a$author$display_name %||% a$raw_author_name))
  }, character(1))
  best <- w$best_oa_location
  oa <- .lit_chr(w$open_access$oa_url)
  if (is.na(oa)) oa <- .lit_chr(best$pdf_url)
  if (is.na(oa)) oa <- .lit_chr(best$landing_page_url)
  .lit_records(
    title = .lit_chr(w$display_name %||% w$title),
    year = .lit_int(w$publication_year),
    doi = normalize_doi(.lit_chr(w$doi)),
    authors = .lit_join_authors(authors),
    source = .lit_chr(w$primary_location$source$display_name),
    abstract = .lit_reconstruct_abstract(w$abstract_inverted_index),
    oa_url = oa,
    id = sub("^https?://openalex\\.org/", "", .lit_chr(w$id)),
    database = "OpenAlex",
    cited_by_count = .lit_int(w$cited_by_count),
    type = .lit_chr(w$type),
    is_retracted = as.logical(.lit_chr(w$is_retracted))
  )
}

# OpenAlex sorts with "field:desc" (https://developers.openalex.org/guides/sort,
# accessed 2026-10-11); "-field", common in other APIs, returns HTTP 400.
.openalex_sort <- function(sort) {
  if (is.null(sort)) return(NULL)
  if (startsWith(sort, "-")) paste0(substring(sort, 2), ":desc") else sort
}

# GET https://api.openalex.org/<path>, with the free key when there is one.
.openalex_get <- function(path, query = list()) {
  key <- .get_key("openalex")
  if (nzchar(key)) query$api_key <- key
  query <- Filter(Negate(is.null), query)
  req <- .request(paste0("https://api.openalex.org/", path), auth = "none",
                  query = query, realm = "openalex", rpm = 600)
  .lit_perform(req, "openalex", 0.1, "OpenAlex")$body
}

# Page through /works until n results (100 per page at most).
.openalex_list <- function(query, n) {
  n <- max(1, as.integer(n))
  rows <- list()
  meta <- list(count = NA, cost_usd = 0, requests = 0)
  page <- 1
  # The page size stays fixed, because OpenAlex computes the offset of page k
  # as (k - 1) * per_page; extra rows on the last page are dropped below.
  per_page <- min(100, n)
  repeat {
    body <- .openalex_get("works", c(query, list(per_page = per_page, page = page,
                                                select = .openalex_select)))
    meta$count <- body$meta$count %||% NA
    meta$cost_usd <- meta$cost_usd + as.numeric(body$meta$cost_usd %||% 0)
    meta$requests <- meta$requests + 1
    res <- body$results %||% list()
    rows <- c(rows, lapply(res, .lit_parse_openalex))
    if (length(res) < per_page || length(rows) >= n || page * per_page >= 10000) break
    page <- page + 1
  }
  out <- .lit_bind(utils::head(rows, n))
  attr(out, "total_found") <- meta$count
  attr(out, "openalex_cost_usd") <- meta$cost_usd
  attr(out, "requests") <- meta$requests
  out
}

#' Search OpenAlex for works
#'
#' Searches the titles, abstracts and full texts indexed by
#' [OpenAlex](https://openalex.org), a free and open catalogue of scholarly
#' works, and returns one row per work. Abstracts are rebuilt from OpenAlex's
#' "inverted index" format.
#'
#' @details OpenAlex works with a daily budget measured in US dollars: about
#'   $0.10 a day without a key and $1 a day with a free key (create an account
#'   at <https://openalex.org/settings/api> and add `OPENALEX_API_KEY=...` to
#'   `~/.Renviron`). A search costs $1 per 1,000 calls, a list with filters
#'   $0.10 per 1,000, and a single record by ID or DOI is free; at most 100
#'   requests per second and 100 results per page. Source:
#'   <https://developers.openalex.org/api-reference/authentication.md>
#'   (accessed 2026-10-11). The list price spent by a call is returned in the
#'   attribute `openalex_cost_usd`.
#'
#'   Filters use OpenAlex syntax `field:value`, joined with commas (AND) and
#'   with `|` inside one field (OR), for example
#'   `"open_access.is_oa:true,language:en"`. See
#'   <https://developers.openalex.org/api-reference/works.md>.
#'
#' @param query Search terms, for example `"exploratory graph analysis"`.
#' @param n Number of works to return. Pages of 100 are fetched as needed.
#' @param filter Optional extra OpenAlex filter string (see Details).
#' @param year_from,year_to Optional first and last publication years.
#' @param type Optional work type, for example `"article"` or `"review"`.
#' @param sort Optional sort, for example `"cited_by_count:desc"` or
#'   `"publication_date:desc"` (OpenAlex syntax: append `:desc` for
#'   descending order; a leading `-` is also accepted). The
#'   default is relevance.
#' @return A data frame with the common columns `title`, `year`, `doi`,
#'   `authors`, `source`, `abstract`, `oa_url`, `id`, `database`, plus
#'   `cited_by_count`, `type` and `is_retracted`. Attributes: `total_found`
#'   (matches in OpenAlex), `openalex_cost_usd`, `requests`, `query`.
#' @seealso [get_work()], [get_references()], [get_citations()],
#'   [tool_openalex()].
#' @examples
#' \dontrun{
#' res <- search_openalex("exploratory graph analysis", n = 5, year_from = 2020)
#' res[, c("title", "year", "doi")]
#' attr(res, "total_found")
#' }
#' @export
search_openalex <- function(query, n = 10, filter = NULL, year_from = NULL, year_to = NULL,
                            type = NULL, sort = NULL) {
  filters <- c(filter,
               if (!is.null(year_from)) paste0("from_publication_date:", as.integer(year_from), "-01-01"),
               if (!is.null(year_to)) paste0("to_publication_date:", as.integer(year_to), "-12-31"),
               if (!is.null(type)) paste0("type:", type))
  q <- list(search = query, sort = .openalex_sort(sort),
            filter = if (length(filters)) paste(filters, collapse = ",") else NULL)
  out <- .openalex_list(q, n)
  attr(out, "query") <- query
  out
}

# An OpenAlex work ID ("W2741809807") or a DOI path ("doi:10....").
.openalex_path_id <- function(x) {
  x <- trimws(x)
  if (grepl("^(https?://openalex\\.org/)?[Ww][0-9]+$", x)) {
    return(toupper(sub("^https?://openalex\\.org/", "", x)))
  }
  doi <- normalize_doi(x)
  if (is.na(doi)) .stop("'", x, "' is neither a DOI nor an OpenAlex work ID (W123...).")
  paste0("doi:", .lit_encode_doi(doi))
}

#' Get one work from OpenAlex by DOI or OpenAlex ID
#'
#' Single-record lookups are free in OpenAlex (they do not use the daily
#' budget; see <https://developers.openalex.org/api-reference/authentication.md>).
#'
#' @param doi_or_id A DOI (any common form) or an OpenAlex work ID such as
#'   `"W2741809807"`.
#' @return A one-row data frame with the columns of [search_openalex()]. The
#'   attribute `referenced_works` holds the OpenAlex IDs of the works it
#'   cites. An error if the work is not found.
#' @examples
#' \dontrun{
#' w <- get_work("10.1080/00273171.2020.1779642")
#' w$title
#' head(attr(w, "referenced_works"))
#' }
#' @export
get_work <- function(doi_or_id) {
  path <- .openalex_path_id(doi_or_id)
  w <- tryCatch(.openalex_get(paste0("works/", path)), error = function(e) {
    if (grepl("404", conditionMessage(e))) .stop("OpenAlex has no work for '", doi_or_id, "'.")
    stop(e)
  })
  out <- .lit_parse_openalex(w)
  attr(out, "referenced_works") <- sub("^https?://openalex\\.org/", "", unlist(w$referenced_works))
  out
}

#' Get many works from OpenAlex by ID or DOI
#'
#' Looks up a list of works in batches of 50 with one list request per batch
#' (filters `openalex:W1|W2|...` or `doi:...|...`), which is much faster than
#' calling [get_work()] for each one. Used, for example, to build a screening
#' set from a list of OpenAlex IDs and human labels (Activity 12).
#'
#' @details A list request costs $0.10 per 1,000 calls of the OpenAlex daily
#'   budget (see [search_openalex()]), so 1,000 works (20 requests) use a
#'   fraction of a cent. Works OpenAlex does not know come back as rows of
#'   `NA`, counted in a message, never dropped silently.
#' @param ids OpenAlex work IDs (`"W2159212729"` or the full URL) or DOIs (any
#'   common form); one kind or a mix.
#' @param batch_size IDs per request (at most 100; 50 keeps URLs short).
#' @return A data frame with one row per input, in the input order, with the
#'   columns of [search_openalex()] plus `requested` (the input value) and
#'   `found`.
#' @examples
#' \dontrun{
#' get_works(c("W2159212729", "10.1007/s11336-021-09820-y"))
#' }
#' @export
get_works <- function(ids, batch_size = 50) {
  ids <- as.character(ids)
  batch_size <- min(100L, max(1L, as.integer(batch_size)))
  is_oa <- grepl("^(https?://openalex\\.org/)?[Ww][0-9]+$", trimws(ids))
  key <- ifelse(is_oa, toupper(sub("^https?://openalex\\.org/", "", trimws(ids))), normalize_doi(ids))
  fetch <- function(keys, field) {
    keys <- unique(keys[!is.na(keys)])
    if (!length(keys)) return(NULL)
    value <- if (field == "openalex") keys else paste0("https://doi.org/", keys)
    batches <- split(value, ceiling(seq_along(value) / batch_size))
    .lit_bind(lapply(batches, function(b) {
      .openalex_list(list(filter = paste0(field, ":", paste(b, collapse = "|"))), length(b))
    }))
  }
  got <- .lit_bind(Filter(Negate(is.null), list(fetch(key[is_oa], "openalex"), fetch(key[!is_oa], "doi"))))
  match_key <- if (nrow(got)) ifelse(is_oa, match(key, got$id), match(key, normalize_doi(got$doi))) else
    rep(NA_integer_, length(ids))
  out <- if (nrow(got)) got[match_key, , drop = FALSE] else
    .lit_records(title = rep(NA, length(ids)), year = NA, doi = NA, authors = NA, source = NA,
                 abstract = NA, oa_url = NA, id = NA, database = "OpenAlex")
  rownames(out) <- NULL
  out$requested <- ids
  out$found <- !is.na(match_key)
  if (any(!out$found)) .msg(sum(!out$found), " of ", length(ids), " works were not found in OpenAlex (rows of NA).")
  out
}

#' Snowball through references and citations (OpenAlex)
#'
#' Backward snowballing lists the works a paper cites (`get_references()`);
#' forward snowballing lists the works that cite it (`get_citations()`).
#' Together they find studies that keyword searches miss.
#'
#' @details `get_references()` makes one free single-record lookup and then
#'   fetches the cited works in batches of 100 with the filter
#'   `openalex:W1|W2|...`; `get_citations()` uses the filter `cites:W...`
#'   (both documented at <https://developers.openalex.org/guides/recipes.md>).
#'   List-and-filter calls cost $0.10 per 1,000 of the OpenAlex daily budget.
#'
#' @param id_or_doi A DOI or OpenAlex work ID.
#' @param n Maximum number of works to return.
#' @param sort For `get_citations()`, the order, for example
#'   `"cited_by_count:desc"` (most cited first, the default) or
#'   `"publication_date:desc"` (newest first).
#' @return A data frame with the columns of [search_openalex()] and the
#'   attribute `seed` (the starting work's OpenAlex ID).
#' @examples
#' \dontrun{
#' refs <- get_references("10.1080/00273171.2020.1779642", n = 50)
#' cites <- get_citations("10.1080/00273171.2020.1779642", n = 50)
#' nrow(refs); nrow(cites)
#' }
#' @name snowball
NULL

#' @rdname snowball
#' @export
get_references <- function(id_or_doi, n = 50) {
  seed <- get_work(id_or_doi)
  ids <- attr(seed, "referenced_works")
  ids <- utils::head(ids, n)
  batches <- split(ids, ceiling(seq_along(ids) / 100))
  out <- .lit_bind(lapply(batches, function(b) {
    .openalex_list(list(filter = paste0("openalex:", paste(b, collapse = "|"))), length(b))
  }))
  rownames(out) <- NULL
  attr(out, "seed") <- seed$id
  attr(out, "total_found") <- length(attr(seed, "referenced_works"))
  out
}

#' @rdname snowball
#' @export
get_citations <- function(id_or_doi, n = 50, sort = "cited_by_count:desc") {
  id <- .openalex_path_id(id_or_doi)
  if (startsWith(id, "doi:")) id <- get_work(id_or_doi)$id
  out <- .openalex_list(list(filter = paste0("cites:", id), sort = .openalex_sort(sort)), n)
  attr(out, "seed") <- id
  out
}

# ---- Crossref ------------------------------------------------------------------

# Rate limits from 1 December 2025
# (https://www.crossref.org/blog/announcing-changes-to-rest-api-rate-limits/):
# public access 1 list request per second and 5 single records per second;
# polite access (with mailto) 3 and 10. One request at a time in both.
.crossref_interval <- function(single = FALSE) {
  polite <- nzchar(.lit_email())
  if (single) (if (polite) 0.125 else 0.25) else (if (polite) 0.4 else 1.1)
}

.crossref_get <- function(path, query = list(), single = FALSE, allow_404 = FALSE) {
  email <- .lit_email()
  if (nzchar(email)) query$mailto <- email
  query <- Filter(Negate(is.null), query)
  interval <- .crossref_interval(single)
  req <- .request(paste0("https://api.crossref.org/", path), auth = "none", query = query,
                  realm = if (single) "crossref_single" else "crossref_list",
                  rpm = floor(60 / interval))
  # verify_citations() needs to see a 404 (DOI unknown to Crossref) as an
  # answer, not as an error; other error statuses still stop.
  if (allow_404) {
    req <- httr2::req_error(req, is_error = function(resp) {
      s <- httr2::resp_status(resp)
      s >= 400 && s != 404
    }, body = .error_body)
  }
  .lit_perform(req, if (single) "crossref_single" else "crossref_list", interval, "Crossref")
}

# Year from a Crossref partial date: list(`date-parts` = list(list(2020, 5, 1))).
.crossref_year <- function(d) .lit_int(d$`date-parts`[[1]][[1]])

# One Crossref work item -> one row of the common data frame.
# Field names: https://github.com/CrossRef/rest-api-doc/blob/master/api_format.md
.lit_parse_crossref <- function(it) {
  authors <- vapply(it$author %||% list(), function(a) {
    fam <- .lit_chr(a$family)
    if (is.na(fam)) return(.lit_chr(a$name))
    ini <- .lit_initials(.lit_chr(a$given))
    if (nzchar(ini)) paste0(fam, ", ", ini) else fam
  }, character(1))
  .lit_records(
    title = .lit_chr(it$title),
    year = .crossref_year(it$issued),
    doi = normalize_doi(.lit_chr(it$DOI)),
    authors = .lit_join_authors(authors),
    source = .lit_chr(it$`container-title`),
    abstract = .lit_strip_tags(.lit_chr(it$abstract)),
    oa_url = NA_character_,
    id = normalize_doi(.lit_chr(it$DOI)),
    database = "Crossref",
    cited_by_count = .lit_int(it$`is-referenced-by-count`),
    type = .lit_chr(it$type)
  )
}

#' Search Crossref for works
#'
#' Searches the bibliographic metadata (titles, authors, journals, years)
#' that publishers deposit with [Crossref](https://www.crossref.org). Crossref
#' has no open-access links and few abstracts, but it is the authority on
#' whether a DOI exists; see [verify_citations()].
#'
#' @details Set a contact email to use Crossref's "polite" access, which has
#'   higher limits: `options(researchAgentR.email = "you@university.edu")` or
#'   `RESEARCHAGENTR_EMAIL=you@university.edu` in `~/.Renviron`. Without it
#'   the public access still works. Limits from 1 December 2025: public, 1
#'   list request per second; polite, 3 per second; one request at a time
#'   (<https://www.crossref.org/blog/announcing-changes-to-rest-api-rate-limits/>,
#'   accessed 2026-10-11). The function waits between requests to stay below
#'   them. At most 1,000 rows per request. Filters use Crossref syntax, for
#'   example `"from-pub-date:2020-01-01,type:journal-article"`
#'   (<https://www.crossref.org/documentation/retrieve-metadata/rest-api/rest-api-filters/>).
#'
#' @param query Search terms matched against the bibliographic metadata
#'   (Crossref's `query.bibliographic`); a full reference string also works.
#' @param n Number of works (at most 1,000).
#' @param filter Optional Crossref filter string (see Details).
#' @return A data frame with the common columns (see [search_openalex()]),
#'   plus `cited_by_count` and `type`. Attribute `total_found`.
#' @examples
#' \dontrun{
#' options(researchAgentR.email = "you@university.edu")
#' search_crossref("network psychometrics tutorial", n = 5)
#' }
#' @export
search_crossref <- function(query, n = 10, filter = NULL) {
  n <- min(1000L, max(1L, as.integer(n)))
  res <- .crossref_get("works", list(
    `query.bibliographic` = query, rows = n, filter = filter,
    select = "DOI,title,author,issued,container-title,abstract,type,is-referenced-by-count"))
  msg <- res$body$message
  out <- .lit_bind(lapply(msg$items %||% list(), .lit_parse_crossref))
  attr(out, "total_found") <- msg$`total-results` %||% NA
  attr(out, "query") <- query
  out
}

# ---- Semantic Scholar ----------------------------------------------------------

# One Semantic Scholar paper -> one row. Field names from the Graph API
# specification (https://api.semanticscholar.org/graph/v1/swagger.json).
.lit_parse_s2 <- function(p) {
  authors <- vapply(p$authors %||% list(), function(a) .lit_author_name(.lit_chr(a$name)),
                    character(1))
  .lit_records(
    title = .lit_chr(p$title), year = .lit_int(p$year),
    doi = normalize_doi(.lit_chr(p$externalIds$DOI)),
    authors = .lit_join_authors(authors), source = .lit_chr(p$venue),
    abstract = .lit_chr(p$abstract), oa_url = .lit_chr(p$openAccessPdf$url),
    id = .lit_chr(p$paperId), database = "Semantic Scholar",
    cited_by_count = .lit_int(p$citationCount),
    type = .lit_chr(p$publicationTypes)
  )
}

#' Search Semantic Scholar for papers
#'
#' Relevance search in the [Semantic Scholar](https://www.semanticscholar.org)
#' Academic Graph, which often has abstracts that Crossref lacks.
#'
#' @details Use it with a key. Keys are free but approved by hand (request one
#'   at <https://www.semanticscholar.org/product/api>; store it as
#'   `SEMANTIC_SCHOLAR_API_KEY` in `~/.Renviron`); it is sent in the
#'   `x-api-key` header. Without a key, requests go to one pool shared by all
#'   unauthenticated users and are often refused (HTTP 429), so the function
#'   tries only twice and the course activities treat Semantic Scholar as
#'   optional; [search_openalex()] and [search_crossref()] need no approval.
#'   The function keeps at least one second
#'   between requests, the limit for keyed access. At most 100 results per request
#'   (<https://api.semanticscholar.org/graph/v1/swagger.json>, accessed
#'   2026-10-11). The Semantic Scholar API license requires attribution: the
#'   `database` column records "Semantic Scholar", and work that uses these
#'   results should credit Semantic Scholar.
#'
#' @param query Plain-text search terms (no special syntax is supported).
#' @param n Number of papers (at most 100).
#' @param fields Comma-separated Graph API fields to request.
#' @param year Optional year or range, for example `"2019-2024"`.
#' @return A data frame with the common columns (see [search_openalex()]),
#'   plus `cited_by_count` and `type`. Attribute `total_found`.
#' @examples
#' \dontrun{
#' search_semantic_scholar("large language models psychometrics", n = 5)
#' }
#' @export
search_semantic_scholar <- function(query, n = 10,
                                    fields = "title,year,externalIds,authors,venue,abstract,openAccessPdf,citationCount,url,publicationTypes",
                                    year = NULL) {
  n <- min(100L, max(1L, as.integer(n)))
  key <- .get_key("semantic_scholar")
  headers <- if (nzchar(key)) list(`x-api-key` = key) else list()
  req <- .request("https://api.semanticscholar.org/graph/v1/paper/search", auth = "none",
                  headers = headers,
                  query = Filter(Negate(is.null), list(query = query, limit = n, fields = fields,
                                                       year = year)),
                  realm = "semantic_scholar", rpm = 60)
  if (!nzchar(key)) {
    .msg("Semantic Scholar without a key: the shared pool often refuses requests (HTTP 429).")
    req <- httr2::req_retry(req, max_tries = 2, is_transient = .is_transient)
  }
  body <- .lit_perform(req, "semantic_scholar", 1.05, "Semantic Scholar")$body
  out <- .lit_bind(lapply(body$data %||% list(), .lit_parse_s2))
  attr(out, "total_found") <- body$total %||% NA
  attr(out, "query") <- query
  out
}

# ---- Europe PMC ----------------------------------------------------------------

# One Europe PMC result -> one row. The official documentation
# (https://europepmc.org/RestfulWebService) could not be read when this was
# written, so every field is read defensively; see NEWS ("to verify live").
.lit_parse_epmc <- function(r) {
  au <- .lit_chr(r$authorString)
  authors <- if (is.na(au)) NA_character_ else {
    # "Golino HF, Epskamp S." -> "Golino, H. F.; Epskamp, S."
    parts <- trimws(strsplit(sub("\\.$", "", au), ",")[[1]])
    .lit_join_authors(vapply(parts, function(p) {
      m <- regmatches(p, regexec("^(.*\\S)\\s+([A-Z]{1,4})$", p))[[1]]
      if (length(m) == 3) paste0(m[2], ", ", paste0(strsplit(m[3], "")[[1]], ".", collapse = " ")) else p
    }, character(1)))
  }
  urls <- r$fullTextUrlList$fullTextUrl %||% list()
  oa <- Filter(function(u) identical(.lit_chr(u$availabilityCode), "OA"), urls)
  .lit_records(
    title = .lit_strip_tags(.lit_chr(r$title)), year = .lit_int(r$pubYear),
    doi = normalize_doi(.lit_chr(r$doi)), authors = authors,
    source = .lit_chr(r$journalInfo$journal$title %||% r$journalTitle),
    abstract = .lit_strip_tags(.lit_chr(r$abstractText)),
    oa_url = if (length(oa)) .lit_chr(oa[[1]]$url) else NA_character_,
    id = paste0(.lit_chr(r$source), ":", .lit_chr(r$id)), database = "Europe PMC",
    pmid = .lit_chr(r$pmid), pmcid = .lit_chr(r$pmcid)
  )
}

#' Search Europe PMC
#'
#' Searches [Europe PMC](https://europepmc.org), which covers PubMed, PubMed
#' Central and preprints in the life and health sciences, with abstracts. No
#' key is needed.
#'
#' @details Uses the REST search endpoint
#'   `https://www.ebi.ac.uk/europepmc/webservices/rest/search` with
#'   `format=json` and `resultType=core` (which includes abstracts). The query
#'   uses Europe PMC syntax, for example `"anxiety AND PUB_YEAR:[2020 TO 2024]"`.
#'   The official documentation (<https://europepmc.org/RestfulWebService>)
#'   should be consulted for the full syntax and current limits; the function
#'   keeps a pause of half a second between requests.
#'
#' @param query Search terms in Europe PMC syntax.
#' @param n Number of records (at most 1,000).
#' @return A data frame with the common columns (see [search_openalex()]),
#'   plus `pmid` and `pmcid`. Attribute `total_found`.
#' @examples
#' \dontrun{
#' search_europepmc("loneliness older adults intervention", n = 5)
#' }
#' @export
search_europepmc <- function(query, n = 10) {
  n <- min(1000L, max(1L, as.integer(n)))
  req <- .request("https://www.ebi.ac.uk/europepmc/webservices/rest/search", auth = "none",
                  query = list(query = query, format = "json", resultType = "core",
                               pageSize = n),
                  realm = "europepmc", rpm = 120)
  body <- .lit_perform(req, "europepmc", 0.5, "Europe PMC")$body
  if (!is.list(body)) .stop("Europe PMC did not return JSON.")
  out <- .lit_bind(lapply(body$resultList$result %||% list(), .lit_parse_epmc))
  attr(out, "total_found") <- body$hitCount %||% NA
  attr(out, "query") <- query
  out
}

# ---- arXiv ---------------------------------------------------------------------

# Plain words become "all:word1 AND all:word2"; a query that already uses
# field prefixes (ti:, au:, abs:, all:, cat: ...) is sent as it is.
.arxiv_query <- function(query) {
  if (grepl("\\b(ti|au|abs|all|cat|co|jr|rn|id):", query)) return(query)
  words <- strsplit(trimws(query), "\\s+")[[1]]
  paste0("all:", words, collapse = " AND ")
}

# Parse the Atom feed returned by the arXiv API. Element names from
# https://info.arxiv.org/help/api/user-manual.html
.lit_parse_arxiv <- function(xml_text) {
  if (requireNamespace("xml2", quietly = TRUE)) .arxiv_xml2(xml_text) else .arxiv_regex(xml_text)
}

.arxiv_row <- function(id_url, title, summary, published, authors, doi, journal_ref, pdf) {
  .lit_records(
    title = trimws(gsub("\\s+", " ", title)),
    year = suppressWarnings(as.integer(substr(published, 1, 4))),
    doi = normalize_doi(doi),
    authors = .lit_join_authors(vapply(authors, .lit_author_name, character(1))),
    source = if (is.na(journal_ref)) "arXiv" else journal_ref,
    abstract = trimws(gsub("\\s+", " ", summary)),
    oa_url = pdf, id = sub("^https?://arxiv\\.org/abs/", "", id_url), database = "arXiv"
  )
}

.arxiv_xml2 <- function(xml_text) {
  ns <- c(a = "http://www.w3.org/2005/Atom", arxiv = "http://arxiv.org/schemas/atom",
          opensearch = "http://a9.com/-/spec/opensearch/1.1/")
  doc <- xml2::read_xml(xml_text)
  txt <- function(node, path) {
    x <- xml2::xml_find_first(node, path, ns)
    if (inherits(x, "xml_missing")) NA_character_ else xml2::xml_text(x)
  }
  entries <- xml2::xml_find_all(doc, ".//a:entry", ns)
  rows <- lapply(entries, function(e) {
    pdf <- xml2::xml_find_first(e, "a:link[@title='pdf']", ns)
    .arxiv_row(
      id_url = txt(e, "a:id"), title = txt(e, "a:title"), summary = txt(e, "a:summary"),
      published = txt(e, "a:published"),
      authors = xml2::xml_text(xml2::xml_find_all(e, "a:author/a:name", ns)),
      doi = txt(e, "arxiv:doi"), journal_ref = txt(e, "arxiv:journal_ref"),
      pdf = if (inherits(pdf, "xml_missing")) NA_character_ else xml2::xml_attr(pdf, "href"))
  })
  out <- .lit_bind(rows)
  attr(out, "total_found") <- suppressWarnings(as.integer(txt(doc, ".//opensearch:totalResults")))
  out
}

# A simple fallback when xml2 is not installed: good enough for arXiv's
# regular feed, not a general XML parser.
.arxiv_regex <- function(xml_text) {
  tag <- function(x, name) {
    m <- regmatches(x, regexpr(paste0("<", name, "[^>]*>[\\s\\S]*?</", name, ">"), x, perl = TRUE))
    if (length(m) == 0) return(NA_character_)
    gsub(paste0("^<", name, "[^>]*>|</", name, ">$"), "", m, perl = TRUE)
  }
  unescape <- function(x) gsub("&amp;", "&", gsub("&lt;", "<", gsub("&gt;", ">", x)))
  entries <- regmatches(xml_text, gregexpr("<entry>[\\s\\S]*?</entry>", xml_text, perl = TRUE))[[1]]
  rows <- lapply(entries, function(e) {
    names <- regmatches(e, gregexpr("<name>[\\s\\S]*?</name>", e, perl = TRUE))[[1]]
    pdf <- regmatches(e, regexpr("<link[^>]*title=\"pdf\"[^>]*>", e, perl = TRUE))
    pdf <- if (length(pdf)) sub('.*href="([^"]+)".*', "\\1", pdf) else NA_character_
    .arxiv_row(
      id_url = tag(e, "id"), title = unescape(tag(e, "title")),
      summary = unescape(tag(e, "summary")), published = tag(e, "published"),
      authors = unescape(gsub("</?name>", "", names)), doi = tag(e, "arxiv:doi"),
      journal_ref = tag(e, "arxiv:journal_ref"), pdf = pdf)
  })
  out <- .lit_bind(rows)
  attr(out, "total_found") <- suppressWarnings(as.integer(tag(xml_text, "opensearch:totalResults")))
  out
}

#' Search arXiv preprints
#'
#' Searches [arXiv](https://arxiv.org) (physics, mathematics, computer
#' science, statistics, quantitative biology and more). Every arXiv preprint
#' is open access, so `oa_url` is the PDF link.
#'
#' @details The arXiv API terms ask for no more than one request every three
#'   seconds, a single connection at a time, and caching of results
#'   (<https://info.arxiv.org/help/api/user-manual.html>, accessed
#'   2026-10-11); the function waits three seconds between live requests and
#'   uses the package cache when [use_cache()] is on. Plain words are combined
#'   as `all:word1 AND all:word2`; a query with field prefixes such as
#'   `ti:`, `au:`, `abs:` or `cat:` is sent unchanged. The feed is parsed with
#'   xml2 when installed, otherwise with a simple fallback.
#'
#' @param query Search terms, or an arXiv query such as
#'   `"ti:psychometrics AND cat:stat.ME"`.
#' @param n Number of preprints (keep it modest; arXiv serves at most 2,000
#'   per request).
#' @param sort `"relevance"`, `"submittedDate"` or `"lastUpdatedDate"`.
#' @return A data frame with the common columns (see [search_openalex()]).
#'   `source` is the journal reference when the preprint was published,
#'   otherwise `"arXiv"`. Attribute `total_found`.
#' @examples
#' \dontrun{
#' search_arxiv("language model agents evaluation", n = 5)
#' }
#' @export
search_arxiv <- function(query, n = 10, sort = c("relevance", "submittedDate", "lastUpdatedDate")) {
  sort <- match.arg(sort)
  n <- min(2000L, max(1L, as.integer(n)))
  req <- .request("http://export.arxiv.org/api/query", auth = "none",
                  query = list(search_query = .arxiv_query(query), start = 0,
                               max_results = n, sortBy = sort, sortOrder = "descending"),
                  realm = "arxiv", rpm = 20)
  body <- .lit_perform(req, "arxiv", 3, "arXiv")$body
  if (!is.character(body)) .stop("arXiv did not return an Atom feed.")
  out <- .lit_parse_arxiv(body)
  # arXiv reports a malformed query as a single entry whose id points to its
  # error pages; turn it into a readable error.
  if (nrow(out) == 1 && grepl("api/errors", out$id)) .stop("arXiv query error: ", out$abstract)
  attr(out, "query") <- query
  out
}

# ---- Open access ---------------------------------------------------------------

#' Find a free, legal full text for DOIs
#'
#' Asks [Unpaywall](https://unpaywall.org) where an open-access copy of each
#' work can be read. Unpaywall needs a contact email; without one, the
#' function asks OpenAlex instead, which serves the same Unpaywall data
#' (`best_oa_location`) through a free single-record lookup.
#'
#' @details Set the email with `options(researchAgentR.email = ...)` or
#'   `RESEARCHAGENTR_EMAIL` in `~/.Renviron`. The Unpaywall endpoint is
#'   `https://api.unpaywall.org/v2/{doi}?email=...`, with a suggested limit of
#'   100,000 requests per day. A DOI that cannot be checked becomes a row with
#'   `is_oa = NA` and the reason in `detail`; it is never dropped.
#'
#' @param doi A character vector of DOIs.
#' @return A data frame with one row per DOI: `doi`, `is_oa`, `oa_status`,
#'   `oa_url` (best reading link), `pdf_url`, `license`, `version`,
#'   `host_type`, `checked_with` (`"Unpaywall"` or `"OpenAlex"`), `detail`.
#' @examples
#' \dontrun{
#' find_open_access(c("10.1080/00273171.2020.1779642", "10.1007/s11336-021-09820-y"))
#' }
#' @export
find_open_access <- function(doi) {
  email <- .lit_email()
  rows <- lapply(doi, function(d) {
    nd <- normalize_doi(d)
    row <- data.frame(doi = if (is.na(nd)) as.character(d) else nd, is_oa = NA,
                      oa_status = NA_character_, oa_url = NA_character_, pdf_url = NA_character_,
                      license = NA_character_, version = NA_character_, host_type = NA_character_,
                      checked_with = if (nzchar(email)) "Unpaywall" else "OpenAlex",
                      detail = "", stringsAsFactors = FALSE)
    if (is.na(nd)) {
      row$detail <- "not a valid DOI"
      return(row)
    }
    tryCatch({
      if (nzchar(email)) {
        req <- .request(paste0("https://api.unpaywall.org/v2/", .lit_encode_doi(nd)),
                        auth = "none", query = list(email = email), realm = "unpaywall", rpm = 600)
        b <- .lit_perform(req, "unpaywall", 0.1, "Unpaywall")$body
        best <- b$best_oa_location
        row$is_oa <- as.logical(.lit_chr(b$is_oa))
        row$oa_status <- .lit_chr(b$oa_status)
        row$oa_url <- .lit_chr(best$url %||% best$url_for_landing_page)
        row$pdf_url <- .lit_chr(best$url_for_pdf)
        row$license <- .lit_chr(best$license)
        row$version <- .lit_chr(best$version)
        row$host_type <- .lit_chr(best$host_type)
      } else {
        w <- .openalex_get(paste0("works/doi:", .lit_encode_doi(nd)),
                           list(select = "doi,open_access,best_oa_location"))
        best <- w$best_oa_location
        row$is_oa <- as.logical(.lit_chr(w$open_access$is_oa))
        row$oa_status <- .lit_chr(w$open_access$oa_status)
        row$oa_url <- .lit_chr(w$open_access$oa_url %||% best$landing_page_url)
        row$pdf_url <- .lit_chr(best$pdf_url)
        row$license <- .lit_chr(best$license)
        row$version <- .lit_chr(best$version)
      }
      row
    }, error = function(e) {
      row$detail <- .shorten(conditionMessage(e), 200)
      row
    })
  })
  out <- do.call(rbind, rows)
  n_fail <- sum(nzchar(out$detail))
  if (n_fail) .msg(n_fail, " of ", nrow(out), " DOIs could not be checked (see the `detail` column).")
  out
}

# ---- Deduplication and PRISMA counts --------------------------------------------

# Lower case, accents removed, punctuation to spaces, single spaces.
.lit_normalize_text <- function(x) {
  x <- iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

#' Remove duplicate records found by several searches
#'
#' Records are duplicates when they share a DOI (after [normalize_doi()]) or,
#' failing that, the same normalized title and year. For each group of
#' duplicates the first record is kept, its missing fields (abstract,
#' open-access link, DOI ...) are filled from the duplicates, and the
#' `database` column lists every database that found it, so provenance is
#' kept for the PRISMA flow diagram.
#'
#' @param df A data frame with at least `title`, `year`, `doi` and
#'   `database`, for example `rbind()` of several search results.
#' @return The deduplicated data frame with two new columns: `n_databases`
#'   (how many different databases found the record) and `n_found` (how many
#'   records were merged into the row, which is larger when one database
#'   returned several versions of a work). The attribute `dedup` is a data
#'   frame with the counts: records in, duplicates removed by DOI, by title
#'   and year, and records out.
#' @examples
#' a <- data.frame(title = c("A toy study of networks", "Another study"),
#'                 year = c(2017, 2020), doi = c("10.1234/toy.1", NA),
#'                 abstract = c(NA, "An abstract."), database = "OpenAlex")
#' b <- data.frame(title = c("A toy study of networks.", "Another Study"),
#'                 year = c(2017, 2020), doi = c("https://doi.org/10.1234/TOY.1", NA),
#'                 abstract = c("Toy abstract.", NA), database = "Crossref")
#' d <- dedupe_records(rbind(a, b))
#' d
#' attr(d, "dedup")
#' @export
dedupe_records <- function(df) {
  needed <- c("title", "year", "doi", "database")
  if (!all(needed %in% names(df))) {
    .stop("dedupe_records() needs the columns: ", paste(needed, collapse = ", "), ".")
  }
  n <- nrow(df)
  if (n == 0) {
    df$n_databases <- integer(0)
    df$n_found <- integer(0)
    attr(df, "dedup") <- data.frame(stage = c("records_in", "duplicates_doi", "duplicates_title_year", "records_out"),
                                    n = 0L, stringsAsFactors = FALSE)
    return(df)
  }
  doi <- normalize_doi(df$doi)
  title_key <- .lit_normalize_text(df$title)
  title_key <- ifelse(nzchar(title_key), paste0(title_key, "|", df$year), NA_character_)
  # Label propagation: every row starts in its own group; rows sharing a key
  # move to the smallest group number among them, repeated until stable.
  group_by <- function(group, keys_list) {
    repeat {
      old <- group
      for (keys in keys_list) {
        ok <- !is.na(keys)
        if (any(ok)) group[ok] <- stats::ave(group[ok], keys[ok], FUN = min)
      }
      if (identical(old, group)) return(group)
    }
  }
  g_doi <- group_by(seq_len(n), list(doi))
  g_all <- group_by(seq_len(n), list(doi, title_key))
  keep_rows <- which(!duplicated(g_all))
  out <- df[keep_rows, , drop = FALSE]
  out$doi <- ifelse(is.na(doi[keep_rows]), as.character(out$doi), doi[keep_rows])
  # Fill missing fields of each kept record from its duplicates (for example
  # an abstract that only Semantic Scholar had).
  for (k in seq_along(keep_rows)) {
    members <- which(g_all == g_all[keep_rows[k]])
    if (length(members) == 1) next
    for (col in setdiff(names(df), c("database", "id"))) {
      if (!is.na(out[[col]][k])) next
      vals <- if (col == "doi") doi[members] else df[[col]][members]
      vals <- vals[!is.na(vals)]
      if (length(vals)) out[[col]][k] <- vals[1]
    }
  }
  # (a table that was deduplicated before lists several databases in one cell)
  found_by <- lapply(g_all[keep_rows], function(g) {
    unique(unlist(strsplit(as.character(df$database[g_all == g]), "; ", fixed = TRUE)))
  })
  out$database <- vapply(found_by, paste, character(1), collapse = "; ")
  out$n_databases <- lengths(found_by)
  out$n_found <- as.integer(table(g_all)[as.character(g_all[keep_rows])])
  rownames(out) <- NULL
  n_doi <- n - length(unique(g_doi))
  attr(out, "dedup") <- data.frame(
    stage = c("records_in", "duplicates_doi", "duplicates_title_year", "records_out"),
    n = as.integer(c(n, n_doi, n - nrow(out) - n_doi, nrow(out))),
    stringsAsFactors = FALSE)
  .msg("dedupe_records(): ", n, " records in, ", n - nrow(out), " duplicates removed (",
       n_doi, " by DOI, ", n - nrow(out) - n_doi, " by title and year), ", nrow(out), " out.")
  out
}

#' PRISMA-style counts for a literature search
#'
#' Builds the numbers of a PRISMA 2020 flow diagram from the objects of a
#' search: records identified per database, duplicates removed, records
#' screened and excluded, and studies included. Every number is computed from
#' the data, so an agent cannot report counts it did not produce.
#'
#' @param identified The search results: a data frame with a `database`
#'   column (for example the `rbind()` of several searches), a named list of
#'   data frames (one per database), or a named vector of counts.
#' @param deduplicated Optional: the output of [dedupe_records()], or the
#'   number of records after removing duplicates.
#' @param screening Optional: the screening decisions, as a logical vector
#'   (`TRUE` include, `FALSE` exclude, `NA` not yet decided) or a data frame
#'   with a logical column `include`.
#' @param included Optional: the number (or a data frame) of studies included
#'   after full-text assessment.
#' @return A data frame with columns `stage`, `database` (for the
#'   identification rows, otherwise `""`) and `n`.
#' @examples
#' oa <- data.frame(title = c("A", "B", "C"), year = 2020, doi = c("10.1/a", "10.1/b", NA),
#'                  database = "OpenAlex")
#' cr <- data.frame(title = c("A", "D"), year = 2020, doi = c("10.1/a", "10.1/d"),
#'                  database = "Crossref")
#' all <- rbind(oa, cr)
#' d <- dedupe_records(all)
#' prisma_counts(all, d, screening = c(TRUE, FALSE, NA, TRUE), included = 1)
#' @export
prisma_counts <- function(identified, deduplicated = NULL, screening = NULL, included = NULL) {
  count <- function(x) if (is.data.frame(x)) nrow(x) else as.integer(sum(x))
  per_db <- if (is.data.frame(identified)) {
    if (!"database" %in% names(identified)) .stop("`identified` needs a `database` column.")
    # A row found by several databases ("OpenAlex; Crossref") counts once per database.
    tab <- table(unlist(strsplit(identified$database, ";\\s*")))
    stats::setNames(as.integer(tab), names(tab))
  } else if (is.list(identified)) {
    vapply(identified, count, integer(1))
  } else {
    stats::setNames(as.integer(identified), names(identified))
  }
  if (is.null(names(per_db)) || any(!nzchar(names(per_db)))) {
    .stop("Name each database in `identified` (for example list(OpenAlex = res1, Crossref = res2)).")
  }
  total <- sum(per_db)
  rows <- list(data.frame(stage = "Records identified", database = names(per_db),
                          n = unname(per_db), stringsAsFactors = FALSE),
               data.frame(stage = "Records identified (total)", database = "", n = total))
  add <- function(stage, n) {
    rows[[length(rows) + 1]] <<- data.frame(stage = stage, database = "", n = as.integer(n))
  }
  if (!is.null(deduplicated)) {
    n_after <- count(deduplicated)
    add("Duplicates removed", total - n_after)
    add("Records after removing duplicates", n_after)
  }
  if (!is.null(screening)) {
    dec <- if (is.data.frame(screening)) screening$include else screening
    if (is.null(dec)) .stop("`screening` needs a logical column `include`.")
    dec <- as.logical(dec)
    add("Records screened", length(dec))
    add("Records excluded at screening", sum(dec %in% FALSE))
    add("Records awaiting a human decision", sum(is.na(dec)))
    add("Records retained for full-text assessment", sum(dec %in% TRUE))
  }
  if (!is.null(included)) add("Studies included", count(included))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ---- Tools for agents ----------------------------------------------------------

# The model sees a compact summary of the records (to save tokens); the full
# data frame stays in R as the tool's value. call_tool() prints a data frame
# to make the text for the model, so a print method on this class is enough.
.as_lit_records <- function(df) {
  class(df) <- unique(c("lit_records", class(df)))
  df
}

#' @export
print.lit_records <- function(x, ...) {
  total <- attr(x, "total_found")
  class(x) <- setdiff(class(x), "lit_records")
  cat(nrow(x), " records", if (!is.null(total) && !is.na(total)) paste0(" (", total, " matches in total)"),
      ":\n", sep = "")
  for (i in seq_len(nrow(x))) {
    au <- strsplit(x$authors[i] %||% NA_character_, "; ", fixed = TRUE)[[1]]
    au <- if (length(au) > 3) paste(paste(au[1:3], collapse = "; "), "et al.") else paste(au, collapse = "; ")
    words <- strsplit(x$abstract[i] %||% "", "\\s+")[[1]]
    abs <- if (is.na(x$abstract[i])) "(no abstract)" else
      paste0(paste(utils::head(words, 60), collapse = " "), if (length(words) > 60) " ..." else "")
    cat(sprintf("[%d] %s (%s). %s. %s.\n    DOI: %s | OA: %s | found in: %s\n    %s\n",
                i, x$title[i], x$year[i], au, x$source[i] %||% NA, x$doi[i], x$oa_url[i],
                x$database[i], abs))
  }
  invisible(x)
}

# Shared argument descriptions (written for the model).
.lit_tool_args <- function(n_default) {
  list(
    query = arg_string("Search terms: the key concepts of the topic, for example 'loneliness intervention older adults randomized'. Short keyword queries work better than sentences."),
    n = arg_integer(paste0("How many records to return (default and at most ", n_default,
                           "; ask for fewer when a few are enough)."), required = FALSE)
  )
}

# How many records a tool call returns. The researcher's `n` is the limit:
# the model may ask for fewer, never for more (each record costs tokens).
.lit_n <- function(n, limit) {
  n <- suppressWarnings(as.integer(n %||% limit))
  if (length(n) != 1 || is.na(n)) n <- as.integer(limit)
  max(1L, min(as.integer(limit), n))
}

#' Research search tools for agents
#'
#' Each function returns a tool (see [define_tool()]) that an [agent()] can
#' call. The model sees a compact list (title, year, authors, source, DOI,
#' open-access link and the first 60 words of the abstract); the complete
#' data frame is kept as the tool's R value. The arguments of these functions
#' are the researcher's settings: the model works within them.
#'
#' * `tool_openalex()`: [search_openalex()]; the model may also set years.
#' * `tool_crossref()`: [search_crossref()].
#' * `tool_semantic_scholar()`: [search_semantic_scholar()].
#' * `tool_europepmc()`: [search_europepmc()].
#' * `tool_arxiv()`: [search_arxiv()].
#' * `tool_unpaywall()`: [find_open_access()].
#' * `tool_get_work()`: [get_work()], one work with its COMPLETE abstract (a
#'   free OpenAlex lookup), for when a verbatim quote is needed: the search
#'   tools show only the start of each abstract.
#' * `tool_snowball()`: [get_references()] and [get_citations()].
#'
#' @inheritParams search_openalex
#' @param n The number of records a call returns, and the most the model
#'   can ask for (it may ask for fewer). Every record costs tokens.
#' @return An object of class `agent_tool`.
#' @examples
#' tool_openalex()
#' cat(tool_schema(tool_snowball()))
#' \dontrun{
#' scout <- agent("scout", "Find key papers on the topic and list their DOIs.",
#'                tools = list(tool_openalex(), tool_snowball()))
#' run(scout, "Key papers on exploratory graph analysis")
#' }
#' @name research_tools
NULL

#' @rdname research_tools
#' @export
tool_openalex <- function(n = 10, filter = NULL, type = NULL, sort = NULL) {
  force(filter); force(type); force(sort)
  n_default <- n
  fn <- function(query, n = NULL, year_from = NULL, year_to = NULL) {
    .as_lit_records(search_openalex(query, n = .lit_n(n, n_default), filter = filter,
                                    year_from = year_from, year_to = year_to,
                                    type = type, sort = sort))
  }
  define_tool(fn, name = "search_openalex",
    description = paste(
      "Search OpenAlex, a free index of scholarly works in every field,",
      "for articles, books and preprints. Returns title, year, authors, source, DOI,",
      "open-access link and the start of the abstract. Use it first for any literature search."),
    args = c(.lit_tool_args(n),
             list(year_from = arg_integer("Earliest publication year, for example 2015.", required = FALSE),
                  year_to = arg_integer("Latest publication year.", required = FALSE))))
}

#' @rdname research_tools
#' @export
tool_crossref <- function(n = 10, filter = NULL) {
  force(filter)
  n_default <- n
  fn <- function(query, n = NULL) {
    .as_lit_records(search_crossref(query, n = .lit_n(n, n_default), filter = filter))
  }
  define_tool(fn, name = "search_crossref",
    description = paste(
      "Search Crossref, the registry of DOIs deposited by publishers. Best for finding the",
      "DOI of a known paper from its title, authors or a full reference string. Few abstracts."),
    args = .lit_tool_args(n))
}

#' @rdname research_tools
#' @export
tool_semantic_scholar <- function(n = 10) {
  n_default <- n
  fn <- function(query, n = NULL) {
    .as_lit_records(search_semantic_scholar(query, n = .lit_n(n, n_default)))
  }
  define_tool(fn, name = "search_semantic_scholar",
    description = paste(
      "Search Semantic Scholar for papers by relevance. Often has abstracts and",
      "open-access PDF links. Plain keywords only (no boolean operators)."),
    args = .lit_tool_args(n))
}

#' @rdname research_tools
#' @export
tool_europepmc <- function(n = 10) {
  n_default <- n
  fn <- function(query, n = NULL) {
    .as_lit_records(search_europepmc(query, n = .lit_n(n, n_default)))
  }
  define_tool(fn, name = "search_europepmc",
    description = paste(
      "Search Europe PMC (PubMed, PubMed Central and preprints) for biomedical, health and",
      "life-science literature with abstracts. Supports AND, OR, NOT and quoted phrases."),
    args = .lit_tool_args(n))
}

#' @rdname research_tools
#' @export
tool_arxiv <- function(n = 10) {
  n_default <- n
  fn <- function(query, n = NULL) {
    .as_lit_records(search_arxiv(query, n = .lit_n(n, n_default)))
  }
  define_tool(fn, name = "search_arxiv",
    description = paste(
      "Search arXiv preprints (computer science, statistics, mathematics, physics,",
      "quantitative biology). Every result has a free PDF. Preprints may not be peer reviewed."),
    args = .lit_tool_args(n))
}

#' @rdname research_tools
#' @export
tool_unpaywall <- function() {
  fn <- function(doi) find_open_access(doi)
  define_tool(fn, name = "find_open_access",
    description = paste(
      "Find a free, legal full-text copy (open access) of a paper from its DOI.",
      "Returns whether it is open access, the best link, the PDF link and the license."),
    args = list(doi = arg_string("The DOI, for example '10.1080/00273171.2020.1779642'.")))
}

#' @rdname research_tools
#' @export
tool_get_work <- function() {
  fn <- function(doi) {
    w <- get_work(doi)
    paste0("Title: ", w$title, " (", w$year, "). ", w$authors, ". ", w$source, ".\n",
           "DOI: ", w$doi, "\n",
           "Abstract: ", if (is.na(w$abstract)) "(OpenAlex has no abstract for this work)" else w$abstract)
  }
  define_tool(fn, name = "get_work",
    description = paste(
      "Get one work from OpenAlex by its DOI, with the COMPLETE abstract. Search results",
      "show only the start of each abstract: call this before quoting an abstract verbatim."),
    args = list(doi = arg_string("The DOI, for example '10.1080/00273171.2020.1779642'.")))
}

#' @rdname research_tools
#' @export
tool_snowball <- function(n = 20) {
  n_default <- n
  fn <- function(work, direction, n = NULL) {
    n <- .lit_n(n, n_default)
    res <- if (direction == "references") get_references(work, n = n) else get_citations(work, n = n)
    .as_lit_records(res)
  }
  define_tool(fn, name = "snowball",
    description = paste(
      "Follow citations from one paper (snowballing). direction 'references' lists the works",
      "the paper cites (older work); 'citations' lists the works that cite it (newer work,",
      "most cited first). Use it on the most relevant papers found by a search."),
    args = list(
      work = arg_string("DOI or OpenAlex ID (W123...) of the starting paper."),
      direction = arg_enum("Which way to follow citations.", c("references", "citations")),
      n = arg_integer(paste0("How many works to return (default and at most ", n, ")."),
                      required = FALSE)))
}
