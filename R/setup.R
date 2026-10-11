# Checking the setup: keys, packages, local models and (optionally) one tiny
# request to each service.

#' Check that everything the course needs is in place
#'
#' Checks the R version, required and optional packages, which API keys are
#' stored (masked), and whether Ollama is running. With `test_apis = TRUE` it
#' also sends one tiny request to each service that has a key: model lists
#' for model providers (no tokens used) and a single-record lookup for
#' OpenAlex and Crossref (free).
#'
#' @param test_apis Send the test requests?
#' @return A data frame with one row per check (`check`, `status`,
#'   `detail`), printed and returned invisibly.
#' @examples
#' \dontrun{
#' verify_setup()
#' verify_setup(test_apis = TRUE)
#' }
#' @export
verify_setup <- function(test_apis = FALSE) {
  rows <- list()
  add <- function(check, status, detail = "") {
    rows[[length(rows) + 1]] <<- data.frame(check = check, status = status, detail = detail,
                                             stringsAsFactors = FALSE)
  }
  rv <- paste(R.version$major, R.version$minor, sep = ".")
  add("R version", if (getRversion() >= "4.3.0") "OK" else if (getRversion() >= "4.1.0") "OK (4.3+ recommended)" else "UPDATE", rv)
  for (p in c("httr2", "jsonlite")) add(paste("package", p), "OK", as.character(utils::packageVersion(p)))
  for (p in c("callr", "xml2", "pdftools", "ggplot2", "processx")) {
    ok <- requireNamespace(p, quietly = TRUE)
    add(paste("package", p), if (ok) "OK" else "optional, missing",
        if (ok) as.character(utils::packageVersion(p)) else paste0("install.packages(\"", p, "\")"))
  }
  for (s in names(.key_vars)) {
    k <- .get_key(s)
    add(paste("key", s), if (nzchar(k)) "set" else "not set", if (nzchar(k)) .mask(k) else .key_vars[[s]])
  }
  email <- .opt("email") %||% Sys.getenv("RESEARCHAGENTR_EMAIL")
  add("e-mail for Crossref/Unpaywall", if (nzchar(email)) "set" else "not set",
      if (nzchar(email)) email else "RESEARCHAGENTR_EMAIL in ~/.Renviron (polite pool)")
  o <- tryCatch(ollama_status(), error = function(e) NULL)
  add("Ollama", if (is.null(o)) "not running" else "running",
      if (is.null(o)) "optional: https://ollama.com" else paste(nrow(o), "models pulled"))
  if (test_apis) {
    test <- function(label, expr) {
      res <- tryCatch({ expr; "OK" }, error = function(e) paste("FAILED:", .shorten(conditionMessage(e), 120)))
      add(paste("API", label), if (identical(res, "OK")) "OK" else "FAILED", if (identical(res, "OK")) "" else res)
    }
    for (p in c("groq", "openai", "anthropic", "gemini", "mistral", "cerebras", "openrouter")) {
      if (nzchar(.get_key(p))) test(p, list_provider_models(p))
    }
    test("openalex", .openalex_ping())
    test("crossref", .crossref_ping())
  }
  out <- do.call(rbind, rows)
  print(out, row.names = FALSE)
  invisible(out)
}

.openalex_ping <- function() {
  q <- list()
  if (nzchar(.get_key("openalex"))) q$api_key <- .get_key("openalex")
  req <- .request("https://api.openalex.org/works/doi:10.1007/s11336-021-09820-y",
                  auth = "none", query = q)
  .perform(req, cache = FALSE)
}

.crossref_ping <- function() {
  req <- .request("https://api.crossref.org/works/10.1007/s11336-021-09820-y", auth = "none")
  .perform(req, cache = FALSE)
}

#' Is Ollama running, and which models are pulled?
#'
#' Ollama runs open-weight models on your own computer (free, and data stay
#' local). Install it from https://ollama.com, then pull a model in a
#' terminal, for example `ollama pull qwen3:4b`.
#'
#' @param host Address of the Ollama server.
#' @return A data frame of pulled models (`name`, `size_gb`), or an error if
#'   Ollama is not running.
#' @examples
#' \dontrun{
#' ollama_status()
#' }
#' @export
ollama_status <- function(host = "http://localhost:11434") {
  req <- .request(paste0(host, "/api/tags"), auth = "none", timeout = 5)
  req <- httr2::req_retry(req, max_tries = 1)
  r <- .perform(req, cache = FALSE)
  m <- r$body$models %||% list()
  data.frame(name = vapply(m, `[[`, character(1), "name"),
             size_gb = round(vapply(m, function(x) as.numeric(x$size %||% NA), numeric(1)) / 1e9, 2),
             stringsAsFactors = FALSE)
}
