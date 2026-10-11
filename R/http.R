# Every HTTP request in the package goes through .request() and .perform():
# one place for authentication, timeouts, retries with backoff, polite rate
# limiting, readable errors and the response cache (see use_cache()).

.transient_statuses <- c(408, 429, 500, 502, 503, 504, 529)

# Build a request. `service` names the key to use (see .key_vars) or is NA.
.request <- function(url, body = NULL, method = NULL, auth = c("bearer", "anthropic", "none"),
                     key = "", headers = list(), query = list(), realm = NULL, rpm = NULL,
                     timeout = .opt("timeout", 120)) {
  auth <- match.arg(auth)
  req <- httr2::request(url)
  if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  if (auth == "bearer" && nzchar(key)) req <- httr2::req_auth_bearer_token(req, key)
  if (auth == "anthropic") {
    req <- httr2::req_headers(req, `x-api-key` = key, `anthropic-version` = "2023-06-01",
                              .redact = "x-api-key")
  }
  if (length(headers)) req <- do.call(httr2::req_headers, c(list(req), headers))
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE, null = "null", digits = NA)
  if (!is.null(method)) req <- httr2::req_method(req, method)
  req <- httr2::req_user_agent(req, paste0("researchAgentR/", utils::packageVersion("researchAgentR"),
                                           " (https://github.com/hfgolino/researchAgentR)"))
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_retry(
    req,
    max_tries = .opt("max_tries", 4),
    is_transient = .is_transient,
    backoff = function(i) min(60, 2^i + stats::runif(1))
  )
  if (!is.null(rpm)) req <- httr2::req_throttle(req, rate = rpm / 60, realm = realm %||% url)
  httr2::req_error(req, body = .error_body)
}

# A 429 is worth retrying unless the provider says the spending cap was
# reached (Anthropic sends such 429s without a retry-after header).
.is_transient <- function(resp) {
  status <- httr2::resp_status(resp)
  if (!status %in% .transient_statuses) return(FALSE)
  if (status == 429 && grepl("anthropic", httr2::resp_url(resp)) &&
      !httr2::resp_header_exists(resp, "retry-after")) return(FALSE)
  TRUE
}

.error_body <- function(resp) {
  msg <- tryCatch({
    b <- httr2::resp_body_json(resp)
    b$error$message %||% b$message %||% b$detail %||% b$error %||% ""
  }, error = function(e) "")
  hint <- switch(as.character(httr2::resp_status(resp)),
    "401" = "Check the API key (see ?set_keys and verify_setup()).",
    "403" = "The key may not have access to this model or endpoint.",
    "404" = "Check the model name with list_provider_models() or model_registry().",
    "413" = "The request is too large: reduce the context (see ?filter_output).",
    "429" = "Rate limit or quota reached: wait, use a smaller model, or see ?budget.",
    NULL)
  c(if (is.character(msg) && nzchar(msg)) msg, hint)
}

# Perform a request, through the cache when caching is on. Returns a list:
# body (parsed JSON), status, latency (seconds), headers, cached (logical).
.perform <- function(req, cache = TRUE) {
  mode <- if (cache) .cache_mode() else "off"
  key <- NULL
  if (mode != "off") {
    key <- .hash(list(url = req$url, body = req$body$data, method = req$method))
    hit <- .cache_read(key)
    if (!is.null(hit)) return(c(hit, cached = TRUE))
    if (mode == "replay") {
      .stop("No recording for this request in the replay cache (",
            .cache_dir(), "). Run it once with use_cache(\"record\") while online.")
    }
  }
  t0 <- Sys.time()
  resp <- httr2::req_perform(req)
  out <- list(
    body = .resp_body(resp),
    status = httr2::resp_status(resp),
    latency = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    headers = .keep_headers(resp),
    recorded_at = .now()
  )
  if (mode %in% c("record", "auto")) .cache_write(key, out)
  c(out, cached = FALSE)
}

.resp_body <- function(resp) {
  type <- httr2::resp_content_type(resp)
  if (grepl("json", type)) return(httr2::resp_body_json(resp, simplifyVector = FALSE))
  httr2::resp_body_string(resp)
}

# Rate-limit headers are worth keeping (they teach how close a run came to the
# free-tier limits); everything else is dropped.
.keep_headers <- function(resp) {
  h <- httr2::resp_headers(resp)
  keep <- grepl("^(x-ratelimit|retry-after|anthropic-ratelimit|request-id|x-request-id)",
                tolower(names(h)))
  as.list(h[keep])
}

# Parallel requests with a cap on simultaneous connections, written to work
# with both httr2 1.0.x (no max_active) and later versions.
.perform_parallel <- function(reqs, max_active = 5) {
  if ("max_active" %in% names(formals(httr2::req_perform_parallel))) {
    httr2::req_perform_parallel(reqs, on_error = "continue", progress = FALSE,
                                max_active = max_active)
  } else {
    httr2::req_perform_parallel(reqs, on_error = "continue", progress = FALSE)
  }
}
