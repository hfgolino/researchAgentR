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
  if (length(headers)) {
    # header keys (x-api-key and similar) never appear when a request is printed
    secret <- names(headers)[grepl("key|token|authorization", names(headers), ignore.case = TRUE)]
    req <- do.call(httr2::req_headers, c(list(req), headers, list(.redact = secret)))
  }
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE, null = "null", digits = NA)
  if (!is.null(method)) req <- httr2::req_method(req, method)
  req <- httr2::req_user_agent(req, paste0("researchAgentR/", utils::packageVersion("researchAgentR"),
                                           " (https://github.com/hfgolino/researchAgentR)"))
  req <- httr2::req_timeout(req, timeout)
  req <- .retry_policy(req)
  if (!is.null(rpm)) req <- .throttle(req, rpm, realm %||% url)
  httr2::req_error(req, body = .error_body)
}

# Retry 429 and 5xx responses with exponential backoff, for at most
# `max_seconds` in total when a time limit is running (see .set_deadline()).
.retry_policy <- function(req, max_tries = .opt("max_tries", 4), max_seconds = NULL) {
  httr2::req_retry(
    req,
    max_tries = max_tries,
    max_seconds = max_seconds,
    is_transient = .is_transient,
    backoff = function(i) min(60, 2^i + stats::runif(1))
  )
}

# ---- Time limits ---------------------------------------------------------------
# R's own time limit (setTimeLimit) cannot interrupt a request that is waiting
# for a server: a tool with a 60-second limit once waited 13 minutes on a
# service that had stopped answering. So the limit of the running tool call
# or episode is also given to the request: `.pkg_env$deadline` is the time by
# which a live request must end. A limit can only tighten the one in force;
# the caller restores the value returned here when it is done.
.set_deadline <- function(seconds) {
  old <- .pkg_env$deadline
  if (is.numeric(seconds) && is.finite(seconds)) {
    new <- Sys.time() + seconds
    if (is.null(old) || new < old) .pkg_env$deadline <- new
  }
  old
}

.seconds_left <- function() {
  if (is.null(.pkg_env$deadline)) return(Inf)
  as.numeric(difftime(.pkg_env$deadline, Sys.time(), units = "secs"))
}

# The request, limited to the time that is left (no change without a deadline).
.within_deadline <- function(req) {
  left <- .seconds_left()
  if (!is.finite(left)) return(req)
  if (left < 1) .stop("The time limit was reached before this request could be sent.")
  # Whole seconds: recent versions of httr2 accept only a whole number of
  # seconds for retries, and the time left is never one (299.99 seconds a
  # moment after a 300-second limit was set).
  left <- floor(left)
  own <- (req$options$timeout_ms %||% Inf) / 1000      # the request's own timeout
  req <- httr2::req_timeout(req, min(left, own))
  # The request keeps its own number of tries (some are sent once only).
  .retry_policy(req, max_tries = req$policies$retry_max_tries %||% .opt("max_tries", 4),
                max_seconds = left)
}

# Space requests evenly at `rpm` per minute. httr2 >= 1.1 uses a token bucket;
# capacity = 1 avoids a burst of requests at the start (polite to free APIs).
.throttle <- function(req, rpm, realm) {
  if ("capacity" %in% names(formals(httr2::req_throttle))) {
    httr2::req_throttle(req, capacity = 1, fill_time_s = 60 / rpm, realm = realm)
  } else {
    httr2::req_throttle(req, rate = rpm / 60, realm = realm)
  }
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
  # Providers put the message in different places: error.message (OpenAI,
  # Groq, Anthropic), error as a plain string (Ollama), message or detail.
  msg <- tryCatch({
    b <- httr2::resp_body_json(resp)
    m <- if (is.list(b$error)) b$error$message else b$error
    m <- m %||% b$message %||% b$detail %||% ""
    if (is.character(m)) paste(m, collapse = " ") else ""
  }, error = function(e) "")
  url <- httr2::resp_url(resp)
  hint <- switch(as.character(httr2::resp_status(resp)),
    "401" = "Check the API key (see ?set_keys and verify_setup()).",
    "403" = if (grepl("/(chat/completions|responses|messages|models|embed|embeddings)$", url))
      "The key may not have access to this model or endpoint." else
      "The server refused the request (some publishers block automated readers; try read_page(via = \"jina\") or another source).",
    "404" = if (grepl(":11434/", url))
      "Ollama does not have this model: pull it first in a terminal (ollama pull <model>), then check ollama_status()." else
      if (grepl("/(chat/completions|responses|messages|models|embed|embeddings)$", url))
      "Check the model name with list_provider_models() or model_registry()." else
      "The resource was not found (check the identifier, for example the DOI).",
    "413" = "The request is too large: reduce the context (see ?filter_output).",
    "429" = "Rate limit or quota reached: wait, use a smaller model, or see ?budget.",
    NULL)
  c(if (is.character(msg) && nzchar(msg)) msg, hint)
}

# Perform a request, through the cache when caching is on. Returns a list:
# body (parsed JSON), status, latency (seconds), headers, cached (logical).
# `need` is the message to stop with when a LIVE call is needed but its key is
# missing (see .key_needed()): a recorded response replays without the key,
# so a student without, say, a Tavily key can still replay a recorded search.
.perform <- function(req, cache = TRUE, need = NULL) {
  mode <- if (cache) .cache_mode() else "off"
  key <- NULL
  if (mode != "off") {
    # The salt separates repeated trials of the same request (see run_eval()):
    # each trial is recorded and replayed separately instead of replaying trial 1.
    key <- .hash(list(url = .url_without_secrets(req$url), body = req$body$data,
                      method = req$method, salt = .pkg_env$cache_salt))
    # "record" always calls the API (and overwrites); "auto" and "replay" read
    hit <- if (mode == "record") NULL else .cache_read(key)
    if (!is.null(hit)) return(c(hit, cached = TRUE))
    if (mode == "replay") {
      .stop("No recording for this request in the replay cache (",
            .cache_dir(), "). Run it once with use_cache(\"record\") while online.")
    }
  }
  if (!is.null(need)) .stop(need)   # a live call, and no key for it
  req <- .within_deadline(req)      # the time limit of the running tool or episode
  # The last request body is kept for inspection (no keys: those travel in
  # headers or are stripped from URLs): researchAgentR:::.pkg_env$last_body
  .pkg_env$last_body <- req$body$data
  t0 <- Sys.time()
  # No answer at all (no connection, nothing listening, a timeout) is reported
  # in plain words instead of as a curl error.
  resp <- tryCatch(httr2::req_perform(req),
                   httr2_failure = function(e) .stop(.connection_message(req$url, e)))
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

# Keys and e-mail addresses in the URL are not part of the cache key, so a demo
# recorded with the instructor's key replays for students with their own key.
.url_without_secrets <- function(url) {
  url <- gsub("([?&])(api_key|apikey|key|mailto|email)=[^&]*&?", "\\1", url)
  # no leftover "?" or "&": the cache key is the same with and without the
  # optional key or e-mail (some students have an OpenAlex key, some do not)
  sub("[?&]$", "", url)
}

# The message for a request that got no answer at all.
.connection_message <- function(url, e) {
  host <- sub("^(https?://[^/?]+).*$", "\\1", url)
  detail <- sub("\\s+$", "", conditionMessage(e$parent %||% e))
  if (grepl("//(localhost|127\\.0\\.0\\.1)(:|$)", host)) {
    what <- if (grepl(":11434$", host)) {
      paste("Ollama is not running: open the Ollama app, or run \"ollama serve\" in a",
            "terminal, then try again. Local models are optional (see Activity 00).")
    } else {
      "Start the local server, or check the address and port."
    }
    return(paste0("Nothing answered at ", host, ". ", what))
  }
  paste0("Could not reach ", host, " (", detail, "). Check the internet connection; ",
         "a VPN, proxy or firewall can also block the address.")
}

# NULL when the key for `service` is available; otherwise the message that
# .perform() stops with if a live call turns out to be needed.
.key_needed <- function(service, key = .get_key(service), how = NULL,
                        var = .key_vars[[service]]) {
  if (nzchar(key)) return(NULL)
  paste0("No ", service, " key, and no recorded response for this request. Add the line ",
         var, "=your-key to ~/.Renviron and restart R (see ?set_keys)",
         if (is.null(how)) "." else paste0("; ", how, "."))
}

.resp_body <- function(resp) {
  if (!httr2::resp_has_body(resp)) return(NULL)
  type <- httr2::resp_content_type(resp)
  if (grepl("json", type)) return(httr2::resp_body_json(resp, simplifyVector = FALSE))
  httr2::resp_body_string(resp)
}

# Rate-limit headers are worth keeping (they teach how close a run came to the
# free-tier limits); everything else is dropped.
.keep_headers <- function(resp) {
  h <- httr2::resp_headers(resp)
  keep <- grepl("^(x-ratelimit|retry-after|anthropic-ratelimit|request-id|x-request-id|mcp-session-id)",
                tolower(names(h)))
  # a plain named list of strings (an httr2_headers object cannot be saved as JSON)
  stats::setNames(lapply(unclass(h)[keep], as.character), names(h)[keep])
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
