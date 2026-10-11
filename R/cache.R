# Two kinds of caching, both saving REAL results to disk:
#   use_cache()  caches every HTTP response (model calls, API calls) by request
#   keep()       caches the result of any R expression under a name
# A live demo can then be replayed offline in class from what was recorded.

#' Cache API responses on disk
#'
#' With caching on, every response from a model or research API is saved
#' under a hash of the request. Repeating the same request reads the saved
#' response instead of calling the API again, so a demo recorded once can be
#' replayed offline and costs nothing the second time.
#'
#' * `"off"`: no caching (the default).
#' * `"auto"`: read a saved response when there is one, otherwise call the
#'   API and save the response.
#' * `"record"`: call the API and save every response (refreshes recordings).
#' * `"replay"`: only read saved responses; a request with no recording is an
#'   error, so a class demo can never touch the network by accident.
#'
#' Cached responses are marked in the episode ledger (`cached = TRUE`), and
#' their cost is reported as zero, because nothing was billed.
#'
#' @param mode One of `"off"`, `"auto"`, `"record"`, `"replay"`.
#' @param dir Folder for the cache. Defaults to a per-user folder.
#' @return The previous mode, invisibly.
#' @examples
#' old <- use_cache("auto", dir = tempfile())
#' cache_dir()
#' use_cache(old)
#' @export
use_cache <- function(mode = c("off", "auto", "record", "replay"), dir = NULL) {
  mode <- match.arg(mode)
  old <- .cache_mode()
  .pkg_env$cache_mode <- mode
  if (!is.null(dir)) .pkg_env$cache_dir <- dir
  if (mode != "off") .msg("Cache mode: ", mode, " (", cache_dir(), ")")
  invisible(old)
}

#' @rdname use_cache
#' @export
cache_dir <- function() {
  d <- .pkg_env$cache_dir %||% .opt("cache_dir") %||%
    tools::R_user_dir("researchAgentR", "cache")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.cache_mode <- function() .pkg_env$cache_mode %||% .opt("cache", "off")
.cache_dir <- function() cache_dir()

.cache_file <- function(key) file.path(cache_dir(), "responses", paste0(key, ".rds"))

.cache_read <- function(key) {
  f <- .cache_file(key)
  if (file.exists(f)) readRDS(f) else NULL
}

.cache_write <- function(key, value) {
  f <- .cache_file(key)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(value, f)
}

#' Run an expression once and keep its result
#'
#' The first call evaluates `expr` and saves the result as `<name>.rds`; later
#' calls with the same name load the saved result without evaluating `expr`.
#' Use it around anything slow or costly: an agent run, an evaluation with
#' many trials, a batch of embeddings.
#'
#' @param name A short file name for the result, such as `"scout_run_1"`.
#' @param expr The expression to evaluate (only evaluated when needed).
#' @param refresh `TRUE` to evaluate again and overwrite the saved result.
#' @param dir Folder for kept results. Defaults to `"kept"` inside
#'   [cache_dir()].
#' @return The value of `expr`, with an attribute `kept_at` (the time it was
#'   first computed).
#' @examples
#' d <- tempfile()
#' x <- keep("slow_sum", {Sys.sleep(1); sum(1:10)}, dir = d)   # computes
#' y <- keep("slow_sum", {Sys.sleep(1); sum(1:10)}, dir = d)   # loads
#' attr(y, "kept_at")
#' @export
keep <- function(name, expr, refresh = FALSE, dir = file.path(cache_dir(), "kept")) {
  if (!grepl("^[A-Za-z0-9_.-]+$", name)) .stop("Use letters, digits, '_', '-' or '.' in `name`.")
  f <- file.path(dir, paste0(name, ".rds"))
  if (file.exists(f) && !refresh) {
    value <- readRDS(f)
    .msg("keep(): loaded '", name, "' (computed ", attr(value, "kept_at"), ").")
    return(value)
  }
  value <- expr
  attr(value, "kept_at") <- .now()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(value, f)
  value
}
