# Small helpers used across the package. None of them are exported.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.pkg_env <- new.env(parent = emptyenv())

# Read a package option with a default ("researchAgentR.<name>").
.opt <- function(name, default = NULL) {
  getOption(paste0("researchAgentR.", name), default)
}

.verbose <- function() isTRUE(.opt("verbose", TRUE))

.msg <- function(...) if (.verbose()) message(...)

.stop <- function(...) stop(paste0(...), call. = FALSE)

.warn <- function(...) warning(paste0(...), call. = FALSE)

.now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")

# JSON helpers: jsonlite with the settings every API in this package expects.
.to_json <- function(x, pretty = FALSE) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null",
                   digits = NA, pretty = pretty)
}

.from_json <- function(x) {
  jsonlite::fromJSON(x, simplifyVector = FALSE)
}

# A stable hash of any R object (used for cache keys and frozen rubrics).
# Written in binary mode so the hash is identical on Windows, macOS and Linux
# (a cache recorded on one computer must replay on another).
.hash <- function(x) {
  f <- tempfile()
  on.exit(unlink(f))
  writeBin(charToRaw(enc2utf8(as.character(.to_json(x)))), f)
  unname(tools::md5sum(f))
}

#' Approximate token count
#'
#' Estimates how many tokens a text uses, with the common rule of thumb of
#' about four characters per token for English text. Providers count tokens
#' with their own tokenizers, so the episode ledger always records the counts
#' the provider reports; this function is for planning context budgets before
#' a call is made.
#'
#' @param x A character vector, or any object (converted to JSON first).
#' @return An integer vector with one estimate per element of `x`.
#' @examples
#' count_tokens("Exploratory graph analysis estimates the number of dimensions.")
#' count_tokens(c("short", strrep("long text ", 100)))
#' @export
count_tokens <- function(x) {
  if (!is.character(x)) x <- as.character(.to_json(x))
  x[is.na(x)] <- ""
  as.integer(ceiling(nchar(x, type = "chars") / 4))
}

# Cut a text to roughly `max_tokens`, keeping the head and the tail, and say
# how much was removed (so the model knows the output is incomplete).
.truncate_tokens <- function(text, max_tokens) {
  if (is.null(max_tokens) || count_tokens(text) <= max_tokens) return(text)
  keep_chars <- max_tokens * 4
  head_n <- floor(keep_chars * 0.7)
  tail_n <- floor(keep_chars * 0.3)
  n <- nchar(text)
  paste0(substr(text, 1, head_n),
         "\n[... ", n - head_n - tail_n, " characters removed to fit the ",
         max_tokens, "-token budget ...]\n",
         substr(text, n - tail_n + 1, n))
}

# Turn any tool result into text for the model.
.as_text <- function(x) {
  if (is.character(x)) return(paste(x, collapse = "\n"))
  if (is.data.frame(x)) {
    return(paste(utils::capture.output(print(x, row.names = FALSE)), collapse = "\n"))
  }
  as.character(.to_json(x, pretty = TRUE))
}

# ---- Subsetting the package's data-frame classes ---------------------------------
# A row subset (results[results$pass, ]) keeps the class and the attributes
# that its print method needs; a column subset (results[, c("id", "pass")])
# becomes a plain data frame, because the special print method needs columns
# that are no longer there.
.subset_keep <- function(x, out) {
  if (!is.data.frame(out)) return(out)
  if (all(names(x) %in% names(out))) {
    extra <- setdiff(names(attributes(x)), c("names", "row.names", "class"))
    for (a in extra) attr(out, a) <- attr(x, a)
    class(out) <- class(x)
  } else {
    class(out) <- "data.frame"
  }
  out
}

#' @export
`[.task_set` <- function(x, ...) .subset_keep(x, NextMethod())
#' @export
`[.eval_results` <- function(x, ...) .subset_keep(x, NextMethod())
#' @export
`[.manuscript_review` <- function(x, ...) .subset_keep(x, NextMethod())
#' @export
`[.skill_check` <- function(x, ...) .subset_keep(x, NextMethod())
#' @export
`[.skill_test` <- function(x, ...) .subset_keep(x, NextMethod())
#' @export
`[.style_profile` <- function(x, ...) .subset_keep(x, NextMethod())
#' @export
`[.cost_ledger` <- function(x, ...) .subset_keep(x, NextMethod())
#' @export
`[.citation_check` <- function(x, ...) .subset_keep(x, NextMethod())
#' @export
`[.lit_records` <- function(x, ...) .subset_keep(x, NextMethod())

#' Save a table as a CSV file that spreadsheet programs open correctly
#'
#' `write.csv()` writes UTF-8 text without a byte-order mark, and Excel and
#' Numbers on macOS then read it in an older encoding, so characters from
#' model output show up garbled (one hyphen as three odd characters).
#' `save_table()` writes
#' UTF-8 with a byte-order mark, which these programs recognize, and by
#' default replaces typographic punctuation that models like to emit
#' (non-breaking hyphens, curly quotes, narrow spaces) with plain ASCII.
#' Letters with accents and symbols such as the greater-or-equal sign are kept.
#'
#' @param x A data frame.
#' @param file Path of the CSV file.
#' @param plain_punctuation Replace typographic hyphens, dashes, quotes and
#'   spaces with ASCII ones?
#' @return The file path, invisibly. `read.csv(file, fileEncoding =
#'   "UTF-8-BOM")` reads it back.
#' @examples
#' d <- data.frame(format = "Five\\u2011point scale from \\u2018never\\u2019 to \\u2018often\\u2019")
#' f <- save_table(d, tempfile(fileext = ".csv"))
#' read.csv(f, fileEncoding = "UTF-8-BOM")
#' @export
save_table <- function(x, file, plain_punctuation = TRUE) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (j in seq_along(x)) {
    if (is.list(x[[j]])) x[[j]] <- vapply(x[[j]], function(v) paste(unlist(v), collapse = "; "), character(1))
    if (is.character(x[[j]]) && plain_punctuation) x[[j]] <- .plain_punctuation(x[[j]])
  }
  con <- file(file, open = "wb")
  on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)   # the UTF-8 byte-order mark
  lines <- utils::capture.output(utils::write.csv(x, row.names = FALSE))
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))), con)
  invisible(file)
}

.plain_punctuation <- function(x) {
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2015\u2212]", "-", x)
  x <- gsub("[\u2018\u2019\u201A\u2032]", "'", x)
  x <- gsub("[\u201C\u201D\u201E\u2033]", "\"", x)
  x <- gsub("[\u00A0\u2007\u2009\u202F]", " ", x)
  gsub("\u2026", "...", x)
}
