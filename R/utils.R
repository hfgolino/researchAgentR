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
    # (a call finds the function plain_punctuation(), not the argument of the same name)
    if (is.character(x[[j]]) && plain_punctuation) x[[j]] <- plain_punctuation(x[[j]])
  }
  # The CSV text is built here rather than with write.csv(), which converts
  # text to the system's encoding first (and loses characters where that is
  # not UTF-8). Quoting follows write.csv(): text in double quotes, inner
  # quotes doubled, NA unquoted.
  q <- function(v) paste0("\"", gsub("\"", "\"\"", enc2utf8(v), fixed = TRUE), "\"")
  cols <- lapply(x, function(v) {
    if (is.factor(v)) v <- as.character(v)
    out <- if (is.character(v)) q(v) else as.character(v)
    out[is.na(v)] <- "NA"
    out
  })
  rows <- if (nrow(x)) do.call(paste, c(cols, sep = ",")) else character(0)
  con <- file(file, open = "wb")
  on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)   # EF BB BF: the UTF-8 byte-order mark
  writeBin(charToRaw(paste0(paste(c(paste(q(names(x)), collapse = ","), rows), collapse = "\n"), "\n")), con)
  invisible(file)
}

#' Replace typographic punctuation with plain characters
#'
#' Models and word processors write characters that look like ordinary
#' punctuation but are other characters: narrow and non-breaking spaces,
#' non-breaking hyphens, dashes, curly quotes. A comparison such as
#' `grepl("p = .03", text)` then fails for a text that reads `p = .03` on
#' the screen. This function replaces them with the plain characters of the
#' keyboard and removes the invisible ones (zero-width spaces, soft hyphens).
#' Letters with accents and symbols such as the greater-or-equal sign are
#' kept.
#'
#' The graders ([grader_code()] and relatives), [check_claims()] and the
#' quotation checks of the writing tools do this before they compare texts,
#' and [save_table()] before it writes a file.
#'
#' @param x A character vector (anything else is returned unchanged).
#' @return `x` with plain hyphens, quotes, spaces and three dots for an
#'   ellipsis.
#' @examples
#' x <- "p\u202f=\u202f.03 on a five\u2011point scale"
#' grepl("p = .03", x, fixed = TRUE)                       # FALSE: narrow no-break spaces
#' grepl("p = .03", plain_punctuation(x), fixed = TRUE)    # TRUE
#' @export
plain_punctuation <- function(x) {
  if (!is.character(x)) return(x)
  # patterns built from code points, so they work in any locale
  u <- function(...) paste0("[", intToUtf8(c(...), multiple = FALSE), "]")
  x <- enc2utf8(x)
  x <- gsub(u(0x2010:0x2015, 0x2212), "-", x, perl = TRUE)                    # hyphens, dashes, minus sign
  x <- gsub(u(0x2018:0x201B, 0x2032), "'", x, perl = TRUE)                    # single quotes, prime
  x <- gsub(u(0x201C:0x201F, 0x2033), "\"", x, perl = TRUE)                   # double quotes, double prime
  x <- gsub(u(0x00A0, 0x1680, 0x2000:0x200A, 0x202F, 0x205F, 0x3000), " ", x, perl = TRUE)   # spaces of other widths
  x <- gsub(u(0x00AD, 0x200B:0x200D, 0x2060, 0xFEFF), "", x, perl = TRUE)     # invisible characters
  gsub(intToUtf8(0x2026), "...", x, fixed = TRUE)
}

#' Read and write text files as UTF-8
#'
#' All text the package saves (episodes, notes, skills, reports, revised
#' manuscripts) is written as UTF-8, and all text it reads is read as UTF-8,
#' whatever the computer's own encoding (Windows often uses another one by
#' default). These two functions do the same for your own files.
#' `read_text()` falls back to Latin-1 for lines that are not valid UTF-8, so
#' an old file still reads.
#'
#' @param text A character vector (one element per line).
#' @param file Path of the file.
#' @param append Add to the end of the file instead of replacing it?
#' @param encoding The encoding of the file; always `"UTF-8"` for files the
#'   package writes.
#' @param n Number of lines to read (`-1` for all).
#' @return `write_text()` returns the path, invisibly; `read_text()` the lines.
#' @examples
#' f <- tempfile(fileext = ".md")
#' write_text(c("# Notes", "Caf\u00e9 \u03b1 = .91"), f)
#' read_text(f)
#' @export
write_text <- function(text, file, append = FALSE, encoding = "UTF-8") {
  text <- as.character(text)
  if (!identical(toupper(encoding), "UTF-8")) {
    con <- file(file, open = if (append) "a" else "w", encoding = encoding)
    on.exit(close(con))
    writeLines(text, con)
    return(invisible(file))
  }
  con <- file(file, open = if (append) "ab" else "wb")
  on.exit(close(con))
  if (length(text)) writeBin(charToRaw(enc2utf8(paste0(paste(text, collapse = "\n"), "\n"))), con)
  invisible(file)
}

#' @rdname write_text
#' @export
read_text <- function(file, n = -1L, encoding = "UTF-8") {
  if (!identical(toupper(encoding), "UTF-8")) {
    con <- file(file, open = "r", encoding = encoding)
    on.exit(close(con))
    return(enc2utf8(readLines(con, n = n, warn = FALSE)))
  }
  x <- readLines(file, n = n, warn = FALSE, encoding = "UTF-8")
  bad <- !validUTF8(x)   # lines that are not UTF-8: read them as Latin-1
  if (any(bad)) x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
  if (length(x)) x[1] <- sub(paste0("^", intToUtf8(0xFEFF)), "", x[1], perl = TRUE)   # a byte-order mark
  x
}
