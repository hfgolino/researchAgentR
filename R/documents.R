# Reading documents into a data frame of paragraphs, with the section each
# paragraph belongs to (used for retrieval and for citations).

.document_types <- c("pdf", "md", "rmd", "qmd", "tex", "txt")

#' Read documents into a data frame of paragraphs
#'
#' Reads one file or every supported file in a folder (and its subfolders):
#'
#' * `.pdf`: text of each page (needs the optional pdftools package); the
#'   section is the page, for example `"page 3"`;
#' * `.md`, `.Rmd`, `.qmd`: YAML header, code blocks and HTML comments are
#'   dropped; `#` headings become sections;
#' * `.tex`: comments, the preamble, math environments, figures' file names,
#'   labels and references are dropped; commands keep their text
#'   (`\emph{word}` becomes `word`), citations become `(cite: key)`, and
#'   `\section{Title}` starts the section "Title";
#' * `.txt`: paragraphs separated by blank lines.
#'
#' A file that cannot be read gets one row with `text = NA`; such files are
#' counted in a warning and listed in the attribute `"failed"`.
#'
#' @param path A file or a folder.
#' @param by `"paragraph"` (one row per paragraph) or `"file"` (one row per
#'   file, paragraphs joined by blank lines).
#' @return A data frame with `file` (the file name, relative to `path` when
#'   `path` is a folder), `section` (or `NA`), `paragraph` (number within the
#'   file; `NA` when `by = "file"`) and `text`.
#' @examples
#' f <- tempfile(fileext = ".md")
#' writeLines(c("---", "title: Notes", "---", "", "# Methods", "",
#'              "Participants were 300 adults.", "", "```{r}", "x <- 1", "```", "",
#'              "# Results", "", "Alpha was .85."), f)
#' read_documents(f)
#' @export
read_documents <- function(path, by = c("paragraph", "file")) {
  by <- match.arg(by)
  if (!file.exists(path)) .stop("No file or folder at ", path, ".")
  if (dir.exists(path)) {
    all <- list.files(path, recursive = TRUE, full.names = TRUE)
    ext <- tolower(tools::file_ext(all))
    files <- all[ext %in% .document_types]
    skipped <- length(all) - length(files)
    if (skipped) .msg("read_documents(): skipped ", skipped, " files of other types.")
    labels <- substring(normalizePath(files, winslash = "/"),
                        nchar(normalizePath(path, winslash = "/")) + 2)
  } else {
    if (!tolower(tools::file_ext(path)) %in% .document_types) {
      .stop("Supported files: ", paste0(".", .document_types, collapse = ", "), ".")
    }
    files <- path
    labels <- basename(path)
  }
  empty <- data.frame(file = character(0), section = character(0), paragraph = integer(0),
                      text = character(0), stringsAsFactors = FALSE)
  if (!length(files)) return(empty)

  failed <- character(0)
  rows <- lapply(seq_along(files), function(i) {
    d <- tryCatch(.read_one(files[i]), error = function(e) e)
    if (inherits(d, "error")) {
      failed <<- c(failed, paste0(labels[i], " (", conditionMessage(d), ")"))
      return(data.frame(file = labels[i], section = NA_character_, paragraph = NA_integer_,
                        text = NA_character_, stringsAsFactors = FALSE))
    }
    if (!nrow(d)) return(NULL)
    if (by == "file") {
      return(data.frame(file = labels[i], section = NA_character_, paragraph = NA_integer_,
                        text = paste(d$text, collapse = "\n\n"), stringsAsFactors = FALSE))
    }
    data.frame(file = labels[i], section = d$section, paragraph = seq_len(nrow(d)),
               text = d$text, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(empty), rows))
  rownames(out) <- NULL
  if (length(failed)) {
    .warn(length(failed), " of ", length(files), " files could not be read: ",
          .shorten(paste(failed, collapse = "; "), 300))
    attr(out, "failed") <- failed
  }
  out
}

# One file -> data.frame(section, text), one row per paragraph.
.read_one <- function(file) {
  ext <- tolower(tools::file_ext(file))
  if (ext == "pdf") return(.read_pdf(file))
  lines <- read_text(file, encoding = "UTF-8")
  switch(ext,
    tex = .paragraphs(strsplit(.tex_to_text(paste(lines, collapse = "\n")), "\n", fixed = TRUE)[[1]],
                      heading = .tex_heading),
    txt = .paragraphs(lines),
    .paragraphs(.markdown_lines(lines), heading = .md_heading))
}

.read_pdf <- function(file) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    .stop("Reading PDF files needs the pdftools package: install.packages(\"pdftools\").")
  }
  pages <- pdftools::pdf_text(file)
  out <- lapply(seq_along(pages), function(i) {
    d <- .paragraphs(strsplit(pages[i], "\n", fixed = TRUE)[[1]])
    if (nrow(d)) d$section <- paste("page", i)
    d
  })
  do.call(rbind, out)
}

# Group lines into paragraphs: a blank line ends a paragraph; a heading line
# (recognized by `heading`, which returns the title or NULL) ends it too and
# names the section of the paragraphs that follow.
.paragraphs <- function(lines, heading = function(line) NULL) {
  section <- NA_character_
  current <- character(0)
  sections <- character(0)
  texts <- character(0)
  flush <- function() {
    text <- trimws(gsub("[[:space:]]+", " ", paste(current, collapse = " ")))
    if (nzchar(text)) {
      sections <<- c(sections, section)
      texts <<- c(texts, text)
    }
    current <<- character(0)
  }
  for (line in lines) {
    h <- heading(line)
    if (!is.null(h)) {
      flush()
      section <- h
    } else if (!nzchar(trimws(line))) {
      flush()
    } else {
      current <- c(current, line)
    }
  }
  flush()
  data.frame(section = sections, text = texts, stringsAsFactors = FALSE)
}

.md_heading <- function(line) {
  if (!grepl("^#{1,6}[[:space:]]+", line)) return(NULL)
  trimws(gsub("^#{1,6}[[:space:]]+|[[:space:]]+#+[[:space:]]*$", "", line))
}

# Markdown, R Markdown and Quarto: drop the YAML header, fenced code blocks
# (R chunks and others) and HTML comments.
.markdown_lines <- function(lines) {
  first <- which(nzchar(trimws(lines)))[1]
  if (!is.na(first) && trimws(lines[first]) == "---") {
    end <- which(trimws(lines) %in% c("---", "..."))
    end <- end[end > first][1]
    if (!is.na(end)) lines <- lines[-seq_len(end)]
  }
  fence <- grepl("^[[:space:]]*(```|~~~)", lines)
  inside <- (cumsum(fence) %% 2 == 1) | fence   # the fences and what lies between them
  lines <- lines[!inside]
  text <- gsub("<!--.*?-->", "", paste(lines, collapse = "\n"), perl = TRUE)
  strsplit(text, "\n", fixed = TRUE)[[1]]
}

# ---- LaTeX ----------------------------------------------------------------------

.tex_section_marker <- "@@SECTION@@"

.tex_heading <- function(line) {
  if (!startsWith(line, .tex_section_marker)) return(NULL)
  trimws(substring(line, nchar(.tex_section_marker) + 1))
}

# Plain text from LaTeX source. Readable regular expressions, applied in an
# order that matters: comments, preamble, math, then commands from the
# innermost outwards, so that \section{The \emph{EGA} model} works.
.tex_to_text <- function(x) {
  x <- gsub("(?<!\\\\)%[^\n]*", "", x, perl = TRUE)                 # comments
  if (grepl("\\begin{document}", x, fixed = TRUE)) {                  # drop the preamble
    x <- sub("(?s)^.*?\\\\begin\\{document\\}", "", x, perl = TRUE)
    x <- sub("(?s)\\\\end\\{document\\}.*$", "", x, perl = TRUE)
  }
  math_env <- "(equation|align|gather|multline|eqnarray|displaymath|math|tabular|tikzpicture)"
  x <- gsub(paste0("(?s)\\\\begin\\{", math_env, "\\*?\\}.*?\\\\end\\{\\1\\*?\\}"), "\n", x, perl = TRUE)
  x <- gsub("(?s)\\\\\\[.*?\\\\\\]", "\n", x, perl = TRUE)            # \[ ... \]
  x <- gsub("(?s)\\$\\$.*?\\$\\$", "\n", x, perl = TRUE)              # $$ ... $$
  # inline math: keep the content, without dollars and backslashes
  m <- gregexpr("(?<!\\\\)\\$[^$]+\\$", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) gsub("\\\\([A-Za-z]+)|\\$", "\\1", v))
  # commands whose argument is not text
  drop <- "(label|ref|eqref|pageref|includegraphics|input|include|bibliography|bibliographystyle|vspace|hspace|url|href)"
  x <- gsub(paste0("\\\\", drop, "\\*?(\\[[^]]*\\])?\\{[^{}]*\\}"), "", x, perl = TRUE)
  x <- gsub("\\\\[A-Za-z]*cite[a-z]*\\*?(\\[[^]]*\\])*\\{([^{}]*)\\}", "(cite: \\2)", x, perl = TRUE)
  x <- gsub("\\\\begin\\{abstract\\}", paste0("\n\n", .tex_section_marker, "Abstract\n\n"), x)
  x <- gsub("\\\\(begin|end)\\{[^}]*\\}(\\[[^]]*\\])?", "\n\n", x)   # other environments
  x <- gsub("\\\\item(\\[[^]]*\\])?", "\n- ", x)
  # a command without argument directly before another (\centering\caption{...})
  # must not merge with the text that the next command leaves behind
  x <- gsub("(\\\\[A-Za-z]+\\*?)(?=\\\\)", "\\1 ", x, perl = TRUE)
  x <- gsub("\\\\footnote\\{", " \\\\footnote{", x)                # "text.\footnote{A note}" -> "text. A note"
  # text commands, innermost first: \emph{word} -> word (headings kept for later)
  heading <- "(?:part|chapter|section|subsection|subsubsection|paragraph)"
  repeat {
    y <- gsub(paste0("\\\\(?!", heading, "\\b)[A-Za-z]+\\*?(?:\\[[^]]*\\])*\\{([^{}]*)\\}"), "\\1", x, perl = TRUE)
    if (identical(y, x)) break
    x <- y
  }
  x <- gsub(paste0("\\\\", heading, "\\*?(\\[[^]]*\\])?\\{([^{}]*)\\}"),
            paste0("\n\n", .tex_section_marker, "\\2\n\n"), x, perl = TRUE)
  x <- gsub("\\\\([%&$_#{}])", "\\1", x)                              # escaped characters
  x <- gsub("\\\\[A-Za-z]+\\*?", "", x)                               # remaining commands
  x <- gsub("\\\\\\\\", "\n", x)                                      # line breaks
  x <- gsub("[{}]", "", x)
  x <- gsub("~", " ", x, fixed = TRUE)
  x <- gsub("``|''", "\"", x)
  x
}
