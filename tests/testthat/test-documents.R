# read_documents() with small files written in a temporary folder.

write_file <- function(dir, name, lines) {
  f <- file.path(dir, name)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  writeLines(lines, f, useBytes = TRUE)
  f
}

test_that("Markdown: headings become sections, paragraphs split on blank lines", {
  d <- tempfile(); dir.create(d)
  f <- write_file(d, "notes.md", c("Intro line one", "continues here.", "", "# Methods ##", "",
                                   "The sample had 300 adults.", "", "## Measures", "Alpha was .85.",
                                   "<!-- hidden -->", "```", "# not a heading", "```"))
  r <- read_documents(f)
  expect_equal(names(r), c("file", "section", "paragraph", "text"))
  expect_equal(r$text, c("Intro line one continues here.", "The sample had 300 adults.", "Alpha was .85."))
  expect_equal(r$section, c(NA, "Methods", "Measures"))
  expect_equal(r$paragraph, 1:3)
  expect_equal(unique(r$file), "notes.md")
})

test_that("R Markdown and Quarto: YAML header and code chunks are dropped", {
  d <- tempfile(); dir.create(d)
  rmd <- c("---", "title: \"Report\"", "output: html_document", "---", "", "# Data", "",
           "Text before the chunk.", "", "```{r setup}", "library(EGAnet)", "# a comment", "```",
           "", "Text after `r 1+1` the chunk.")
  r <- read_documents(write_file(d, "report.Rmd", rmd))
  expect_equal(r$text, c("Text before the chunk.", "Text after `r 1+1` the chunk."))
  expect_equal(r$section, c("Data", "Data"))
  q <- read_documents(write_file(d, "doc.qmd", sub("```{r setup}", "```{python}", rmd, fixed = TRUE)))
  expect_equal(q$text, r$text)
})

test_that("LaTeX: preamble, comments, math and labels dropped; text and sections kept", {
  d <- tempfile(); dir.create(d)
  tex <- c("\\documentclass{article}", "\\usepackage{amsmath}", "\\title{Ignored}",
           "\\begin{document}", "\\maketitle",
           "\\begin{abstract}", "The study examines \\emph{networks}.", "\\end{abstract}",
           "\\section{The \\emph{EGA} model}\\label{sec:ega}",
           "EGA estimates dimensions \\citep[see][p. 2]{golino2017}. % a comment",
           "It uses $\\alpha = .85$ and 50\\% of items.",
           "\\begin{equation}", "x = y^2", "\\end{equation}",
           "\\begin{align*}", "a &= b", "\\end{align*}",
           "See Figure~\\ref{fig:1}.\\footnote{A note.}", "",
           "\\begin{figure}[h]\\centering\\includegraphics[width=3in]{plot.pdf}",
           "\\caption{A \\textbf{bold} plot.}\\label{fig:1}\\end{figure}",
           "\\subsection*{Details}", "More \\textit{text}.", "\\end{document}", "trailing junk")
  r <- read_documents(write_file(d, "paper.tex", tex))
  all <- paste(r$text, collapse = " | ")
  expect_equal(r$section[1], "Abstract")
  expect_equal(r$text[1], "The study examines networks.")
  expect_true("The EGA model" %in% r$section)
  expect_true("Details" %in% r$section)
  expect_match(all, "EGA estimates dimensions (cite: golino2017).", fixed = TRUE)
  expect_match(all, "It uses alpha = .85 and 50% of items.", fixed = TRUE)
  expect_match(all, "A note.", fixed = TRUE)
  expect_match(all, "A bold plot.", fixed = TRUE)
  expect_match(all, "More text.", fixed = TRUE)
  for (gone in c("x = y^2", "a &= b", "comment", "Ignored", "plot.pdf", "fig:1", "sec:ega",
                 "trailing junk", "\\", "{", "}")) {
    expect_false(grepl(gone, all, fixed = TRUE), info = gone)
  }
})

test_that("Plain text: paragraphs without sections", {
  d <- tempfile(); dir.create(d)
  r <- read_documents(write_file(d, "a.txt", c("One.", "", "", "Two", "lines.", "  ", "Three.")))
  expect_equal(r$text, c("One.", "Two lines.", "Three."))
  expect_true(all(is.na(r$section)))
})

test_that("PDF: text of each page, with the page as section", {
  skip_if_not_installed("pdftools")
  d <- tempfile(); dir.create(d)
  f <- file.path(d, "tiny.pdf")
  grDevices::pdf(f)
  graphics::plot.new(); graphics::text(0.5, 0.8, "Exploratory graph analysis")
  graphics::text(0.5, 0.5, "Reliability was 0.85")
  graphics::plot.new(); graphics::text(0.5, 0.5, "Second page text")
  grDevices::dev.off()
  r <- read_documents(f)
  expect_equal(r$section, c("page 1", "page 1", "page 2"))
  expect_equal(r$text, c("Exploratory graph analysis", "Reliability was 0.85", "Second page text"))
})

test_that("Folders: supported files read recursively, others skipped, failures reported", {
  d <- tempfile(); dir.create(d)
  write_file(d, "a.md", c("# A", "", "Alpha text."))
  write_file(d, "sub/b.txt", c("Beta text."))
  write_file(d, "data.csv", c("x,y", "1,2"))
  write_file(d, "broken.pdf", "this is not a PDF")
  if (requireNamespace("pdftools", quietly = TRUE)) {
    expect_warning(r <- read_documents(d), "1 of 3 files could not be read")
    expect_match(attr(r, "failed"), "^broken.pdf \\(")
    expect_true(is.na(r$text[r$file == "broken.pdf"]))
  } else {
    expect_warning(r <- read_documents(d), "pdftools")
  }
  expect_setequal(r$file, c("a.md", "broken.pdf", "sub/b.txt"))
  expect_equal(r$text[r$file == "sub/b.txt"], "Beta text.")
})

test_that("by = \"file\" gives one row per file", {
  d <- tempfile(); dir.create(d)
  write_file(d, "a.md", c("# A", "", "One.", "", "Two."))
  write_file(d, "b.txt", c("Three."))
  r <- read_documents(d, by = "file")
  expect_equal(nrow(r), 2)
  expect_equal(r$text[r$file == "a.md"], "One.\n\nTwo.")
  expect_true(all(is.na(r$paragraph)))
})

test_that("read_documents explains bad paths and file types", {
  expect_error(read_documents(file.path(tempdir(), "nope.md")), "No file or folder")
  f <- tempfile(fileext = ".docx")
  writeLines("x", f)
  expect_error(read_documents(f), "Supported files")
  d <- tempfile(); dir.create(d)
  expect_equal(nrow(read_documents(d)), 0)
})

test_that("text is written and read as UTF-8, with a Latin-1 fallback", {
  f <- tempfile(fileext = ".txt")
  s <- c(intToUtf8(c(67, 97, 102, 233)), paste0(intToUtf8(945), " = .91"))
  write_text(s, f)
  expect_equal(readBin(f, "raw", 5), as.raw(c(0x43, 0x61, 0x66, 0xc3, 0xa9)))
  write_text("more", f, append = TRUE)
  expect_equal(read_text(f), c(s, "more"))
  g <- tempfile(fileext = ".txt")
  writeBin(as.raw(c(0x43, 0x61, 0x66, 0xe9, 0x0a)), g)   # "Cafe" with e-acute in Latin-1
  expect_equal(read_text(g), intToUtf8(c(67, 97, 102, 233)))
})
