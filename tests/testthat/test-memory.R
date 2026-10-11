# Notes, the vector store and chunking: offline, with the shipped MiniLM
# embeddings. Live tests at the end need Ollama.

minilm <- "hf:sentence-transformers/all-MiniLM-L6-v2"

test_that("notes_write appends dated entries and notes_read returns them", {
  f <- tempfile(fileext = ".md")
  expect_equal(notes_read(f), "")
  notes_write("First note.", f, "Sources")
  notes_write(c("Second", "note."), f)
  txt <- notes_read(f)
  expect_match(txt, "^## Sources \\(")
  expect_match(txt, "First note.", fixed = TRUE)
  expect_match(txt, "## Note (", fixed = TRUE)
  expect_match(txt, "Second\nnote.", fixed = TRUE)
})

test_that("notes_tools write and read through call_tool", {
  f <- tempfile(fileext = ".md")
  tools <- notes_tools(f)
  expect_equal(call_tool("note_read", tools)$output, "<tool_output tool=\"note_read\" trust=\"untrusted\">\n(no notes yet)\n</tool_output>")
  w <- call_tool("note_write", tools, arguments = list(text = "Found 3 papers."))
  expect_false(w$is_error)
  expect_equal(w$output, "Note saved.")
  expect_match(call_tool("note_read", tools)$output, "Found 3 papers.", fixed = TRUE)
  expect_true(call_tool("note_write", tools, arguments = list())$is_error)   # text is required
})

test_that("vector_store holds unit-length rows, texts, metadata and the model", {
  data(extraversion, package = "researchAgentR", envir = environment())
  data(minilm_embeddings, package = "researchAgentR", envir = environment())
  E <- minilm_embeddings$texts$extraversion
  store <- vector_store(extraversion$text, E * 3, metadata = extraversion[, c("doc_id", "subtopic")],
                        model = minilm)
  expect_s3_class(store, "vector_store")
  expect_equal(dim(store$embeddings), c(120L, 384L))
  expect_equal(rowSums(store$embeddings^2), rep(1, 120), tolerance = 1e-8)
  expect_equal(store$model, minilm)
  expect_output(print(store), "120 passages \\| 384 dimensions")
  expect_equal(vector_store(c("a", "b"), diag(2), model = minilm)$metadata$id, 1:2)
})

test_that("vector_store refuses mismatched or missing embeddings", {
  expect_error(vector_store(c("a", "b"), diag(3), model = minilm), "2 texts but 3 rows")
  bad <- diag(2)
  bad[2, ] <- NA
  expect_error(vector_store(c("a", "b"), bad, model = minilm), "1 texts have no embedding")
  E <- structure(diag(2), model = "ollama:nomic-embed-text")
  expect_error(vector_store(c("a", "b"), E, model = minilm), "were made with ollama")
  expect_equal(vector_store(c("a", "b"), E)$model, "ollama:nomic-embed-text")   # model taken from the attribute
  expect_error(vector_store(c("a", "b"), diag(2), metadata = data.frame(x = 1), model = minilm),
               "one row per text")
})

test_that("store_search ranks by cosine similarity and returns metadata", {
  data(extraversion, package = "researchAgentR", envir = environment())
  data(minilm_embeddings, package = "researchAgentR", envir = environment())
  E <- minilm_embeddings$texts$extraversion
  store <- vector_store(extraversion$text, E, metadata = extraversion[, c("doc_id", "subtopic")],
                        model = minilm)
  hits <- store_search(store, E[17, ], k = 3)
  expect_equal(nrow(hits), 3)
  expect_equal(hits$doc_id[1], extraversion$doc_id[17])
  expect_equal(hits$score[1], 1, tolerance = 1e-3)
  expect_true(all(diff(hits$score) <= 0))
  expect_equal(names(hits), c("rank", "score", "text", "doc_id", "subtopic"))
  # the description of a facet retrieves texts of that facet
  q <- minilm_embeddings$labels$extraversion["Assertive", ]
  expect_true(mean(store_search(store, q, k = 10)$subtopic == "Assertive") >= 0.5)
  # a query from another model is refused; a wrong dimension too
  q_other <- structure(E[1, ], model = "ollama:nomic-embed-text")
  expect_error(store_search(store, q_other), "embedded with ollama:nomic-embed-text")
  expect_error(store_search(store, rep(1, 10)), "10 dimensions")
  expect_error(store_search(store, E[1:2, ]), "one query")
})

test_that("store_add appends and fills metadata; save and load round-trip", {
  E <- diag(4)
  s <- vector_store(c("a", "b"), E[1:2, ], metadata = data.frame(file = c("x", "y")), model = minilm)
  s <- store_add(s, c("c", "d"), E[3:4, ], metadata = data.frame(page = 3:4))
  expect_equal(s$texts, c("a", "b", "c", "d"))
  expect_equal(nrow(s$embeddings), 4)
  expect_equal(s$metadata$file, c("x", "y", NA, NA))
  expect_equal(s$metadata$page, c(NA, NA, 3L, 4L))
  expect_error(store_add(s, "e", matrix(1, 1, 3)), "dimension")
  f <- store_save(s, file.path(tempdir(), "sub", "store.rds"))
  s2 <- store_load(f)
  expect_identical(s2, s)
  saveRDS(list(1), f2 <- tempfile(fileext = ".rds"))
  expect_error(store_load(f2), "does not contain a vector store")
})

test_that("chunk_documents makes overlapping chunks within sections", {
  docs <- data.frame(file = c("a.txt", "a.txt", "a.txt", "b.txt"),
                     section = c("Intro", "Intro", "Methods", NA),
                     paragraph = c(1L, 2L, 3L, 1L),
                     text = c(paste(paste0("w", 1:30), collapse = " "),
                              paste(paste0("v", 1:30), collapse = " "), "short methods", NA),
                     stringsAsFactors = FALSE)
  ch <- chunk_documents(docs, chunk_words = 25, overlap = 5)
  intro <- ch[ch$section %in% "Intro", ]
  expect_equal(intro$words, c(25, 25, 20))
  expect_equal(intro$paragraphs, c("1", "1-2", "2"))
  # consecutive chunks share 5 words
  w1 <- strsplit(intro$text[1], " ")[[1]]
  w2 <- strsplit(intro$text[2], " ")[[1]]
  expect_equal(utils::tail(w1, 5), utils::head(w2, 5))
  expect_equal(ch$text[ch$section %in% "Methods"], "short methods")
  expect_equal(ch$chunk, 1:4)                 # numbered within the file; NA text dropped
  expect_false("b.txt" %in% ch$file)
  expect_error(chunk_documents(docs, 10, 10), "overlap")
  expect_equal(nrow(chunk_documents(docs[4, ], 10, 2)), 0)
})

test_that("retrieve_tool builds a tool and labels passages for citation", {
  s <- vector_store(c("a", "b"), diag(2), metadata = data.frame(
    file = c("paper.pdf", "notes.md"), section = c("page 3", NA), paragraphs = c("2-3", "1")), model = minilm)
  tool <- retrieve_tool(s, k = 2)
  expect_s3_class(tool, "agent_tool")
  expect_equal(tool$name, "retrieve")
  expect_true(tool$untrusted)
  expect_equal(.citation_label(s$metadata[1, ]), "paper.pdf, section 'page 3', paragraphs 2-3")
  expect_equal(.citation_label(s$metadata[2, ]), "notes.md, paragraphs 1")
  expect_equal(.citation_label(data.frame(id = 7)), "passage 7")
})

test_that(".rbind_fill fills missing columns with NA", {
  out <- .rbind_fill(data.frame(a = 1), data.frame(b = "x"))
  expect_equal(names(out), c("a", "b"))
  expect_equal(out$a, c(1, NA))
  expect_equal(out$b, c(NA, "x"))
})

# ---- live tests (need Ollama with nomic-embed-text) -------------------------------

ollama_has <- function(model) {
  m <- tryCatch(ollama_status(), error = function(e) NULL)
  !is.null(m) && any(sub(":latest$", "", m$name) == model)
}

test_that("ingest_library and retrieve_tool work end to end with Ollama (live)", {
  skip_on_cran()
  skip_if_not(ollama_has("nomic-embed-text"))
  d <- tempfile()
  dir.create(d)
  writeLines(c("# Methods", "", "Participants were 300 adults.", "", "# Results",
               "", "Cronbach's alpha was .85."), file.path(d, "study.md"))
  lib <- ingest_library(d, model = "ollama:nomic-embed-text", chunk_words = 50, overlap = 5)
  expect_s3_class(lib, "vector_store")
  hits <- store_search(lib, "internal consistency", k = 1)
  expect_equal(hits$section, "Results")
  out <- call_tool("retrieve", list(retrieve_tool(lib, k = 1)), arguments = list(query = "sample size"))
  expect_match(out$output, "study.md, section 'Methods'", fixed = TRUE)
})
