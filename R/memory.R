# Memory that lives outside the model's context:
#   - a notes file (Markdown) the agent reads and writes with tools;
#   - a vector store: texts, their embeddings and metadata, searched by
#     cosine similarity, written in base R (a matrix and a data frame).
# The episode log itself (save_episode()) is the episodic memory.

# ---- Notes ---------------------------------------------------------------------

#' Write and read a Markdown notes file
#'
#' Notes are a simple, readable long-term memory: each call to
#' `notes_write()` appends a dated entry to a Markdown file, and
#' `notes_read()` returns the whole file. Agents use them through
#' [notes_tools()] or the [ctx_notes()] context strategy; people can open
#' and edit the file in any editor.
#'
#' @param text The note.
#' @param file Path of the notes file (created if needed).
#' @param section Optional heading for the entry, for example `"Sources"`.
#' @return `notes_write()` returns the file path, invisibly; `notes_read()`
#'   returns the notes as one string (`""` when the file does not exist).
#' @examples
#' f <- tempfile(fileext = ".md")
#' notes_write("Golino & Epskamp (2017), doi 10.1371/journal.pone.0174035", f, "Sources")
#' notes_write("Still missing: a reliability estimate.", f)
#' cat(notes_read(f))
#' @export
notes_write <- function(text, file, section = NULL) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  heading <- paste0("## ", section %||% "Note", " (", format(Sys.time(), "%Y-%m-%d %H:%M"), ")")
  write_text(c(heading, "", text, ""), file, append = TRUE, encoding = "UTF-8")
  invisible(file)
}

#' @rdname notes_write
#' @export
notes_read <- function(file) {
  if (!file.exists(file)) return("")
  trimws(paste(read_text(file, encoding = "UTF-8"), collapse = "\n"))
}

#' Tools that let an agent keep notes
#'
#' Two tools for an agent: `note_write` (append a note, optionally under a
#' heading) and `note_read` (read all notes). Use them with any context
#' strategy; [ctx_notes()] also shows the notes to the model at every step.
#'
#' @param file Path of the notes file.
#' @return A list of two `agent_tool`s.
#' @examples
#' tools <- notes_tools(tempfile(fileext = ".md"))
#' tools[[1]]
#' call_tool("note_write", tools, arguments = list(text = "Found 3 papers."))$output
#' call_tool("note_read", tools)$output
#' @export
notes_tools <- function(file) {
  note_write <- function(text, section = NULL) {
    notes_write(text, file, section)
    "Note saved."
  }
  note_read <- function() {
    notes <- notes_read(file)
    if (nzchar(notes)) notes else "(no notes yet)"
  }
  list(
    define_tool(note_write, name = "note_write", untrusted = FALSE,
      description = paste("Save a short note to your notes file: sources found (with DOIs),",
                          "decisions, numbers and open questions. Notes stay available when",
                          "older messages are removed from the conversation."),
      args = list(text = arg_string("The note, in a few lines"),
                  section = arg_string("Optional heading, for example \"Sources\"", required = FALSE))),
    define_tool(note_read, name = "note_read", args = list(),
      description = "Read all your notes.")
  )
}

# ---- Vector store ------------------------------------------------------------------

#' A simple vector store in base R
#'
#' Holds texts, their embeddings (a matrix with one unit-length row per
#' text), a metadata data frame (for example file and section, used for
#' citations) and the id of the embedding model. Search is a matrix product:
#' because rows have length 1, the dot product is the cosine similarity.
#' Nothing more is needed for libraries of up to tens of thousands of
#' passages.
#'
#' @param texts A character vector of passages.
#' @param embeddings Optional precomputed embeddings (one row per text). If
#'   `NULL`, the texts are embedded with `model` ([embed_texts()]).
#' @param metadata Optional data frame with one row per text.
#' @param model The embedding model. Must be the model that made
#'   `embeddings` (queries are embedded with it); when `embeddings` carry a
#'   `"model"` attribute and `model` is not given, that model is used.
#' @return An object of class `vector_store`.
#' @examples
#' data(extraversion)
#' data(minilm_embeddings)
#' store <- vector_store(extraversion$text,
#'                       embeddings = minilm_embeddings$texts$extraversion,
#'                       metadata = extraversion[, c("doc_id", "subtopic")],
#'                       model = "hf:sentence-transformers/all-MiniLM-L6-v2")
#' store
#' # Search with a query vector (here the embedded description of "Assertive")
#' q <- minilm_embeddings$labels$extraversion["Assertive", ]
#' store_search(store, q, k = 3)[, c("rank", "score", "subtopic")]
#' @export
vector_store <- function(texts, embeddings = NULL, metadata = NULL, model = default_model("embed")) {
  texts <- as.character(texts)
  if (!is.null(embeddings) && missing(model) && !is.null(attr(embeddings, "model"))) {
    model <- attr(embeddings, "model")
  }
  parse_model(model)
  if (is.null(embeddings)) embeddings <- embed_texts(texts, model = model, input_type = "document")
  if (!is.null(attr(embeddings, "model")) && attr(embeddings, "model") != model) {
    .stop("The embeddings were made with ", attr(embeddings, "model"), ", not ", model, ".")
  }
  usage <- attr(embeddings, "usage")
  embeddings <- .as_row_matrix(embeddings)
  if (nrow(embeddings) != length(texts)) {
    .stop("There are ", length(texts), " texts but ", nrow(embeddings), " rows of embeddings.")
  }
  bad <- which(!stats::complete.cases(embeddings))
  if (length(bad)) {
    .stop(length(bad), " texts have no embedding (rows ", .shorten(paste(bad, collapse = ", "), 60),
          "). Remove or fix them before building the store.")
  }
  if (is.null(metadata)) metadata <- data.frame(id = seq_along(texts))
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE)
  if (nrow(metadata) != length(texts)) .stop("`metadata` needs one row per text.")
  rownames(metadata) <- NULL
  E <- .unit_rows(unname(embeddings))
  structure(list(embeddings = E, texts = texts, metadata = metadata, model = model,
                 created = .now(), usage = usage), class = "vector_store")
}

#' @export
print.vector_store <- function(x, ...) {
  cat(sprintf("<vector_store> %s passages | %d dimensions | model %s\n",
              format(length(x$texts), big.mark = ","), ncol(x$embeddings), x$model))
  cat("Metadata:", paste(names(x$metadata), collapse = ", "), "\n")
  invisible(x)
}

#' Add passages to, search, save and load a vector store
#'
#' `store_add()` embeds new passages with the store's model and appends
#' them. `store_search()` returns the `k` passages most similar to a query.
#' The query is embedded with the store's model; a query vector made with a
#' different model (its `"model"` attribute) is refused, because vectors from
#' different models cannot be compared. `store_save()` and `store_load()`
#' write and read the store as an RDS file.
#'
#' @param store A [vector_store()].
#' @param texts New passages.
#' @param embeddings Optional precomputed embeddings of `texts`.
#' @param metadata Optional data frame with one row per new passage;
#'   columns missing on either side are filled with `NA`.
#' @param query A text, or an embedding vector made with the store's model.
#' @param k Number of passages to return.
#' @param file Path of the RDS file.
#' @return `store_add()` returns the updated store. `store_search()` returns
#'   a data frame with `rank`, `score` (cosine similarity), `text` and the
#'   metadata columns. `store_save()` returns the file path, invisibly;
#'   `store_load()` returns the store.
#' @examples
#' data(extraversion)
#' data(minilm_embeddings)
#' E <- minilm_embeddings$texts$extraversion
#' model <- "hf:sentence-transformers/all-MiniLM-L6-v2"
#' store <- vector_store(extraversion$text[1:60], E[1:60, ], model = model)
#' store <- store_add(store, extraversion$text[61:120], E[61:120, ])
#' store
#' f <- store_save(store, tempfile(fileext = ".rds"))
#' store2 <- store_load(f)
#' store_search(store2, E[100, ], k = 2)[, c("rank", "score", "id")]
#' @name store_search
NULL

#' @rdname store_search
#' @export
store_add <- function(store, texts, embeddings = NULL, metadata = NULL) {
  stopifnot(inherits(store, "vector_store"))
  if (is.null(metadata)) metadata <- data.frame(id = length(store$texts) + seq_along(texts))
  new <- vector_store(texts, embeddings = embeddings, metadata = metadata, model = store$model)
  if (ncol(new$embeddings) != ncol(store$embeddings)) .stop("The new embeddings have a different dimension.")
  store$embeddings <- rbind(store$embeddings, new$embeddings)
  store$texts <- c(store$texts, new$texts)
  store$metadata <- .rbind_fill(store$metadata, new$metadata)
  store$usage <- .add_usage(store$usage, new$usage)
  store
}

#' @rdname store_search
#' @export
store_search <- function(store, query, k = 5) {
  stopifnot(inherits(store, "vector_store"))
  usage <- NULL
  if (is.character(query)) {
    if (length(query) != 1) .stop("Search with one query at a time.")
    q <- embed_texts(query, model = store$model, input_type = "query")
    usage <- attr(q, "usage")
  } else {
    q <- query
    if (!is.null(attr(q, "model")) && attr(q, "model") != store$model) {
      .stop("The query was embedded with ", attr(q, "model"), " but the store with ", store$model,
            ". Embed the query with the store's model.")
    }
  }
  q <- .as_row_matrix(q)
  if (nrow(q) != 1) .stop("Search with one query at a time.")
  if (ncol(q) != ncol(store$embeddings)) {
    .stop("The query has ", ncol(q), " dimensions and the store ", ncol(store$embeddings), ".")
  }
  q <- .unit_rows(q)
  scores <- as.vector(store$embeddings %*% t(q))
  top <- utils::head(order(scores, decreasing = TRUE), k)
  out <- data.frame(rank = seq_along(top), score = round(scores[top], 4), text = store$texts[top],
                    stringsAsFactors = FALSE)
  out <- cbind(out, store$metadata[top, , drop = FALSE])
  rownames(out) <- NULL
  structure(out, usage = usage)
}

#' @rdname store_search
#' @export
store_save <- function(store, file) {
  stopifnot(inherits(store, "vector_store"))
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(store, file)
  invisible(file)
}

#' @rdname store_search
#' @export
store_load <- function(file) {
  store <- readRDS(file)
  if (!inherits(store, "vector_store")) .stop(file, " does not contain a vector store.")
  store
}

# rbind two data frames, filling columns missing on either side with NA.
.rbind_fill <- function(a, b) {
  for (nm in setdiff(names(b), names(a))) a[[nm]] <- rep(NA, nrow(a))
  for (nm in setdiff(names(a), names(b))) b[[nm]] <- rep(NA, nrow(b))
  out <- rbind(a, b[names(a)])
  rownames(out) <- NULL
  out
}

# ---- A searchable library of documents ----------------------------------------------

#' Split documents into overlapping chunks of words
#'
#' Retrieval works on passages of a few hundred words: long enough to carry
#' meaning, short enough to be specific. Chunks never cross a file or
#' section boundary; consecutive chunks share `overlap` words, so a sentence
#' cut at a boundary appears whole in one of them.
#'
#' @param docs A data frame from [read_documents()].
#' @param chunk_words Words per chunk.
#' @param overlap Words shared by consecutive chunks (less than
#'   `chunk_words`).
#' @return A data frame with `file`, `section`, `paragraphs` (the paragraph
#'   range, such as `"3-5"`), `chunk` (number within the file), `words` and
#'   `text`.
#' @examples
#' docs <- data.frame(file = "a.txt", section = "Intro", paragraph = 1:2,
#'                    text = c(paste(rep("alpha", 30), collapse = " "),
#'                             paste(rep("beta", 30), collapse = " ")))
#' chunks <- chunk_documents(docs, chunk_words = 25, overlap = 5)
#' chunks[, c("chunk", "paragraphs", "words")]
#' @export
chunk_documents <- function(docs, chunk_words = 250, overlap = 40) {
  if (overlap >= chunk_words) .stop("`overlap` must be smaller than `chunk_words`.")
  docs <- docs[!is.na(docs$text) & nzchar(trimws(docs$text)), , drop = FALSE]
  section <- ifelse(is.na(docs$section), "", docs$section)
  group <- paste(docs$file, section, sep = "\r")
  rows <- list()
  for (g in unique(group)) {
    d <- docs[group == g, , drop = FALSE]
    words <- strsplit(trimws(d$text), "\\s+")
    para <- rep(d$paragraph, lengths(words))
    words <- unlist(words)
    starts <- seq(1, max(1, length(words) - overlap), by = chunk_words - overlap)
    for (s in starts) {
      idx <- s:min(length(words), s + chunk_words - 1)
      p <- range(para[idx])
      rows[[length(rows) + 1]] <- data.frame(
        file = d$file[1], section = d$section[1],
        paragraphs = if (anyNA(p)) NA_character_ else if (p[1] == p[2]) as.character(p[1]) else paste0(p[1], "-", p[2]),
        words = length(idx), text = paste(words[idx], collapse = " "), stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) {
    return(data.frame(file = character(0), section = character(0), paragraphs = character(0),
                      chunk = integer(0), words = integer(0), text = character(0)))
  }
  out <- do.call(rbind, rows)
  out$chunk <- stats::ave(seq_len(nrow(out)), out$file, FUN = seq_along)
  out[, c("file", "section", "paragraphs", "chunk", "words", "text")]
}

#' Turn a folder of documents into a searchable library
#'
#' Reads every supported file in `path` ([read_documents()]: PDF, Markdown,
#' R Markdown, Quarto, LaTeX, text), splits them into overlapping chunks
#' ([chunk_documents()]) and embeds the chunks into a [vector_store()] whose
#' metadata keep the file, section and paragraphs of every chunk, for
#' citations. With a local embedding model (Ollama or `"hf:"`), the
#' documents never leave your computer.
#'
#' Embedding a library takes time: wrap the call in [keep()] or save the
#' result with [store_save()].
#'
#' @param path A file or folder.
#' @param model Embedding model (see [embed_texts()]).
#' @param chunk_words,overlap Chunk size and overlap in words.
#' @return A `vector_store`.
#' @examples
#' \dontrun{
#' lib <- keep("my_library", ingest_library("~/papers", model = "ollama:nomic-embed-text"))
#' store_search(lib, "test-retest reliability of the scale", k = 3)
#' }
#' @export
ingest_library <- function(path, model = default_model("embed"), chunk_words = 250, overlap = 40) {
  docs <- read_documents(path, by = "paragraph")
  chunks <- chunk_documents(docs, chunk_words = chunk_words, overlap = overlap)
  if (!nrow(chunks)) .stop("No text found in ", path, ".")
  .msg(sprintf("Embedding %d chunks from %d files with %s ...", nrow(chunks),
               length(unique(chunks$file)), model))
  vector_store(chunks$text, metadata = chunks[, c("file", "section", "paragraphs", "chunk")],
               model = model)
}

#' A tool that searches a vector store
#'
#' Gives an agent a `retrieve` tool: it sends a query and receives the `k`
#' most similar passages, each labelled with its file, section and
#' paragraphs so the agent can cite them. Passages are marked as untrusted
#' data, like every tool output from documents.
#'
#' @param store A [vector_store()], for example from [ingest_library()].
#' @param k Number of passages per search.
#' @param name Tool name.
#' @return An `agent_tool`.
#' @examples
#' data(extraversion)
#' data(minilm_embeddings)
#' store <- vector_store(extraversion$text, minilm_embeddings$texts$extraversion,
#'                       model = "hf:sentence-transformers/all-MiniLM-L6-v2")
#' retrieve_tool(store, k = 3)
#' \dontrun{
#' lib <- ingest_library("~/papers", model = "ollama:nomic-embed-text")
#' librarian <- agent("librarian",
#'   "Answer from the user's papers only. Cite file and section for every claim.",
#'   model = "ollama:qwen3:4b", tools = list(retrieve_tool(lib)))
#' run(librarian, "What reliability did the pilot study report?")
#' }
#' @export
retrieve_tool <- function(store, k = 5, name = "retrieve") {
  stopifnot(inherits(store, "vector_store"))
  retrieve <- function(query) {
    hits <- store_search(store, query, k = k)
    if (!nrow(hits)) return("No passages found.")
    labels <- vapply(seq_len(nrow(hits)), function(i) .citation_label(hits[i, , drop = FALSE]), character(1))
    paste(sprintf("[%d] %s (similarity %.2f)\n%s", hits$rank, labels, hits$score, hits$text),
          collapse = "\n\n")
  }
  define_tool(retrieve, name = name, max_output_tokens = max(2000, k * 450),
    description = paste("Search the user's document library for passages relevant to a query.",
                        "Returns passages labelled with file and section; cite them in the answer."),
    args = list(query = arg_string("What to look for, in a few words or a question")))
}

# "file.pdf, section 'Methods', paragraphs 3-5" from whatever metadata exist.
.citation_label <- function(row) {
  parts <- character(0)
  get <- function(nm) if (nm %in% names(row) && !is.na(row[[nm]])) as.character(row[[nm]]) else NULL
  if (!is.null(get("file"))) parts <- c(parts, get("file"))
  if (!is.null(get("section")) && nzchar(get("section"))) parts <- c(parts, paste0("section '", get("section"), "'"))
  if (!is.null(get("paragraphs"))) parts <- c(parts, paste("paragraphs", get("paragraphs")))
  if (!length(parts) && !is.null(get("id"))) parts <- paste("passage", get("id"))
  paste(parts, collapse = ", ")
}
