# Embeddings: one function, embed_texts(), turns texts into vectors with a
# local model (Ollama, or sentence-transformers through Python) or an API
# (Jina, Voyage, OpenAI). Every row is scaled to length 1, so the cosine
# similarity of two texts is simply the dot product of their rows.
#
# Endpoints (official documentation, accessed 2026-10-11):
#   Ollama  POST http://localhost:11434/api/embed       https://docs.ollama.com/api/embed
#   Jina    POST https://api.jina.ai/v1/embeddings      https://api.jina.ai/openapi.json
#   Voyage  POST https://api.voyageai.com/v1/embeddings https://docs.voyageai.com/reference/embeddings-api
#   OpenAI  POST https://api.openai.com/v1/embeddings   https://developers.openai.com/api/reference/resources/embeddings/methods/create

.embed_providers <- c("ollama", "jina", "voyage", "openai", "hf")

# List prices in US dollars per million input tokens, used when the model is
# not in model_registry() (a registry_set() entry takes precedence).
# Sources: https://docs.voyageai.com/docs/pricing and
# https://developers.openai.com/api/docs/models/text-embedding-3-small
# (accessed 2026-10-11). Voyage gives 200M free tokens per account for the
# voyage-4 family; the ledger still records the list price as a shadow cost.
.embed_prices <- c(
  "openai:text-embedding-3-small" = 0.02,
  "voyage:voyage-4" = 0.06,
  "voyage:voyage-4-lite" = 0.02,
  "voyage:voyage-4-large" = 0.12
)

#' Embed texts as vectors
#'
#' Turns each text into a numeric vector (an "embedding") so that texts with
#' similar meaning get similar vectors. Used for retrieval ([vector_store()],
#' [retrieve_tool()]), for [select_relevant()] and by routers.
#'
#' Models are written as `"provider:model"`:
#'
#' | Model string | Where it runs | Key | Cost |
#' |---|---|---|---|
#' | `"ollama:nomic-embed-text"`, `"ollama:all-minilm"` | your computer ([Ollama](https://ollama.com)) | none | free |
#' | `"hf:sentence-transformers/all-MiniLM-L6-v2"` | your computer (Python, see [setup_python_env()]) | none | free |
#' | `"jina:jina-embeddings-v3"` | Jina AI API | `JINA_API_KEY` | free key: 100 requests and 100,000 tokens per minute |
#' | `"voyage:voyage-4"` | Voyage AI API | `VOYAGE_API_KEY` | $0.06 per million tokens; first 200M tokens free |
#' | `"openai:text-embedding-3-small"` | OpenAI API | `OPENAI_API_KEY` | $0.02 per million tokens (paid) |
#'
#' Texts sent to an API leave your computer: use a local model for
#' confidential documents or participant data.
#'
#' Queries and documents must be embedded with the **same model**. Some
#' models embed queries and documents differently (`input_type`): use
#' `"document"` for the texts you store and `"query"` for search queries.
#' Ollama, OpenAI and sentence-transformers ignore `input_type`.
#'
#' Empty texts, and texts in a batch whose request failed, get a row of
#' `NA`; they are counted in a warning and listed in the attribute
#' `"failed"`, never silently replaced.
#'
#' @param texts A character vector.
#' @param model An embedding model string (see the table).
#' @param batch_size Texts per request.
#' @param input_type `NULL`, `"query"` or `"document"`. For Jina, a task name
#'   (`"retrieval.query"`, `"retrieval.passage"`, `"text-matching"`,
#'   `"classification"`, `"separation"`) is also accepted.
#' @param dimensions Optional shorter vectors, for models that support it
#'   (Jina v3 up to 1024, Voyage 256/512/1024/2048, OpenAI
#'   `text-embedding-3-*`, some Ollama models).
#' @return A numeric matrix with one unit-length row per text, and
#'   attributes `model`, `usage` (`tokens`, `cost` in US dollars,
#'   `requests`, `cached_requests`) and `failed` (row numbers).
#' @examples
#' # Offline: the shipped MiniLM embeddings are already unit length
#' data(minilm_embeddings)
#' E <- minilm_embeddings$texts$extraversion
#' range(rowSums(E^2))
#' \dontrun{
#' E <- embed_texts(c("network psychometrics", "item response theory"),
#'                  model = "ollama:nomic-embed-text")
#' attr(E, "usage")
#' embed_texts("a query", model = "jina:jina-embeddings-v3", input_type = "query")
#' }
#' @export
embed_texts <- function(texts, model = default_model("embed"), batch_size = 64,
                        input_type = NULL, dimensions = NULL) {
  m <- parse_model(model)
  if (!m$provider %in% .embed_providers) {
    .stop("Embedding providers: ", paste(.embed_providers, collapse = ", "),
          ". Write the model as, for example, \"ollama:nomic-embed-text\".")
  }
  if (m$provider == "jina" && !nzchar(m$model)) m$model <- "jina-embeddings-v3"
  embed_batch <- switch(m$provider, ollama = .embed_ollama, jina = .embed_jina,
                        voyage = .embed_voyage, openai = .embed_openai, hf = .embed_hf)
  texts <- as.character(texts)
  n <- length(texts)
  todo <- which(!is.na(texts) & nzchar(trimws(texts)))   # empty texts are not sent
  batches <- split(todo, ceiling(seq_along(todo) / batch_size))

  pieces <- list()
  errors <- character(0)
  tokens <- 0
  billed <- 0
  cached_requests <- 0
  for (b in batches) {
    res <- tryCatch(embed_batch(texts[b], m$model, input_type, dimensions), error = function(e) e)
    if (!inherits(res, "error") && nrow(res$embeddings) != length(b)) {
      res <- simpleError(sprintf("expected %d embeddings, got %d", length(b), nrow(res$embeddings)))
    }
    if (inherits(res, "error")) {
      errors <- c(errors, conditionMessage(res))
      next
    }
    pieces[[length(pieces) + 1]] <- list(rows = b, E = res$embeddings)
    tokens <- tokens + (res$tokens %||% NA_real_)
    if (isTRUE(res$cached)) cached_requests <- cached_requests + 1 else billed <- billed + (res$tokens %||% NA_real_)
  }
  if (!length(pieces)) {
    .stop("No text could be embedded with ", model, ". ",
          if (length(errors)) paste("First error:", errors[1]) else "All texts were empty.")
  }

  E <- matrix(NA_real_, n, ncol(pieces[[1]]$E))
  for (p in pieces) E[p$rows, ] <- p$E
  E <- .unit_rows(E)
  rownames(E) <- names(texts)
  failed <- which(!stats::complete.cases(E))
  if (length(failed)) {
    .warn(sprintf("%d of %d texts were not embedded (rows set to NA)%s", length(failed), n,
                  if (length(errors)) paste0(": ", errors[1]) else ": empty texts."))
  }
  price <- .embed_price(m)
  structure(E, model = model, failed = failed,
            usage = list(tokens = tokens,
                         cost = if (identical(price, 0)) 0 else billed * price / 1e6,
                         requests = length(pieces) + length(errors),
                         cached_requests = cached_requests))
}

# Scale every row to length 1 (rows of zeros become NA).
.unit_rows <- function(E) {
  norms <- sqrt(rowSums(E^2))
  norms[!is.na(norms) & norms == 0] <- NA
  E / norms
}

.embed_price <- function(m) {
  if (m$provider %in% c("ollama", "hf")) return(0)
  info <- .model_info(m)
  if (!is.null(info) && !is.na(info$input)) return(info$input)
  if (m$id %in% names(.embed_prices)) return(.embed_prices[[m$id]])
  NA_real_
}

# Sum two usage lists (for functions that embed twice).
.add_usage <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  list(tokens = a$tokens + b$tokens, cost = a$cost + b$cost,
       requests = a$requests + b$requests, cached_requests = a$cached_requests + b$cached_requests)
}

.need_key <- function(service) {
  key <- .get_key(service)
  if (!nzchar(key)) {
    .stop("No ", service, " key. Add the line ", .key_vars[[service]],
          "=your-key to ~/.Renviron and restart R (see ?set_keys).")
  }
  key
}

# A list of vectors (as JSON arrays arrive) to a matrix with one row each.
.rows_to_matrix <- function(x) {
  do.call(rbind, lapply(x, function(v) as.numeric(unlist(v))))
}

# Responses that return data = [{index, embedding}, ...] (Jina, Voyage,
# OpenAI): put the rows back in input order.
.data_to_matrix <- function(data) {
  idx <- vapply(data, function(d) as.numeric(d$index %||% NA), numeric(1))
  if (!anyNA(idx)) data <- data[order(idx)]
  .rows_to_matrix(lapply(data, `[[`, "embedding"))
}

# ---- Providers -------------------------------------------------------------------
# Each returns list(embeddings = <matrix>, tokens = <number or NA>, cached = <lgl>).

.embed_ollama <- function(texts, model, input_type, dimensions) {
  host <- sub("/v1/?$", "", .provider_info("ollama")$base_url)
  body <- list(model = model, input = as.list(texts))
  if (!is.null(dimensions)) body$dimensions <- as.integer(dimensions)
  r <- .perform(.request(paste0(host, "/api/embed"), body = body, auth = "none"))
  list(embeddings = .rows_to_matrix(r$body$embeddings),
       tokens = as.numeric(r$body$prompt_eval_count %||% NA), cached = r$cached)
}

.jina_tasks <- c("retrieval.query", "retrieval.passage", "text-matching", "classification", "separation")

.jina_task <- function(input_type) {
  if (is.null(input_type)) return("text-matching")
  if (input_type %in% .jina_tasks) return(input_type)
  switch(input_type, query = "retrieval.query", document = "retrieval.passage",
         .stop("input_type must be NULL, \"query\", \"document\" or a Jina task: ",
               paste(.jina_tasks, collapse = ", ")))
}

.embed_jina <- function(texts, model, input_type, dimensions) {
  body <- list(model = model, input = as.list(texts), task = .jina_task(input_type))
  if (!is.null(dimensions)) body$dimensions <- as.integer(dimensions)
  req <- .request("https://api.jina.ai/v1/embeddings", body = body, key = .need_key("jina"),
                  realm = "jina", rpm = .opt("rpm.jina", 100))
  r <- .perform(req)
  list(embeddings = .data_to_matrix(r$body$data),
       tokens = as.numeric(r$body$usage$total_tokens %||% NA), cached = r$cached)
}

.embed_voyage <- function(texts, model, input_type, dimensions) {
  if (length(texts) > 1000) .stop("Voyage accepts at most 1,000 texts per request: lower batch_size.")
  body <- list(model = model, input = as.list(texts))
  if (!is.null(input_type)) body$input_type <- match.arg(input_type, c("query", "document"))
  if (!is.null(dimensions)) body$output_dimension <- as.integer(dimensions)
  # Without a payment method Voyage allows very few requests per minute; set
  # options(researchAgentR.rpm.voyage = 3) to pace requests in that case.
  req <- .request("https://api.voyageai.com/v1/embeddings", body = body, key = .need_key("voyage"),
                  realm = "voyage", rpm = .opt("rpm.voyage"))
  r <- .perform(req)
  list(embeddings = .data_to_matrix(r$body$data),
       tokens = as.numeric(r$body$usage$total_tokens %||% NA), cached = r$cached)
}

.embed_openai <- function(texts, model, input_type, dimensions) {
  body <- list(model = model, input = as.list(texts))
  if (!is.null(dimensions)) body$dimensions <- as.integer(dimensions)
  req <- .request("https://api.openai.com/v1/embeddings", body = body, key = .need_key("openai"),
                  realm = "openai", rpm = .opt("rpm.openai"))
  r <- .perform(req)
  list(embeddings = .data_to_matrix(r$body$data),
       tokens = as.numeric(r$body$usage$prompt_tokens %||% NA), cached = r$cached)
}

# Local sentence-transformers model through reticulate (optional). The model
# is loaded once per session and reused.
.embed_hf <- function(texts, model, input_type, dimensions) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    .stop("\"hf:\" models need the reticulate package and a Python environment: ",
          "install.packages(\"reticulate\"), then setup_python_env() once and ",
          "activate_python_env() in each session.")
  }
  if (!is.null(dimensions)) .stop("`dimensions` is not supported for \"hf:\" models.")
  enc <- .pkg_env$st_models[[model]]
  if (is.null(enc)) {
    if (!reticulate::py_module_available("sentence_transformers")) {
      .stop("Python module sentence_transformers not found. Run setup_python_env() once, ",
            "then activate_python_env() at the start of the session.")
    }
    st <- reticulate::import("sentence_transformers")
    .msg("Loading ", model, " (first use downloads it) ...")
    enc <- st$SentenceTransformer(model)
    .pkg_env$st_models[[model]] <- enc
  }
  E <- enc$encode(as.list(texts), batch_size = 16L)
  list(embeddings = matrix(as.numeric(E), nrow = length(texts)), tokens = NA_real_, cached = FALSE)
}

#' Cosine similarity between embeddings
#'
#' The cosine of the angle between two vectors: 1 for the same direction,
#' 0 for unrelated, -1 for opposite. For unit-length rows (as returned by
#' [embed_texts()]) it equals the dot product.
#'
#' @param a A matrix (one vector per row) or a single vector.
#' @param b Another matrix or vector with the same number of columns.
#'   `NULL` compares the rows of `a` with each other.
#' @return A matrix with `nrow(a)` rows and `nrow(b)` columns.
#' @examples
#' data(minilm_embeddings)
#' S <- cosine_similarity(minilm_embeddings$texts$extraversion[1:3, ],
#'                        minilm_embeddings$labels$extraversion)
#' round(S, 2)
#' cosine_similarity(c(1, 0), c(1, 1))
#' @export
cosine_similarity <- function(a, b = NULL) {
  a <- .as_row_matrix(a)
  b <- if (is.null(b)) a else .as_row_matrix(b)
  if (ncol(a) != ncol(b)) {
    .stop("The vectors have different lengths (", ncol(a), " and ", ncol(b),
          "): were they made with the same embedding model?")
  }
  S <- (a %*% t(b)) / outer(sqrt(rowSums(a^2)), sqrt(rowSums(b^2)))
  S[is.nan(S)] <- NA
  S
}

.as_row_matrix <- function(x) {
  if (is.matrix(x)) return(x)
  if (is.numeric(x)) return(matrix(x, nrow = 1, dimnames = list(NULL, names(x))))
  .stop("Embeddings must be a numeric matrix or vector.")
}

# ---- Optional Python environment for "hf:" models --------------------------------

.python_env_dir <- function(env_name) file.path(tools::R_user_dir("researchAgentR", "data"), env_name)

.python_path <- function(env_name) {
  dir <- .python_env_dir(env_name)
  if (.Platform$OS.type == "windows") file.path(dir, "Scripts", "python.exe") else file.path(dir, "bin", "python")
}

#' Set up the optional Python environment for local sentence embeddings
#'
#' Only needed for `"hf:"` embedding models ([embed_texts()]); everything
#' else in the package is pure R, and Ollama embeddings need no Python.
#' Creates a Python environment with [uv](https://docs.astral.sh/uv/) in the
#' package's user data folder (`tools::R_user_dir("researchAgentR",
#' "data")`) and installs `sentence-transformers` (which brings PyTorch).
#' Run it once; then call [activate_python_env()] at the start of each
#' session, before other packages that use Python.
#'
#' On Intel Macs, PyTorch 2.2.2 is the last version with installers, so it is
#' pinned there together with `numpy<2`.
#'
#' @param env_name Name of the environment folder.
#' @param python_version Python version for uv to install.
#' @return The path of the Python interpreter, invisibly.
#' @examples
#' \dontrun{
#' setup_python_env()      # once
#' activate_python_env()   # every session
#' E <- embed_texts(c("one text", "another"),
#'                  model = "hf:sentence-transformers/all-MiniLM-L6-v2")
#' }
#' @export
setup_python_env <- function(env_name = "st_env", python_version = "3.11") {
  uv <- Sys.which("uv")
  if (!nzchar(uv)) {
    .stop("uv is not installed or not on the PATH. Install it:\n",
          "  macOS/Linux: curl -LsSf https://astral.sh/uv/install.sh | sh\n",
          "  Windows:     powershell -c \"irm https://astral.sh/uv/install.ps1 | iex\"\n",
          "then restart RStudio.")
  }
  q <- function(x) shQuote(x, type = if (.Platform$OS.type == "windows") "cmd" else "sh")
  dir <- .python_env_dir(env_name)
  dir.create(dirname(dir), recursive = TRUE, showWarnings = FALSE)
  .msg("Creating Python ", python_version, " environment in ", dir)
  if (system2(uv, c("venv", "--python", python_version, q(dir))) != 0) .stop("uv venv failed (see above).")
  py <- .python_path(env_name)
  intel_mac <- Sys.info()[["sysname"]] == "Darwin" && Sys.info()[["machine"]] == "x86_64"
  pkgs <- c("sentence-transformers", if (intel_mac) c(q("torch==2.2.2"), q("numpy<2")))
  .msg("Installing ", paste(pkgs, collapse = " "), " (a few minutes) ...")
  if (system2(uv, c("pip", "install", "--python", q(py), pkgs)) != 0) {
    .stop("Installation failed (see above).")
  }
  .msg("Done. In each session, call activate_python_env(\"", env_name, "\") first.")
  invisible(py)
}

#' @rdname setup_python_env
#' @export
activate_python_env <- function(env_name = "st_env") {
  if (!requireNamespace("reticulate", quietly = TRUE)) .stop("Install reticulate: install.packages(\"reticulate\").")
  py <- .python_path(env_name)
  if (!file.exists(py)) .stop("No Python environment at ", py, ". Run setup_python_env() first.")
  # Set before Python starts: avoids tokenizer fork warnings, and on macOS a
  # second OpenMP runtime (PyTorch's) then warns instead of aborting R.
  Sys.setenv(TOKENIZERS_PARALLELISM = "false")
  if (Sys.info()[["sysname"]] == "Darwin" && !nzchar(Sys.getenv("KMP_DUPLICATE_LIB_OK"))) {
    Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
  }
  reticulate::use_python(py, required = TRUE)
  .msg("Python: ", py)
  invisible(py)
}
