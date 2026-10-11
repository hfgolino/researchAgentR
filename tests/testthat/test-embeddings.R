# Embeddings: offline tests of the arithmetic and the response parsers; live
# tests for each provider are skipped unless its key (or Ollama) is present.

test_that("cosine_similarity matches the definition", {
  a <- rbind(c(1, 0), c(1, 1), c(0, 0))
  S <- cosine_similarity(a, c(1, 0))
  expect_equal(dim(S), c(3L, 1L))
  expect_equal(S[1:2, 1], c(1, 1 / sqrt(2)))
  expect_true(is.na(S[3, 1]))                        # a zero vector has no direction
  expect_equal(diag(cosine_similarity(a[1:2, ])), c(1, 1))
  expect_equal(cosine_similarity(c(1, 2, 3), c(-1, -2, -3))[1, 1], -1)
  expect_error(cosine_similarity(c(1, 2), c(1, 2, 3)), "different lengths")
  expect_error(cosine_similarity("a"), "numeric")
})

test_that("cosine_similarity on the shipped MiniLM embeddings is a dot product", {
  data(minilm_embeddings, package = "researchAgentR", envir = environment())
  E <- minilm_embeddings$texts$extraversion[1:5, ]
  L <- minilm_embeddings$labels$extraversion
  expect_equal(cosine_similarity(E, L), E %*% t(L) / outer(sqrt(rowSums(E^2)), sqrt(rowSums(L^2))))
  expect_equal(colnames(cosine_similarity(E, L)), rownames(L))
})

test_that(".unit_rows scales rows to length 1 and marks zero rows NA", {
  U <- .unit_rows(rbind(c(3, 4), c(0, 0), c(NA, 1)))
  expect_equal(U[1, ], c(0.6, 0.8))
  expect_true(all(is.na(U[2:3, ])))
})

test_that("response parsers put rows back in input order", {
  # Shape documented at https://api.jina.ai/openapi.json (EmbeddingResponse:
  # data[] with index and embedding) and for OpenAI at
  # https://developers.openai.com/api/reference/resources/embeddings/methods/create
  data <- list(list(object = "embedding", index = 1, embedding = list(0, 1)),
               list(object = "embedding", index = 0, embedding = list(1, 0)))
  expect_equal(.data_to_matrix(data), rbind(c(1, 0), c(0, 1)))
  # Ollama /api/embed returns "embeddings": [[...], [...]] (https://docs.ollama.com/api/embed)
  expect_equal(.rows_to_matrix(list(list(0.1, 0.2), list(0.3, 0.4))), rbind(c(0.1, 0.2), c(0.3, 0.4)))
})

test_that("Jina tasks follow input_type", {
  expect_equal(.jina_task(NULL), "text-matching")
  expect_equal(.jina_task("query"), "retrieval.query")
  expect_equal(.jina_task("document"), "retrieval.passage")
  expect_equal(.jina_task("separation"), "separation")
  expect_error(.jina_task("other"), "input_type")
})

test_that("embedding prices: local models are free, known APIs priced, others NA", {
  expect_equal(.embed_price(parse_model("ollama:nomic-embed-text")), 0)
  expect_equal(.embed_price(parse_model("hf:sentence-transformers/all-MiniLM-L6-v2")), 0)
  expect_equal(.embed_price(parse_model("openai:text-embedding-3-small")), 0.02)
  expect_equal(.embed_price(parse_model("voyage:voyage-4")), 0.06)
  expect_true(is.na(.embed_price(parse_model("jina:jina-embeddings-v3"))))
})

test_that("embed_texts rejects unknown providers and needs keys", {
  expect_error(embed_texts("x", model = "groq:openai/gpt-oss-20b"), "Embedding providers")
  withr::with_envvar(c(JINA_API_KEY = ""), {
    old <- .pkg_env$keys$jina
    .pkg_env$keys$jina <- NULL
    expect_error(embed_texts("x", model = "jina:jina-embeddings-v3"), "No jina key")
    .pkg_env$keys$jina <- old
  })
  expect_error(embed_texts(c("", NA), model = "ollama:nomic-embed-text"), "All texts were empty")
})

test_that(".add_usage sums usage lists", {
  a <- list(tokens = 10, cost = 0.1, requests = 1, cached_requests = 0)
  expect_equal(.add_usage(a, a)$tokens, 20)
  expect_identical(.add_usage(NULL, a), a)
})

test_that("the Python environment lives in the package's user data folder", {
  expect_match(.python_env_dir("st_env"), "researchAgentR", fixed = TRUE)
  expect_match(.python_path("st_env"), if (.Platform$OS.type == "windows") "python.exe$" else "bin/python$")
})

# ---- live tests --------------------------------------------------------------------

check_live <- function(E, n) {
  expect_equal(nrow(E), n)
  expect_equal(rowSums(E^2), rep(1, n), tolerance = 1e-6)
  expect_true(is.list(attr(E, "usage")))
  S <- cosine_similarity(E)
  expect_gt(S[1, 2], S[1, 3])   # two reliability sentences are closer than a cooking one
}
texts <- c("The scale showed high internal consistency.",
           "Cronbach's alpha indicated good reliability.",
           "Simmer the onions until golden.")

test_that("Ollama embeddings (live)", {
  skip_on_cran()
  m <- tryCatch(ollama_status(), error = function(e) NULL)
  skip_if(is.null(m) || !any(grepl("^nomic-embed-text", m$name)))
  E <- embed_texts(texts, model = "ollama:nomic-embed-text")
  check_live(E, 3)
  expect_equal(attr(E, "usage")$cost, 0)
})

test_that("Jina embeddings (live)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("JINA_API_KEY")))
  E <- embed_texts(texts, model = "jina:jina-embeddings-v3", input_type = "document", dimensions = 256)
  check_live(E, 3)
  expect_equal(ncol(E), 256)
})

test_that("Voyage embeddings (live)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("VOYAGE_API_KEY")))
  E <- embed_texts(texts, model = "voyage:voyage-4", input_type = "document", dimensions = 256)
  check_live(E, 3)
  expect_gt(attr(E, "usage")$tokens, 0)
})

test_that("OpenAI embeddings (live)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")))
  E <- embed_texts(texts, model = "openai:text-embedding-3-small")
  check_live(E, 3)
})

test_that("sentence-transformers embeddings through reticulate (live)", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  skip_if_not(file.exists(.python_path("st_env")))
  activate_python_env()
  E <- embed_texts(texts, model = "hf:sentence-transformers/all-MiniLM-L6-v2")
  check_live(E, 3)
  expect_equal(ncol(E), 384)
})
