# API keys. Keys live in ~/.Renviron (one line per key, for example
# GROQ_API_KEY=gsk_...). Every set_*_key() reads its key with Sys.getenv(), so
# a key never has to appear in a script.

.key_vars <- c(
  groq = "GROQ_API_KEY", openai = "OPENAI_API_KEY",
  anthropic = "ANTHROPIC_API_KEY", gemini = "GEMINI_API_KEY",
  mistral = "MISTRAL_API_KEY", cerebras = "CEREBRAS_API_KEY",
  openrouter = "OPENROUTER_API_KEY", openalex = "OPENALEX_API_KEY",
  tavily = "TAVILY_API_KEY", jina = "JINA_API_KEY", voyage = "VOYAGE_API_KEY",
  semantic_scholar = "SEMANTIC_SCHOLAR_API_KEY", ncbi = "NCBI_API_KEY",
  brave = "BRAVE_API_KEY",
  typesafe = "TYPESAFE_API_KEY"
)

.set_key <- function(service, key) {
  if (!nzchar(key)) {
    .stop("No key found for ", service, ". Add the line\n  ", .key_vars[[service]],
          "=your-key\nto ~/.Renviron (usethis::edit_r_environ() opens it), save, ",
          "and restart R.")
  }
  .pkg_env$keys[[service]] <- key
  .msg(service, " key set (", .mask(key), ").")
  invisible(TRUE)
}

# The key for a service: the one set in this session, else the environment
# variable. Returns "" when there is none.
.get_key <- function(service) {
  if (!service %in% names(.key_vars)) return("")
  .pkg_env$keys[[service]] %||% Sys.getenv(.key_vars[[service]])
}

.mask <- function(key) {
  if (!nzchar(key)) return("")
  paste0(substr(key, 1, 4), strrep("*", 6), substr(key, nchar(key) - 3, nchar(key)))
}

#' Set API keys for a session
#'
#' Each function reads its key from an environment variable (stored in
#' `~/.Renviron`) and keeps it for the session. Calling them is optional:
#' every client also reads the environment variable directly. Never type a
#' key into a script you might share.
#'
#' | Function | Environment variable | Needed for |
#' |---|---|---|
#' | `set_groq_key()` | `GROQ_API_KEY` | Groq models (free tier) |
#' | `set_openai_key()` | `OPENAI_API_KEY` | OpenAI models and embeddings (paid) |
#' | `set_anthropic_key()` | `ANTHROPIC_API_KEY` | Claude models (paid) |
#' | `set_gemini_key()` | `GEMINI_API_KEY` | Google Gemini models |
#' | `set_mistral_key()` | `MISTRAL_API_KEY` | Mistral models |
#' | `set_cerebras_key()` | `CEREBRAS_API_KEY` | Cerebras models |
#' | `set_openrouter_key()` | `OPENROUTER_API_KEY` | OpenRouter models |
#' | `set_openalex_key()` | `OPENALEX_API_KEY` | OpenAlex literature search (free key) |
#' | `set_tavily_key()` | `TAVILY_API_KEY` | Tavily web search (free plan) |
#' | `set_jina_key()` | `JINA_API_KEY` | Jina embeddings |
#' | `set_voyage_key()` | `VOYAGE_API_KEY` | Voyage embeddings |
#' | `set_semantic_scholar_key()` | `SEMANTIC_SCHOLAR_API_KEY` | Semantic Scholar (optional) |
#' | `set_ncbi_key()` | `NCBI_API_KEY` | PubMed E-utilities (optional) |
#' | `set_typesafe_key()` | `TYPESAFE_API_KEY` | Jev classification (paid) |
#' | `set_brave_key()` | `BRAVE_API_KEY` | Brave web search (optional, paid) |
#'
#' Crossref and Unpaywall ask for an e-mail address (the "polite pool"): add
#' `RESEARCHAGENTR_EMAIL=you@university.edu` to `~/.Renviron`, or set
#' `options(researchAgentR.email = "you@university.edu")`.
#'
#' @param key The key. Leave the default, which reads the environment variable.
#' @return `TRUE`, invisibly. An error with instructions if no key is found.
#' @examples
#' \dontrun{
#' set_groq_key()       # reads GROQ_API_KEY from ~/.Renviron
#' }
#' @name set_keys
NULL

#' @rdname set_keys
#' @export
set_groq_key <- function(key = Sys.getenv("GROQ_API_KEY")) .set_key("groq", key)
#' @rdname set_keys
#' @export
set_openai_key <- function(key = Sys.getenv("OPENAI_API_KEY")) .set_key("openai", key)
#' @rdname set_keys
#' @export
set_anthropic_key <- function(key = Sys.getenv("ANTHROPIC_API_KEY")) .set_key("anthropic", key)
#' @rdname set_keys
#' @export
set_gemini_key <- function(key = Sys.getenv("GEMINI_API_KEY")) .set_key("gemini", key)
#' @rdname set_keys
#' @export
set_mistral_key <- function(key = Sys.getenv("MISTRAL_API_KEY")) .set_key("mistral", key)
#' @rdname set_keys
#' @export
set_cerebras_key <- function(key = Sys.getenv("CEREBRAS_API_KEY")) .set_key("cerebras", key)
#' @rdname set_keys
#' @export
set_openrouter_key <- function(key = Sys.getenv("OPENROUTER_API_KEY")) .set_key("openrouter", key)
#' @rdname set_keys
#' @export
set_openalex_key <- function(key = Sys.getenv("OPENALEX_API_KEY")) .set_key("openalex", key)
#' @rdname set_keys
#' @export
set_tavily_key <- function(key = Sys.getenv("TAVILY_API_KEY")) .set_key("tavily", key)
#' @rdname set_keys
#' @export
set_jina_key <- function(key = Sys.getenv("JINA_API_KEY")) .set_key("jina", key)
#' @rdname set_keys
#' @export
set_voyage_key <- function(key = Sys.getenv("VOYAGE_API_KEY")) .set_key("voyage", key)
#' @rdname set_keys
#' @export
set_semantic_scholar_key <- function(key = Sys.getenv("SEMANTIC_SCHOLAR_API_KEY")) {
  .set_key("semantic_scholar", key)
}
#' @rdname set_keys
#' @export
set_ncbi_key <- function(key = Sys.getenv("NCBI_API_KEY")) .set_key("ncbi", key)
#' @rdname set_keys
#' @export
set_typesafe_key <- function(key = Sys.getenv("TYPESAFE_API_KEY")) .set_key("typesafe", key)
#' @rdname set_keys
#' @export
set_brave_key <- function(key = Sys.getenv("BRAVE_API_KEY")) .set_key("brave", key)
