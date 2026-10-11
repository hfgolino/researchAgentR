# Providers, models, prices and default models.
#
# A model is always written as "provider:model", split at the FIRST colon:
#   "groq:openai/gpt-oss-20b"   provider "groq",   model "openai/gpt-oss-20b"
#   "ollama:qwen3:4b"           provider "ollama", model "qwen3:4b"

# ---- Providers ---------------------------------------------------------------

# api: which wire format the provider speaks
#   "chat"       OpenAI-compatible Chat Completions (/chat/completions)
#   "responses"  OpenAI Responses API (/responses)
#   "anthropic"  Anthropic Messages API (/messages)
.default_providers <- function() {
  data.frame(
    provider = c("groq", "openai", "anthropic", "gemini", "mistral", "cerebras",
                 "openrouter", "ollama", "lmstudio"),
    base_url = c("https://api.groq.com/openai/v1",
                 "https://api.openai.com/v1",
                 "https://api.anthropic.com/v1",
                 "https://generativelanguage.googleapis.com/v1beta/openai",
                 "https://api.mistral.ai/v1",
                 "https://api.cerebras.ai/v1",
                 "https://openrouter.ai/api/v1",
                 "http://localhost:11434/v1",
                 "http://localhost:1234/v1"),
    api = c("chat", "responses", "anthropic", "chat", "chat", "chat", "chat",
            "chat", "chat"),
    key = c("groq", "openai", "anthropic", "gemini", "mistral", "cerebras",
            "openrouter", NA, NA),
    local = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}

#' List the model providers the package knows
#'
#' Every provider is reached by one of three adapters: OpenAI-compatible Chat
#' Completions (`api = "chat"`), the OpenAI Responses API (`"responses"`) or
#' the Anthropic Messages API (`"anthropic"`). Add any other
#' OpenAI-compatible endpoint with [register_provider()].
#'
#' @return A data frame with one row per provider.
#' @examples
#' providers()
#' @export
providers <- function() {
  .pkg_env$providers %||% .default_providers()
}

#' Add or change a model provider
#'
#' @param name Provider name, used before the colon in model strings.
#' @param base_url Base URL of the API, without the endpoint path.
#' @param api Wire format: `"chat"`, `"responses"` or `"anthropic"`.
#' @param key_env Name of the environment variable that holds the key, or
#'   `NULL` for local servers that need no key.
#' @param local `TRUE` when the server runs on this computer (data stay local).
#' @return The updated provider table, invisibly.
#' @examples
#' register_provider("myserver", "http://localhost:8080/v1", local = TRUE)
#' providers()
#' @export
register_provider <- function(name, base_url, api = c("chat", "responses", "anthropic"),
                              key_env = NULL, local = FALSE) {
  api <- match.arg(api)
  p <- providers()
  p <- p[p$provider != name, , drop = FALSE]
  key <- NA_character_
  if (!is.null(key_env)) {
    key <- name
    .pkg_env$extra_key_vars[[name]] <- key_env
  }
  p <- rbind(p, data.frame(provider = name, base_url = sub("/$", "", base_url),
                           api = api, key = key, local = local,
                           stringsAsFactors = FALSE))
  .pkg_env$providers <- p
  invisible(p)
}

.provider_info <- function(provider) {
  p <- providers()
  row <- p[p$provider == provider, , drop = FALSE]
  if (nrow(row) == 0) {
    .stop("Unknown provider '", provider, "'. Known providers: ",
          paste(p$provider, collapse = ", "), ". Add one with register_provider().")
  }
  as.list(row)
}

.provider_key <- function(info) {
  if (is.na(info$key)) return("")
  extra <- .pkg_env$extra_key_vars[[info$key]]
  if (!is.null(extra)) return(Sys.getenv(extra))
  .get_key(info$key)
}

#' Split a model string into provider and model
#'
#' @param model A string such as `"groq:openai/gpt-oss-20b"`. Only the first
#'   colon separates provider and model, so `"ollama:qwen3:4b"` works.
#' @return A list with `provider`, `model` and `id` (the full string).
#' @examples
#' parse_model("anthropic:claude-haiku-5-5")
#' parse_model("ollama:qwen3:4b")
#' @export
parse_model <- function(model) {
  if (inherits(model, "model_spec")) return(model)
  if (!is.character(model) || length(model) != 1 || !grepl(":", model, fixed = TRUE)) {
    .stop("Write models as \"provider:model\", for example \"groq:openai/gpt-oss-20b\" ",
          "or \"anthropic:claude-haiku-5-5\". Got: ", format(model))
  }
  i <- regexpr(":", model, fixed = TRUE)
  out <- list(provider = substr(model, 1, i - 1),
              model = substr(model, i + 1, nchar(model)),
              id = model)
  class(out) <- "model_spec"
  out
}

# ---- Model registry ----------------------------------------------------------

# Prices are US dollars per million tokens, from each provider's pricing page
# (accessed October 10-11, 2026). Free tiers are recorded at list price as a
# "shadow cost", so episodes can be compared across providers.
# Columns *_long apply when a request's total prompt reaches long_threshold.
.default_registry <- function() {
  r <- utils::read.csv(text = "
provider,model,context_window,max_output,tools,strict,parallel_tools,reasoning,input,cached_input,cache_write,output,long_threshold,input_long,cached_input_long,cache_write_long,output_long,source
groq,openai/gpt-oss-120b,131072,65536,TRUE,TRUE,FALSE,TRUE,0.15,0.075,NA,0.60,NA,NA,NA,NA,NA,https://groq.com/pricing
groq,openai/gpt-oss-20b,131072,65536,TRUE,TRUE,FALSE,TRUE,0.075,0.0375,NA,0.30,NA,NA,NA,NA,NA,https://groq.com/pricing
groq,openai/gpt-oss-safeguard-20b,131072,65536,TRUE,FALSE,FALSE,TRUE,0.075,NA,NA,0.30,NA,NA,NA,NA,NA,https://groq.com/pricing
groq,qwen/qwen3.8-27b,131072,16384,TRUE,TRUE,TRUE,TRUE,0.80,NA,NA,4.00,NA,NA,NA,NA,NA,https://console.groq.com/docs/models
anthropic,claude-opus-5-5,1000000,128000,TRUE,TRUE,TRUE,TRUE,4,0.20,5,20,NA,NA,NA,NA,NA,https://platform.claude.com/docs/en/about-claude/pricing
anthropic,claude-sonnet-5-5,1000000,128000,TRUE,TRUE,TRUE,TRUE,2,0.10,2.50,10,NA,NA,NA,NA,NA,https://platform.claude.com/docs/en/about-claude/pricing
anthropic,claude-haiku-5-5,1000000,128000,TRUE,TRUE,TRUE,TRUE,0.10,0.01,0.125,0.50,100000,0.50,0.05,0.625,2.50,https://platform.claude.com/docs/en/about-claude/pricing
openai,gpt-6-astra,1050000,128000,TRUE,TRUE,TRUE,TRUE,10,1.00,12.50,50,NA,NA,NA,NA,NA,https://developers.openai.com/api/docs/pricing
openai,gpt-6.1-sol,1050000,128000,TRUE,TRUE,TRUE,TRUE,2,0.10,2.50,10,NA,NA,NA,NA,NA,https://developers.openai.com/api/docs/pricing
openai,gpt-6-luna,1050000,128000,TRUE,TRUE,TRUE,TRUE,0.10,0.01,0.125,0.50,NA,NA,NA,NA,NA,https://developers.openai.com/api/docs/pricing
gemini,gemini-3.8-flash,NA,NA,TRUE,TRUE,NA,TRUE,0.75,NA,NA,3.75,NA,NA,NA,NA,NA,https://ai.google.dev/gemini-api/docs/pricing
ollama,qwen3:4b,262144,NA,TRUE,FALSE,NA,TRUE,0,0,0,0,NA,NA,NA,NA,NA,https://registry.ollama.ai/library/qwen3:4b
ollama,qwen3:8b,40960,NA,TRUE,FALSE,NA,TRUE,0,0,0,0,NA,NA,NA,NA,NA,https://registry.ollama.ai/library/qwen3:8b
ollama,phi4-mini,131072,NA,TRUE,FALSE,NA,FALSE,0,0,0,0,NA,NA,NA,NA,NA,https://registry.ollama.ai/library/phi4-mini/tags
ollama,gpt-oss:20b,131072,NA,TRUE,FALSE,NA,TRUE,0,0,0,0,NA,NA,NA,NA,NA,https://registry.ollama.ai/library/gpt-oss
", stringsAsFactors = FALSE, strip.white = TRUE)
  r$checked <- "2026-10-11"
  r
}

#' The model registry: context windows, capabilities and prices
#'
#' One row per known model, with prices in US dollars per million tokens.
#' Free tiers are recorded at list price, so the episode ledger reports a
#' comparable "shadow cost" even when nothing is billed. Prices change: check
#' the `source` column and update a row with [registry_set()].
#'
#' @param provider Optional provider name to filter by.
#' @return A data frame.
#' @examples
#' model_registry("anthropic")[, c("model", "input", "output")]
#' @export
model_registry <- function(provider = NULL) {
  r <- .pkg_env$registry %||% .default_registry()
  if (!is.null(provider)) r <- r[r$provider == provider, , drop = FALSE]
  r
}

#' Add or update a model in the registry
#'
#' @param model A `"provider:model"` string.
#' @param ... Columns to set, for example `input = 0.2, output = 0.8` (US
#'   dollars per million tokens) or `context_window = 32768`.
#' @return The updated registry, invisibly.
#' @examples
#' registry_set("ollama:llama3.2:3b", input = 0, output = 0, tools = TRUE)
#' tail(model_registry(), 1)
#' @export
registry_set <- function(model, ...) {
  m <- parse_model(model)
  r <- model_registry()
  vals <- list(...)
  bad <- setdiff(names(vals), names(r))
  if (length(bad)) .stop("Unknown registry columns: ", paste(bad, collapse = ", "))
  i <- which(r$provider == m$provider & r$model == m$model)
  if (length(i) == 0) {
    new <- r[NA_integer_, , drop = FALSE][1, ]
    new$provider <- m$provider
    new$model <- m$model
    r <- rbind(r, new)
    i <- nrow(r)
  }
  for (nm in names(vals)) r[i, nm] <- vals[[nm]]
  r$checked[i] <- format(Sys.Date())
  rownames(r) <- NULL
  .pkg_env$registry <- r
  invisible(r)
}

.model_info <- function(m) {
  r <- model_registry()
  row <- r[r$provider == m$provider & r$model == m$model, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  as.list(row[1, ])
}

# Cost of one call from normalized usage (see .usage()).
.cost <- function(m, usage) {
  info <- .model_info(m)
  if (is.null(info)) {
    if (!isTRUE(.pkg_env$warned_price[[m$id]])) {
      .pkg_env$warned_price[[m$id]] <- TRUE
      .msg("No price for ", m$id, " in model_registry(); its cost is recorded as NA. ",
           "Add one with registry_set().")
    }
    return(NA_real_)
  }
  prompt <- usage$input + usage$cache_read + usage$cache_write
  long <- !is.na(info$long_threshold) && prompt >= info$long_threshold
  price <- function(col) {
    v <- if (long) info[[paste0(col, "_long")]] else info[[col]]
    if (is.na(v) && col != "input") v <- info$input   # no discount listed
    if (is.na(v)) 0 else v
  }
  (usage$input * price("input") + usage$cache_read * price("cached_input") +
     usage$cache_write * price("cache_write") + usage$output * price("output")) / 1e6
}

# ---- Default models per role -------------------------------------------------

.default_models <- c(
  lead = "groq:openai/gpt-oss-120b",
  worker = "groq:openai/gpt-oss-20b",
  judge = "groq:openai/gpt-oss-120b",
  compress = "groq:openai/gpt-oss-20b",
  embed = "ollama:nomic-embed-text"
)

#' Choose the default model for each role
#'
#' Functions in the package take a `model` argument whose default is the
#' model for a role: `"lead"` (agents that plan), `"worker"` (small, stable
#' jobs), `"judge"` (graders), `"compress"` (summaries) and `"embed"`
#' (embeddings). Groq models are the defaults only because they are free;
#' any provider works.
#'
#' @param role One of `"lead"`, `"worker"`, `"judge"`, `"compress"`, `"embed"`.
#' @param model A `"provider:model"` string.
#' @return `set_default_model()` returns the previous model, invisibly;
#'   `default_model()` returns the current model string.
#' @examples
#' default_model("worker")
#' old <- set_default_model("worker", "anthropic:claude-haiku-5-5")
#' default_model("worker")
#' set_default_model("worker", old)
#' @export
set_default_model <- function(role = names(.default_models), model) {
  role <- match.arg(role)
  parse_model(model)
  old <- default_model(role)
  .pkg_env$defaults[[role]] <- model
  invisible(old)
}

#' @rdname set_default_model
#' @export
default_model <- function(role = names(.default_models)) {
  role <- match.arg(role)
  .pkg_env$defaults[[role]] %||% .opt(paste0("model.", role)) %||% .default_models[[role]]
}
