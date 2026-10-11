# Three adapters translate between the package's message format (see chat.R)
# and each provider's wire format. Each returns a list with content,
# tool_calls, reasoning, native (blocks to send back next turn), usage,
# latency, stop_reason, cached, headers, executed_tools and notes.

# ---- 1. OpenAI-compatible Chat Completions ----------------------------------
# Groq, Ollama, LM Studio, OpenRouter, Gemini, Mistral, Cerebras, any base_url.

.chat_completions <- function(m, info, messages, tools, output_schema, max_tokens,
                              effort, temperature, extra) {
  q <- .chat_quirks(m$provider)
  body <- list(model = m$model, messages = lapply(messages, .to_chat_message, quirks = q))
  body[[q$max_tokens_field]] <- max_tokens
  if (!is.null(effort)) body$reasoning_effort <- effort
  if (!is.null(temperature)) body$temperature <- temperature
  if (length(tools)) {
    t <- .split_tools(tools)
    body$tools <- c(
      lapply(t$functions, function(x) list(type = "function", `function` = list(
        name = x$name, description = x$description, parameters = x$schema))),
      lapply(t$builtins, function(x) x$spec))
  }
  if (!is.null(output_schema)) {
    body$response_format <- list(type = "json_schema", json_schema = list(
      name = "output", strict = TRUE, schema = output_schema))
  }
  body <- utils::modifyList(body, extra)
  req <- .request(paste0(info$base_url, "/chat/completions"), body = body,
                  auth = if (is.na(info$key)) "none" else "bearer", key = .provider_key(info),
                  realm = m$provider, rpm = .opt(paste0("rpm.", m$provider)))
  r <- .perform(req)
  msg <- r$body$choices[[1]]$message
  calls <- lapply(msg$tool_calls %||% list(), function(tc) {
    .tool_call(tc$id, tc$`function`$name, tc$`function`$arguments)
  })
  u <- r$body$usage %||% list()
  cached <- u$prompt_tokens_details$cached_tokens %||% 0
  list(
    content = msg$content %||% "",
    tool_calls = calls,
    reasoning = msg$reasoning %||% msg$reasoning_content %||% "",
    native = msg, native_api = "chat",
    usage = .usage(input = (u$prompt_tokens %||% 0) - cached, cache_read = cached,
                   output = u$completion_tokens %||% 0,
                   reasoning = u$completion_tokens_details$reasoning_tokens %||% 0),
    latency = r$latency, stop_reason = r$body$choices[[1]]$finish_reason %||% "",
    cached = r$cached, headers = r$headers,
    executed_tools = msg$executed_tools %||% list(), notes = character(0)
  )
}

.to_chat_message <- function(x, quirks) {
  switch(x$role,
    system = list(role = "system", content = x$content),
    user = list(role = "user", content = x$content),
    tool = list(role = "tool", tool_call_id = x$tool_call_id, content = x$content),
    assistant = {
      out <- list(role = "assistant", content = x$content %||% "")
      if (length(x$tool_calls)) {
        out$tool_calls <- lapply(x$tool_calls, function(tc) list(
          id = tc$id, type = "function",
          `function` = list(name = tc$name, arguments = tc$arguments_raw %||% .json_args(tc$arguments))))
      }
      if (quirks$send_reasoning && nzchar(x$reasoning %||% "")) out$reasoning <- x$reasoning
      out
    })
}

# ---- 2. OpenAI Responses API ------------------------------------------------
# OpenAI (recommended there for new projects); Groq also offers it (needed for
# Groq remote MCP). Requests are stateless: the whole history is sent each
# time, including the reasoning items the model returned, as OpenAI advises.

.responses_api <- function(m, info, messages, tools, output_schema, max_tokens,
                           effort, temperature, extra) {
  system <- unlist(lapply(Filter(function(x) x$role == "system", messages), `[[`, "content"))
  input <- list()
  for (x in Filter(function(x) x$role != "system", messages)) {
    input <- c(input, .to_responses_items(x, m))
  }
  body <- list(model = m$model, input = input, max_output_tokens = max_tokens)
  if (length(system)) body$instructions <- paste(system, collapse = "\n\n")
  if (m$provider == "openai") body$store <- FALSE
  if (!is.null(effort)) body$reasoning <- list(effort = effort)
  if (!is.null(temperature)) body$temperature <- temperature
  if (length(tools)) {
    t <- .split_tools(tools)
    body$tools <- c(
      lapply(t$functions, function(x) list(type = "function", name = x$name,
                                           description = x$description,
                                           parameters = x$schema, strict = isTRUE(x$strict))),
      lapply(t$builtins, function(x) x$spec))
  }
  if (!is.null(output_schema)) {
    body$text <- list(format = list(type = "json_schema", name = "output",
                                    schema = output_schema, strict = TRUE))
  }
  body <- utils::modifyList(body, extra)
  req <- .request(paste0(info$base_url, "/responses"), body = body, auth = "bearer",
                  key = .provider_key(info), realm = m$provider,
                  rpm = .opt(paste0("rpm.", m$provider)))
  r <- .perform(req)
  out <- r$body$output %||% list()
  text <- character(0); calls <- list(); reasoning <- character(0); executed <- list()
  for (item in out) {
    type <- item$type %||% ""
    if (type == "message") {
      for (part in item$content) if (identical(part$type, "output_text")) text <- c(text, part$text)
    } else if (type == "function_call") {
      calls[[length(calls) + 1]] <- .tool_call(item$call_id, item$name, item$arguments)
    } else if (type == "reasoning") {
      s <- unlist(lapply(c(item$summary, item$content), function(p) p$text))
      reasoning <- c(reasoning, s)
    } else {
      executed[[length(executed) + 1]] <- item   # web_search_call, mcp_call, ...
    }
  }
  u <- r$body$usage %||% list()
  cached <- u$input_tokens_details$cached_tokens %||% 0
  written <- u$input_tokens_details$cache_write_tokens %||% u$cache_write_tokens %||% 0
  list(
    content = paste(text, collapse = "\n"), tool_calls = calls,
    reasoning = paste(reasoning, collapse = "\n"),
    native = out, native_api = "responses",
    usage = .usage(input = (u$input_tokens %||% 0) - cached - written, cache_read = cached,
                   cache_write = written, output = u$output_tokens %||% 0,
                   reasoning = u$output_tokens_details$reasoning_tokens %||% 0),
    latency = r$latency, stop_reason = r$body$status %||% "",
    cached = r$cached, headers = r$headers, executed_tools = executed,
    notes = character(0)
  )
}

.to_responses_items <- function(x, m) {
  if (x$role == "user") return(list(list(role = "user", content = x$content)))
  if (x$role == "tool") {
    return(list(list(type = "function_call_output", call_id = x$tool_call_id, output = x$content)))
  }
  # assistant: replay the provider's own output items when they came from the
  # same API (this keeps reasoning items), otherwise rebuild them
  if (identical(x$native_api, "responses") && length(x$native) && isTRUE(.opt("replay_native", TRUE))) {
    return(x$native)
  }
  items <- list()
  if (nzchar(x$content %||% "")) items <- c(items, list(list(role = "assistant", content = x$content)))
  for (tc in x$tool_calls) {
    items <- c(items, list(list(type = "function_call", call_id = tc$id, name = tc$name,
                                arguments = tc$arguments_raw %||% .json_args(tc$arguments))))
  }
  items
}

# ---- 3. Anthropic Messages API ----------------------------------------------
# Native client: Anthropic's OpenAI-compatible endpoint ignores `strict`,
# prompt caching and thinking output, so it is not used.

.anthropic_messages <- function(m, info, messages, tools, output_schema, max_tokens,
                                effort, temperature, extra) {
  notes <- character(0)
  system <- unlist(lapply(Filter(function(x) x$role == "system", messages), `[[`, "content"))
  body <- list(model = m$model, max_tokens = max_tokens,
               messages = .to_anthropic_messages(Filter(function(x) x$role != "system", messages)))
  if (length(system)) body$system <- paste(system, collapse = "\n\n")
  if (isTRUE(.opt("prompt_caching", TRUE))) body$cache_control <- list(type = "ephemeral")
  if (!is.null(temperature)) {
    notes <- c(notes, "temperature not sent: Claude 5.x models reject non-default sampling settings.")
  }
  config <- list()
  if (!is.null(effort)) config$effort <- effort
  if (!is.null(output_schema)) config$format <- list(type = "json_schema", schema = output_schema)
  if (length(config)) body$output_config <- config
  if (length(tools)) {
    t <- .split_tools(tools)
    body$tools <- c(
      lapply(t$functions, function(x) {
        out <- list(name = x$name, description = x$description, input_schema = x$schema)
        if (isTRUE(x$strict)) out$strict <- TRUE
        out
      }),
      lapply(t$builtins, function(x) x$spec))
  }
  body <- utils::modifyList(body, extra)
  req <- .request(paste0(info$base_url, "/messages"), body = body, auth = "anthropic",
                  key = .provider_key(info), realm = m$provider,
                  rpm = .opt(paste0("rpm.", m$provider)))
  r <- .perform(req)
  blocks <- r$body$content %||% list()
  text <- character(0); calls <- list(); reasoning <- character(0); executed <- list()
  for (b in blocks) {
    type <- b$type %||% ""
    if (type == "text") text <- c(text, b$text)
    else if (type == "tool_use") calls[[length(calls) + 1]] <- .tool_call(b$id, b$name, b$input)
    else if (type == "thinking") reasoning <- c(reasoning, b$thinking %||% "")
    else if (type != "redacted_thinking") executed[[length(executed) + 1]] <- b
  }
  u <- r$body$usage %||% list()
  list(
    content = paste(text, collapse = "\n"), tool_calls = calls,
    reasoning = paste(reasoning, collapse = "\n"),
    native = blocks, native_api = "anthropic",
    usage = .usage(input = u$input_tokens, cache_read = u$cache_read_input_tokens,
                   cache_write = u$cache_creation_input_tokens, output = u$output_tokens,
                   reasoning = u$output_tokens_details$thinking_tokens),
    latency = r$latency, stop_reason = r$body$stop_reason %||% "",
    cached = r$cached, headers = r$headers, executed_tools = executed, notes = notes
  )
}

# Anthropic rules: roles alternate; every tool result goes in ONE user message
# right after the assistant turn that asked for it, results first; thinking
# blocks are sent back unchanged (so assistant turns replay `native` blocks).
.to_anthropic_messages <- function(messages) {
  out <- list()
  pending <- list()
  flush <- function() {
    if (length(pending)) {
      out[[length(out) + 1]] <<- list(role = "user", content = pending)
      pending <<- list()
    }
  }
  for (x in messages) {
    if (x$role == "tool") {
      pending[[length(pending) + 1]] <- list(type = "tool_result", tool_use_id = x$tool_call_id,
                                             content = x$content, is_error = isTRUE(x$is_error))
    } else if (x$role == "user") {
      pending[[length(pending) + 1]] <- list(type = "text", text = x$content)
    } else {
      flush()
      if (identical(x$native_api, "anthropic") && length(x$native)) {
        content <- x$native
      } else {
        content <- list()
        if (nzchar(x$content %||% "")) content <- list(list(type = "text", text = x$content))
        for (tc in x$tool_calls) {
          content[[length(content) + 1]] <- list(type = "tool_use", id = tc$id, name = tc$name,
                                                 input = tc$arguments %||% .empty_object())
        }
      }
      out[[length(out) + 1]] <- list(role = "assistant", content = content)
    }
  }
  flush()
  out
}

#' List the models a provider offers right now
#'
#' Calls the provider's model-listing endpoint, the reliable way to check
#' which model IDs exist today (models get retired). No tokens are used.
#'
#' @param provider A provider name from [providers()].
#' @return A data frame with model `id` and, where reported, `owned_by` and
#'   `context_window`.
#' @examples
#' \dontrun{
#' list_provider_models("groq")
#' list_provider_models("ollama")   # the models pulled on this computer
#' }
#' @export
list_provider_models <- function(provider = "groq") {
  info <- .provider_info(provider)
  auth <- if (info$api == "anthropic") "anthropic" else if (is.na(info$key)) "none" else "bearer"
  req <- .request(paste0(info$base_url, "/models"), auth = auth, key = .provider_key(info))
  r <- .perform(req, cache = FALSE)
  d <- r$body$data %||% list()
  data.frame(
    id = vapply(d, function(x) x$id %||% NA_character_, character(1)),
    owned_by = vapply(d, function(x) x$owned_by %||% NA_character_, character(1)),
    context_window = vapply(d, function(x) as.numeric(x$context_window %||% NA), numeric(1)),
    stringsAsFactors = FALSE
  )
}
