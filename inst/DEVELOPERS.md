# researchAgentR: notes for developers

The package source is teaching material. Prefer short, readable functions with
comments that explain *why*, over clever code. Students read this code.

## Conventions (all files)

- English; no em dashes; no "we/our" in docs and comments; never "delve".
- Imports are only `httr2`, `jsonlite`, `stats`, `tools`, `utils`. Anything else
  goes in Suggests and is used through `requireNamespace(pkg, quietly = TRUE)`
  with a clear error telling the user to install it.
- One function per step, few arguments, sensible defaults.
- Common output format: data frames for tabular results, with metadata in
  attributes. Failures become `NA` rows that are counted and reported, never
  silently dropped or replaced.
- Every HTTP request goes through `.request()` + `.perform()` (R/http.R): they
  handle auth, retries with backoff on 429/5xx, rate limits, readable errors and
  the response cache (`use_cache()`). Do not call `httr2::req_perform()`
  directly.
- Every model call goes through `chat()` (R/chat.R). Never call a provider API
  directly.
- Roxygen with markdown. Every exported function: title, description,
  `@param`, `@return`, `@examples`. Examples that need the network or a key go
  in `\dontrun{}`; examples that do not, run.
- Messages to users: `.msg()` (respects `researchAgentR.verbose`), errors
  `.stop()`, warnings `.warn()` (all in R/utils.R).
- There is **no mock model**. Unit tests cover only logic that needs no model
  or network (parsing, schemas, arithmetic, validation). Anything that calls a
  model or API is tested live, guarded by
  `skip_if_not(nzchar(Sys.getenv("GROQ_API_KEY")))` (or the relevant key) and
  `skip_on_cran()`. Do not fabricate API responses to test model behavior.
  Where a parser must be tested, use small response fragments whose shape is
  documented in the provider's official docs, and cite the doc URL in a comment.

## Core API (do not change these files: R/utils.R, R/keys.R, R/registry.R,
## R/http.R, R/cache.R, R/chat.R, R/adapters.R, R/tools.R, R/agent.R,
## R/episode.R, R/setup.R)

Internal helpers you may use: `%||%`, `.pkg_env`, `.opt(name, default)`,
`.msg()`, `.stop()`, `.warn()`, `.now()`, `.to_json(x, pretty)`,
`.from_json(text)`, `.hash(x)`, `.truncate_tokens(text, max_tokens)`,
`.as_text(x)`, `.shorten(x, n)`, `.empty_object()`, `.get_key(service)`
(services: see `.key_vars` in R/keys.R), `.request(url, body, method, auth =
"bearer"|"anthropic"|"none", key, headers, query, realm, rpm, timeout)`,
`.perform(req, cache = TRUE)` which returns
`list(body, status, latency, headers, recorded_at, cached)`.

Exported core:

- `chat(messages, model, system, tools, output_schema, max_tokens, effort,
  temperature, ...)` returns an `llm_response`: `content`, `tool_calls`
  (each `list(id, name, arguments, arguments_raw, parse_error)`), `reasoning`,
  `usage` (`input` = uncached input, `cache_read`, `cache_write`, `output`,
  `reasoning`), `cost` (USD at list price), `latency`, `model`, `provider`,
  `stop_reason`, `cached`, `notes`, `executed_tools`.
  `output_schema` is a raw JSON Schema (R list) and cannot be combined with tools.
- `ask(prompt, model, system, ...)` returns text.
- `define_tool(fn, description, args, name, approval, timeout,
  max_output_tokens, cost_per_call, untrusted)` returns `agent_tool`
  (fields: `name`, `description`, `fn`, `args`, `schema`, `strict`, ...).
  Argument specs: `arg_string`, `arg_number`, `arg_integer`, `arg_boolean`,
  `arg_enum(description, values)`, `arg_array(description, items)`,
  `arg_object(description, fields)`. An `agent_tool` may carry
  `validate = function(arguments) list(arguments=, problems=)` to replace the
  default argument check.
- `call_tool(tool_call, tools, arguments)` runs a call safely; returns
  `list(id, name, output, is_error, value, truncated, cost, latency)`.
- `builtin_tool(type, ...)`, `remote_mcp_tool(...)`.
- `agent(name, instructions, model, tools, description, output_schema,
  context, max_tokens, effort)`; `output_schema` is a named list of `arg_*()`
  specs (the agent finishes by calling `final_answer`).
- `run(agent, task, budget, verbose)` returns an `episode` (list): `id`,
  `agent`, `model`, `task`, `steps` (model steps: `type="model"`, `input`,
  `cache_read`, `cache_write`, `output`, `cost`, `latency`, `context_tokens`,
  `content`, `tool_calls`; tool steps: `type="tool"`, `tool`, `arguments`,
  `result`, `is_error`, `latency`, `cost`), `answer`, `stop_reason`,
  `seconds`, `messages`. `.totals(ep)` gives model_calls, tool_calls,
  tool_errors, input, cache_read, cache_write, output, tokens, peak_context,
  cost, seconds.
- `budget(max_steps, max_tokens, max_cost, max_seconds, max_tool_errors)`.
- Context strategies are lists of class `context_strategy` with `name` and
  `prepare = function(messages, episode) messages`. `ctx_full()` is in core;
  other `ctx_*()` live in the context module.
- `episode_steps(ep)`, `episodes_df(list_or_dir)`, `save_episode()`,
  `read_episode()`.
- `default_model(role)`, `set_default_model(role, model)` with roles lead,
  worker, judge, compress, embed. `model_registry()`, `registry_set()`,
  `providers()`, `register_provider()`, `parse_model()`.
- `use_cache(mode)`, `cache_dir()`, `keep(name, expr)`.
- `count_tokens(x)` (about 4 characters per token).
- `verify_setup()`, `ollama_status()`, `list_provider_models()`.

## Shared interfaces between modules

- `read_documents(path, by = c("paragraph", "file"))` (context/memory module)
  reads a file or folder of .pdf (pdftools), .Rmd/.qmd (drops YAML and code
  chunks), .tex (drops comments, commands, math), .md, .txt; returns a data
  frame `file`, `section`, `paragraph` (integer), `text`. The writing module
  uses it.
- `embed_texts(texts, model = default_model("embed"))` (context/memory module)
  returns a numeric matrix, texts in rows, unit-length rows, with attributes
  `model` and `usage`. The multi-agent router and writing module use it.
- Research search functions return data frames with at least `title`,
  `year`, `doi`, `authors` (one string, "Last, F.; Last, F."), `source`,
  `abstract` (may be NA), `oa_url` (may be NA), `id`, `database`.
