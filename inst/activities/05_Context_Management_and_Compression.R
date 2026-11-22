# Activity 05: Context Management and Compression
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: See why an agent's context (and its bill) grows with every step, and
# compare strategies that keep it small by the only measure that counts:
# verified task success per token and per dollar.
#
# OVERVIEW: An agent re-sends the whole conversation to the model at every
# step. Tool outputs (search results, web pages) pile up, so step 6 pays for
# steps 1 to 5 again. On Groq's free tier this is not an abstract cost: the
# gpt-oss models are limited to 8,000 tokens per minute and 200,000 tokens per
# day (https://console.groq.com/docs/rate-limits, checked by the instructor on
# 2026-10-10; your own limits are at https://console.groq.com/settings/limits).
# One literature search that pastes 10 abstracts into the context can use a
# large share of a minute's budget by itself.
#
# This activity runs the SAME literature task under four context strategies,
# compares them on verified success, billed tokens and peak context, and then
# shows the tools that keep tool outputs small before they ever reach the
# model: filters in code, offloading with just-in-time recall, selecting
# relevant passages, model compression (and checking what it lost), sub-agents
# with a fresh context, and skills loaded only when needed.
#
# Ideas adapted from the context-management lecture and readings of Harvard
# CS2680 "Modern AI Systems: Agents and System Optimizations" (Juncheng Yang,
# Fall 2026), https://cs2680.seas.harvard.edu (readings of September 16).
# Ideas only: no course material is copied.
#
# researchAgentR functions used here:
#   count_tokens()                         estimate tokens before a call
#   ctx_full(), ctx_window(), ctx_compact(),
#   ctx_notes(), notes_tools()             context strategies for agent()
#   context_report(), episodes_df()        how the context grew, step by step
#   filter_output()                        trim a tool output in code
#   offload(), recall(), offload_tools()   store big results outside the context
#   select_relevant()                      keep the k most relevant passages
#   compress_text()                        summarize with a model, report what was lost
#   subagent()                             a worker with a fresh context
#   list_skills(), use_skills()            skills as progressive disclosure
# TIME: about 75 minutes.  COST: free tier (Groq, OpenAlex, Crossref); the
# select_relevant() step uses a local Ollama embedding model (free).
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

# Install or update the package (once):
# remotes::install_github("hfgolino/researchAgentR")

library(researchAgentR)
if (packageVersion("researchAgentR") < "0.1.0") {
  stop("This activity needs researchAgentR 0.1.0 or later. Update with:\n",
       "  remotes::install_github(\"hfgolino/researchAgentR\")", call. = FALSE)
}
library(ggplot2)

# Every API response is saved on disk the first time and replayed after that:
# re-running this script (or the class demo) costs nothing and needs no network.
use_cache("auto")

dir.create("results", showWarnings = FALSE)

# Keys live in ~/.Renviron (GROQ_API_KEY, OPENALEX_API_KEY), never in scripts.
# verify_setup(test_apis = TRUE)   # run once if anything below fails

SCOUT_MODEL <- "groq:openai/gpt-oss-20b"   # free tier; the small model saves tokens

# keep() saves whatever the expression returns, INCLUDING an episode that
# stopped on a network error. This helper lists such episodes, so they can be
# run again with keep(..., refresh = TRUE) once the network is back.
check_network <- function(episodes) {
  bad <- names(episodes)[vapply(episodes, function(ep) grepl("^api_error", ep$stop_reason),
                                logical(1))]
  if (length(bad)) message("Network error in: ", paste(bad, collapse = ", "),
                           ". Run them again with keep(..., refresh = TRUE).")
  invisible(bad)
}


# ==============================================================================
# STEP 1: THE PROBLEM. EVERY STEP RE-SENDS THE CONVERSATION
# ==============================================================================

# count_tokens() estimates tokens with the rule of thumb of about 4 characters
# per token. Providers count with their own tokenizers, so the episode ledger
# always records the provider's numbers; count_tokens() is for planning.
count_tokens("Exploratory graph analysis estimates the number of dimensions.")

data(extraversion)
count_tokens(extraversion)    # the whole data frame as JSON
# EXPECTED OUTPUT: two integers, 16 and 7918; the data frame is several
# thousand tokens, about the size of one generous search result.

# A TOY CONVERSATION (no model involved): a system message, a task, and six
# tool turns. Each "tool result" is 10 rows of the shipped extraversion
# texts, standing in for 10 search results. The assistant messages are empty
# placeholders, so the counts below are a lower bound.
toy <- list(list(role = "system", content = "You are a literature scout."),
            list(role = "user", content = "Find three papers on exploratory graph analysis."))
for (i in 1:6) {
  rows <- extraversion[(10 * i - 9):(10 * i), ]
  toy <- c(toy, list(
    list(role = "assistant", content = "",
         tool_calls = list(list(id = paste0("call_", i), name = "search_openalex",
                                arguments = list(query = paste("query", i))))),
    list(role = "tool", tool_call_id = paste0("call_", i), name = "search_openalex",
         content = filter_output(rows, fields = c("doc_id", "subtopic", "text"),
                                 max_items = 10, as_text = TRUE))))
}
length(toy)   # 14 messages

# Tokens of the message contents that a strategy would send
tokens_sent <- function(messages) {
  sum(count_tokens(vapply(messages, function(m) if (is.null(m$content)) "" else m$content,
                          character(1))))
}

# The model is called 7 times: before each of the 6 tool calls, and once more
# for the final answer. Each call sends everything that came before it.
call_ends <- seq(2, length(toy), by = 2)
full_sent <- vapply(call_ends, function(k) tokens_sent(toy[1:k]), numeric(1))
data.frame(call = seq_along(call_ends), tokens_sent = full_sent,
           cumulative_billed = cumsum(full_sent))
# EXPECTED OUTPUT: tokens_sent grows at every call (each call carries all
# earlier tool results), and cumulative_billed grows roughly with the SQUARE
# of the number of steps. Computed offline in this environment: 12,167 tokens
# in total for the 7 calls, for a conversation of 3,312 tokens. The billed
# total is already above the 8,000 tokens per minute of the free tier.

# EXPLAINER (worked example): with a 400-token system prompt and task and
# 1,600 new tokens per step (one tool result plus the model's reply), call k
# sends 400 + 1600 * (k - 1) tokens. Six calls bill
#   6 * 400 + 1600 * (0 + 1 + 2 + 3 + 4 + 5) = 2,400 + 24,000 = 26,400 tokens,
# about three minutes of the free tier's token budget, for one episode.
sum(400 + 1600 * (0:5))


# ==============================================================================
# STEP 2: WHAT EACH STRATEGY SENDS (OFFLINE)
# ==============================================================================

# A context strategy is a function prepare(messages, episode) that returns the
# messages actually sent. It never changes the full record of the episode.
# Two rules always hold: the system message and the task are kept, and a tool
# call is never separated from its result (providers reject orphans).
#
#   ctx_full()     send everything (the default; lossless, grows every step)
#   ctx_window(k)  keep only the last k turns; a note says what was removed
#   ctx_compact()  above a token threshold, a small model summarizes older turns
#                  (that call is billed, and recorded as a "context" step)
#   ctx_notes()    the agent writes notes to a Markdown file with note_write;
#                  the notes are sent at every step, old turns are dropped

window_view <- ctx_window(keep_last = 2)$prepare(toy, NULL)
vapply(window_view, `[[`, "", "role")
window_view[[3]]$content
# EXPECTED OUTPUT: system, user, a user note "[8 earlier messages removed to
# save context]", then the last two assistant/tool pairs.

toy_notes_file <- tempfile(fileext = ".md")
notes_write("Golino & Epskamp (2017), PLoS ONE, doi 10.1371/journal.pone.0174035",
            toy_notes_file, section = "Sources")
toy_notes <- ctx_notes(toy_notes_file, keep_last = 1)
notes_view <- toy_notes$prepare(toy, NULL)
vapply(notes_view, `[[`, "", "role")
cat(notes_view[[3]]$content)
# EXPECTED OUTPUT: system, user, the notes (with the DOI), a note about the
# removed messages, and only the last assistant/tool pair.

# The notes live OUTSIDE the context window, in a Markdown file. The agent
# reaches them through two tools made by notes_tools(): note_write (append a
# dated note, optionally under a heading) and note_read (read all notes).
# ctx_notes() creates them for you (toy_notes$tools); here they are called by
# hand, exactly as the agent loop would call them, so nothing is sent anywhere.
agent_notes_file <- tempfile(fileext = ".md")
note_tools <- notes_tools(agent_notes_file)
note_tools
# EXPECTED OUTPUT (computed offline): two <agent_tool> objects. note_write
# takes text (string) and section (string, optional); note_read takes no
# arguments and is flagged "output untrusted" (a notes file can be edited
# by people, so its content is treated as data).

call_tool("note_read", note_tools)$output
call_tool("note_write", note_tools,
          arguments = list(text = "EGA (2017), PLoS ONE: doi 10.1371/journal.pone.0174035",
                           section = "Sources"))$output
call_tool("note_write", note_tools,
          arguments = list(text = "Still missing: the DOI of the DynEGA article (Psychometrika, 2022)."))$output
cat(call_tool("note_read", note_tools)$output, "\n")
# EXPECTED OUTPUT (computed offline): "(no notes yet)" wrapped in
# <tool_output tool="note_read" trust="untrusted"> tags; then "Note saved."
# twice; then the file, wrapped the same way, with a dated "## Sources"
# entry holding the DOI and a dated "## Note" entry with the open question.

# A malformed call is an error the model can fix, never a crash:
call_tool("note_write", note_tools, arguments = list(section = "Sources"))$output
# EXPECTED OUTPUT (computed offline): "Error: invalid arguments for
# note_write: 'text' is required. Call the tool again with corrected arguments."

# The same two tools work with ANY context strategy. With a sliding window,
# old tool results are dropped, but the agent can still read its notes on
# demand (ctx_notes() differs only in also SHOWING the notes at every step):
window_scout_with_notes <- agent("scout_notes", "You find scholarly articles.",
  model = SCOUT_MODEL, tools = c(list(tool_openalex(n = 10)), notes_tools(agent_notes_file)),
  context = ctx_window(keep_last = 2))
window_scout_with_notes
# EXPECTED OUTPUT (computed offline): <agent> scout_notes |
# groq:openai/gpt-oss-20b, tools search_openalex, note_write, note_read, and
# "Context: window(2)". Nothing runs until run() is called.

sent_by <- function(strategy) {
  vapply(call_ends, function(k) tokens_sent(strategy$prepare(toy[1:k], NULL)), numeric(1))
}
offline_sent <- data.frame(call = seq_along(call_ends), full = sent_by(ctx_full()),
                           window_2 = sent_by(ctx_window(2)), notes_1 = sent_by(toy_notes))
offline_sent
colSums(offline_sent[, -1])
# EXPECTED OUTPUT: one row per call. "full" keeps growing; "window_2" and
# "notes_1" stop growing once old turns are dropped. Computed offline in this
# environment (column totals): full 12,167; window_2 6,334; notes_1 3,836.
# Notes cost tokens at EVERY call (call 1 sends 64 tokens instead of 19), and
# pay off only when they replace longer turns. The saving is real only if the
# task still succeeds: Steps 3 and 4 check.

# ctx_compact() needs a model to write the summary, so it runs in Step 3.
ctx_compact(trigger_tokens = 2500, keep_last = 2)

# EXPLAINER: "Lost in the Middle" (Liu et al., 2024, Transactions of the
# Association for Computational Linguistics, https://doi.org/10.1162/tacl_a_00638).
# In multi-document question answering, models used information at the
# BEGINNING or END of a long context more reliably than information in the
# MIDDLE, even when the context fit in the window. For agents this means:
#   - a longer context is not just more expensive, it can be WORSE;
#   - put what matters (task, notes, current evidence) at the edges;
#   - ctx_notes() places the notes right after the task, near the beginning.
# Strategies are therefore judged by verified task success, not by tokens
# saved: a strategy that halves the tokens and loses one DOI has failed.


# ==============================================================================
# STEP 3: THE SAME LITERATURE TASK UNDER FOUR STRATEGIES (LIVE, CACHED)
# ==============================================================================

lit_task <- paste(
  "Find the DOI of each of these three articles. Search for each one separately.",
  "1. 'Exploratory graph analysis: A new approach for estimating the number of",
  "dimensions in psychological research' (PLoS ONE, 2017).",
  "2. 'Modeling latent topics in social media using dynamic exploratory graph analysis:",
  "The case of the right-wing and left-wing trolls in the 2016 US elections'",
  "(Psychometrika, 2022).",
  "3. 'Entropy fit indices: New fit measures for assessing the structure and",
  "dimensionality of multiple latent variables' (Multivariate Behavioral Research, 2020).")

# The answer key (DOIs read from the published articles) and a code grader:
# the answer passes only if all three DOIs are in it.
expected_dois <- c("10.1371/journal.pone.0174035", "10.1007/s11336-021-09820-y",
                   "10.1080/00273171.2020.1779642")
doi_grader <- grader_doi(min_recall = 1)
verified <- function(ep) doi_grader(ep$answer, list(answer = expected_dois))$pass

scout_instructions <- paste(
  "You find scholarly articles. Search OpenAlex once for each article the user lists,",
  "check that title, journal and year match, and finish with one line per article:",
  "the title and its DOI. Never guess a DOI; write 'not found' instead.")

# One search tool for every strategy, with a FIXED number of results. With
# tool_openalex() the model may ask for fewer results than n, and the scouts
# would then differ in how much they read, not only in how they manage it.
# (filter_output() is explained in Step 5.)
search_tool <- define_tool(
  function(query) {
    hits <- search_openalex(query, n = 8)
    filter_output(hits, fields = c("title", "year", "source", "doi", "abstract"),
                  max_items = 8, max_chars_per_field = 400, as_text = TRUE)
  },
  "Search OpenAlex for scholarly works. Returns title, year, source, DOI and the start of the abstract of 8 works.",
  args = list(query = arg_string("Title words, authors or topic")),
  name = "search_openalex", untrusted = TRUE)

make_scout <- function(context, extra_tools = list(), extra_instructions = NULL) {
  agent("scout", paste(c(scout_instructions, extra_instructions), collapse = " "),
        model = SCOUT_MODEL, tools = c(list(search_tool), extra_tools), context = context)
}

notes_file <- file.path("results", "scout_notes.md")
notes_ctx <- ctx_notes(notes_file, keep_last = 2)

# The task needs three searches and then an answer that uses all three
# results. keep_last = 2 is deliberately too short for that: at the last call
# the first result has left the window. The compact and notes scouts drop the
# same turns but keep what matters, in a summary or in notes.
scouts <- list(
  full    = make_scout(ctx_full()),
  window  = make_scout(ctx_window(keep_last = 2)),
  compact = make_scout(ctx_compact(trigger_tokens = 2500, keep_last = 2)),
  notes   = make_scout(notes_ctx, extra_tools = notes_ctx$tools,   # notes need their tools
                       extra_instructions = paste(
                         "After each search, save the title and DOI you found with note_write",
                         "before the next search: older results will no longer be visible.")))
scouts$notes
# EXPECTED OUTPUT: <agent> scout | groq:openai/gpt-oss-20b, tools
# search_openalex, note_write, note_read, context "notes".

# Budgets are set BEFORE comparing strategies, and are the same for all.
lit_budget <- budget(max_steps = 10, max_tokens = 50000, max_cost = 0.05)

# Independent runs. The full, window and compact scouts send IDENTICAL first
# requests (same instructions, task and tool), so with the response cache on,
# the later scouts would replay the first scout's opening steps (and report
# them at cost 0). For a fair comparison the cache is switched off here;
# keep() still saves every episode, so the class demo replays offline.
old_cache <- use_cache("off")
episodes <- list()
for (s in names(scouts)) {
  episodes[[s]] <- keep(paste0("act05_scout_", s), {
    if (s == "notes") unlink(notes_file)   # start from empty notes
    run(scouts[[s]], lit_task, budget = lit_budget)
  })
}
use_cache(old_cache)
check_network(episodes)
# EXPECTED OUTPUT: each run prints its steps (model calls with tokens and
# cost, OpenAlex calls). On the free tier, the "full" run may pause on a 429
# (rate limit) and retry automatically. Later runs load from keep().
# A scout that needs 3 searches finishes in about 4 model calls (7 with
# notes). The window scout often searches again for what it has forgotten
# and stops with max_steps: more tokens, no answer.

episodes$full$answer
# EXPECTED OUTPUT: three lines, each a title and a DOI (or "not found").


# ==============================================================================
# STEP 4: COMPARE: VERIFIED SUCCESS, BILLED TOKENS, PEAK CONTEXT
# ==============================================================================

comparison <- episodes_df(episodes)
comparison$strategy <- names(episodes)
comparison$verified <- vapply(episodes, verified, logical(1))
comparison[, c("strategy", "verified", "stop_reason", "model_calls", "tool_calls",
               "tokens", "peak_context", "cost")]
# EXPECTED OUTPUT: one row per strategy.
#   verified      TRUE when all three DOIs are in the answer (graded in code)
#   tokens        billed tokens over the episode (input + cached + output),
#                 including the summary calls of ctx_compact()
#   peak_context  the largest context sent in one call
# Read "tokens" and "peak_context" separately: a strategy can lower the peak
# (what fits in the window) while billing more in total (summary calls).
# With one trial per strategy, differences are anecdotes (see Exercise 1).

# Step by step: what was sent (context_tokens) versus what the full
# conversation held (conversation_tokens), and where the tokens came from.
context_report(episodes$full)
# EXPECTED OUTPUT: one row per model call with context_tokens, billed input,
# cache_read, output, cost, and the split of the conversation into system,
# task, assistant and tool tokens. tool_tokens dominates.

reports <- do.call(rbind, lapply(names(episodes), function(s) {
  cbind(strategy = s, context_report(episodes[[s]]))
}))

ggplot(reports, aes(x = call, y = context_tokens, colour = strategy)) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "Context sent at each model call, by strategy",
       subtitle = "Same task, same model, same budget",
       x = "Model call", y = "Estimated tokens sent", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
# EXPECTED OUTPUT: the "full" line climbs at every call; "window" and "notes"
# stay low (a window that forgot a result may go on for many calls);
# "compact" climbs until the trigger (2,500 tokens), then drops after a summary.

# The cost of the summaries written by ctx_compact():
attr(context_report(episodes$compact), "context_steps")
# EXPECTED OUTPUT: one row per summary (step, model, tokens, cost), or NULL if
# the conversation never crossed trigger_tokens.

# What the notes agent wrote down (read from the episode, so it works offline):
note_steps <- episode_steps(episodes$notes)
note_steps[note_steps$name %in% c("note_write", "note_read"), c("step", "name", "detail")]
# EXPECTED OUTPUT: note_write calls with titles and DOIs, if the model used
# its notes. A notes agent that never writes notes behaves like a window.


# ==============================================================================
# STEP 5: TRIM TOOL OUTPUTS IN CODE (filter_output)
# ==============================================================================

# Tool outputs are usually the largest part of the context. Deciding in CODE
# what the model needs is cheaper and more reliable than asking the model to
# ignore the rest (a CS2680 principle: move deterministic decisions into code).

trimmed <- filter_output(extraversion, fields = c("doc_id", "subtopic", "text"),
                         max_items = 3, max_chars_per_field = 60)
trimmed
attr(trimmed, "note")
# EXPECTED OUTPUT: 3 rows, 3 columns, texts cut at 60 characters ending in
# "...", and the note "3 of 6 columns kept; first 3 of 120 rows shown".

trimmed_text <- filter_output(extraversion, fields = c("doc_id", "subtopic", "text"),
                              max_items = 3, max_chars_per_field = 60, as_text = TRUE)
cat(trimmed_text)
c(full = count_tokens(extraversion), trimmed = count_tokens(trimmed_text))
# EXPECTED OUTPUT: the text the model would read, ending with the note in
# brackets. In the live run of November 22, 2026: 7,918 versus 93 tokens.

# Logs and error messages: the first and last lines are where the information is
fit_log <- paste(c("Fitting model", paste("iteration", 1:300),
                   "Warning: convergence not reached"), collapse = "\n")
cat(filter_output(fit_log, max_items = 3), "\n")
# EXPECTED OUTPUT: 3 first lines, "[... 296 lines removed ...]", 3 last lines
# (including the warning).

# A lean literature tool: same search, only the fields needed to pick a DOI
lean_openalex <- define_tool(
  function(query) {
    hits <- search_openalex(query, n = 10)
    filter_output(hits, fields = c("title", "year", "source", "doi"), max_items = 5,
                  max_chars_per_field = 150, as_text = TRUE)
  },
  description = paste("Search OpenAlex for scholarly works. Returns title, year,",
                      "journal and DOI of the top 5 matches."),
  args = list(query = arg_string("Key words of the title, for example 'entropy fit indices'")),
  name = "search_openalex_lean")

lean_scout <- agent("scout_lean", scout_instructions, model = SCOUT_MODEL,
                    tools = list(lean_openalex))
episodes$lean <- keep("act05_scout_lean", run(lean_scout, lit_task, budget = lit_budget))

comparison <- episodes_df(episodes)
comparison$strategy <- names(episodes)
comparison$verified <- vapply(episodes, verified, logical(1))
comparison[, c("strategy", "verified", "model_calls", "tokens", "peak_context", "cost")]
# EXPECTED OUTPUT: a new row "lean" (full context, lean tool). Compare it
# with "full": trimming at the source often saves more than any context
# strategy, and loses nothing the task needs (check "verified").


# ==============================================================================
# STEP 6: OFFLOAD BIG RESULTS, RECALL ONLY WHAT IS NEEDED
# ==============================================================================

# offload() stores an object OUTSIDE the context (in a session folder) and
# returns a short handle with a preview. recall() reads rows by number or by
# pattern. This is "just-in-time retrieval": the data stay available without
# filling the context.

h <- offload(extraversion, name = "extraversion_texts")
h
count_tokens(h)
# EXPECTED OUTPUT: 'Stored outside the context as handle "extraversion_texts":
# data frame, 120 rows x 6 columns', a 3-row preview and how to recall it.
# Computed offline in this environment: 217 tokens instead of 7,918.

leaders <- recall(h, pattern = "leader")
attr(leaders, "note")
leaders[, c("doc_id", "subtopic")]
recall("extraversion_texts", rows = "1-3")[, c("doc_id", "subtopic")]
# EXPECTED OUTPUT: "2 of 120 items selected" and the matching rows (both
# Assertive); then rows 1 to 3.

# The agent's side: offload_tools() gives it a "recall" tool.
recall_tools <- offload_tools()
out <- call_tool("recall", recall_tools,
                 arguments = list(handle = "extraversion_texts", pattern = "leader"))
cat(out$output)
# EXPECTED OUTPUT: the two matching rows wrapped in <tool_output ...
# trust="untrusted"> tags, and the note "[2 of 120 items selected]".

# Live: a search tool that stores 50 records and returns only the handle
search_and_store <- define_tool(
  function(query) offload(search_openalex(query, n = 50)),
  description = paste("Search OpenAlex for up to 50 works and store them outside the",
                      "conversation. Returns a handle; read parts with the recall tool."),
  args = list(query = arg_string("Key words of the topic")),
  name = "search_and_store")
librarian <- agent("offload_scout",
  paste("Find works with search_and_store, then use recall with a pattern to read only",
        "the records you need. Report titles and DOIs."),
  model = SCOUT_MODEL, tools = c(list(search_and_store), offload_tools()))
ep_offload <- keep("act05_offload",
  run(librarian, "Which 2020 articles on 'entropy fit indices' are there? Give titles and DOIs.",
      budget = lit_budget))
episode_steps(ep_offload)[, c("step", "type", "name", "context_tokens", "detail")]
# EXPECTED OUTPUT: a search_and_store call, then one or more recall calls with
# a pattern; context_tokens stay small although 50 records were retrieved.
# Recorded run (November 22, 2026): 5 steps (search_and_store, then recall
# with pattern "2020"); context_tokens 140, 707 and 2776 at the 3 model calls.


# ==============================================================================
# STEP 7: SELECT ONLY THE RELEVANT PASSAGES (select_relevant)
# ==============================================================================

# A paper is long; a question needs two or three paragraphs. select_relevant()
# embeds the passages and the query and keeps the k most similar ones.
# The draft below is SYNTHETIC (a test fixture shipped with the package):
# its authors, numbers and references are invented.
draft_file <- system.file("extdata", "synthetic_methods_draft.md", package = "researchAgentR")
draft <- read_documents(draft_file)
draft[, c("section", "paragraph")]
count_tokens(paste(draft$text, collapse = "\n\n"))
# EXPECTED OUTPUT: 11 paragraphs in Introduction, Method, Results, Discussion
# and References; about 350 tokens in total (computed offline: 347).

# Live, with a local embedding model (Ollama; the text never leaves your computer)
best <- keep("act05_select_relevant",
  select_relevant(draft$text, "Which statistical test was reported, and what was the result?",
                  k = 2, model = "ollama:nomic-embed-text"))
best
# EXPECTED OUTPUT: 2 rows (index, score, text), best first; the Results
# paragraph with the t test should be among them.
# Recorded run (November 22, 2026): index 6 (the Results paragraph, score
# 0.7082), then index 2 (score 0.7005).

# What select_relevant() computes, offline, with the shipped MiniLM embeddings:
# cosine similarity between a query vector and every passage, then the top k.
data(minilm_embeddings)
E <- minilm_embeddings$texts$extraversion
query_vector <- minilm_embeddings$labels$extraversion["Energetic", ]   # an embedded description
scores <- as.vector(cosine_similarity(E, query_vector))
top <- order(scores, decreasing = TRUE)[1:3]
data.frame(index = top, score = round(scores[top], 4), subtopic = extraversion$subtopic[top])
# EXPECTED OUTPUT: 3 passages, all Energetic (computed offline with the
# shipped embeddings: rows 82, 34 and 106, scores about 0.55).


# ==============================================================================
# STEP 8: COMPRESS WITH A MODEL, AND CHECK WHAT WAS LOST (compress_text)
# ==============================================================================

# A summary is DERIVED text written by a model: it can omit or distort.
# compress_text() asks for a short summary that keeps DOIs, numbers and quotes
# verbatim, then checks IN CODE which of them are missing ("lost").
draft_text <- paste(draft$text, collapse = "\n\n")
short <- keep("act05_compress", compress_text(draft_text, max_tokens = 60))
short
attr(short, "lost")
c(source_tokens = attr(short, "source_tokens"), summary_tokens = count_tokens(short))
attr(short, "cost")
# EXPECTED OUTPUT: "<derived text: summary by groq:openai/gpt-oss-20b; check
# against the source before quoting>", the summary, then a data frame of lost
# facts (type "doi", "number" or "quote", and the value). With 60 tokens for
# a 347-token text, some numbers (years, page ranges) will be lost. Ask: were
# the LOST facts ones the task needed?
# Recorded run (November 22, 2026): 3 lost facts, all of type "number" (11,
# 31, 40); source_tokens 347, summary_tokens 139; cost 9.54e-05.


# ==============================================================================
# STEP 9: SUB-AGENTS WITH A FRESH CONTEXT (subagent)
# ==============================================================================

# A sub-agent runs its own episode from an EMPTY conversation, sees only the
# subtask the lead wrote, and returns a short summary. The lead's context
# holds 60 words instead of a whole search. The sub-episode's cost is added to
# the lead's ledger, and its id is linked from the tool step.
doi_finder <- agent("doi_finder",
  paste("Find the DOI of the one article described. Search Crossref, check title,",
        "journal and year, and report the DOI with the matched title."),
  model = SCOUT_MODEL, tools = list(tool_crossref(n = 5)),
  description = "Finds the DOI of one article from its title, journal and year.")

finder_tool <- subagent(doi_finder, summary_words = 60,
                        budget = budget(max_steps = 5, max_cost = 0.01),
                        save_dir = file.path("results", "sub_episodes"))
finder_tool
# EXPECTED OUTPUT: <agent_tool> doi_finder, its description (ending "It starts
# with no context: describe the subtask completely.") and one argument, task.

lead <- agent("lead",
  paste("You coordinate. For each article in the task, call doi_finder once, with the",
        "full title, journal and year. Then list each title with its DOI. Never guess a DOI."),
  model = SCOUT_MODEL, tools = list(finder_tool))
episodes$subagent <- keep("act05_lead_subagent", run(lead, lit_task, budget = lit_budget))
check_network(episodes)

episode_steps(episodes$subagent)[, c("step", "type", "name", "context_tokens", "cost")]
# EXPECTED OUTPUT: model steps of the lead and three doi_finder tool steps;
# each tool step's cost is the whole sub-episode's cost.
# Recorded run (November 22, 2026): 7 steps (4 model, 3 doi_finder); the
# doi_finder costs were 0.000156075, 0.000166425 and 0.000161475.

# The sub-episodes themselves (saved as JSON):
episodes_df(file.path("results", "sub_episodes"))[, c("agent", "stop_reason", "model_calls",
                                                      "tokens", "peak_context", "cost")]

comparison <- episodes_df(episodes)
comparison$strategy <- names(episodes)
comparison$verified <- vapply(episodes, verified, logical(1))
comparison[, c("strategy", "verified", "model_calls", "tool_calls", "tokens",
               "peak_context", "cost")]
# EXPECTED OUTPUT: the "subagent" row has the smallest peak_context of the
# lead. Its "tokens" counts only the LEAD's own calls; the sub-agents' tokens
# are in their own episodes, and their cost is inside "cost". Sub-agents
# lower the context, not necessarily the bill: more agents, more calls.


# ==============================================================================
# STEP 10: SKILLS AS PROGRESSIVE DISCLOSURE (list_skills, use_skills)
# ==============================================================================

# A skill is a folder with a SKILL.md file (name, description, instructions)
# and optional reference files (open Agent Skills format,
# https://agentskills.io/specification). The agent always sees each skill's
# NAME and DESCRIPTION (a line or two), loads the instructions with load_skill
# only when a task needs them, and reads bundled files with read_skill_file
# only when the instructions point to them. Context is spent on demand.

skills_dir <- system.file("extdata", "skills", package = "researchAgentR")
skills <- list_skills(skills_dir)
skills[, c("name", "valid", "n_files", "scripts")]
skills$description
# EXPECTED OUTPUT: one skill, summarizing-methods, valid, 2 files, 0 scripts,
# and its description ("Summarizes the methods section of an empirical
# paper ... Use when the user asks for a methods summary ...").

helper <- agent("methods_helper", "Help researchers with their papers.", model = SCOUT_MODEL)
helper_skilled <- use_skills(helper, skills_dir)
helper_skilled
cat(helper_skilled$instructions)
c(without_skills = count_tokens(helper$instructions),
  with_skills = count_tokens(helper_skilled$instructions))
# EXPECTED OUTPUT: tools load_skill and read_skill_file; the instructions end
# with "Available skills:" and one line per skill. Computed offline in this
# environment: 9 versus 152 tokens. The full instructions cost tokens only
# when loaded:
loaded <- call_tool("load_skill", helper_skilled$tools,
                    arguments = list(name = "summarizing-methods"))
cat(loaded$output)
count_tokens(loaded$output)
# EXPECTED OUTPUT: the five fields to report, the rule "never infer a value",
# an example, and "Files in this skill: references/checklist.md" (computed
# offline: 216 tokens).

# Live: the agent decides to load the skill when the task matches it
methods_text <- paste(draft$text[draft$section %in% "Method"], collapse = "\n\n")
ep_skill <- keep("act05_skill_run",
  run(helper_skilled, paste("Summarize the methods of this draft:\n\n", methods_text),
      budget = budget(max_steps = 5, max_cost = 0.01)))
episode_steps(ep_skill)[, c("step", "type", "name", "detail")]
ep_skill$answer
# EXPECTED OUTPUT: a load_skill call before the answer, then the five fields
# (Design, Sample: 30 simulated respondents, Measures: 12 items on a
# five-point scale, Analysis, Preregistration: not reported). If no load_skill
# call appears, the description did not trigger the skill: see Exercise 5.


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. ctx_window(keep_last = 1): does the scout still report all three DOIs?
# 2. ctx_compact(trigger_tokens = 1200): how many "context" steps, at what cost?
# 3. search_openalex(query, n = 3) in search_tool instead of n = 8: tokens
#    versus verified success.
# 4. ctx_window(keep_last = 3): does the window scout finish now?
# 5. subagent(doi_finder, summary_words = 20): what does the lead lose?


# ==============================================================================
# EXERCISES (solutions in solutions/05_solutions.R)
# ==============================================================================

# Exercise 1. One trial per strategy is an anecdote. Turn the literature task
#   into a task_set() with grader_doi(), run 3 trials for the full, window and
#   notes scouts with run_eval(), and compare success rate, pass^3 and cost
#   per success with eval_summary(). Which strategy would you report?
#
# Exercise 2. A small "lost in the middle" test. Build a context of 60
#   extraversion texts and hide one sentence ("The participant code is
#   K7-ALPHA.") at the beginning, in the middle, or at the end. Ask the model
#   for the code with ask(), 3 times per position, and count correct answers.
#
# Exercise 3. Write a lean Crossref tool (title, year, DOI; at most 3 rows)
#   with define_tool() and filter_output(), give it to a scout, and compare
#   billed tokens and verified success with the scout that uses tool_crossref().
#
# Exercise 4. Offline. For the toy conversation of Step 1, find the smallest
#   keep_last for ctx_window() at which the FIRST tool result (tool_call_id
#   "call_1") is still sent at the last call. What does this say about windows
#   for tasks whose answer is found in an early step?
#
# Exercise 5. Write a second skill by hand (a folder "extracting-reliability"
#   with a SKILL.md that says how to report reliability coefficients), check it
#   with list_skills(), give the agent both skills, and confirm with
#   episode_steps() that it loads the right one for a reliability question.


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - "429 Too Many Requests" or long pauses: the 8,000 tokens per minute limit.
#   The package retries with backoff; to avoid it, use lean tools (Step 5),
#   smaller n, or run the strategies a minute apart. Cached runs never hit it.
# - "max_tokens" as stop_reason: the budget stopped the episode before an
#   answer. Report it as a failure; do not raise the budget only for the
#   strategy you like.
# - The notes scout never calls note_write: ctx_notes() then behaves like a
#   window and forgets early DOIs. Say in the instructions what to write down.
# - Notes from an earlier run appear in a new run: the notes file persists.
#   Delete it (unlink(notes_file)) before a fresh run, as Step 3 does.
# - ctx_compact() warns "the summary call failed": it fell back to a window
#   for that step. The episode continues, but early facts may be gone.
# - select_relevant() fails with a connection error: Ollama is not running, or
#   nomic-embed-text is not pulled (in a terminal: ollama pull nomic-embed-text).
# - compress_text() reports many "lost" facts: max_tokens is too small for the
#   text, or the model paraphrased numbers. A lost fact is a warning, not proof
#   of an error; check the source.
# - use_skills() stops with "No valid skills found": the folder does not
#   contain skill folders with a valid SKILL.md (see list_skills()).
# - keep() returns an old result after you changed the agent: keep() does not
#   know the code changed. Use keep(..., refresh = TRUE) or a new name.
# - stop_reason "api_error: ..." (no network, proxy, expired key): keep() has
#   SAVED that failed episode, and will load it again next time.
#   check_network() lists them; re-run each with keep(..., refresh = TRUE).
# - Identical runs give identical results: with use_cache("auto"), the same
#   request is answered from the cache, so a second "trial" replays the first.
#   Switch the cache off for repeated trials and comparisons (as in Step 3 and
#   Exercise 1) and save the results with keep() instead.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. Why does billed input grow faster than the conversation itself? (Each
#    call pays again for everything before it.) Which part grows: system,
#    task, assistant or tool tokens? (context_report())
#
# 2. Peak context and billed tokens are different quantities. Which one
#    matters for the free tier's tokens-per-minute limit, which one for a
#    model with a small window (for example a local model with 8 GB of RAM)?
#
# 3. ctx_compact() spends tokens to save tokens. When does a summary pay for
#    itself? What is the risk of summarizing tool outputs that contain text
#    from web pages? (The summary is wrapped as untrusted data for this reason.)
#
# 4. "Lost in the Middle": where in the context does each strategy put the
#    information the final answer needs? Is a shorter context always better?
#
# 5. filter_output() and offload() decide in CODE what the model sees.
#    compress_text() and ctx_compact() let a MODEL decide. Which decisions in
#    your own research workflow could move from the model into code?
#
# 6. Sub-agents and skills both keep the lead's context clean. A sub-agent
#    costs a whole extra episode; a skill costs a load_skill call. When is
#    each one worth it?
#
# 7. A strategy that saves 60% of the tokens and fails one task in ten: would
#    you use it for a systematic review? For a first scan of a new field?
