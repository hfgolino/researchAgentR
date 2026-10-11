# Activity 01: Lens, Watch an Agent Work
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: see what a language model call and an agent run really consist of:
# every step, its tokens, its cost at list price and its latency.
# OVERVIEW: first one model call (no agent): what goes in, what comes out,
# what it costs. Then an agent that searches the web with Groq's built-in
# browser_search tool answers a research question, and every part of the
# run (the "episode") is inspected: printed, tabulated, plotted, opened as an
# HTML page, saved as JSON, and compared with a second run on a smaller model.
# The idea of starting a course by WATCHING an agent before building one
# (the "Lens" assignment) comes from Harvard CS2680, Modern AI Systems:
# Agents and System Optimizations (Juncheng Yang, Fall 2026,
# https://cs2680.seas.harvard.edu). The code here is original.
#
# researchAgentR functions used here:
#   chat(), ask()                    one model call (full response, or text only)
#   count_tokens()                   estimate tokens BEFORE sending
#   builtin_tool()                   a tool that runs on the provider's servers
#   agent(), run(), budget()         an agent and one episode (details in Activity 02)
#   episode_steps()                  one row per step of an episode
#   view_episode(), view_episodes()  self-contained HTML trajectory pages
#   plot_episode()                   tokens, cost or context per step (for slides)
#   save_episode(), read_episode()   episodes as JSON files
#   episodes_df()                    one row per episode, to compare runs
#   reliability_curve()              why long chains of steps fail
# TIME: about 60 minutes.  COST: free tier (Groq). One OPTIONAL paid line.
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

use_cache("auto")      # first run calls the APIs and saves; later runs replay
set_groq_key()         # reads GROQ_API_KEY from ~/.Renviron (see Activity 00)

# Free-tier limits are per minute and per day. If you see 429 errors, slow
# the requests down (for example to 20 per minute):
# options(researchAgentR.rpm.groq = 20)


# ==============================================================================
# STEP 1: ONE MODEL CALL WITH chat()
# ==============================================================================

# A language model reads a sequence of tokens (pieces of words) and writes
# new tokens, one at a time. chat() sends a message to any provider and
# returns the reply together with what it cost.
question <- "In two sentences, what is the difference between a chatbot and an LLM agent?"

r <- chat(question, model = "groq:openai/gpt-oss-20b")
r

# EXPECTED OUTPUT: "<llm_response> groq:openai/gpt-oss-20b", possibly a short
# "Reasoning:" line (gpt-oss models think before answering), the two-sentence
# answer, and a line "Tokens: N in (0 cached), M out | cost $... | ... s".

# The response is a list. The parts every activity uses:
r$content       # the answer text
r$usage         # tokens: input (not cached), cache_read, cache_write, output, reasoning
r$cost          # US dollars at LIST price (see STEP 2)
r$latency       # seconds the request took
r$model         # exactly which model answered
r$cached        # TRUE when the response was replayed from the cache

# EXPECTED OUTPUT: the text; a list with five token counts (reasoning tokens
# are part of output); a small number (dollars); a number of seconds; the model
# string; FALSE on the first run and TRUE on later runs (replayed from disk).

# ask() is the shortcut when only the text matters. The full response is
# still attached, so nothing is lost:
answer <- ask("Name three threats to internal validity, one line each.",
              model = "groq:openai/gpt-oss-20b")
cat(answer)
attr(answer, "response")$usage

# A system prompt sets the role and the rules for the whole conversation:
r_sys <- chat(question, model = "groq:openai/gpt-oss-20b",
              system = "You answer for an audience of psychology graduate students.",
              max_tokens = 1000, effort = "low")
r_sys$usage$output

# EXPECTED OUTPUT: three numbered threats; the usage list of that call; the
# number of output tokens of the system-prompt call (with effort = "low" the
# model usually spends fewer reasoning tokens).


# ==============================================================================
# STEP 2: TOKENS AND THE "SHADOW COST" OF A FREE TIER
# ==============================================================================

# Before sending a long text, estimate its tokens (about 4 characters each).
# Agents resend the whole conversation at every step, so this adds up fast.
count_tokens(question)
count_tokens(c(short = "Exploratory graph analysis.",
               long = strrep("A long methods section. ", 200)))

# The provider counts tokens with its own tokenizer and adds formatting and
# instructions, so its count for the same message differs from the estimate:
r$usage$input + r$usage$cache_read      # tokens the provider counted as input

# Groq's free tier bills nothing, but the ledger records every call at LIST
# price: the "shadow cost". It answers "what would this cost at scale, or on
# a paid plan?" and makes runs on different providers comparable.
# Recompute the cost of the first call by hand from the registry:
price <- model_registry("groq")
price <- price[price$model == "openai/gpt-oss-20b", c("input", "cached_input", "output")]
price                                   # US dollars per MILLION tokens

by_hand <- (r$usage$input * price$input +
            r$usage$cache_read * price$cached_input +
            r$usage$output * price$output) / 1e6
c(by_hand = by_hand, ledger = r$cost)

# EXPECTED OUTPUT: two equal numbers on the first (live) run. On a replayed
# run, ledger is 0: a response read from the cache was not billed, and the
# ledger says so honestly. Worked example of the formula: 1,000 input tokens
# at $0.075 per million plus 500 output tokens at $0.30 per million is
# (1000 * 0.075 + 500 * 0.30) / 1e6 = $0.000225.
(1000 * 0.075 + 500 * 0.30) / 1e6


# ==============================================================================
# STEP 3: THE SAME QUESTION, ANOTHER PROVIDER (OPTIONAL)
# ==============================================================================

# Only the model string changes; everything else stays the same.
# OPTIONAL, PAID (needs ANTHROPIC_API_KEY; a fraction of a cent):
r_claude <- chat(question, model = "anthropic:claude-haiku-5-5")
r_claude
# OPTIONAL, LOCAL AND FREE (needs Ollama and `ollama pull qwen3:4b`):
r_local <- chat(question, model = "ollama:qwen3:4b")
r_local

# EXPECTED OUTPUT: the same kind of response object. The local model reports
# cost $0 (your own computer) and usually a longer latency on a laptop.


# ==============================================================================
# STEP 4: AN AGENT THAT SEARCHES THE WEB (GROQ'S BUILT-IN browser_search)
# ==============================================================================

# An agent is a model in a loop: it may ask for a TOOL, the tool runs, the
# result is shown to the model, and so on until it answers. Groq's gpt-oss
# models have a built-in search tool that runs on GROQ's servers: R only
# passes it along with builtin_tool(), and Groq reports which searches ran.
#
# Two limits to know (Groq documentation, console.groq.com/docs/browser-search,
# checked October 11, 2026):
# - The documentation lists NO domain filter for browser_search: the
#   instructions can ASK for publisher pages, but nothing enforces it.
#   Activity 06 builds a search tool with real domain filtering.
# - "Browser search is not compatible with structured outputs" (a JSON
#   schema for the reply), so this agent answers in plain text.
# Groq recommends low reasoning effort with browser search (fewer tokens).

web_scout <- agent(
  name = "web_scout",
  instructions = paste(
    "You are a careful research assistant. Use browser search to answer.",
    "Prefer publisher pages, doi.org, PubMed and university pages over blogs.",
    "Give the URL of every page you rely on.",
    "If you cannot confirm a detail, say so; never guess a DOI."),
  model = "groq:openai/gpt-oss-120b",
  tools = list(builtin_tool("browser_search")),
  effort = "low"
)
web_scout

# EXPECTED OUTPUT: "<agent> web_scout | groq:openai/gpt-oss-120b", the
# shortened instructions, "Tools: browser_search", "Context: full".

research_question <- paste(
  "Which article introduced Exploratory Graph Analysis (EGA), by Golino and Epskamp?",
  "Give the title, journal, year and DOI, and the URL where you confirmed them.")

# keep() saves the whole episode under a name: the class demo replays it on
# any day (the agent's instructions include today's date, so the cache of
# raw requests would not match on another day). Change the name, or use
# refresh = TRUE, to run it again.
ep_120b <- keep("lens_web_scout_120b",
                run(web_scout, research_question,
                    budget = budget(max_steps = 4, max_cost = 0.05)))

ep_120b$stop_reason

# EXPECTED OUTPUT (first run): one line per step, such as
# "[1] model  N in  M out  $... ...s -> <start of the answer>", then
# "Stopped: final_answer". A replay prints only the keep() message.
# Then "final_answer".
#
# IMPORTANT: run() does not stop R when the API fails; it returns an episode
# whose stop reason starts with "api_error" (no key, no network, a 429 that
# persisted). keep() saves THAT episode too, and would replay the failure
# every time. If the stop reason starts with "api_error", fix the cause and
# run the line again with keep(..., refresh = TRUE).


# ==============================================================================
# STEP 5: INSPECT THE EPISODE (PRINT, STEPS, ANSWER)
# ==============================================================================

ep_120b

# EXPECTED OUTPUT: "<episode> <id> | web_scout | groq:openai/gpt-oss-120b",
# the task, "Stop reason: final_answer | 1 model calls, 0 tool calls (0
# errors)" or similar, the token, cost and time totals, and the answer.

ep_120b$stop_reason
cat(ep_120b$answer)

# One row per step: model calls and tool calls, with tokens, cost, latency.
episode_steps(ep_120b)

# EXPECTED OUTPUT: a data frame with columns step, type, name, input,
# cache_read, cache_write, output, context_tokens, cost, latency, is_error,
# cached, detail. A built-in tool runs INSIDE one model call on Groq's
# servers, so the searches do not appear as separate "tool" rows. They are
# listed in the model step instead:
searches <- unlist(lapply(ep_120b$steps, function(s) s$executed_tools))
searches
length(searches)

# EXPECTED OUTPUT: one entry per built-in tool call Groq ran (the type name
# Groq reports for each search), and how many there were. Compare the input tokens of
# the step with count_tokens(research_question): the difference is text the
# provider added (instructions, tool definitions, search results).
count_tokens(research_question)

# A NOTE ON COST: the ledger prices the model's tokens at list price. If the
# provider lists a separate fee per search on its pricing page
# (https://groq.com/pricing), researchAgentR 0.1.0 does NOT add it
# automatically. Add it by hand: look up the fee and multiply.
search_fee <- NA_real_   # US dollars per search: fill in from the pricing page
episode_cost <- sum(episode_steps(ep_120b)$cost, na.rm = TRUE)
episode_cost + length(searches) * search_fee

# EXPECTED OUTPUT: the token cost of the episode; NA until search_fee is set
# (an unknown cost stays NA, never zero).


# ==============================================================================
# STEP 6: SEE THE TRAJECTORY (HTML PAGE AND PLOTS)
# ==============================================================================

# One self-contained HTML page (works offline, can be e-mailed or archived
# with a paper): task, totals, a timeline of every step with its reasoning,
# tool calls and results, and the final answer. Text from the web is escaped,
# so a malicious page shows up as text and never runs.
view_episode(ep_120b, file = "lens_web_scout_120b.html")

# EXPECTED OUTPUT: the page opens in your browser; the path is returned
# invisibly. In RStudio Server or Posit Cloud, open the file from the Files
# pane (View in Web Browser).

# Figures for slides and papers (ggplot2 when installed):
plot_episode(ep_120b, what = "tokens")    # input, cached input, output per model call
plot_episode(ep_120b, what = "cost")      # cost per step, model and tool calls
plot_episode(ep_120b, what = "context")   # tokens SENT at each model call

# EXPECTED OUTPUT: three figures. With a single model call each shows one bar
# or one point; Activity 02 builds agents with several steps, where the
# context line climbs at every step because the whole conversation is resent.


# ==============================================================================
# STEP 7: SAVE AND READ EPISODES (JSON)
# ==============================================================================

# One JSON file per episode: readable by people, by other programs, and by
# ai_use_statement() at the end of the course.
f_120b <- save_episode(ep_120b, dir = "episodes")
f_120b

ep_back <- read_episode(f_120b)
ep_back
identical(ep_back$stop_reason, ep_120b$stop_reason)

# EXPECTED OUTPUT: the path "episodes/<id>.json"; the same printed summary as
# in STEP 5; TRUE.


# ==============================================================================
# STEP 8: COMPARE TWO RUNS (A SMALLER MODEL)
# ==============================================================================

# Same instructions, same tool, same question, same budget; only the model
# changes. gpt-oss-20b costs half as much per token at list price.
web_scout_20b <- agent(
  name = "web_scout_20b",
  instructions = web_scout$instructions,
  model = "groq:openai/gpt-oss-20b",
  tools = list(builtin_tool("browser_search")),
  effort = "low"
)

ep_20b <- keep("lens_web_scout_20b",
               run(web_scout_20b, research_question,
                   budget = budget(max_steps = 4, max_cost = 0.05)))
ep_20b
f_20b <- save_episode(ep_20b, dir = "episodes")

# One row per episode: the table to compare runs.
episodes_df(list(ep_120b, ep_20b))

# EXPECTED OUTPUT: two rows with columns id, agent, model, task, stop_reason,
# model_calls, tool_calls, tool_errors, tokens, cache_read, peak_context,
# cost, seconds, started. Compare tokens, cost and seconds; then READ both
# answers: a cheaper run is only better if its answer is still right.
cat(ep_20b$answer)

# The same table from the folder of saved files:
episodes_df("episodes")

# Both runs on one page, with a link from each row to its full trajectory:
view_episodes(list(ep_120b, ep_20b), file = "lens_compare.html")

# EXPECTED OUTPUT: a table with one row per saved episode (two, or more if
# you ran other variants), and an HTML page with the comparison table.


# ==============================================================================
# STEP 9: WHY LONG AGENT RUNS FAIL (RELIABILITY COMPOUNDS)
# ==============================================================================

# If each step succeeds with probability p, independently, a task that needs
# n steps succeeds with probability p^n. A step that works 95% of the time
# sounds reliable; ten of them in a row do not:
0.95^10

# EXPECTED OUTPUT: 0.5987369, about 60%. Fifty steps: 0.95^50 = 0.0769, about 8%.
0.95^50

reliability_curve(p = 0.95, steps = c(1, 5, 10, 20, 50))

# EXPECTED OUTPUT: a data frame with columns p, steps, success, label, with
# success 0.95, 0.774, 0.599, 0.358, 0.077 for 1, 5, 10, 20 and 50 steps.

reliability_curve(plot = TRUE)    # 99%, 95% and 90% per step, 1 to 50 steps

# EXPECTED OUTPUT: three curves falling from left to right, labeled at
# n = 50. Real failures are not independent (one wrong search result misleads
# every later step), which is why every step is recorded, and why Day 3
# measures pass^k (does the agent succeed EVERY time?) over repeated trials.
# This example follows the agent-reliability discussion in Harvard CS2680
# (https://cs2680.seas.harvard.edu).


# ==============================================================================
# TRY IT
# ==============================================================================

# - Ask STEP 1's question with effort = "high" and compare r$usage$reasoning.
# - Change research_question to a question about your own field (use a new
#   name in keep()) and read the URLs the agent cites.
# - Open lens_compare.html and find, for each run, the step with the largest
#   input. What made it large?


# ==============================================================================
# EXERCISES (solutions in solutions/01_solutions.R)
# ==============================================================================

# 1. Send the same question with effort = "low" and effort = "high" to
#    "groq:openai/gpt-oss-20b". Make a small data frame with the effort,
#    output tokens, reasoning tokens, cost and latency of each call.
# 2. For the first model step of ep_120b, recompute its cost by hand from
#    episode_steps() and model_registry(), as in STEP 2. (If the episode was
#    replayed from the response cache, the ledger cost is 0: explain why.)
# 3. Extract the DOI from ep_120b$answer with a regular expression and open
#    https://doi.org/<DOI> in the browser. Does it resolve to the article the
#    agent named? (Activity 03 automates this check.)
# 4. With reliability_curve(), find the largest number of steps for which an
#    agent with 99% per-step success still completes a task at least 80% of
#    the time.
# 5. Run web_scout on the research question with budget(max_steps = 1). What
#    is the stop reason, and why does a one-step budget not stop this agent
#    early?


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - "No key found for groq": GROQ_API_KEY is missing from ~/.Renviron, or R
#   was not restarted after saving it (Activity 00, STEP 1).
# - "401 ... Check the API key": the key is wrong or revoked. Create a new one.
# - "429 ... Rate limit or quota reached": free tiers limit requests per minute
#   and TOKENS per minute and per day. Searching pastes web pages into the
#   context, so one agent run can use thousands of tokens. Wait a minute and
#   run again, or slow down: options(researchAgentR.rpm.groq = 20).
# - Inside run(), an API failure does not crash R: the episode ends with stop
#   reason "api_error: <message>". Always check ep$stop_reason.
# - "404 ... Check the model name": the model was retired. Check
#   list_provider_models("groq") and change the model string.
# - "400" mentioning browser_search or tools: built-in search works only on
#   gpt-oss models on Groq, and not together with an output schema.
# - The optional lines: "No key found for anthropic" (no paid key; skip the
#   line) or "Failed to connect to localhost port 11434" (Ollama is
#   not running).
# - The answer did not change after editing the question: keep() loaded the
#   saved episode. Use a new name or keep(..., refresh = TRUE).
# - A failure that keeps coming back: an episode that ended with
#   "api_error: ..." was saved by keep() like any other. Fix the cause and
#   rerun with keep(..., refresh = TRUE).
# - episode_steps(ep) returns an empty data frame: the episode has no steps
#   (it stopped before the first model call). Check ep$stop_reason.
# - Costs of 0 on a second run are not an error: replayed responses are not
#   billed (cached = TRUE in episode_steps()).
# - Budget too small: with max_cost or max_tokens below what one call needs,
#   the run stops after the first call with stop reason "max_cost" or
#   "max_tokens" and no answer. Budgets are checked BEFORE each model call.
# - view_episode() does not open a browser in RStudio Server or Posit Cloud:
#   open the HTML file from the Files pane.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. The answer in STEP 5 reads well. What, exactly, in the episode lets you
#    check it, and what would you still have to check outside the episode?
# 2. browser_search has no domain filter. For a literature question, what can
#    go wrong when an agent relies on whatever pages a search engine returns?
# 3. Cost per CALL versus cost per EPISODE: why does the course always report
#    the cost of the whole episode, including failed and retried steps?
# 4. The smaller model was cheaper. Under what conditions is it the better
#    choice for a research task, and how would you know?
# 5. If each step of your planned agent succeeds 95% of the time, how many
#    steps would you allow it? What could raise the per-step success rate?
