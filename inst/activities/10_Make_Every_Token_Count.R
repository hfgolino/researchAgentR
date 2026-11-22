# Activity 10: Make Every Token Count (Opt)
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: Hold task success fixed while driving cost and latency down, under the
# limits of a free tier, and report the result honestly: baselines first,
# every technique measured with the evaluation harness, tuning costs kept in
# a separate ledger.
#
# OVERVIEW: On Groq's free tier, gpt-oss models are limited to about 8,000
# tokens per minute and 200,000 tokens per day (check your own limits at
# https://console.groq.com/settings/limits). An agent that pastes three web
# pages into its context hits the per-minute limit in one step. "Make every
# token count" is therefore not an optimization for later: it decides whether
# the agent runs at all.
#
# The rules (Harvard CS2680, Juncheng Yang, Fall 2026, "Cost-effective agents",
# https://cs2680.seas.harvard.edu/notes/cost-effective-agents):
#   1. Cost is measured per complete task EPISODE (retries and failed cheap
#      attempts included), and reported as cost per SUCCESSFUL task.
#   2. Success criteria and budgets are set BEFORE tuning.
#   3. Every technique is compared with simple baselines on the same tasks.
#   4. Offline optimization costs (all the runs in this activity) go in a
#      separate ledger from runtime costs.
#
# Techniques measured here, one at a time:
#   a smaller model for a stable role       limiting the tool menu
#   filtering tool output                   a stable prompt prefix (caching)
#   a cascade with a verifier               reuse with keep() and use_cache()
#   budgets with early stopping
#
# researchAgentR functions used here:
#   baselines()                  fixed cheap, fixed strong, cheap with retries
#   run_eval(), eval_summary()   measure every variant on the same tasks
#   paired_bootstrap()           is the cheaper variant really as good?
#   filter_output()              keep only the fields the model needs
#   cascade()                    cheap first, escalate when a check fails
#   keep(), use_cache()          reuse expensive results (with freshness checks)
#   cost_ledger()                runtime and offline costs, kept apart
#   plot_frontier()              the quality-cost frontier of all variants
#   model_registry()             list prices per million tokens
#
# TIME: about 75 minutes.  COST: free tier (Groq, Crossref, OpenAlex, NIH
# RePORTER). STEP 11 has OPTIONAL PAID lines (Anthropic, OpenAI), clearly
# labeled and commented out; together they cost a few cents.
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

use_cache("auto")      # every API response of the first live run is saved
set_groq_key()         # reads GROQ_API_KEY from ~/.Renviron
if (nzchar(Sys.getenv("OPENALEX_API_KEY"))) set_openalex_key()   # optional free key

dir.create("results", showWarnings = FALSE)
EP_DIR <- file.path("episodes", "activity10")

# Repeated trials must be independent: switch the response cache off while an
# evaluation runs (otherwise trial 2 replays trial 1 at cost 0), and keep()
# the finished results so later runs load them offline (see Activity 09).
fresh_trials <- function(expr) {
  old <- use_cache("off")
  on.exit(use_cache(old))
  expr
}

data(research_tasks)
dev     <- research_tasks[research_tasks$split == "dev", ]
heldout <- research_tasks[research_tasks$split == "heldout", ]

CHEAP  <- "groq:openai/gpt-oss-20b"
STRONG <- "groq:openai/gpt-oss-120b"
TRIALS <- 3


# ==============================================================================
# STEP 1: SUCCESS CRITERIA AND BUDGETS, BEFORE ANY TUNING
# ==============================================================================

# Write these down now. Changing them after seeing results is how a "cheaper"
# agent quietly becomes a worse one. The values are YOUR research decisions.
TARGET_SUCCESS <- 0.75     # a variant must keep at least this success rate on dev
MAX_COST_PER_SUCCESS <- 0.005   # US dollars at list price
B <- budget(max_steps = 6, max_tokens = 30000, max_cost = 0.01, max_seconds = 120)
B

# The free tier, as numbers to plan with (check your own account's limits)
FREE_TPM <- 8000           # tokens per minute, gpt-oss on Groq's free plan
FREE_TPD <- 200000         # tokens per day
# Groq does not count CACHED input tokens toward its rate limits and bills
# them at 50% of the input price (https://console.groq.com/docs/prompt-caching;
# re-check this page before class).


# ==============================================================================
# STEP 2: WHAT A TOKEN COSTS: LIST PRICES FROM THE REGISTRY
# ==============================================================================

prices <- model_registry()[, c("provider", "model", "input", "cached_input", "output",
                               "context_window", "source")]
prices[prices$provider %in% c("groq", "anthropic", "openai", "ollama"), ]
# EXPECTED OUTPUT: one row per model with prices in US dollars per million
# tokens (input, cached input, output), the context window and the pricing
# page. Free tiers are recorded at list price ("shadow cost"), so episodes can
# be compared across providers; local Ollama models cost 0.

# The cost of one episode with 5,000 input and 500 output tokens, of which
# `cached` input tokens were read from the provider's prompt cache:
episode_price <- function(model, input = 5000, output = 500, cached = 0) {
  m <- parse_model(model)
  r <- model_registry()
  row <- r[r$provider == m$provider & r$model == m$model, ]
  cached_price <- ifelse(is.na(row$cached_input), row$input, row$cached_input)
  ((input - cached) * row$input + cached * cached_price + output * row$output) / 1e6
}
data.frame(
  model = c(STRONG, CHEAP, "anthropic:claude-haiku-5-5", "openai:gpt-6-luna"),
  no_cache = sapply(c(STRONG, CHEAP, "anthropic:claude-haiku-5-5", "openai:gpt-6-luna"),
                    episode_price),
  cached_4000 = sapply(c(STRONG, CHEAP, "anthropic:claude-haiku-5-5", "openai:gpt-6-luna"),
                       episode_price, cached = 4000),
  row.names = NULL)
# EXPECTED OUTPUT (arithmetic on the registry prices of October 2026):
# gpt-oss-120b 0.00105 without cache and 0.00075 with 4,000 cached tokens;
# gpt-oss-20b half of that (0.000525 and 0.000375). The two paid rows
# (claude-haiku-5-5 and gpt-6-luna) both show 0.00075 and 0.00039. Check the
# paid rows against the pricing pages in the `source` column before relying
# on them.


# ==============================================================================
# STEP 3: BASELINES FIRST
# ==============================================================================

# One agent DESIGN, built for any model (instructions and tools fixed)
INSTRUCTIONS <- paste(
  "You answer bibliographic and research-funding questions.",
  "For a DOI or a journal: search Crossref with the exact title and check that the",
  "record's title, year and journal match before you answer.",
  "For an NIH project: use the NIH RePORTER tool and return the core project number.",
  "Reply with the answer only.")

ALL_TOOLS <- list(tool_crossref(n = 5), tool_openalex(n = 5), tool_unpaywall(),
                  tool_europepmc(n = 5), tool_arxiv(n = 5),
                  tool_nih_reporter(n = 5, fiscal_years = 2025), tool_nsf_awards(n = 5),
                  tool_grants_gov(n = 5))

make_finder <- function(model, tools = ALL_TOOLS, instructions = INSTRUCTIONS) {
  agent("finder", instructions, model = model, tools = tools,
        description = "Finds DOIs, journals and NIH project numbers.")
}

# baselines() runs the same design three ways on the same tasks and budget:
# fixed cheap model, fixed strong model, cheap model with one retry.
# 3 baselines x 4 tasks x 3 trials = 36 episodes.
base <- keep("opt10_baselines", fresh_trials(
  baselines(make_finder, dev, models = list(cheap = CHEAP, strong = STRONG),
            trials = TRIALS, retries = 1, budget = B,
            save_dir = file.path(EP_DIR, "baselines"))))

eval_summary(base, k = c(1, 3))
# EXPECTED OUTPUT: three rows ("cheap: groq:openai/gpt-oss-20b", "strong:
# groq:openai/gpt-oss-120b", "cheap + 1 retry: groq:openai/gpt-oss-20b") with
# success_rate, pass@k, pass^k, cost_per_attempt, cost_per_success, p50 and
# p95 seconds, errors, timeouts and ungraded trials.
# Recorded run (November 22, 2026): success_rate 0.6666667 (cheap; pass^3
# 0.50), 0.75 (strong) and 0.75 (cheap + 1 retry); cost_per_success
# 0.0004760344, 0.0008143667 and 0.0003438917, in the same order.

# How many such episodes fit in the free tier?
tokens_per_episode <- tapply(base$tokens, base$solver, mean, na.rm = TRUE)
data.frame(solver = names(tokens_per_episode),
           tokens_per_episode = round(tokens_per_episode),
           episodes_per_day = floor(FREE_TPD / tokens_per_episode),
           episodes_per_minute = round(FREE_TPM / tokens_per_episode, 2),
           row.names = NULL)
# EXPECTED OUTPUT: mean billed tokens per episode for each baseline, and how
# many episodes the daily and per-minute limits allow. This is the number
# every technique below tries to raise without lowering success.
# Recorded run (November 22, 2026): 4040 (cheap + 1 retry), 4081 (cheap) and
# 4345 (strong) tokens per episode: 49, 49 and 46 episodes per day, and
# 1.98, 1.96 and 1.84 per minute.


# ==============================================================================
# STEP 4: TECHNIQUE 1, A SMALLER MODEL FOR A STABLE ROLE
# ==============================================================================

# Finding a DOI from an exact title is a stable, narrow role. Does the small
# model do it as well as the large one? Compare them TASK BY TASK.
cheap_runs  <- base[base$solver == unique(base$solver)[1], ]
strong_runs <- base[base$solver == unique(base$solver)[2], ]
paired_bootstrap(strong_runs, cheap_runs)                    # success: cheap minus strong
pb <- paired_bootstrap(strong_runs, cheap_runs, metric = "cost")
signif(unlist(pb[c("a_mean", "b_mean", "estimate", "lower", "upper")]), 3)
# EXPECTED OUTPUT: the difference in success (cheap minus strong) with a 95%
# interval over tasks, and the difference in cost per trial. A cheaper model
# that is "not worse" needs an interval that excludes meaningful losses, not
# just a point estimate near zero.
# Recorded run (November 22, 2026): success: difference = -0.08333, 95% CI
# [-0.25, 0], 0 gains, 3 same, 1 regressions; cost: estimate -0.000293,
# lower -0.000467, upper -0.000120.


# ==============================================================================
# STEP 5: TECHNIQUE 2, LIMIT THE TOOL MENU
# ==============================================================================

# Every tool's name, description and JSON schema is sent with EVERY model call.
# Approximate tokens per tool (about 4 characters per token, no API call):
data.frame(tool = vapply(ALL_TOOLS, `[[`, character(1), "name"),
           schema_tokens = vapply(ALL_TOOLS, function(t) count_tokens(tool_schema(t)), numeric(1)))
sum(vapply(ALL_TOOLS, function(t) count_tokens(tool_schema(t)), numeric(1)))
# EXPECTED OUTPUT: one row per tool with its approximate schema size, and the
# total that every call pays before the task is even read: 8 tools, from 136
# (find_open_access) to 341 (nih_reporter) tokens, 1817 in total.

SMALL_MENU <- list(tool_crossref(n = 5), tool_nih_reporter(n = 5, fiscal_years = 2025))
res_menu <- keep("opt10_small_menu", fresh_trials(
  run_eval(make_finder(CHEAP, tools = SMALL_MENU), dev, trials = TRIALS, budget = B,
           save_dir = file.path(EP_DIR, "small_menu"), name = "cheap, 2 tools")))
eval_summary(list(cheap_runs, res_menu), k = 1)
# EXPECTED OUTPUT: two rows; compare success_rate, cost_per_success and
# p50_seconds. Fewer tools also means fewer wrong tool choices.
# Recorded run (November 22, 2026): the cheap baseline (8 tools) had
# success_rate 0.6666667, cost_per_success 0.0004760344 and p50_seconds
# 1.305935; "cheap, 2 tools" had 0.75, 0.0002350417 and 1.120783.


# ==============================================================================
# STEP 6: TECHNIQUE 3, FILTER TOOL OUTPUT
# ==============================================================================

# A search tool that returns abstracts, author lists and URLs for 5 records
# fills the context with text the model does not need to find a DOI.
# filter_output() keeps chosen fields, cuts long values and says what it removed.
lean_crossref <- define_tool(
  function(query) {
    hits <- search_crossref(query, n = 5)
    filter_output(hits, fields = c("title", "year", "source", "doi"),
                  max_items = 5, max_chars_per_field = 200, as_text = TRUE)
  },
  description = paste("Search Crossref for a paper by its title. Returns title, year,",
                      "journal (source) and DOI of up to 5 records."),
  args = list(query = arg_string("The exact title of the paper, or key words from it")),
  name = "search_crossref_lean")

res_filtered <- keep("opt10_filtered", fresh_trials(
  run_eval(make_finder(CHEAP, tools = list(lean_crossref,
                                           tool_nih_reporter(n = 5, fiscal_years = 2025))),
           dev, trials = TRIALS, budget = B,
           save_dir = file.path(EP_DIR, "filtered"), name = "cheap, 2 tools, filtered")))
eval_summary(list(res_menu, res_filtered), k = 1)
# EXPECTED OUTPUT: two rows; the filtered variant should use fewer tokens per
# trial (compare cost_per_attempt). Check that success did not drop: a
# filter that removes the field the model needs saves tokens and loses tasks.
# Recorded run (November 22, 2026): both rows had success_rate 0.75, and the
# filtered variant was not cheaper: cost_per_attempt 0.0001794625, against
# 0.0001762813 without the filter.


# ==============================================================================
# STEP 7: TECHNIQUE 4, A STABLE PROMPT PREFIX (PROVIDER-SIDE CACHING)
# ==============================================================================

# Providers cache the START of a request (system prompt, then tools, then
# messages). If the start is identical to an earlier request, those tokens are
# read from cache: cheaper and, on Groq, not counted toward rate limits.
# researchAgentR already puts the agent's instructions first and unchanged.
# The anti-pattern: something that changes on every run at the TOP of the
# instructions (a timestamp, a request id, the task itself).

unstable_solver <- function(task) {
  a <- make_finder(CHEAP, tools = SMALL_MENU,
                   instructions = paste0("Request ", format(Sys.time(), "%H:%M:%OS3"),
                                         ". ", INSTRUCTIONS))
  run(a, task$instruction, B, verbose = FALSE)
}
res_unstable <- keep("opt10_unstable_prefix", fresh_trials(
  run_eval(unstable_solver, dev, trials = TRIALS, budget = B,
           save_dir = file.path(EP_DIR, "unstable"), name = "cheap, 2 tools, unstable prefix")))

# Cached tokens per variant, from the saved episodes:
cache_share <- function(dir) {
  e <- episodes_df(dir)
  c(episodes = nrow(e), tokens = sum(e$tokens), cache_read = sum(e$cache_read),
    share_cached = round(sum(e$cache_read) / sum(e$tokens), 3))
}
rbind(stable = cache_share(file.path(EP_DIR, "small_menu")),
      unstable = cache_share(file.path(EP_DIR, "unstable")))
# EXPECTED OUTPUT: one row per variant with the number of episodes, billed
# tokens, tokens read from Groq's prompt cache, and their share. Whether and
# how much is cached depends on the provider and the model; record what you
# observe and the date.
# Recorded run (November 22, 2026): stable: 12 episodes, 24562 tokens, 8704
# read from the cache (share 0.354); unstable: 12 episodes, 29987 tokens, 0
# read from the cache.


# ==============================================================================
# STEP 8: TECHNIQUE 5, A CASCADE WITH A VERIFIER
# ==============================================================================

# Try the cheap model first; escalate to the strong one only when a check
# fails. The check runs at ANSWER time, so it may not use the answer key: it
# checks what can be checked without it. Place a verifier where the expected
# cost of an error is larger than the cost of checking.
runtime_check <- grader_code(function(answer, task) {
  a <- paste(answer, collapse = " ")
  if (grepl("DOI", task$instruction)) {
    doi <- regmatches(a, regexpr("10\\.[0-9]{4,9}/[^[:space:]]+", a))
    if (!length(doi)) return(FALSE)
    v <- verify_citations(doi, check_retraction = FALSE)   # does the DOI exist?
    return(v$status[1] == "verified")
  }
  if (grepl("NIH", task$instruction)) {
    return(grepl("[A-Z][0-9]{2}[A-Z]{2}[0-9]{6}", toupper(a)))   # looks like a project number
  }
  nzchar(trimws(a)) && nchar(a) < 200                            # a short, non-empty answer
}, label = "runtime check (no answer key)")

finder_cascade <- cascade(c(CHEAP, STRONG), verifier = runtime_check,
                          agent = function(model) make_finder(model, tools = SMALL_MENU),
                          budget = B)
res_cascade <- keep("opt10_cascade", fresh_trials(
  run_eval(finder_cascade, dev, trials = TRIALS, budget = B,
           save_dir = file.path(EP_DIR, "cascade"), name = "cascade 20b -> 120b")))
eval_summary(list(cheap_runs, strong_runs, res_cascade), k = c(1, 3))
# EXPECTED OUTPUT: three rows. The cascade's cost includes EVERY attempt
# (the failed cheap one too) and the verifier. A good cascade sits between
# the two fixed models on cost while keeping success close to the strong one.
# Recorded run (November 22, 2026): the cascade had success_rate 0.75 and
# cost_per_success 0.0002488750, below both baselines (cheap 0.0004760344,
# strong 0.0008143667), which carry 8 tools against the cascade's 2.

table(res_cascade$model)      # which model gave the final answer?
# EXPECTED OUTPUT: counts of trials answered by each model (escalations
# appear under the strong model).
# Recorded run (November 22, 2026): all 12 trials were answered by
# groq:openai/gpt-oss-20b (no escalation to the strong model).


# ==============================================================================
# STEP 9: TECHNIQUE 6, REUSE (WITH A FRESHNESS CHECK)
# ==============================================================================

# Exact-match reuse: an identical request is answered from the local cache.
q <- "In one sentence, what does pass^k measure for an agent?"
r1 <- chat(q, model = CHEAP)
r2 <- chat(q, model = CHEAP)
c(first_cached = r1$cached, second_cached = r2$cached, second_cost = r2$cost)
# EXPECTED OUTPUT: first_cached 0, second_cached 1 and second_cost 0: the
# second call is cached and costs 0 (c() prints TRUE and FALSE as 1 and 0).
# (first_cached is also 1 when you re-run the script: it was saved on the
# first run.)

# Reuse is safe only while the answer cannot change. Funding data, retractions
# and "latest" anything go stale: refresh kept results older than a week.
is_stale <- function(name, max_days = 7) {
  f <- file.path(cache_dir(), "kept", paste0(name, ".rds"))
  !file.exists(f) || difftime(Sys.time(), file.mtime(f), units = "days") > max_days
}
nih_ema <- keep("opt10_nih_ema_2025",
                search_nih_projects("ecological momentary assessment", fiscal_years = 2025),
                refresh = is_stale("opt10_nih_ema_2025", max_days = 7))
attr(nih_ema, "kept_at")
# EXPECTED OUTPUT: the time the search was run (a new time once it is older
# than 7 days). Note that the response cache (use_cache) has no expiry: a
# refreshed keep() inside use_cache("auto") can still replay an old response.
# Use use_cache("record") when you need fresh data.


# ==============================================================================
# STEP 10: TECHNIQUE 7, BUDGETS WITH EARLY STOPPING
# ==============================================================================

# A tight budget stops runaway episodes early. It also stops some episodes
# that would have succeeded: measure both effects.
B_tight <- budget(max_steps = 3, max_tokens = 8000, max_cost = 0.002, max_seconds = 60)
res_tight <- keep("opt10_tight_budget", fresh_trials(
  run_eval(make_finder(CHEAP, tools = SMALL_MENU), dev, trials = TRIALS, budget = B_tight,
           save_dir = file.path(EP_DIR, "tight"), name = "cheap, 2 tools, tight budget")))
table(res_tight$stop_reason, res_tight$pass, useNA = "ifany")
# EXPECTED OUTPUT: stop reasons by outcome. Trials stopped by max_steps or
# max_tokens with pass = FALSE are the price of the tighter budget.
# Recorded run (November 22, 2026): all 12 trials stopped with final_answer
# (9 TRUE, 3 FALSE); none was stopped by the tighter budget.


# ==============================================================================
# STEP 11: OPTIONAL PAID COMPARISON LINES (CLEARLY LABELED)
# ==============================================================================

# PAID, OPTIONAL. Each line below costs real money (a few cents for these
# 12 trials at the list prices in STEP 2). Uncomment only if you added
# credit and a key (ANTHROPIC_API_KEY or OPENAI_API_KEY in ~/.Renviron).
# set_anthropic_key()
# res_haiku <- keep("opt10_paid_haiku", fresh_trials(
#   run_eval(make_finder("anthropic:claude-haiku-5-5", tools = SMALL_MENU), dev,
#            trials = TRIALS, budget = B, save_dir = file.path(EP_DIR, "paid_haiku"),
#            name = "PAID claude-haiku-5-5, 2 tools")))
# set_openai_key()
# res_luna <- keep("opt10_paid_luna", fresh_trials(
#   run_eval(make_finder("openai:gpt-6-luna", tools = SMALL_MENU), dev,
#            trials = TRIALS, budget = B, save_dir = file.path(EP_DIR, "paid_luna"),
#            name = "PAID gpt-6-luna, 2 tools")))


# ==============================================================================
# STEP 12: ALL VARIANTS ON ONE FRONTIER, AND A LEADERBOARD
# ==============================================================================

variants <- list(base, res_menu, res_filtered, res_unstable, res_cascade, res_tight)
# if (exists("res_haiku")) variants <- c(variants, list(res_haiku))
# if (exists("res_luna"))  variants <- c(variants, list(res_luna))
summary_all <- eval_summary(variants, k = c(1, 3))
summary_all[, c("solver", "success_rate", "pass^3", "cost_per_success", "p50_seconds")]
plot_frontier(summary_all, log_cost = TRUE)
# EXPECTED OUTPUT: a table with one row per variant, and the quality-cost
# frontier (success rate against cost per attempted task, log scale) with the
# Pareto front marked.
# Recorded run (November 22, 2026): 8 rows; success_rate 0.75 for all but the
# cheap baseline (0.6666667, pass^3 0.50); cost_per_success from 0.0002350417
# ("cheap, 2 tools") to 0.0008143667 (strong baseline); ggplot2 also printed
# "`geom_line()`: Each group consists of only one observation."

# A leaderboard by Pareto LAYERS on three criteria: success (higher is
# better), cost per success and median time (lower is better). Layer 1 holds
# the variants no other variant beats on all three at once; remove them, and
# layer 2 is the front of what remains; and so on.
pareto_layers <- function(s, maximize = "success_rate",
                          minimize = c("cost_per_success", "p50_seconds")) {
  m <- cbind(-s[[maximize]], as.matrix(s[, minimize]))   # turn everything into "lower is better"
  layer <- rep(NA_integer_, nrow(s))
  left <- which(stats::complete.cases(m))
  current <- 1L
  while (length(left)) {
    dominated <- vapply(left, function(i) any(vapply(left, function(j)
      all(m[j, ] <= m[i, ]) && any(m[j, ] < m[i, ]), logical(1))), logical(1))
    layer[left[!dominated]] <- current
    left <- left[dominated]
    current <- current + 1L
  }
  layer
}
summary_all$layer <- pareto_layers(summary_all)
summary_all$meets_target <- summary_all$success_rate >= TARGET_SUCCESS &
  summary_all$cost_per_success <= MAX_COST_PER_SUCCESS
leaderboard <- summary_all[order(summary_all$layer, -summary_all$success_rate,
                                 summary_all$cost_per_success),
                           c("layer", "solver", "success_rate", "pass^3", "cost_per_success",
                             "p50_seconds", "meets_target")]
leaderboard
# EXPECTED OUTPUT: the variants sorted by layer (NA last: unknown cost or
# success), with a TRUE/FALSE column for the criteria set in STEP 1. Choose
# among variants that meet the targets, and write down why.
# Recorded run (November 22, 2026): layer 1 held three variants:
# "cheap, 2 tools", "cheap, 2 tools, filtered" and the cascade; layer 3 held
# the two fixed-model baselines (strong and cheap), and meets_target was
# FALSE only for the cheap baseline.


# ==============================================================================
# STEP 13: TWO LEDGERS: OFFLINE (TUNING) AND RUNTIME (THE CHOSEN AGENT)
# ==============================================================================

# Everything above was OFFLINE optimization work: it cost tokens to find the
# cheap agent. Report it, but separately from what the agent costs per use.
offline <- rbind(
  cost_ledger(base, kind = "offline", note = "baselines"),
  cost_ledger(res_menu, kind = "offline", note = "tool menu"),
  cost_ledger(res_filtered, kind = "offline", note = "filtered output"),
  cost_ledger(res_unstable, kind = "offline", note = "unstable prefix"),
  cost_ledger(res_cascade, kind = "offline", note = "cascade"),
  cost_ledger(res_tight, kind = "offline", note = "tight budget"))

# The runtime ledger: the CHOSEN configuration, run once on the held-out tasks.
CHOSEN <- make_finder(CHEAP, tools = list(lean_crossref,
                                          tool_nih_reporter(n = 5, fiscal_years = 2025)))
# (replace with the configuration your leaderboard supports; decide BEFORE running)
res_runtime <- keep("opt10_chosen_heldout", fresh_trials(
  run_eval(CHOSEN, heldout, trials = TRIALS, budget = B,
           save_dir = file.path(EP_DIR, "chosen_heldout"), name = "chosen (held-out)")))
runtime <- cost_ledger(res_runtime, kind = "runtime", note = "chosen configuration, held-out")

rbind(offline, runtime)
aggregate(cbind(cost, tokens) ~ kind + note, data = rbind(offline, runtime), FUN = sum)
eval_summary(res_runtime, k = c(1, 3))
# EXPECTED OUTPUT: the ledger printout with one line per kind (offline and
# runtime: rows, model cost, grading cost, tokens), a table of cost and tokens
# by kind and note, and the held-out summary of the chosen configuration.
# Recorded run (November 22, 2026): runtime 9 rows, model cost $0.00130,
# 15,044 tokens; offline 96 rows, model cost $0.02644, 278,923 tokens;
# held-out success_rate 0.6666667 and cost_per_success 0.0002161875.

saveRDS(list(summary = summary_all, leaderboard = leaderboard,
             ledger = rbind(offline, runtime), date = Sys.Date()),
        "results/activity10_opt.rds")


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Set max_chars_per_field = 60 in lean_crossref: what breaks first?
# 2. Put the task text at the TOP of the instructions in unstable_solver
#    instead of a timestamp: is the cache share different?
# 3. Swap the order of the cascade (strong first): why is that never cheaper?


# ==============================================================================
# EXERCISES (solutions in solutions/10_solutions.R)
# ==============================================================================

# 1. Using the registry, compute the cost of 1,000 screening calls of 900
#    input and 60 output tokens each with gpt-oss-20b, gpt-oss-120b and
#    claude-haiku-5-5, with and without 800 cached input tokens per call.
# 2. Build pareto_layers() leaderboards on CONSTRUCTED summaries (5 variants;
#    write the numbers yourself) and check by hand that layer 1 is correct.
# 3. A cascade's cheap model succeeds on 70% of tasks at $0.0005 per attempt;
#    the strong model on 90% at $0.0020. Assume the verifier catches every
#    cheap failure and costs nothing. Compute the cascade's expected cost per
#    attempted task and per success, and compare with the strong model alone.
# 4. Build an offline ledger from two constructed tuning runs and a runtime
#    ledger from one, combine them, and report the totals by kind.
# 5. (Live) Add ctx_window(keep_last = 4) to the chosen configuration and
#    measure whether it changes cost or success on dev.


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - 429 errors and long waits: the per-minute token limit, not a bug. Fewer and
#   leaner tools, smaller outputs and cached prefixes help; so do fewer trials.
# - "Daily limit reached": the 200K tokens per day are shared by every run of
#   that model on your key. Run the variants on different days, or use
#   different models for different roles (limits are per model).
# - All trials identical with cost 0: the response cache replayed them; use
#   fresh_trials() around run_eval() (see SETUP).
# - The cheaper variant "wins" because a task became ungraded (pass = NA):
#   compare the ungraded column before comparing success rates.
# - cost_per_success is NA: no trial passed, or a model has no price; add it
#   with registry_set().
# - A filter removed the field the model needed: success drops while tokens
#   fall. Always measure success and cost together.
# - The cascade verifier used the answer key: the cascade looks perfect in the
#   evaluation and fails in real use. A runtime verifier sees the task only.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. Why report cost per SUCCESSFUL task, and not cost per call?
# 2. Which roles in your own research pipeline are "stable" enough for a
#    small model (extraction, screening, routing)? Which are not?
# 3. Moving deterministic decisions into code: which checks in STEP 8 could
#    replace a model call entirely?
# 4. The offline ledger is often larger than the runtime ledger. Should a
#    paper report both? (CS2680: yes, separately.)
# 5. Free tiers shape research designs. What are the equity implications of
#    agents that only run well with paid models?
