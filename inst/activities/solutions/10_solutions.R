# Solutions: Activity 10, Make Every Token Count
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Exercises 1 to 4 run offline (no model, no key). Exercise 5 needs the
# objects of Activity 10 (INSTRUCTIONS, lean_crossref, CHEAP, dev, TRIALS, B,
# fresh_trials, EP_DIR, res_filtered) and a live run.

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")


# ==============================================================================
# EXERCISE 1: the cost of 1,000 screening calls, with and without caching
# ==============================================================================

call_price <- function(model, input, output, cached = 0) {
  m <- parse_model(model)
  r <- model_registry()
  row <- r[r$provider == m$provider & r$model == m$model, ]
  cached_price <- ifelse(is.na(row$cached_input), row$input, row$cached_input)
  ((input - cached) * row$input + cached * cached_price + output * row$output) / 1e6
}
models <- c("groq:openai/gpt-oss-20b", "groq:openai/gpt-oss-120b", "anthropic:claude-haiku-5-5")
data.frame(
  model = models,
  usd_1000_calls = 1000 * sapply(models, call_price, input = 900, output = 60),
  usd_1000_calls_800_cached = 1000 * sapply(models, call_price, input = 900, output = 60,
                                            cached = 800),
  row.names = NULL)
# gpt-oss-20b:      0.0855 and 0.0555 US dollars
# gpt-oss-120b:     0.1710 and 0.1110
# claude-haiku-5-5: 0.1200 and 0.0480
# (Anthropic also bills the FIRST write of a cached prefix at the cache_write
# price; with 1,000 calls sharing one prefix, that one write barely matters.
# On Groq's free tier the "cost" is a shadow cost, but cached tokens also do
# not count toward the rate limits, which is what matters there.)


# ==============================================================================
# EXERCISE 2: Pareto layers on CONSTRUCTED summaries
# ==============================================================================

# The same function as in the activity
pareto_layers <- function(s, maximize = "success_rate",
                          minimize = c("cost_per_success", "p50_seconds")) {
  m <- cbind(-s[[maximize]], as.matrix(s[, minimize]))
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

# CONSTRUCTED numbers, typed in for practice (not results of any run)
practice <- data.frame(
  solver           = c("A", "B", "C", "D", "E"),
  success_rate     = c(0.90, 0.90, 0.80, 0.70, 0.60),
  cost_per_success = c(0.0030, 0.0040, 0.0010, 0.0012, NA),
  p50_seconds      = c(12, 10, 8, 9, 5))
practice$layer <- pareto_layers(practice)
practice[order(practice$layer), ]
# By hand: A is not beaten by B (A is cheaper, B is faster) and not by C or D
# (both less successful): layer 1. B: nobody is at least as good on all three
# and better on one: layer 1. C: the cheapest and fastest of the rest, beaten
# by nobody: layer 1. D: C is more successful, cheaper and faster, so D is
# dominated: layer 2. E has an unknown cost: NA (never ranked as if it were free).


# ==============================================================================
# EXERCISE 3: the expected cost of a cascade (arithmetic)
# ==============================================================================

p_cheap <- 0.70; cost_cheap <- 0.0005
p_strong <- 0.90; cost_strong <- 0.0020

# Every task starts with the cheap model; the 30% it fails escalate.
cost_cascade <- cost_cheap + (1 - p_cheap) * cost_strong        # 0.0011 per attempted task

# (a) Optimistic: the strong model solves 90% of the cheap model's failures too
success_a <- p_cheap + (1 - p_cheap) * p_strong                 # 0.97
# (b) Realistic: the tasks the cheap model fails are the hard ones; suppose
#     every failure of the strong model is among them (it then solves 2 of 3)
success_b <- p_cheap + (p_strong - p_cheap)                     # 0.90

data.frame(
  design = c("strong model alone", "cascade (optimistic)", "cascade (realistic)"),
  success = c(p_strong, success_a, success_b),
  cost_per_attempt = c(cost_strong, cost_cascade, cost_cascade),
  cost_per_success = round(c(cost_strong / p_strong, cost_cascade / success_a,
                             cost_cascade / success_b), 6))
# strong alone: 0.002222 per success; cascade: 0.001134 (a) or 0.001222 (b).
# The cascade halves the cost per success here, but only because the verifier
# catches every cheap failure. A verifier that misses failures lets wrong
# cheap answers through: success falls, and the saving is not real.


# ==============================================================================
# EXERCISE 4: offline and runtime ledgers (CONSTRUCTED PRACTICE DATA)
# ==============================================================================

# Constructed eval-like results (typed in; not from any run)
tuning_v1 <- data.frame(solver = "v1", task_id = c("a", "b", "c"), trial = 1,
                        pass = c(TRUE, FALSE, TRUE), cost = c(0.0011, 0.0019, 0.0012),
                        grading_cost = c(0.0004, 0.0004, 0.0004), tokens = c(7300, 12100, 8000),
                        seconds = c(6, 11, 7), model = "groq:openai/gpt-oss-20b",
                        episode_id = c("p1", "p2", "p3"))
tuning_v2 <- transform(tuning_v1, solver = "v2", cost = c(0.0007, 0.0009, 0.0008),
                       tokens = c(4600, 6100, 5200), episode_id = c("p4", "p5", "p6"))
deployed  <- transform(tuning_v1, solver = "final", cost = c(0.0007, 0.0008, 0.0007),
                       grading_cost = 0, tokens = c(4500, 5300, 4700),
                       episode_id = c("p7", "p8", "p9"))

ledger <- rbind(cost_ledger(tuning_v1, kind = "offline", note = "prompt v1"),
                cost_ledger(tuning_v2, kind = "offline", note = "prompt v2"),
                cost_ledger(deployed, kind = "runtime", note = "final agent"))
ledger
aggregate(cbind(cost, grading_cost, tokens) ~ kind, data = ledger, FUN = sum)
# offline: model cost 0.0066, grading 0.0024, 43,300 tokens
# runtime: model cost 0.0022, grading 0, 14,500 tokens
# Building the agent cost three times as much as running it on these tasks.


# ==============================================================================
# EXERCISE 5: a sliding context window (LIVE RUN NEEDED)
# ==============================================================================

# Needs INSTRUCTIONS, lean_crossref, CHEAP, dev, TRIALS, B, fresh_trials(),
# EP_DIR and res_filtered from Activity 10.
windowed <- agent("finder_window", INSTRUCTIONS, model = CHEAP,
                  tools = list(lean_crossref, tool_nih_reporter(n = 5, fiscal_years = 2025)),
                  context = ctx_window(keep_last = 4))
res_window <- keep("opt10_ex5_window", fresh_trials(
  run_eval(windowed, dev, trials = TRIALS, budget = B,
           save_dir = file.path(EP_DIR, "ex5_window"), name = "filtered + window(4)")))
eval_summary(list(res_filtered, res_window), k = 1)
paired_bootstrap(res_filtered, res_window)
# These tasks take few steps, so a window of 4 turns rarely removes anything:
# expect little change. Windows pay off in long episodes (many tool calls).
