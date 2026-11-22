# Activity 02: Loop, Your First Tool and Your First Agent
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: turn ordinary R functions into tools, see exactly what the model sees
# and what happens when a call goes wrong, and run an agent loop with a
# budget, explicit stop reasons and a structured final answer.
# OVERVIEW: an agent is a loop. The MODEL PROPOSES (it asks for a tool, with
# arguments); the HARNESS EXECUTES (R checks the arguments, runs the function
# with a time limit, and shows the result to the model). This repeats until
# the model answers or a budget runs out. The activity builds three research
# tools (a sample-size calculator, a lookup in a small instrument catalog, an
# effect size from summary statistics), calls them by hand to see every kind
# of failure, then gives them to an agent. The loop itself is short enough to
# read in full (STEP 9).
#
# researchAgentR functions used here:
#   define_tool()                       an R function + a description -> a tool
#   arg_string(), arg_number(),         describe each argument for the model
#   arg_integer(), arg_boolean(),
#   arg_enum(), arg_array(), arg_object()
#   tool_schema()                       the JSON Schema the model reads
#   call_tool()                         run one tool call safely, by hand
#   agent(), run()                      an agent and one episode
#   budget()                            limits: steps, tokens, cost, time, errors
#   ctx_full()                          the default context strategy (send everything)
# TIME: about 75 minutes.  COST: free tier (Groq). One OPTIONAL paid line.
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

use_cache("auto")      # first run calls the APIs and saves; later runs replay
set_groq_key()         # reads GROQ_API_KEY from ~/.Renviron (see Activity 00)

# options(researchAgentR.rpm.groq = 20)   # uncomment if you hit 429 errors


# ==============================================================================
# STEP 1: A FIRST TOOL: SAMPLE SIZE FOR A t TEST
# ==============================================================================

# A tool is an ordinary R function. Language models are unreliable at
# arithmetic; R is not. Moving a deterministic computation into code is the
# first rule of building agents.
sample_size_t <- function(effect_size, power = 0.80, alpha = 0.05,
                          design = "two.sample", alternative = "two.sided") {
  res <- stats::power.t.test(delta = abs(effect_size), sd = 1, sig.level = alpha,
                             power = power, type = design, alternative = alternative)
  list(n = ceiling(res$n),
       n_is = if (design == "two.sample") "participants per group" else "participants (or pairs) in total",
       effect_size = effect_size, power = power, alpha = alpha,
       design = design, alternative = alternative)
}

sample_size_t(0.5)          # it works as plain R first

# define_tool() adds a description of the tool and of each argument. These
# texts are written FOR THE MODEL: say what the value means, give the units
# and an example. Arguments with required = FALSE use the function's default
# when the model leaves them out.
sample_size_tool <- define_tool(
  sample_size_t,
  description = paste(
    "Compute the sample size for a t test with stats::power.t.test.",
    "Use it whenever a sample size is needed; never compute sample sizes yourself."),
  args = list(
    effect_size = arg_number("Expected Cohen's d, for example 0.5"),
    power = arg_number("Desired power between 0 and 1 (default 0.80)", required = FALSE),
    alpha = arg_number("Significance level (default 0.05)", required = FALSE),
    design = arg_enum("Study design", values = c("two.sample", "one.sample", "paired"),
                      required = FALSE),
    alternative = arg_enum("Test direction", values = c("two.sided", "one.sided"),
                           required = FALSE)
  ),
  untrusted = FALSE     # a computation in your own R session, not text from outside
)
sample_size_tool

# EXPECTED OUTPUT: first the list from sample_size_t(0.5): n = 64 participants
# per group (stats::power.t.test gives 63.77, rounded up). Then
# "<agent_tool> sample_size_t", the description, and one line per argument
# with its type, "optional" where required = FALSE, and its description.


# ==============================================================================
# STEP 2: WHAT THE MODEL SEES: THE JSON SCHEMA
# ==============================================================================

# The model never sees your R code. It sees a name, a description and a JSON
# Schema of the arguments, and nothing else. This is the whole interface.
tool_schema(sample_size_tool)

# EXPECTED OUTPUT: JSON with "name", "description" and "parameters":
# type "object", one entry per argument under "properties" (effect_size and
# power with "type": "number"; design and alternative with "type": "string"
# and an "enum" list of allowed values), "required": ["effect_size"], and
# "additionalProperties": false (no invented arguments).


# ==============================================================================
# STEP 3: TWO MORE TOOLS: A LOOKUP AND A STRUCTURED INPUT
# ==============================================================================

# A small local catalog of instruments. It summarizes widely documented facts
# for teaching; check the license and the current version of any instrument
# before using it in a study (Activity 06 builds an agent that verifies
# instruments with citations).
instruments <- data.frame(
  construct = c("depression", "depression", "anxiety", "anxiety",
                "self-esteem", "big five", "big five"),
  instrument = c("PHQ-9", "BDI-II", "GAD-7", "STAI",
                 "Rosenberg Self-Esteem Scale", "Mini-IPIP", "BFI-2"),
  items = c(9L, 21L, 7L, 40L, 10L, 20L, 60L),
  response_scale = c("0-3", "0-3", "0-3", "1-4", "4-point agreement",
                     "5-point accuracy", "5-point agreement"),
  free_for_research = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
instruments

# The tool's output is also written for the model: when nothing matches, it
# says which constructs the catalog has, so the model can try again.
find_instrument <- function(constructs, max_items = 100L, free_only = FALSE) {
  hits <- instruments[tolower(instruments$construct) %in% tolower(constructs) &
                        instruments$items <= max_items, ]
  if (free_only) hits <- hits[hits$free_for_research, ]
  if (nrow(hits) == 0) {
    return(paste0("No instrument in the catalog matches. Constructs in the catalog: ",
                  paste(unique(instruments$construct), collapse = ", "), "."))
  }
  hits
}

instrument_tool <- define_tool(
  find_instrument,
  description = paste(
    "Look up validated instruments in the local catalog.",
    "Use it before naming any instrument; never name an instrument that it did not return."),
  args = list(
    constructs = arg_array("Constructs to look up, in lower case, for example [\"depression\", \"anxiety\"]",
                           items = arg_string("One construct")),
    max_items = arg_integer("Keep instruments with at most this many items", required = FALSE),
    free_only = arg_boolean("true to keep only instruments free for research use", required = FALSE)
  )
)

# An effect size from published summary statistics. Each group is an OBJECT
# with three fields, described with arg_object().
cohens_d_from_summary <- function(group1, group2) {
  pooled_sd <- sqrt(((group1$n - 1) * group1$sd^2 + (group2$n - 1) * group2$sd^2) /
                      (group1$n + group2$n - 2))
  list(d = round((group1$mean - group2$mean) / pooled_sd, 3),
       pooled_sd = round(pooled_sd, 3))
}

group_fields <- list(mean = arg_number("Group mean"),
                     sd = arg_number("Group standard deviation"),
                     n = arg_integer("Group sample size"))
d_tool <- define_tool(
  cohens_d_from_summary,
  description = "Compute Cohen's d (pooled SD) from the means, SDs and sizes of two groups.",
  args = list(group1 = arg_object("The first group (for example treatment)", fields = group_fields),
              group2 = arg_object("The second group (for example control)", fields = group_fields)),
  untrusted = FALSE
)

tool_schema(instrument_tool)
tool_schema(d_tool)

# EXPECTED OUTPUT: the catalog (7 rows). The instrument_tool schema has
# "constructs" of type "array" with "items": {"type": "string"}, an integer
# and a boolean argument, and only "constructs" in "required". The d_tool
# schema has two arguments of type "object", each with its own "properties"
# (mean, sd, n), "required" and "additionalProperties": false.

tools <- list(sample_size_tool, instrument_tool, d_tool)


# ==============================================================================
# STEP 4: CALL TOOLS BY HAND, AND BREAK THEM ON PURPOSE
# ==============================================================================

# call_tool() is exactly what the agent loop does when the model asks for a
# tool. A correct call:
res <- call_tool("sample_size_t", tools, arguments = list(effect_size = 0.4))
res$output        # the TEXT the model will read
res$value         # the R object the function returned
res$is_error

# EXPECTED OUTPUT: the output is the result as JSON text (n = 100 per group
# for d = 0.4: power.t.test gives 99.08, rounded up); value is the R list;
# is_error is FALSE.

call_tool("find_instrument", tools,
          arguments = list(constructs = c("depression", "anxiety"),
                           max_items = 10, free_only = TRUE))$output

# EXPECTED OUTPUT: the catalog rows for PHQ-9 and GAD-7 as a text table,
# wrapped in <tool_output tool="find_instrument" trust="untrusted"> ...
# </tool_output>. The wrapper marks text that came from a tool as DATA; the
# agent's instructions tell the model never to obey instructions found
# inside it (a defence against prompt injection).

call_tool("cohens_d_from_summary", tools,
          arguments = list(group1 = list(mean = 14.2, sd = 5.1, n = 40),
                           group2 = list(mean = 17.0, sd = 5.6, n = 38)))$output

# EXPECTED OUTPUT: d = -0.523 with pooled SD 5.349 (the first group has the
# lower mean, so d is negative).

# Now the failures. Each one returns an ERROR OBSERVATION: text the model can
# read and act on. Nothing crashes, and every failure is counted.

# (a) A tool that does not exist (models do invent tool names):
call_tool("sample_size", tools, arguments = list(effect_size = 0.4))$output

# (b) The wrong type (a word where a number is needed):
call_tool("sample_size_t", tools, arguments = list(effect_size = "medium"))$output

# (c) A missing required argument:
call_tool("sample_size_t", tools, arguments = list(power = 0.9))$output

# (d) A value outside the allowed list (enum):
call_tool("sample_size_t", tools, arguments = list(effect_size = 0.4, design = "between"))$output

# (e) An argument the tool does not have:
call_tool("sample_size_t", tools, arguments = list(effect_size = 0.4, tails = 2))$output

# EXPECTED OUTPUT: five lines starting with "Error:": (a) "there is no tool
# called 'sample_size'. Available tools: sample_size_t, find_instrument,
# cohens_d_from_summary."; (b) "'effect_size' must be a number"; (c)
# "'effect_size' is required"; (d) "'design' must be one of: two.sample,
# one.sample, paired"; (e) "unknown argument(s): tails". Each ends with
# "Call the tool again with corrected arguments." where that applies.

# (f) A function that throws an error (a service that is down):
lookup_norms <- function(instrument) {
  stop("the norms server is not responding")
}
norms_tool <- define_tool(lookup_norms, "Look up published norms for an instrument.",
                          args = list(instrument = arg_string("Instrument name, for example 'PHQ-9'")))
call_tool("lookup_norms", list(norms_tool), arguments = list(instrument = "PHQ-9"))$output

# EXPECTED OUTPUT: "Error while running lookup_norms: the norms server is not
# responding".

# (g) A slow computation that hits the time limit (timeout, in seconds):
bootstrap_mean <- function(values, resamples = 2000L) {
  means <- numeric(resamples)
  for (b in seq_len(resamples)) means[b] <- mean(sample(values, replace = TRUE))
  list(mean = mean(values), ci_95 = unname(stats::quantile(means, c(0.025, 0.975))))
}
boot_tool <- define_tool(bootstrap_mean, "Bootstrap 95% CI of a mean.",
                         args = list(values = arg_array("The scores", items = arg_number("A score")),
                                     resamples = arg_integer("Number of bootstrap resamples")),
                         timeout = 2, untrusted = FALSE)
scores <- c(12, 15, 9, 14, 18, 11, 13, 16, 10, 17)
call_tool("bootstrap_mean", list(boot_tool),
          arguments = list(values = scores, resamples = 2000))$output
slow <- call_tool("bootstrap_mean", list(boot_tool),
                  arguments = list(values = scores, resamples = 1e6))
slow$output
slow$latency

# EXPECTED OUTPUT: the first call returns the mean (13.5) and a 95% interval
# (random, so it varies slightly); the second returns "Error while running
# bootstrap_mean: the tool took longer than 2 seconds", with a latency of
# about 2 seconds.


# ==============================================================================
# STEP 5: THE FIRST AGENT
# ==============================================================================

# An agent = a model + instructions + tools (+ a context strategy). The
# instructions say WHEN to use each tool; the tool descriptions say WHAT each
# one does. ctx_full() is the default context strategy: every message so far
# is sent to the model at every step (simple and lossless; Day 2 shows
# cheaper strategies).
planner <- agent(
  name = "study_planner",
  instructions = paste(
    "You help researchers plan quantitative studies.",
    "Use cohens_d_from_summary when group means and SDs are given.",
    "Use sample_size_t for every sample size; never compute one yourself.",
    "Use find_instrument before naming any instrument.",
    "Report the numbers the tools return, and say which tool produced each."),
  model = "groq:openai/gpt-oss-120b",
  tools = tools,
  description = "Plans effect sizes, sample sizes and instruments for a study.",
  context = ctx_full()
)
planner

# EXPECTED OUTPUT: "<agent> study_planner | groq:openai/gpt-oss-120b", the
# description, the shortened instructions, "Tools: sample_size_t,
# find_instrument, cohens_d_from_summary", "Context: full".

# The budget: every run stops at the FIRST limit reached. Set it before you
# run, not after looking at the results.
b <- budget(max_steps = 6, max_tokens = 30000, max_cost = 0.02, max_seconds = 120,
            max_tool_errors = 3)
b

# EXPECTED OUTPUT: "<budget> 6 steps | 30,000 tokens | $0.0200 | 120 s | 3
# tool errors in a row".

task_pilot <- paste(
  "A pilot study found a mean of 14.2 (SD 5.1, n = 40) in the treatment group and",
  "17.0 (SD 5.6, n = 38) in the control group. What is the effect size, and how many",
  "participants per group would a two-group study need for 90% power at alpha = .05,",
  "two-sided, if the true effect is that large?")

# keep() saves the episode, so the class demo replays it on any day.
ep_pilot <- keep("loop_planner_pilot",
                 run(planner, task_pilot, budget = b, verbose = TRUE))
ep_pilot$stop_reason

# IMPORTANT: run() does not stop R when the API fails; it returns an episode
# whose stop reason starts with "api_error" (no key, no network, a 429 that
# persisted). keep() saves THAT episode too, and would replay the failure
# every time. If the stop reason starts with "api_error", fix the cause and
# run the line again with keep(..., refresh = TRUE).


# ==============================================================================
# STEP 6: READ THE TRACE: THE MODEL PROPOSES, THE HARNESS EXECUTES
# ==============================================================================

# EXPECTED OUTPUT of the run above (first run; the details vary):
#   [1] model ... -> cohens_d_from_summary({"group1": ..., "group2": ...})
#        The MODEL read the task and the three tool schemas and PROPOSED a call.
#        It ran nothing: it only wrote the tool name and JSON arguments.
#   [2] tool   cohens_d_from_summary ... ok ...
#        The HARNESS (R) checked the arguments against the schema, ran the R
#        function with a time limit, and added the result to the conversation.
#   [3] model ... -> sample_size_t({"effect_size": ..., "power": 0.9, ...})
#        The model read the result and proposed the next call. The whole
#        conversation, including step 2's result, was sent again (ctx_full).
#   [4] tool   sample_size_t ... ok ...
#   [5] model ... -> <the answer text>
#        No tool call in the reply: the loop treats it as the final answer.
#   Stopped: final_answer
# A model may also request two tools in one reply; they then appear as two
# tool steps after one model step.

ep_pilot
episode_steps(ep_pilot)[, c("step", "type", "name", "input", "output", "context_tokens", "is_error")]
cat(ep_pilot$answer)

# Check the agent against R, by hand:
cohens_d_from_summary(list(mean = 14.2, sd = 5.1, n = 40), list(mean = 17.0, sd = 5.6, n = 38))
sample_size_t(effect_size = 0.523, power = 0.90)

# EXPECTED OUTPUT: the episode summary with "Stop reason: final_answer";
# a step table alternating model and tool rows, where context_tokens grows at
# every model step; the answer text, which should report d of about -0.52
# (magnitude 0.52) and the per-group n that sample_size_t returned; then the
# same two numbers computed directly in R (d = -0.523; n = 78 per group).
# If the agent's numbers differ from R's, the trace shows where it went wrong.


# ==============================================================================
# STEP 7: STOP REASONS: EVERY RUN ENDS FOR A RECORDED REASON
# ==============================================================================

# (a) A budget of zero steps: the harness stops BEFORE spending anything.
#     (No model call is made, so this runs offline.)
ep_zero <- run(planner, task_pilot, budget = budget(max_steps = 0))
ep_zero$stop_reason

# EXPECTED OUTPUT: "Stopped: max_steps" and "max_steps"; the answer is NULL.

# (b) max_steps: one model call is not enough for a task that needs tools.
ep_short <- keep("loop_planner_one_step",
                 run(planner, task_pilot, budget = budget(max_steps = 1)))
ep_short$stop_reason
ep_short$answer

# EXPECTED OUTPUT: "max_steps" and NULL. The model asked for a tool, R ran it,
# but the budget allowed no second model call to read the result. The
# ledger still records the cost of the step that was used.
# Recorded run (November 22, 2026): one model step and one tool step, then
# "Stopped: max_steps". The model step showed $0.00000 with the same 713
# input and 167 output tokens as step 1 in STEP 5: the request was identical,
# so the response was replayed from the cache, and a replay is not billed.

# (c) max_tool_errors: a tool that always fails. The run stops after N
#     failed tool calls in a row instead of retrying forever.
norms_agent <- agent(
  name = "norms_finder",
  instructions = "Find published norms with lookup_norms. If it fails, try again.",
  model = "groq:openai/gpt-oss-20b",
  tools = list(norms_tool)
)
ep_errors <- keep("loop_norms_errors",
                  run(norms_agent, "Find the published norms for the PHQ-9.",
                      budget = budget(max_steps = 6, max_tool_errors = 2), verbose = TRUE))
ep_errors$stop_reason
episode_steps(ep_errors)[, c("step", "type", "name", "is_error", "detail")]

# EXPECTED OUTPUT: usually "max_tool_errors": two tool rows with is_error TRUE
# and the detail "Error while running lookup_norms: the norms server is not
# responding" (when the model repeats the same call, the second detail adds
# "[Note: this exact call was already made 1 time(s) ..."). A model may
# instead give up after the first error and explain that the service failed
# (stop reason "final_answer"); both are recorded.
#
# The other stop reasons: "max_tokens", "max_cost", "max_seconds" (the other
# budget limits) and "api_error: <message>" (for example a 429 that persisted
# after the retries). Always check ep$stop_reason before using ep$answer.


# ==============================================================================
# STEP 8: A STRUCTURED FINAL ANSWER (output_schema)
# ==============================================================================

# Free text is hard to use downstream. With output_schema, the agent must
# finish by calling a final_answer tool whose arguments R checks like any
# other tool. An invalid answer goes back to the model as an error to fix
# (counted, never silently repaired). The schema uses the same arg_*()
# helpers; here an ARRAY of OBJECTS (one object per instrument).
plan_schema <- list(
  instruments = arg_array(
    "The instruments chosen, one per construct",
    items = arg_object("One instrument", fields = list(
      construct = arg_string("The construct it measures"),
      instrument = arg_string("Its name exactly as find_instrument returned it"),
      items = arg_integer("Number of items"),
      reason = arg_string("One sentence: why this instrument")))),
  n_per_group = arg_integer("Participants per group, as returned by sample_size_t"),
  notes = arg_string("Assumptions and caveats, in one or two sentences")
)

structured_planner <- agent(
  name = "structured_planner",
  instructions = planner$instructions,
  model = "groq:openai/gpt-oss-120b",
  tools = tools,
  output_schema = plan_schema
)
structured_planner

# What the model sees for the final answer (a peek inside the package):
tool_schema(researchAgentR:::.final_answer_tool(plan_schema))

# EXPECTED OUTPUT: the agent prints "Final answer: structured ( instruments,
# n_per_group, notes )". The schema is a tool named "final_answer" whose
# "instruments" property is an array of objects with four required fields.

task_plan <- paste(
  "Plan a two-group intervention study that measures depression and anxiety with",
  "short instruments (at most 10 items each) that are free for research use.",
  "The expected effect is d = 0.4; use power .80 and alpha .05, two-sided.")

ep_plan <- keep("loop_structured_plan",
                run(structured_planner, task_plan, budget = b, verbose = TRUE))
ep_plan$stop_reason
str(ep_plan$answer)

# The answer is an R list, ready for analysis code:
plan_table <- do.call(rbind, lapply(ep_plan$answer$instruments, as.data.frame))
plan_table
ep_plan$answer$n_per_group == sample_size_t(0.4)$n

# EXPECTED OUTPUT: "final_answer"; a list with instruments (a list of lists),
# n_per_group (an integer) and notes (text); a data frame with columns
# construct, instrument, items, reason, which should contain PHQ-9 and GAD-7
# (the only catalog entries that meet the conditions); and TRUE when the
# agent reported the n that sample_size_t computes for d = 0.4 (100).


# ==============================================================================
# STEP 9: THE LOOP IS SHORT: READ IT
# ==============================================================================

# Print the function and read it next to this walk-through:
run

# 1. tools <- .agent_tools(agent)   the agent's tools, plus final_answer when
#                                   there is an output_schema
# 2. ep <- .new_episode(...)        an empty ledger: id, task, budget, steps
# 3. messages <- system + user      the instructions and the task
# 4. repeat {
#      reason <- .over_budget(...)  check EVERY limit BEFORE each model call;
#                                   stop with the name of the limit reached
#      sent <- agent$context$prepare(messages, ep)
#                                   the context strategy decides what is sent
#                                   (ctx_full() sends everything)
#      ep <- .record_context(...)   strategies that summarize old turns make
#                                   model calls of their own; their cost is
#                                   recorded too (nothing for ctx_full())
#      reply <- chat(sent, ...)     ONE model call; an API error ends the
#                                   episode as "api_error: ...", never a crash
#      ep <- .record_model(...)     tokens, cost, latency of the call
#      no tool calls?               plain text is the final answer (or, with
#                                   an output_schema, a reminder to call
#                                   final_answer)
#      for each tool call:          the HARNESS executes: call_tool() checks
#        call_tool(), .record_tool  and runs it; the result (or the error) is
#                                   added to the conversation and the ledger
#        final_answer accepted?     stop with "final_answer"
#    }
# Everything else in the package (memory, multi-agent teams, evaluation) is
# built around this loop. The budget check is a few lines too:
researchAgentR:::.over_budget


# ==============================================================================
# STEP 10: THE SAME AGENT ON ANOTHER MODEL (OPTIONAL)
# ==============================================================================

# Only the model string changes. Compare the traces with STEP 6.
# OPTIONAL, PAID (needs ANTHROPIC_API_KEY; a fraction of a cent):
planner_claude <- agent(name = "study_planner_claude", instructions = planner$instructions,
                        model = "anthropic:claude-haiku-5-5", tools = tools)
ep_claude <- keep("loop_planner_pilot_claude", run(planner_claude, task_pilot, budget = b))
# OPTIONAL, LOCAL AND FREE (needs Ollama and `ollama pull qwen3:4b`; an agent
# loop on a laptop can take minutes per task, see Activity 11):
planner_local <- agent(name = "study_planner_local", instructions = planner$instructions,
                       model = "ollama:qwen3:4b", tools = tools,
                       effort = "none")   # thinking off where the model allows it (qwen3:4b
                                          # exists only as a thinking model: expect a minute or more)
ep_local <- keep("loop_planner_pilot_local", run(planner_local, task_pilot, budget = b))

episodes_df(list(ep_pilot, ep_claude, ep_local))

# EXPECTED OUTPUT: one row per episode (leave out the ones you did not run).
# Small local models make more tool-call mistakes (wrong argument types,
# invented tool names); the error observations of STEP 4 let them recover,
# and Day 3 measures how often they do.
# Recorded run (November 22, 2026): three rows, all final_answer.
# gpt-oss-120b: 3 model and 2 tool calls, 2857 tokens, cost 0.00062, 1.5 s;
# claude-haiku-5-5: 3 and 2, 5620 tokens, cost 0.00055, 5.6 s; qwen3:4b: 2
# and 2, 10269 tokens, cost 0, 155 s (its trace shows cohens_d_from_summary
# twice and no call to sample_size_t).


# ==============================================================================
# TRY IT
# ==============================================================================

# - Remove "never compute one yourself" from the instructions and run the
#   pilot task again (new keep() name). Does the model still call the tool?
# - Make a tool description vague ("Does statistics.") and watch the trace.
# - Give the planner a task that needs the catalog but uses another word
#   ("depressive symptoms"). Does the helpful "no match" message help?


# ==============================================================================
# EXERCISES (solutions in solutions/02_solutions.R)
# ==============================================================================

# 1. Write a tool power_t() that returns the POWER of a two-group t test for a
#    given effect size and n per group (stats::power.t.test with n = ...).
#    Describe its arguments, check the schema, and call it by hand for
#    d = 0.5 and n = 64. Then call it with n = "sixty-four".
# 2. Write a tool alpha_from_items() that takes an arg_array of per-item
#    variances and the total-score variance (arg_number) and returns
#    Cronbach's alpha, k / (k - 1) * (1 - sum(item variances) / total variance).
#    Check it by hand with item variances c(1.2, 0.9, 1.1, 1.0) and total
#    variance 10.5.
# 3. Add power_t to the planner and ask: "With 50 participants per group, what
#    power does a two-group study have to detect d = 0.4?" Which tool did the
#    model use, and was the answer right?
# 4. Run the norms_finder with budget(max_tool_errors = 1) and with
#    budget(max_tool_errors = 4). Compare the stop reasons and the costs with
#    episodes_df().
# 5. Add a field to plan_schema: design = arg_enum("Study design", values =
#    c("two.sample", "paired")). Run the structured planner again and check
#    that the answer contains the new field.


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - "No key found for groq" or "401": see Activity 00 (key in ~/.Renviron,
#   restart R).
# - "429 ... Rate limit": every step resends the conversation plus the tool
#   schemas, so agents use far more tokens than one question. Wait a minute,
#   lower the rate (options(researchAgentR.rpm.groq = 20)), or use the
#   smaller "groq:openai/gpt-oss-20b". A 429 that persists after the retries
#   ends the episode with stop reason "api_error: ...".
# - "404 ... Check the model name": the model was retired; see
#   list_provider_models("groq").
# - A step named "(invalid tool call)": the provider rejected the model's
#   reply before R saw it (a call to a tool that does not exist, such as
#   "json" or "answer", or arguments that are not valid JSON). The loop tells
#   the model why and asks again. Each one uses a step and counts as a tool
#   error; several in a row end the episode with "max_tool_errors".
# - "[Note: this exact call was already made ...]" at the end of a tool
#   result: the model repeated a call with the same arguments. The call runs
#   again, and the note tells the model so, which often ends the loop.
# - define_tool() error "These arguments are not arguments of the function":
#   the names in args must match the R function's argument names exactly.
# - A required argument without a default in the R function, declared with
#   required = FALSE: when the model leaves it out, the function fails and the
#   model gets an error. Give optional arguments a default in R.
# - Budget too small: max_steps = 1 (or a tiny max_tokens or max_cost) stops
#   before the answer; the stop reason says which limit.
# - The time limit stops R code that is COMPUTING. It cannot interrupt
#   Sys.sleep() or a function that waits inside compiled code; HTTP requests
#   have their own timeout (options(researchAgentR.timeout = 120)).
# - Ollama: "Failed to connect to localhost port 11434": Ollama is
#   not running, or the model was not pulled (`ollama pull qwen3:4b`).
# - A changed task returns the old answer: keep() loaded the saved episode.
#   Use a new name or keep(..., refresh = TRUE).
# - A failure that keeps coming back: a run that ended with "api_error: ..."
#   was saved by keep() like any other episode. Fix the cause (key, network,
#   rate limit) and rerun with keep(..., refresh = TRUE).
# - episode_steps(ep)[, c(...)] fails with "undefined columns selected": the
#   episode has no steps (it stopped with an api_error before the first model
#   call). Check ep$stop_reason.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. "The model proposes, the harness executes." Which decisions in this
#    activity belong to the model, and which should never leave R?
# 2. The model only sees names, descriptions and schemas. How is writing a
#    tool description like writing an item for a questionnaire?
# 3. Errors are returned to the model as observations instead of stopping
#    the run. What are the benefits, and what is the risk (think of costs and
#    of a model that "fixes" an error by inventing a value)?
# 4. Which budget would you set for a literature-screening agent before you
#    run it the first time, and why before rather than after?
# 5. The structured answer is checked in R, but its CONTENT is not verified.
#    What else would you check before using the plan in a grant proposal?
