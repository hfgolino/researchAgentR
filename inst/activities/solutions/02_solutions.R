# Solutions: Activity 02, Loop, Your First Tool and Your First Agent
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# Run STEPS 1 to 8 of 02_Loop_First_Tool_and_Agent.R first: these solutions
# use its objects (tools, planner, norms_tool, plan_schema, sample_size_t).
library(researchAgentR)
use_cache("auto")
set_groq_key()
stopifnot(exists("tools"), exists("planner"), exists("norms_tool"),
          exists("plan_schema"), exists("sample_size_t"))


# ------------------------------------------------------------------------------
# Exercise 1: a power tool
# ------------------------------------------------------------------------------

power_t <- function(effect_size, n_per_group, alpha = 0.05) {
  res <- stats::power.t.test(n = n_per_group, delta = abs(effect_size), sd = 1,
                             sig.level = alpha)
  list(power = round(res$power, 3), effect_size = effect_size,
       n_per_group = n_per_group, alpha = alpha)
}

power_tool <- define_tool(
  power_t,
  description = paste("Compute the power of a two-group t test (stats::power.t.test)",
                      "for a given effect size and number of participants per group."),
  args = list(
    effect_size = arg_number("Expected Cohen's d, for example 0.4"),
    n_per_group = arg_integer("Participants per group, for example 50"),
    alpha = arg_number("Significance level (default 0.05)", required = FALSE)
  ),
  untrusted = FALSE
)
tool_schema(power_tool)

call_tool("power_t", list(power_tool), arguments = list(effect_size = 0.5, n_per_group = 64))$output
# power about 0.80 (64 per group is the classic answer for d = 0.5).

call_tool("power_t", list(power_tool), arguments = list(effect_size = 0.5, n_per_group = "sixty-four"))$output
# "Error: invalid arguments for power_t: 'n_per_group' must be a whole number. ..."


# ------------------------------------------------------------------------------
# Exercise 2: Cronbach's alpha from variances
# ------------------------------------------------------------------------------

alpha_from_items <- function(item_variances, total_variance) {
  k <- length(item_variances)
  list(alpha = round(k / (k - 1) * (1 - sum(item_variances) / total_variance), 3), k = k)
}

alpha_tool <- define_tool(
  alpha_from_items,
  description = "Compute Cronbach's alpha from the item variances and the variance of the total score.",
  args = list(
    item_variances = arg_array("Variance of each item, for example [1.2, 0.9, 1.1, 1.0]",
                               items = arg_number("The variance of one item")),
    total_variance = arg_number("Variance of the total (sum) score")
  ),
  untrusted = FALSE
)
call_tool("alpha_from_items", list(alpha_tool),
          arguments = list(item_variances = c(1.2, 0.9, 1.1, 1.0), total_variance = 10.5))$output
# k = 4, sum of item variances = 4.2: alpha = 4/3 * (1 - 4.2/10.5) = 0.8


# ------------------------------------------------------------------------------
# Exercise 3: the planner with the power tool
# ------------------------------------------------------------------------------

planner_power <- agent(
  name = "study_planner_power",
  instructions = paste(planner$instructions,
                       "Use power_t whenever the question asks for the power of a design."),
  model = "groq:openai/gpt-oss-120b",
  tools = c(tools, list(power_tool))
)
ep_power <- keep("loop_solution_power",
                 run(planner_power,
                     "With 50 participants per group, what power does a two-group study have to detect d = 0.4?",
                     budget = budget(max_steps = 5)))
ep_power$stop_reason
episode_steps(ep_power)[, c("step", "type", "name", "is_error")]
cat(ep_power$answer)

# The correct value, computed directly in R:
power_t(effect_size = 0.4, n_per_group = 50)
# The trace shows which tool was called (it should be power_t, not
# sample_size_t) and the answer should report the same power as R.


# ------------------------------------------------------------------------------
# Exercise 4: the tool-error budget
# ------------------------------------------------------------------------------

norms_agent <- agent(
  name = "norms_finder",
  instructions = "Find published norms with lookup_norms. If it fails, try again.",
  model = "groq:openai/gpt-oss-20b",
  tools = list(norms_tool)
)
ep_err1 <- keep("loop_solution_errors_1",
                run(norms_agent, "Find the published norms for the PHQ-9.",
                    budget = budget(max_steps = 8, max_tool_errors = 1)))
ep_err4 <- keep("loop_solution_errors_4",
                run(norms_agent, "Find the published norms for the PHQ-9.",
                    budget = budget(max_steps = 8, max_tool_errors = 4)))
episodes_df(list(ep_err1, ep_err4))[, c("stop_reason", "model_calls", "tool_calls",
                                        "tool_errors", "tokens", "cost")]
# With max_tool_errors = 1 the run stops after the first failed call. With 4,
# the model may retry up to four times; every retry resends the growing
# conversation, so tokens and cost rise with each attempt, and the answer is
# no better (the tool can never succeed). A tool-error budget caps the cost
# of a broken tool.


# ------------------------------------------------------------------------------
# Exercise 5: a new field in the structured answer
# ------------------------------------------------------------------------------

plan_schema_2 <- c(plan_schema,
                   list(design = arg_enum("Study design", values = c("two.sample", "paired"))))
structured_planner_2 <- agent(
  name = "structured_planner_2",
  instructions = planner$instructions,
  model = "groq:openai/gpt-oss-120b",
  tools = tools,
  output_schema = plan_schema_2
)
ep_plan_2 <- keep("loop_solution_plan_design",
                  run(structured_planner_2,
                      paste("Plan a two-group intervention study that measures depression and anxiety",
                            "with short instruments (at most 10 items each) that are free for research use.",
                            "The expected effect is d = 0.4; use power .80 and alpha .05, two-sided."),
                      budget = budget(max_steps = 6)))
ep_plan_2$stop_reason
names(ep_plan_2$answer)
ep_plan_2$answer$design
# "design" is one of the names and its value is "two.sample" or "paired":
# the final_answer tool rejects any other value and asks the model to fix it.
