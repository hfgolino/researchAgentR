# Solutions: Activity 00, Setting Up researchAgentR
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

library(researchAgentR)
use_cache("auto")


# ------------------------------------------------------------------------------
# Exercise 1: Groq models that are not in the registry
# ------------------------------------------------------------------------------

groq_today <- list_provider_models("groq")
in_registry <- model_registry("groq")$model
setdiff(groq_today$id, in_registry)

# A model without a registry row still works, but its cost is recorded as NA
# (unknown), and so is the cost of any episode that uses it: one unpriced step
# makes the total unknown rather than too low. Add a price with registry_set(),
# for example (check https://groq.com/pricing for the real values first):
# registry_set("groq:<model id>", input = <price>, output = <price>)


# ------------------------------------------------------------------------------
# Exercise 2: a second name for LM Studio
# ------------------------------------------------------------------------------

register_provider("lms2", "http://localhost:1234/v1", api = "chat", local = TRUE)
providers()[providers()$provider == "lms2", ]
parse_model("lms2:qwen3-4b")
# provider "lms2", model "qwen3-4b": the model string is "lms2:qwen3-4b".


# ------------------------------------------------------------------------------
# Exercise 3: change the judge model and restore it
# ------------------------------------------------------------------------------

old_judge <- set_default_model("judge", "groq:openai/gpt-oss-20b")
default_model("judge")
set_default_model("judge", old_judge)
default_model("judge")


# ------------------------------------------------------------------------------
# Exercise 4: keep today's model list
# ------------------------------------------------------------------------------

models_today <- keep("groq_models_today", list_provider_models("groq"))
attr(models_today, "kept_at")
nrow(models_today)

# Useful for documentation: the saved list records which model IDs existed on
# the day of the analysis (with the time in the "kept_at" attribute). If a
# model is retired later, the record shows that it was available when the
# study ran. Note that keep() loads the saved list on later calls; use
# refresh = TRUE to check again.
