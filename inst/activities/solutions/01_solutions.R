# Solutions: Activity 01, Lens, Watch an Agent Work
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

library(researchAgentR)
use_cache("auto")
set_groq_key()

# Objects from the activity (replayed by keep() when they were saved before):
question <- "In two sentences, what is the difference between a chatbot and an LLM agent?"
research_question <- paste(
  "Which article introduced Exploratory Graph Analysis (EGA), by Golino and Epskamp?",
  "Give the title, journal, year and DOI, and the URL where you confirmed them.")
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
ep_120b <- keep("lens_web_scout_120b",
                run(web_scout, research_question,
                    budget = budget(max_steps = 4, max_cost = 0.05)))


# ------------------------------------------------------------------------------
# Exercise 1: reasoning effort, tokens and cost
# ------------------------------------------------------------------------------

efforts <- c("low", "high")
calls <- lapply(efforts, function(e) chat(question, model = "groq:openai/gpt-oss-20b", effort = e))
data.frame(
  effort = efforts,
  output = sapply(calls, function(x) x$usage$output),
  reasoning = sapply(calls, function(x) x$usage$reasoning),
  cost = sapply(calls, function(x) x$cost),
  latency = sapply(calls, function(x) x$latency),
  cached = sapply(calls, function(x) x$cached)
)
# Higher effort usually means more reasoning tokens, hence more output tokens,
# more cost and more time. Whether the answer improves is a separate question
# (Day 3 measures it). Rows with cached = TRUE were replayed: cost 0.


# ------------------------------------------------------------------------------
# Exercise 2: the cost of one step, by hand
# ------------------------------------------------------------------------------

steps <- episode_steps(ep_120b)
s1 <- steps[steps$type == "model", ][1, ]
s1[, c("input", "cache_read", "output", "cost", "cached")]

price <- model_registry("groq")
price <- price[price$model == "openai/gpt-oss-120b", ]
by_hand <- (s1$input * price$input + s1$cache_read * price$cached_input +
            s1$output * price$output) / 1e6
c(by_hand = by_hand, ledger = s1$cost)
# On a live run the two numbers match. When the step was replayed from the
# response cache (cached = TRUE), the ledger cost is 0 because nothing was
# billed; by_hand still shows what the call cost when it was recorded.


# ------------------------------------------------------------------------------
# Exercise 3: does the DOI resolve to the article the agent named?
# ------------------------------------------------------------------------------

doi <- regmatches(ep_120b$answer, regexpr("10\\.[0-9]{4,9}/[^[:space:]\"<>)]+", ep_120b$answer))
doi <- sub("[.,;]+$", "", doi)     # drop punctuation at the end of a sentence
doi
if (length(doi) == 1) utils::browseURL(paste0("https://doi.org/", doi))
# Compare the landing page (title, authors, journal, year) with the answer.
# character(0) means the answer contained no DOI: the agent could not confirm
# one, or wrote it in another form. Activity 03 checks DOIs with Crossref and
# OpenAlex (verify_citations()).


# ------------------------------------------------------------------------------
# Exercise 4: how many steps at 99% per step keep success at or above 80%?
# ------------------------------------------------------------------------------

rc <- reliability_curve(p = 0.99, steps = 1:100)
max(rc$steps[rc$success >= 0.80])
0.99^22    # 0.8016 (at least 80%)
0.99^23    # 0.7936 (below 80%)
# 22 steps.


# ------------------------------------------------------------------------------
# Exercise 5: a one-step budget
# ------------------------------------------------------------------------------

ep_one <- keep("lens_web_scout_one_step",
               run(web_scout, research_question, budget = budget(max_steps = 1)))
ep_one$stop_reason
# Usually "final_answer". Budgets are checked BEFORE each model call. Built-in
# search runs on Groq's servers INSIDE that single call, so the model can
# search and answer in one step; the loop never needs a second call. A budget
# on steps does not limit how many searches happen inside a call (count them
# in executed_tools). With function tools (Activity 02), every tool round
# trip costs another model call, and max_steps = 1 stops the run after the
# first one with stop reason "max_steps".
