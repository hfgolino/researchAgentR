# Solutions: Activity 08b (MCP Clients)
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# All exercises need the network (DeepWiki); Exercise 3 also needs a Groq key.
# Exercise 1 uses dw_tools from STEP 3 (or the kept copy, offline).

library(researchAgentR)
set_groq_key()
use_cache("auto")
deepwiki_url <- "https://mcp.deepwiki.com/mcp"


# ==============================================================================
# Exercise 1: what the tool menu costs on every model call
# ==============================================================================

dw_tools <- keep("deepwiki_tools", {
  dw <- mcp_connect(deepwiki_url)
  listed <- mcp_list_tools(dw)
  mcp_close(dw)
  listed
})
menu_tokens <- count_tokens(paste(dw_tools$name, dw_tools$description, dw_tools$input_schema))
data.frame(tool = dw_tools$name, tokens = menu_tokens)
c(all_tools = sum(menu_tokens), ask_question_only = sum(menu_tokens[dw_tools$name == "ask_question"]))
# EXPECTED OUTPUT: approximate tokens per tool (about 4 characters per token)
# and the total for the full menu versus one tool. The menu is sent with
# EVERY model call of the episode, so multiply by the number of calls.


# ==============================================================================
# Exercise 2: list a server's tools safely
# ==============================================================================

server_tool_names <- function(url, timeout = 20) {
  client <- tryCatch(mcp_connect(url, timeout = timeout), error = function(e) e)
  if (inherits(client, "error")) {
    message("Could not connect to ", url, ": ", conditionMessage(client))
    return(NA_character_)
  }
  on.exit(mcp_close(client))      # close even if listing fails
  tools <- tryCatch(mcp_list_tools(client)$name, error = function(e) e)
  if (inherits(tools, "error")) {
    message("Connected, but could not list the tools: ", conditionMessage(tools))
    return(NA_character_)
  }
  tools
}
server_tool_names(deepwiki_url)
server_tool_names("https://example.invalid/mcp", timeout = 5)
# EXPECTED OUTPUT: DeepWiki's tool names; then a message and NA for the URL
# that does not exist (the error is reported, not hidden, and not a crash).


# ==============================================================================
# Exercise 3: one agent, a server tool and a package tool
# ==============================================================================

dw <- mcp_connect(deepwiki_url)
method_agent <- agent(
  name = "method_explainer",
  model = "groq:openai/gpt-oss-120b",
  instructions = paste(
    "Explain what an R package function does, using deepwiki_ask_question, and find",
    "the DOI of the paper that introduced the method, using crossref. Report the DOI",
    "exactly as the crossref tool returned it, and say which source each statement",
    "comes from."),
  tools = c(mcp_as_tools(dw, tools = "ask_question", prefix = "deepwiki_"),
            list(tool_crossref(n = 5))))
ep3 <- run(method_agent,
  "What does EGA() in the R package hfgolino/EGAnet do, and what is the DOI of the article that introduced exploratory graph analysis?",
  budget = budget(max_steps = 6, max_cost = 0.03))
ep3
episode_steps(ep3)[, c("step", "type", "name", "is_error")]
mcp_close(dw)
# Verify the DOI in code before trusting it (Activity 03):
# verify_citations(<the DOI from ep3$answer>)
# EXPECTED OUTPUT: tool steps for deepwiki_ask_question and crossref, and an
# answer that names EGA(), gives a DOI and attributes each statement.


# ==============================================================================
# Exercise 4: what the log contains when R calls the server versus the provider
# ==============================================================================

# STEP 5 (R calls the server): every call is a TOOL STEP of the episode, with
# the exact arguments, the server's full result (as untrusted data), its
# latency and any error, so the trace is complete and can be reviewed.
# STEP 7 (Groq calls the server): the exchange happens inside ONE model call.
# The episode or response shows only the items the provider chose to return
# (executed_tools); the server's tokens are part of the model's billed input;
# nothing passed through your R session, so nothing could be filtered.
# For an AI-use statement, prefer STEP 5: it documents what was sent where.
