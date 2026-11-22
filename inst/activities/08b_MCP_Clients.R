# Activity 08b: MCP Clients
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: Connect R to Model Context Protocol (MCP) servers, call their tools
# directly and from an agent, and know the security difference between
# calling a server from R and letting the model provider call it.
#
# OVERVIEW: MCP is a common protocol through which any client (Claude Desktop,
# an IDE, this R package) can use the tools of any server. A server publishes
# a list of tools with JSON Schemas; a client lists them and calls them with
# JSON-RPC 2.0 messages. This activity reads those messages offline, connects
# to DeepWiki (a public server that answers questions about public GitHub
# repositories, no authentication), calls its tools from R, gives one tool to
# an agent, optionally starts a LOCAL server over stdio, and finally lets
# Groq call the server for the model (remote MCP).
#
# researchAgentR functions used here:
#   mcp_connect()       connect to an MCP server over Streamable HTTP
#   mcp_list_tools()    the server's tools, descriptions and JSON Schemas
#   mcp_call()          call one tool from R
#   mcp_as_tools()      turn server tools into agent tools
#   mcp_close()         end the session (legacy servers) or stop a local server
#   mcp_stdio()         start a local server process and talk over stdin/stdout
#   remote_mcp_tool()   let the provider (Groq Responses API) call the server
#   (also: agent(), run(), budget(), chat(), episode_steps(),
#    register_provider(), model_registry(), registry_set(), keep())
# TIME: about 45 minutes.  COST: free tier (Groq); DeepWiki is free.
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

set_groq_key()          # reads GROQ_API_KEY from ~/.Renviron
use_cache("auto")       # model calls are recorded and replayed. MCP exchanges
                        # are NOT cached (each one is live): keep() the results
                        # you want to show again offline (STEP 3).

deepwiki_url <- "https://mcp.deepwiki.com/mcp"


# ==============================================================================
# STEP 1: THE PROTOCOL IN BRIEF (OFFLINE)
# ==============================================================================

# Plain-English explainer.
# - Messages are JSON-RPC 2.0: {"jsonrpc": "2.0", "id": 1, "method": ..., "params": ...}.
#   The two methods an agent needs are tools/list and tools/call.
# - Transports: Streamable HTTP (an HTTP POST per message; the reply is JSON or
#   a Server-Sent Events stream) or stdio (one JSON message per line through
#   the standard input and output of a local program).
# - The current specification, 2026-07-28, is STATELESS: no initialize
#   handshake and no session id. Every request carries the protocol version,
#   the client's identity and its capabilities in params$`_meta`, and the HTTP
#   headers repeat the version, the method and the tool name, so proxies can
#   route requests without reading the body. A client first asks
#   server/discover.
# - Older servers speak 2025-11-25: initialize, then notifications/initialized,
#   then every request carries the Mcp-Session-Id the server returned.
#   mcp_connect() tries the modern protocol first and falls back to the old
#   one, as the specification recommends for clients that support both.
# Sources (checked by the package author on 2026-10-10):
# https://modelcontextprotocol.io/specification/latest/changelog
# https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http.md

# Look under the hood: the exact message and headers the client sends.
# (These are internal helpers, reached with :::, shown only for teaching.)
msg <- researchAgentR:::.mcp_message("tools/call", id = 2L,
  params = list(name = "ask_wiki_question",
                arguments = list(repoName = "tidyverse/ggplot2", question = "What is geom_smooth()?")))
cat(jsonlite::toJSON(msg, auto_unbox = TRUE, pretty = TRUE))
str(researchAgentR:::.mcp_headers(msg, version = "2026-07-28"))
# EXPECTED OUTPUT: a JSON-RPC request with method "tools/call", the tool name
# and arguments, and params$`_meta` holding the protocol version 2026-07-28,
# the client info (researchAgentR and its version) and its capabilities;
# then the headers Accept, MCP-Protocol-Version, Mcp-Method (tools/call) and
# Mcp-Name (ask_wiki_question).


# ==============================================================================
# STEP 2: CONNECT TO A PUBLIC SERVER
# ==============================================================================

dw <- mcp_connect(deepwiki_url)
dw
# EXPECTED OUTPUT: "<mcp_client> http | https://mcp.deepwiki.com/mcp", the
# server name and version, the protocol in use (2026-07-28, or 2025-11-25
# followed by "| session" for a legacy server; a message says when the client
# fell back) and the server's capabilities. Server instructions, if any, are
# marked untrusted.
# Recorded run (November 22, 2026): server DeepWiki 4.0.2, protocol
# 2026-07-28, capabilities logging, prompts, resources, tools, extensions,
# and a line "Server instructions (untrusted): ...".

dw$state$version


# ==============================================================================
# STEP 3: LIST THE TOOLS, READ THEIR DESCRIPTIONS
# ==============================================================================

dw_tools <- mcp_list_tools(dw)
dw_tools[, c("name", "description")]
cat(dw_tools$input_schema[dw_tools$name == "ask_wiki_question"], "\n")
# EXPECTED OUTPUT: the server's tools (DeepWiki documents read_wiki_structure,
# read_wiki_contents and ask_wiki_question) with their descriptions, then the JSON
# Schema of ask_wiki_question: the argument names to use (repoName and question).

# READ the descriptions before giving any tool to an agent: they go into the
# model's context on every call, they cost tokens, and they are written by
# the server's owner (a malicious description is a prompt injection).

# Keep a copy for offline demos (MCP exchanges are not cached):
dw_tools_kept <- keep("deepwiki_tools", dw_tools)


# ==============================================================================
# STEP 4: CALL TOOLS FROM R
# ==============================================================================

structure_ega <- mcp_call(dw, "read_wiki_structure", list(repoName = "hfgolino/EGAnet"))
structure_ega$is_error
cat(substr(structure_ega$text, 1, 1500), "\n")
# EXPECTED OUTPUT: FALSE, then the list of topics of DeepWiki's generated wiki
# for the EGAnet repository.

answer_gg <- mcp_call(dw, "ask_wiki_question", list(
  repoName = "tidyverse/ggplot2",
  question = "How does geom_smooth() choose its default smoothing method?"))
cat(substr(answer_gg$text, 1, 2000), "\n")
# EXPECTED OUTPUT: a text answer about how geom_smooth() picks loess or a GAM by sample size.
# The answer is written by DeepWiki's own model: it is a claim to verify
# (?ggplot2::geom_smooth or the source code), not a fact.

# Two kinds of failure:
# (a) A tool error is a normal result with is_error = TRUE (the server ran
#     the tool and it failed); the text explains why.
bad_repo <- mcp_call(dw, "read_wiki_structure", list(repoName = "no-such-owner/no-such-repo"))
bad_repo$is_error
cat(substr(bad_repo$text, 1, 300), "\n")
# (b) A protocol error (unknown tool, malformed request) stops with an error
#     of class mcp_error.
tryCatch(mcp_call(dw, "no_such_tool", list()),
         mcp_error = function(e) conditionMessage(e))
# EXPECTED OUTPUT: for (a), is_error TRUE (or an explanatory text, depending
# on how the server reports it); for (b), "MCP error <code>: ..." when the
# server answers with a protocol error.
# Recorded run (November 22, 2026): (a) TRUE, then "Error fetching wiki for
# no-such-owner/no-such-repo: Repository not found. ..."; (b) no mcp_error
# was raised: DeepWiki answered with a normal result whose is_error is TRUE
# and whose text is "Unknown tool: 'no_such_tool'", printed as a list.


# ==============================================================================
# STEP 5: SERVER TOOLS INSIDE AN AGENT
# ==============================================================================

# Keep the menu short: only the tool the job needs. The prefix keeps names
# unique when an agent uses several servers.
repo_tools <- mcp_as_tools(dw, tools = "ask_wiki_question", prefix = "deepwiki_")
repo_tools[[1]]
# EXPECTED OUTPUT: "<agent_tool> deepwiki_ask_wiki_question" with the server's
# description, its arguments (from the server's schema) and the flag
# "output untrusted".

repo_helper <- agent(
  name = "repo_helper",
  model = "groq:openai/gpt-oss-120b",
  instructions = paste(
    "Answer questions about R packages on GitHub using the deepwiki_ask_wiki_question tool.",
    "Name the functions involved, say that the answer comes from DeepWiki, and say what",
    "the researcher should check in the package documentation."),
  tools = repo_tools)

ep_repo <- run(repo_helper,
  "In the R package hfgolino/EGAnet, which function estimates the stability of dimensions with a bootstrap, and what does it return?",
  budget = budget(max_steps = 5, max_cost = 0.02))
ep_repo
episode_steps(ep_repo)[, c("step", "type", "name", "is_error", "detail")]
# EXPECTED OUTPUT: a model step, a deepwiki_ask_wiki_question tool step with the
# server's answer (wrapped as untrusted data), and a final answer. Check the
# function names it gives against the EGAnet documentation.
# Recorded run (November 22, 2026): final_answer after 2 model calls and 1
# tool call (0 errors), 1,425 billed tokens, cost $0.00044, 9.9 s; the
# answer named dimensionStability().

mcp_close(dw)   # ends the session with a legacy server; nothing to do for a modern one


# ==============================================================================
# STEP 6 (OPTIONAL): A LOCAL SERVER OVER STDIO
# ==============================================================================

# Many MCP servers are local programs, often started with npx (Node.js) or
# uvx (Python). They run with YOUR user's permissions, and npx downloads the
# package from npm when it starts: start only servers you trust, and pin a
# version ("@modelcontextprotocol/server-filesystem@<version>") in real work.
# This example needs Node.js; it is skipped when npx is not installed.
if (nzchar(Sys.which("npx"))) {
  fs <- mcp_stdio("npx", c("-y", "@modelcontextprotocol/server-filesystem", tempdir()))
  print(fs)
  fs_tools <- mcp_list_tools(fs)
  print(fs_tools$name)
  if ("list_directory" %in% fs_tools$name) {
    cat(mcp_call(fs, "list_directory", list(path = tempdir()))$text, "\n")
  }
  mcp_close(fs)
}
# EXPECTED OUTPUT: a client with transport "stdio", the server's tool names
# (file tools limited to the folder given), and a listing of tempdir().

# Serving R tools TO other MCP clients (Claude Desktop, an IDE) is the other
# direction: see mcptools::mcp_server() in the mcptools package. This package
# only includes a client.


# ==============================================================================
# STEP 7: REMOTE MCP: THE PROVIDER CALLS THE SERVER
# ==============================================================================

# Up to now YOUR R session called the server: you saw every request and every
# result, and the agent loop marked results as untrusted. With remote MCP,
# the model provider (Groq) calls the server for the model, inside one API
# request. You cannot inspect or filter that exchange before it happens, and
# the server receives whatever the model sends it: treat the server as seeing
# the full context. Use only servers you trust, limit allowed_tools, and use
# require_approval = "always" to see what the model WOULD send.

dw_remote <- remote_mcp_tool(deepwiki_url, server_label = "deepwiki",
                             allowed_tools = "ask_wiki_question", require_approval = "never")
class(dw_remote)
str(dw_remote$spec)
# EXPECTED OUTPUT: "builtin_tool", then its spec: type "mcp", server_label,
# server_url, require_approval and allowed_tools: exactly what is sent to
# the provider.

# Groq offers remote MCP through its Responses API
# (https://console.groq.com/docs/tool-use/remote-mcp). The default "groq"
# provider uses Chat Completions, so register Groq's Responses endpoint as a
# second provider, and copy the model's prices from the registry so the
# ledger stays complete:
register_provider("groq_responses", "https://api.groq.com/openai/v1",
                  api = "responses", key_env = "GROQ_API_KEY")
reg <- model_registry("groq")
row <- reg[reg$model == "openai/gpt-oss-120b", ]
registry_set("groq_responses:openai/gpt-oss-120b", context_window = row$context_window,
             tools = row$tools, input = row$input, cached_input = row$cached_input,
             output = row$output)
tail(model_registry(), 1)[, c("provider", "model", "input", "output")]

r_remote <- chat(
  "Using the deepwiki tool, which function in the R package hfgolino/EGAnet estimates the number of dimensions? Answer in two sentences.",
  model = "groq_responses:openai/gpt-oss-120b", tools = list(dw_remote))
r_remote
length(r_remote$executed_tools)
vapply(r_remote$executed_tools, function(x) if (is.null(x$type)) NA_character_ else x$type,
       character(1))
# EXPECTED OUTPUT: the model's answer, its tokens and cost, and the items the
# provider executed (for example mcp_list_tools and mcp_call entries). Your
# R session did not call DeepWiki: Groq did.

# With require_approval = "always", the provider returns an approval request
# instead of calling the server. This package records it in executed_tools
# but does not send approval responses, so the call does not happen: use it
# to SEE what the model wanted to send.
r_ask <- chat("Using the deepwiki tool, what does the R package tidyverse/ggplot2 do?",
              model = "groq_responses:openai/gpt-oss-120b",
              tools = list(remote_mcp_tool(deepwiki_url, "deepwiki", allowed_tools = "ask_wiki_question",
                                           require_approval = "always")))
str(r_ask$executed_tools, max.level = 2)
# EXPECTED OUTPUT: an item whose type names an approval request, with the
# tool name and the arguments the model wanted to send.
# Recorded run (November 22, 2026): two items, of type "mcp_list_tools" and
# "mcp_approval_request"; the second had name "ask_wiki_question" and, as
# arguments, a question and repoName "tidyverse/ggplot2".


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Ask DeepWiki about a repository you use (for example "tidyverse/dplyr").
# 2. Give repo_helper all of DeepWiki's tools (mcp_as_tools(dw)) and compare the
#    tokens of the first model call with the one-tool version. Why the difference?
# 3. Force the legacy protocol: mcp_connect(deepwiki_url, protocol = "2025-11-25").
#    What changes in the print?


# ==============================================================================
# EXERCISES (solutions in solutions/08b_solutions.R)
# ==============================================================================

# Exercise 1. Measure what a tool menu costs: count the tokens of the JSON
#   Schemas and descriptions of all DeepWiki tools versus ask_wiki_question only
#   (count_tokens() on dw_tools).
# Exercise 2. Write a function that connects, lists the tools and closes the
#   connection, and returns the tool names, or NA with a message when the
#   server cannot be reached. Test it with DeepWiki and with a wrong URL.
# Exercise 3. Build an agent that combines an MCP tool (ask_wiki_question) with a
#   package tool (tool_crossref() from Activity 03): it should find what a
#   package function does AND the DOI of the paper that introduced the method.
# Exercise 4. List the differences between STEP 5 and STEP 7 for a researcher
#   who must document AI use: what does the episode log contain in each case?


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - "Could not reach the MCP server": no network, a firewall, or the server
#   is down. MCP exchanges are not cached; demo from kept results (keep()).
# - The client falls back to 2025-11-25: normal for servers that do not yet
#   speak 2026-07-28. Force a version with protocol = "..." to test.
# - mcp_call() complains about arguments: use the names in the tool's
#   input_schema (STEP 3), not names you expect. Wrap array arguments in
#   list() or I(): a length-one R vector is sent as a single value.
# - "MCP tool '...' left out": the server's schema breaks the specification's
#   rules for x-mcp-header annotations; the client skips the tool, as the
#   specification requires.
# - mcp_stdio(): "needs the processx package", npx not found (install
#   Node.js), or the server exits at once (read the stderr lines in the
#   error message).
# - Remote MCP with the plain "groq" provider: that provider sends Chat
#   Completions requests; register the Responses endpoint (STEP 7).
# - Cost NA for "groq_responses:..." models: copy the prices with
#   registry_set() (STEP 7).
# - A server's tool description or output contains instructions ("ignore
#   your previous instructions..."): that is a prompt injection. Outputs are
#   wrapped as untrusted data, but descriptions are read as they are: read
#   them first, and do not connect agents to servers you do not trust.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. MCP standardizes the plumbing, not the trust: who wrote the server, who
#    runs it, and what does it see?
# 2. Calling a server from R (STEP 5) versus letting the provider call it
#    (STEP 7): which one gives you a complete trace for an AI-use statement?
# 3. A DeepWiki answer is a model's summary of code. How would you verify a
#    claim about what a function does?
# 4. Every tool on the menu costs tokens on every call. When is one more
#    server worth it?
# 5. Stateless (2026-07-28) versus sessions (2025-11-25): why does a stateless
#    protocol make servers easier to scale and to put behind proxies?
