# Activity 00: Setting Up researchAgentR (run once, before the first session)
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: install researchAgentR, store your API keys safely, and confirm that
# every service the course uses answers from your computer.
# OVERVIEW: the package talks to many model providers (Groq, OpenAI,
# Anthropic, Ollama on your laptop, ...) and research APIs (OpenAlex,
# Crossref, ...) through one R interface. This script sets up the keys,
# checks them, shows how models are named ("provider:model"), where prices
# come from, how to change the default models, and how caching lets a demo
# run again offline. Python is NOT needed (one optional step at the end).
#
# researchAgentR functions used here:
#   set_groq_key(), set_openalex_key(), ...   read each key from ~/.Renviron
#   verify_setup()                            check R, packages, keys, services
#   providers(), register_provider()          the model providers the package knows
#   model_registry(), registry_set()          context windows, capabilities, prices
#   parse_model()                             split "provider:model"
#   default_model(), set_default_model()      the default model for each role
#   list_provider_models()                    which model IDs exist TODAY
#   ollama_status()                           is Ollama running, which models are pulled
#   use_cache(), cache_dir(), keep()          record once, replay offline
#   setup_python_env(), activate_python_env() OPTIONAL: local Hugging Face encoders
# TIME: about 30 minutes.  COST: free (Groq, OpenAlex free tiers).
# ==============================================================================


# ==============================================================================
# SETUP: INSTALL THE PACKAGES
# ==============================================================================

# R 4.3.0 or later is recommended (verify_setup() flags older versions), with
# a recent version of RStudio. Open a NEW folder for the course as an RStudio
# project (File > New Project > New Directory), so that results, saved
# episodes and HTML pages land in one place.

# Only what is missing is installed, so running this again is quick.
needed <- c("remotes", "usethis", "httr2", "jsonlite", "callr", "xml2", "pdftools",
            "ggplot2", "processx", "pwr")   # pwr: power analysis for the design agents
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
if (!requireNamespace("researchAgentR", quietly = TRUE)) {
  remotes::install_github("hfgolino/researchAgentR")
}

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")
packageVersion("researchAgentR")

# Copy the activity scripts into your course folder:
file.copy(system.file("activities", package = "researchAgentR"), ".", recursive = TRUE)

use_cache("auto")      # explained in STEP 9; every activity starts with this line

# EXPECTED OUTPUT: the package version (0.1.0 or later), TRUE from
# file.copy(), and a message "Cache mode: auto (<a folder on your computer>)".


# ==============================================================================
# STEP 1: STORE YOUR API KEYS ONCE (NEVER IN YOUR SCRIPTS)
# ==============================================================================

# A key is a password. A key typed into a script ends up in e-mails, GitHub
# repositories and shared folders. Keys live in ONE private file, ~/.Renviron,
# which R reads when it starts. Open it with:

usethis::edit_r_environ()

# Add one line per key (no quotes, no spaces around "="), save the file, and
# RESTART R (Session > Restart R). Lines you do not need can be left out.
#
# FREE keys used in class:
#   GROQ_API_KEY=gsk_...              https://console.groq.com/keys
#                                     (free tier; the course's default models)
#   OPENALEX_API_KEY=...              https://openalex.org (free key; without a
#                                     key OpenAlex allows a much smaller daily budget)
#   TAVILY_API_KEY=tvly-...           https://app.tavily.com (free plan with
#                                     monthly credits; web search for agents)
#   JINA_API_KEY=jina_...             https://jina.ai (free key; embeddings and
#                                     the page reader)
#   RESEARCHAGENTR_EMAIL=you@university.edu
#                                     not a key: your e-mail, sent to Crossref and
#                                     Unpaywall so your requests join their
#                                     "polite pool" (faster, more reliable)
#
# OPTIONAL PAID keys (never required; each activity marks them as optional):
#   OPENAI_API_KEY=sk-...             https://platform.openai.com/api-keys
#   ANTHROPIC_API_KEY=sk-ant-...      https://platform.claude.com
#   TYPESAFE_API_KEY=...              https://console.typesafe.ai/settings/keys (Jev)
#   VOYAGE_API_KEY=pa-...             https://dash.voyageai.com (embeddings;
#                                     check the site for the free starting allowance)
#
# OTHER OPTIONAL keys the package understands (check each site for current
# free-tier terms before relying on them):
#   GEMINI_API_KEY, MISTRAL_API_KEY, CEREBRAS_API_KEY, OPENROUTER_API_KEY,
#   SEMANTIC_SCHOLAR_API_KEY, NCBI_API_KEY, BRAVE_API_KEY
#
# After restarting R, check that R sees a key WITHOUT printing it:
nzchar(Sys.getenv("GROQ_API_KEY"))

# EXPECTED OUTPUT: TRUE. FALSE means the line is missing, misspelled, or R was
# not restarted after saving ~/.Renviron.


# ==============================================================================
# STEP 2: SET THE KEYS FOR THIS SESSION
# ==============================================================================

library(researchAgentR)

# Each set_*_key() reads its environment variable (no argument needed) and
# prints the key MASKED, so you can see which key is in use without exposing it.
set_groq_key()
set_openalex_key()
set_tavily_key()
set_jina_key()

# EXPECTED OUTPUT: one line per key, such as "groq key set (gsk_******abcd)".

# Optional keys: try() turns the error for a key you do not have into a
# message and lets the script continue.
try(set_openai_key())
try(set_anthropic_key())
try(set_typesafe_key())
try(set_voyage_key())
try(set_gemini_key())
try(set_mistral_key())
try(set_cerebras_key())
try(set_openrouter_key())
try(set_semantic_scholar_key())   # optional; keys are approved by hand
try(set_ncbi_key())
try(set_brave_key())

# EXPECTED OUTPUT: "... key set (...)" for keys you stored, and an error
# message "No key found for <service>. Add the line ..." for the others.
# Calling set_*_key() is optional: every function also reads ~/.Renviron
# directly. It is a quick way to confirm which key a session uses.

# The e-mail for the Crossref and Unpaywall polite pool:
nzchar(Sys.getenv("RESEARCHAGENTR_EMAIL"))
# Alternative for one session only:
# options(researchAgentR.email = "you@university.edu")


# ==============================================================================
# STEP 3: CHECK EVERYTHING WITH verify_setup()
# ==============================================================================

# Without test_apis, verify_setup() only looks at your computer: R version,
# packages, which keys are stored (masked), the e-mail, and Ollama.
verify_setup()

# With test_apis = TRUE it also sends ONE tiny request to each service that
# has a key: the model list of each model provider (uses no tokens) and a
# single-record lookup at OpenAlex and Crossref (free).
verify_setup(test_apis = TRUE)

# EXPECTED OUTPUT: a table with columns check, status, detail. Rows "key groq",
# "key openalex", ... say "set" or "not set"; rows "API groq", "API openalex",
# "API crossref" say "OK" or "FAILED: <reason>". "Ollama" says "running" or
# "not running" (Ollama is optional, see STEP 8). Fix every FAILED row that
# belongs to a FREE service before the first session.


# ==============================================================================
# STEP 4: PROVIDERS AND MODEL NAMES ("provider:model")
# ==============================================================================

# A model is always written as "provider:model". The provider decides the
# address (base_url) and the wire format (api): "chat" = OpenAI-compatible
# Chat Completions, "responses" = OpenAI Responses API, "anthropic" =
# Anthropic Messages API. local = TRUE means the data never leave your computer.
providers()

# EXPECTED OUTPUT: a data frame with columns provider, base_url, api, key,
# local; rows include groq, openai, anthropic, gemini, mistral, cerebras,
# openrouter, ollama and lmstudio (the last two with local = TRUE).

# The string is split at the FIRST colon only. This matters for Ollama, whose
# model names contain a colon themselves ("qwen3:4b" = model qwen3, size 4b):
parse_model("ollama:qwen3:4b")
parse_model("groq:openai/gpt-oss-20b")

# EXPECTED OUTPUT: provider "ollama" and model "qwen3:4b" (not "qwen3");
# provider "groq" and model "openai/gpt-oss-20b".

# LM Studio (https://lmstudio.ai) is already registered as "lmstudio"
# (http://localhost:1234/v1). Any other OpenAI-compatible server can be added
# with ONE line. Example: a local llama.cpp server (llama-server listens on
# port 8080 by default):
register_provider("llamacpp", "http://localhost:8080/v1", api = "chat", local = TRUE)
providers()[, c("provider", "base_url", "api", "local")]

# EXPECTED OUTPUT: the same table with a new row "llamacpp".
# Models on it are then written "llamacpp:<model name>". A server that needs a
# key gets key_env = "NAME_OF_THE_VARIABLE" (the key stays in ~/.Renviron).


# ==============================================================================
# STEP 5: THE MODEL REGISTRY (CONTEXT WINDOWS, TOOLS, PRICES)
# ==============================================================================

# The registry lists, for each known model, the context window, whether it
# supports tools and strict schemas, and the LIST PRICE in US dollars per
# million tokens (input, cached input, output). The "source" column is the
# page the price came from and "checked" the date it was checked.
model_registry()[, c("provider", "model", "context_window", "tools", "input", "output", "checked")]
model_registry("groq")

# EXPECTED OUTPUT: one row per model; Ollama rows have price 0 (your own
# computer); Groq rows have a list price even though the free tier bills
# nothing. That list price is a "shadow cost": the ledger records what a run
# WOULD cost at list price, so runs on different providers can be compared,
# and you know what scaling a study up would cost.

# Prices change and new models appear. Add or update a row yourself:
registry_set("ollama:llama3.2:3b", input = 0, output = 0, tools = TRUE)
registry_set("llamacpp:qwen3-4b", input = 0, output = 0, tools = TRUE,
             context_window = 32768)
tail(model_registry(), 2)

# EXPECTED OUTPUT: the two new rows, with "checked" set to today's date.
# A model with no row in the registry still works; its cost is recorded as NA
# (unknown), never as zero.


# ==============================================================================
# STEP 6: DEFAULT MODELS FOR EACH ROLE
# ==============================================================================

# Functions in the package choose a model by ROLE when you do not give one:
#   lead      agents that plan            worker   small, stable jobs
#   judge     graders                     compress summaries
#   embed     embeddings
default_model("lead")
default_model("worker")
default_model("embed")

# EXPECTED OUTPUT: "groq:openai/gpt-oss-120b", "groq:openai/gpt-oss-20b" and
# "ollama:nomic-embed-text". Groq models are the defaults only because they are
# free; any provider works.

# OPTIONAL (paid): switch the worker role to Anthropic or OpenAI, then back.
# set_default_model() returns the previous model, so switching back is one line.
old_worker <- set_default_model("worker", "anthropic:claude-haiku-5-5")
default_model("worker")
set_default_model("worker", "openai:gpt-6-luna")
default_model("worker")
set_default_model("worker", old_worker)
default_model("worker")

# EXPECTED OUTPUT: "anthropic:claude-haiku-5-5", then "openai:gpt-6-luna",
# then the original "groq:openai/gpt-oss-20b". Changing a default sends
# nothing and costs nothing; only a model call made with a paid default would.
# A default set this way lasts for the session. To keep it, add to
# ~/.Rprofile: options(researchAgentR.model.worker = "anthropic:claude-haiku-5-5")


# ==============================================================================
# STEP 7: WHICH MODELS EXIST TODAY?
# ==============================================================================

# Providers retire models (Groq decommissioned its "compound" systems in
# September 2026). The model list endpoint is the reliable source; it uses
# no tokens.
groq_models <- list_provider_models("groq")
groq_models
"openai/gpt-oss-120b" %in% groq_models$id
"openai/gpt-oss-20b" %in% groq_models$id

# EXPECTED OUTPUT: a data frame with columns id, owned_by, context_window, one
# row per model Groq offers today, and TRUE for both gpt-oss models. If one of
# them is FALSE, tell the instructor: the activities need a replacement.


# ==============================================================================
# STEP 8: OPTIONAL: LOCAL MODELS WITH OLLAMA
# ==============================================================================

# Ollama (https://ollama.com) runs open-weight models on your own computer:
# free, offline, and your data never leave the laptop. Activity 11 (Day 3)
# uses Ollama for local EMBEDDINGS ("ask my library"), which are small and
# fast on any laptop. Local chat models are optional: on a laptop they are
# far too slow to drive an agent loop (Activity 11, Step 6, shows the
# instructor's recorded run). Install Ollama, then pull models in a TERMINAL
# (not in R):
#
#   ollama pull nomic-embed-text    274 MB         embeddings ("ask my library")
#   ollama pull all-minilm          46 MB          optional, smaller embeddings
#   ollama pull qwen3:4b            about 2.5 GB   optional local agent demo
#   ollama pull qwen3:8b            about 5.2 GB   optional; 16 GB RAM recommended
#
# Do not pull gpt-oss:120b: it needs about 60 GB of memory.

ollama_status()
list_provider_models("ollama")

# EXPECTED OUTPUT: with Ollama running, a data frame (name, size_gb) with the
# models you pulled, and the same names from list_provider_models("ollama").
# Without Ollama: an error "Failed to connect to localhost port 11434".
# That is fine if you do not plan to use local models.


# ==============================================================================
# STEP 9: CACHING: RECORD ONCE, REPLAY OFFLINE
# ==============================================================================

# Two kinds of caching save REAL results to disk:
#
# 1. use_cache() saves every HTTP response (model calls, API calls) under a
#    fingerprint of the request. Modes:
#      "off"     no caching
#      "auto"    reuse a saved response when the SAME request was sent before,
#                otherwise call the API and save the answer (used in class)
#      "record"  always call the API and save (refreshes the recordings)
#      "replay"  only use saved responses; a request with no recording is an
#                error, so a demo can never touch the network by accident
#    A replayed response costs nothing and is marked cached = TRUE.
use_cache("auto")
cache_dir()

# EXPECTED OUTPUT: the message "Cache mode: auto (...)" and the folder path.

# 2. keep() saves the result of ANY R expression under a name. The first call
#    runs the expression; later calls load the saved result without running it.
slow_result <- keep("setup_test", {
  Sys.sleep(2)          # pretend this is an expensive agent run
  sum(1:10)
})
slow_result <- keep("setup_test", {
  Sys.sleep(2)
  sum(1:10)
})
attr(slow_result, "kept_at")

# EXPECTED OUTPUT: the first call takes about 2 seconds; the second returns
# at once with the message "keep(): loaded 'setup_test' (computed <time>)".
# Both return 55. keep(..., refresh = TRUE) runs the expression again.
#
# Why both? An agent's request contains today's date in its instructions, so
# the SAME agent run on another day sends a different request and use_cache()
# will not find it. keep() stores the whole result under a name you choose,
# so a recorded agent run replays on any day. The activities wrap agent runs
# in keep() for that reason.


# ==============================================================================
# STEP 10: OPTIONAL: PYTHON, ONLY FOR LOCAL HUGGING FACE ENCODERS
# ==============================================================================

# Everything in the course runs in R. The ONLY use of Python is the optional
# "hf:" embedding models (sentence-transformers on your computer) in the
# self-study embeddings activity. Ollama embeddings need no Python. Skip this
# step unless you want them. It needs uv (https://docs.astral.sh/uv/) and the
# reticulate package, and downloads PyTorch (a large download; allow a few minutes).

want_python <- FALSE    # change to TRUE to set up the optional environment
if (want_python) {
  install.packages("reticulate")
  setup_python_env()       # ONCE: creates the environment "st_env"
  activate_python_env()    # every session that uses "hf:" models, before other Python packages
}

# EXPECTED OUTPUT (only with want_python <- TRUE): uv's progress messages, then
# "Done. In each session, call activate_python_env(...)", then "Python: <path>".


# ==============================================================================
# EXERCISES (solutions in solutions/00_solutions.R)
# ==============================================================================

# 1. Which Groq models listed today are NOT in model_registry("groq")? What
#    would the ledger record as their cost?
# 2. Register LM Studio's default server under a second name, "lms2", and
#    write the model string you would use for a model called "qwen3-4b".
#    Check it with parse_model().
# 3. Set the default "judge" model to the free "groq:openai/gpt-oss-20b",
#    confirm it with default_model(), and restore the old value.
# 4. Use keep() to save the output of list_provider_models("groq") under the
#    name "groq_models_today". Why is this useful for documenting a study?


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - "No key found for groq": the line is missing from ~/.Renviron, misspelled
#   (GROQ_API_KEY), has quotes or spaces, or R was not restarted after saving.
# - "FAILED: ... 401 ... Check the API key": the key is wrong, revoked, or was
#   copied with a missing character. Create a new key and replace the line.
# - "FAILED: ... 429 ... Rate limit or quota reached": free tiers allow a few
#   requests per minute and tokens per day. Wait a minute and try again, or
#   lower the request rate: options(researchAgentR.rpm.groq = 20).
# - "FAILED: ... 404 ... Check the model name": the model was retired. Use
#   list_provider_models() and choose a current model.
# - Ollama: "Failed to connect to localhost port 11434": Ollama is
#   not running (open the Ollama app, or run `ollama serve` in a terminal).
# - remotes::install_github() fails with a GitHub rate-limit message: wait an
#   hour or set a GitHub token (usethis::create_github_token()).
# - Behind a university proxy or VPN, some APIs may be blocked: try another
#   network before class.
# - setup_python_env(): "uv is not installed": install uv and restart RStudio.


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. Why keep keys in ~/.Renviron rather than in a script, even a private one?
# 2. The registry records list prices even for free tiers ("shadow cost").
#    Why would a methods section report the cost at list price?
# 3. A model ID you used in October may be retired in December. How would you
#    document the exact model, provider and date of every run in a paper?
# 4. When is a local model (Ollama) the only acceptable option? Think about
#    participant data, IRB protocols and data-use agreements.


# ==============================================================================
# WHICH ACTIVITIES NEED WHAT?
# ==============================================================================

#  Activity                                   Keys (free)              Ollama      Paid (optional)
#  00_Setup_researchAgentR.R                  all you have             optional    -
#  01_Lens_Watch_an_Agent.R                   Groq                     optional    Anthropic or OpenAI (one line)
#  02_Loop_First_Tool_and_Agent.R             Groq                     optional    Anthropic (one line)
#  03_Literature_Scout_and_Verification.R     Groq, OpenAlex, e-mail   no          -
#  04_Graded_Task_Set.R                       Groq, OpenAlex, e-mail   no          -
#  05_Context_Management_and_Compression.R    Groq                     no          -
#  06_Measurement_Finder.R                    Groq, Tavily, Jina (*)   no          -
#  07_Design_Advisor_and_Funding_Scout.R      Groq (NIH, NSF and       no          -
#                                             Grants.gov need no key)
#  08_Multi_Agent_Manuscript_Review.R         Groq                     no          -
#  08b_MCP_Clients.R                          Groq                     no          -
#  09_Evaluation_Harness.R                    Groq                     no          -
#  10_Make_Every_Token_Count.R                 Groq                     no          stronger baseline model
#  11_Small_Models_and_Ask_My_Library.R       Groq                     YES (**)    -
#  12_Screening_Assistant.R                   Groq                     no          TypeSafe (Jev)
#  13_Data_Agent.R                            Groq                     no          -
#  14_Capstone_Team_Brief.R                   Groq, OpenAlex, Tavily   optional    -
#  15_Embeddings_for_Agents.R                 Jina                     optional    Voyage, OpenAI (+ optional Python)
#  16_Create_Your_Own_Skills.R                Groq                     no          -
#  17_Learn_Your_Writing_Style.R              Groq                     optional    -
#  18_Review_Your_Own_Paper.R                 Groq                     no          -
#
#  (*) Jina only for the page reader option; SearXNG (self-hosted, free) can
#      replace Tavily. Each activity's header lists exactly what it needs.
#  (**) Only for local embeddings (nomic-embed-text, 274 MB), which run fast
#      on any laptop; the local agent of Activity 11 is optional.
#  Packages used by several activities: callr (sandboxed R code, Activities
#  07 and 13), pdftools (PDF reading, Activities 11, 17, 18), ggplot2 (figures).
