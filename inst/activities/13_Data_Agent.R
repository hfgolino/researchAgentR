# Activity 13: Data Agent
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: Write your own tool for an open-data API, and build a data agent that
# retrieves data and analyzes it with R code it runs in a sandbox, so every
# number in its answer comes from R output you can rerun.
#
# OVERVIEW: Part 1 writes a client for the World Bank Indicators API (free,
# no key) with httr2, and turns it into a tool with define_tool(). The tool
# does NOT paste the data into the model's context: it saves a CSV file in a
# working folder and returns a short summary. Part 2 gives the agent that
# tool plus tool_run_r() on the SAME folder, with the network off: the
# analysis code reads the CSV and needs no internet. Part 3 answers questions
# about a local data frame (a package dataset) the same way, and discusses
# what must never be sent to a cloud model.
#
# researchAgentR functions used here:
#   define_tool()     turn your own R function into a tool (here: an API client)
#   run_r_code()      run R code in a separate, time-limited R process
#   tool_run_r()      the same, as an agent tool sharing a working folder
#   (also: arg_string(), arg_array(), arg_object(), tool_schema(), call_tool(),
#    agent(), run(), budget(), episode_steps(), keep())
# TIME: about 60 minutes (self-study).  COST: free tier (Groq); the World Bank
# API is free and needs no key.
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")

set_groq_key()          # reads GROQ_API_KEY from ~/.Renviron
use_cache("auto")       # caches model calls. The World Bank client below uses
                        # httr2 directly, so its results are cached with keep().

# The working folder shared by the data tool and the R-code tool.
work_dir <- file.path(tempdir(), "data_agent")
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
work_dir


# ==============================================================================
# STEP 1: AN OPEN-DATA API, CALLED FROM R
# ==============================================================================

# The World Bank Indicators API, version 2 (checked 2026-10-11):
# - Documentation: https://datahelpdesk.worldbank.org/knowledgebase/articles/889392
#   and "Basic Call Structure": https://datahelpdesk.worldbank.org/knowledgebase/articles/898581
# - No key: "API keys and other authentication methods are no longer necessary."
# - Calls: https://api.worldbank.org/v2/country/<codes>/indicator/<indicator>
#   Several countries are separated by ";" (BRA;MEX;USA). Query parameters:
#   format=json (the default is XML), date=2015:2024 (a range), per_page
#   (default 50) and page.
# - JSON reply: an array of two elements. The first is page information
#   (page, pages, per_page, total, sourceid, lastupdated); the second is the
#   records, each with indicator {id, value}, country {id, value},
#   countryiso3code, date, value (null when missing), unit, obs_status, decimal.
# - License: most World Bank datasets are CC BY 4.0, with attribution
#   (https://datacatalog.worldbank.org/public-licenses); some datasets use
#   other licenses and are labeled as such.

wb_indicator <- function(country, indicator, years = "2015:2024") {
  get_page <- function(page) {
    req <- httr2::request("https://api.worldbank.org/v2/")
    req <- httr2::req_url_path_append(req, "country", country, "indicator", indicator)
    # I() keeps "2015:2024" as written (otherwise ":" is percent-encoded).
    req <- httr2::req_url_query(req, format = "json", date = I(years),
                                per_page = 1000, page = page)
    req <- httr2::req_user_agent(req, "researchAgentR workshop activity 13")
    req <- httr2::req_timeout(req, 30)
    req <- httr2::req_retry(req, max_tries = 3)          # 429 and 503, with backoff
    httr2::resp_body_json(httr2::req_perform(req), simplifyVector = FALSE)
  }
  body <- get_page(1)
  # An invalid country or indicator returns a message instead of records.
  if (length(body) < 2 || is.null(body[[2]])) {
    msg <- body[[1]]$message
    stop("World Bank API returned no records",
         if (length(msg)) paste0(": ", paste(vapply(msg, function(m)
           paste(m$key, m$value, sep = ": "), character(1)), collapse = "; ")) else "",
         call. = FALSE)
  }
  meta <- body[[1]]
  records <- body[[2]]
  if (meta$pages > 1) {
    for (p in 2:meta$pages) records <- c(records, get_page(p)[[2]])
  }
  value_or_na <- function(x) if (is.null(x) || !length(x)) NA else x   # JSON null -> NA
  out <- data.frame(
    iso3 = vapply(records, function(r) as.character(value_or_na(r$countryiso3code)), character(1)),
    country = vapply(records, function(r) as.character(value_or_na(r$country$value)), character(1)),
    indicator = vapply(records, function(r) as.character(value_or_na(r$indicator$id)), character(1)),
    indicator_name = vapply(records, function(r) as.character(value_or_na(r$indicator$value)), character(1)),
    year = as.integer(vapply(records, function(r) as.character(value_or_na(r$date)), character(1))),
    value = vapply(records, function(r) as.numeric(value_or_na(r$value)), numeric(1)),
    stringsAsFactors = FALSE)
  out <- out[order(out$iso3, out$year), ]
  rownames(out) <- NULL
  # Missing values stay NA: they are counted and reported, never filled in.
  attr(out, "missing") <- sum(is.na(out$value))
  attr(out, "lastupdated") <- meta$lastupdated
  attr(out, "source") <- "World Bank, World Development Indicators (CC BY 4.0)"
  attr(out, "retrieved_at") <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  out
}

# keep() saves the result the first time; later runs load it (offline, free).
# Use keep(..., refresh = TRUE) to download a newer vintage.
pop <- keep("wb_SP_POP_TOTL_USA_2015_2024",       # the same name the tool uses in STEP 2
            wb_indicator("USA", "SP.POP.TOTL", "2015:2024"))
pop
attributes(pop)[c("missing", "lastupdated", "source", "retrieved_at")]
# EXPECTED OUTPUT: one row per year (2015 to 2024) with iso3 USA, the country
# name, the indicator id and name ("Population, total"), the year and the
# value; then the number of missing values, the data's last update date, the
# source and the retrieval time.


# ==============================================================================
# STEP 2: THE API AS A TOOL THAT SAVES A FILE, NOT A WALL OF NUMBERS
# ==============================================================================

# Design choice: the model never needs the raw table in its context. The tool
# saves the CSV in the working folder of run_r and returns a summary; the
# model then writes R code that reads the file. Smaller context, lower cost,
# and the numbers come from R, not from the model's memory.
world_bank <- function(country, indicator, years) {
  if (!grepl("^\\d{4}:\\d{4}$", years)) stop("years must look like '2015:2023'")
  if (length(strsplit(country, ";", fixed = TRUE)[[1]]) > 10) stop("at most 10 countries per call")
  name <- gsub("[^A-Za-z0-9]+", "_", paste(indicator, country, years))
  d <- keep(paste0("wb_", name), wb_indicator(country, indicator, years))
  file <- paste0(name, ".csv")
  utils::write.csv(d, file.path(work_dir, file), row.names = FALSE, fileEncoding = "UTF-8")
  paste0("Saved ", nrow(d), " rows to '", file, "' in the run_r working folder ",
         "(read it with read.csv('", file, "', fileEncoding = 'UTF-8')). Columns: ", paste(names(d), collapse = ", "),
         ". Indicator: ", d$indicator_name[1], ". Countries: ", paste(unique(d$iso3), collapse = ", "),
         ". Years: ", min(d$year), " to ", max(d$year), ". Missing values: ", attr(d, "missing"),
         ". Source: ", attr(d, "source"), ", last updated ", attr(d, "lastupdated"), ".")
}

wb_tool <- define_tool(
  world_bank, name = "world_bank",
  description = paste(
    "Download a World Bank indicator for one or more countries and years, and save it as a",
    "CSV file in the working folder of run_r. Returns the file name and a summary, not the",
    "data: analyze the file with run_r. Common indicators: SP.POP.TOTL (population, total),",
    "NY.GDP.PCAP.CD (GDP per capita, current US$), SP.DYN.LE00.IN (life expectancy at birth,",
    "years), SE.XPD.TOTL.GD.ZS (government expenditure on education, % of GDP)."),
  args = list(
    country = arg_string("ISO3 country codes separated by ';', for example 'BRA;MEX;USA'"),
    indicator = arg_string("A World Bank indicator code, for example 'SP.DYN.LE00.IN'"),
    years = arg_string("A year range 'YYYY:YYYY', for example '2015:2023'")),
  timeout = 90, max_output_tokens = 400)
wb_tool
tool_schema(wb_tool)
# EXPECTED OUTPUT: a tool named world_bank with three required string
# arguments (country, indicator, years), marked "output untrusted".

# Call it by hand, as the agent loop will:
res <- call_tool("world_bank", list(wb_tool),
                 arguments = list(country = "USA", indicator = "SP.POP.TOTL", years = "2015:2024"))
cat(res$output)
list.files(work_dir)
# EXPECTED OUTPUT: "Saved <one row per year> rows to 'SP_POP_TOTL_USA_2015_2024.csv' ..." in
# <tool_output> tags, and the CSV file in the working folder. Without network
# access (and nothing kept), the output is an error observation instead
# ("Error while running world_bank: ..."): the agent can read it and adapt.

# A bad argument is caught before any request is made:
cat(call_tool("world_bank", list(wb_tool),
              arguments = list(country = "USA", indicator = "SP.POP.TOTL", years = "last ten"))$output)
# EXPECTED OUTPUT: "Error while running world_bank: years must look like '2015:2023'".


# ==============================================================================
# STEP 3: THE ANALYSIS RUNS IN A SANDBOX WITHOUT NETWORK
# ==============================================================================

# The R-code tool shares work_dir, so code can read the CSV the data tool
# saved. allow_network = FALSE (the default): the analysis needs no internet,
# and code written by a model cannot send your data anywhere through the
# ordinary web functions (a soft barrier: see Activity 07).
r_tool <- tool_run_r(dir = work_dir, timeout = 60, allow_network = FALSE)

# Try the sandbox directly on the file from STEP 2 (if it was saved):
csv_files <- list.files(work_dir, pattern = "\\.csv$")
if (length(csv_files)) {
  print(run_r_code(paste0("d <- read.csv('", csv_files[1], "', fileEncoding = 'UTF-8')\n",
                          "summary(d$value)\nnrow(d)"), dir = work_dir))
}
# EXPECTED OUTPUT: summary statistics of the value column and the number of rows.


# ==============================================================================
# STEP 4: THE DATA AGENT
# ==============================================================================

data_schema <- list(
  answer = arg_string("The answer in two to four sentences, with the numbers"),
  numbers = arg_array("Every number used in the answer, copied from the run_r output",
    items = arg_object("One number", fields = list(
      quantity = arg_string("What the number is, for example 'change in life expectancy, Brazil'"),
      value = arg_string("The value exactly as R printed it"),
      unit = arg_string("Unit, for example 'years'")))),
  r_code = arg_string("The final R code that produced the numbers; it reads the CSV file(s)"),
  data_files = arg_array("CSV files used", items = arg_string("A file name")),
  caveats = arg_string("Missing values, indicator definitions and data vintage"))

data_agent <- agent(
  name = "data_agent",
  model = "groq:openai/gpt-oss-120b",
  description = "Retrieves World Bank indicators and analyzes them with R.",
  instructions = paste(
    "You answer quantitative questions with World Bank data. First call world_bank to save",
    "the data as a CSV file; then write R code that reads the file with read.csv(file, fileEncoding = 'UTF-8') and",
    "prints every result you need, and run it with run_r. Never state a number that R did",
    "not print, and never type data values into code. Keep missing values as NA and report",
    "how many there are. The final r_code must reproduce all the numbers on its own."),
  tools = list(wb_tool, r_tool),
  output_schema = data_schema)
data_agent

ep_data <- run(data_agent,
  paste("How did life expectancy at birth change in Brazil, Mexico and the United States",
        "between 2015 and 2023? Give each country's change in years and say which changed most."),
  budget = budget(max_steps = 8, max_cost = 0.03, max_seconds = 300))
ep_data
episode_steps(ep_data)[, c("step", "type", "name", "is_error", "detail")]
# EXPECTED OUTPUT: a world_bank call, one or more run_r calls (an error is
# fine: the agent reads it and fixes its code), then a final answer.

ans <- ep_data$answer
cat(ans$answer, "\n")
do.call(rbind, lapply(ans$numbers, as.data.frame))
cat(ans$caveats, "\n")
# EXPECTED OUTPUT: the answer, one row per number (quantity, value, unit)
# and the caveats (for example missing years and the data's last update).

# --- Verify: rerun the agent's code and look for each number in the output ----
check <- run_r_code(ans$r_code, dir = work_dir)
check
numbers <- do.call(rbind, lapply(ans$numbers, as.data.frame))
numbers$found_in_rerun <- vapply(numbers$value, function(v)
  grepl(v, check$output, fixed = TRUE), logical(1))
numbers
# EXPECTED OUTPUT: the code runs again without error, and found_in_rerun is
# TRUE for every number. A FALSE means the model rounded, reformatted or
# invented a number: compare by hand and report it.

# An independent check in your own session (no model involved):
le <- read.csv(file.path(work_dir, unlist(ans$data_files)[1]), fileEncoding = "UTF-8")
le_wide <- reshape(le[le$year %in% c(2015, 2023), c("iso3", "year", "value")],
                   idvar = "iso3", timevar = "year", direction = "wide")
le_wide$change <- le_wide$value.2023 - le_wide$value.2015
le_wide
# EXPECTED OUTPUT: one row per country with the 2015 and 2023 values and the
# change; NA where a year is missing (then the agent's answer must say so).


# ==============================================================================
# STEP 5: QUESTIONS ABOUT A LOCAL DATA FRAME
# ==============================================================================

# The same pattern works for any data frame: save it in the working folder,
# and let the agent analyze it with code. The model sees ONLY what the code
# prints. The extraversion dataset (n = 120) contains texts generated by a
# language model, so sending its printed output to a cloud model is fine.
data(extraversion, package = "researchAgentR")
write.csv(extraversion[, c("doc_id", "subtopic", "text")],
          file.path(work_dir, "extraversion.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# The sandbox alone, offline:
run_r_code("d <- read.csv('extraversion.csv', fileEncoding = 'UTF-8')\ntable(d$subtopic)", dir = work_dir)
# EXPECTED OUTPUT: the number of texts per subtopic (Assertive, Energetic,
# Friendly, Positive).

local_agent <- agent(
  name = "local_data_agent",
  model = "groq:openai/gpt-oss-120b",
  instructions = paste(
    "Answer questions about the data file extraversion.csv (columns doc_id, subtopic, text)",
    "in the working folder, by running R code with run_r. Print aggregates (counts, means,",
    "tables), never more than 3 raw texts. Report numbers exactly as R printed them."),
  tools = list(r_tool))
ep_local <- run(local_agent,
  "How many texts are there per subtopic, and which subtopic has the longest texts on average, in words?",
  budget = budget(max_steps = 6, max_cost = 0.02))
ep_local
cat(ep_local$answer, "\n")
# EXPECTED OUTPUT: run_r calls that count texts and average word counts by
# subtopic, and an answer naming the subtopic with the longest texts.

# Verify in your own session:
table(extraversion$subtopic)
words <- lengths(strsplit(trimws(extraversion$text), "\\s+"))
sort(tapply(words, extraversion$subtopic, mean), decreasing = TRUE)
# EXPECTED OUTPUT: 30 texts per subtopic, and the mean number of words per
# subtopic, largest first. Compare with the agent's answer.

# PARTICIPANT DATA: never give a cloud-model agent a file with participant
# data. Whatever the code prints (a head(), an error message that quotes a
# row, a summary of free-text answers) is sent to the provider. IRB protocols
# and data use agreements often forbid it outright. Use a local model
# (Ollama, Activity 11), de-identified data, or no agent at all.


# ==============================================================================
# TRY IT
# ==============================================================================

# 1. Ask the data agent about GDP per capita (NY.GDP.PCAP.CD) for five
#    countries of your choice. Does it mention that the values are in current
#    US dollars (not adjusted for inflation)?
# 2. Ask a question whose answer is not in the data (for example a year after
#    the last update). Does the agent say so, or does it guess?
# 3. Rerun the data agent with the same question. Are the numbers identical?
#    (They should be: they come from R, not from the model.)


# ==============================================================================
# EXERCISES (solutions in solutions/13_solutions.R)
# ==============================================================================

# Exercise 1. With wb_indicator() and run_r_code(), plot GDP per capita
#   (NY.GDP.PCAP.CD) for Brazil, India and South Africa, 2010 to 2023, as a PNG
#   saved in work_dir by the sandbox. Check that the file exists.
# Exercise 2. Ask the data agent a question that needs TWO indicators (for
#   example the correlation between GDP per capita and life expectancy across
#   ten countries in 2022), then verify the correlation in your own session.
# Exercise 3. Write a "leak detector": after ep_local, check in code whether
#   any tool output contains a full raw text from extraversion$text. How many
#   texts reached the model?
# Exercise 4. Make the world_bank tool record its own provenance: add the
#   request URL to the summary it returns, and explain why that helps the
#   AI-use statement.


# ==============================================================================
# WHAT CAN GO WRONG
# ==============================================================================

# - "World Bank API returned no records: ...": an invalid country code or
#   indicator; the message comes from the API. The agent sees it as an error
#   observation and can correct the code.
# - Values are NA: the indicator is not yet available for recent years (data
#   lag one or two years). They are kept as NA and counted, never filled in.
# - keep() returns an old download: delete the kept file or use
#   keep(..., refresh = TRUE); the attribute lastupdated tells the vintage.
# - The run_r code cannot find the CSV: the agent must use the file name the
#   tool returned, and both tools must share work_dir.
# - The analysis code tries to download data itself: it fails, because
#   allow_network = FALSE. That is intended; the data come from the tool.
# - A number in the answer is not in the rerun output (found_in_rerun FALSE):
#   the model rounded or invented it. Trust the R output.
# - Free-tier limits: keep the tool output short (this tool returns about 100
#   tokens instead of the whole table).


# ==============================================================================
# DISCUSSION POINTS
# ==============================================================================

# 1. Why save a file and return a summary instead of returning the data? Think
#    of context size, cost, and who computes the numbers.
# 2. Every number in the answer can be regenerated from r_code and the CSV
#    file. What else would a reviewer need to reproduce the result (data
#    vintage, indicator definitions, package versions)?
# 3. Open data come with licenses. World Bank data are mostly CC BY 4.0: what
#    does attribution look like in a paper that used this agent?
# 4. The sandbox has no network, but the model provider sees everything the
#    code prints. Where is the real privacy boundary in this design?
# 5. When would you skip the agent and write the ten lines of R yourself?
