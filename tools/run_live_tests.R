# run_live_tests.R
# Installs researchAgentR from the local source folder and runs the live smoke
# test against the real services (Groq, OpenAI, Anthropic, OpenAlex, Crossref,
# NIH, NSF, Grants.gov, Tavily, Jina, Voyage, DeepWiki MCP, Ollama if running).
#
# How to run (RStudio): open "LLM Agents Package.Rproj", then in the Console:
#   source("run_live_tests.R")
#
# Keys are read from ~/.Renviron with Sys.getenv(); none are printed or saved.
# Results go to live_logs/<timestamp>/ in this folder. Expected cost: well under
# US$0.50 (most calls use free tiers; OpenAI and Anthropic calls use small models).
# Takes about 10 to 20 minutes, mostly waiting on free-tier rate limits.

stopifnot(getRversion() >= "4.1.0")
if (getRversion() < "4.3.0") message("R ", getRversion(), " works, but R 4.3 or later is recommended.")

if (!file.exists("researchAgentR/DESCRIPTION")) {
  stop("Open 'LLM Agents Package.Rproj' first (or setwd() to the folder that ",
       "contains run_live_tests.R and the researchAgentR folder).")
}

# Reinstalling a package while it is loaded corrupts it for this R session
# ("lazy-load database ... is corrupt"), so the runner needs a fresh session.
if ("researchAgentR" %in% loadedNamespaces()) {
  stop("researchAgentR is already loaded in this R session. Restart R first ",
       "(RStudio: Session > Restart R, or Cmd/Ctrl + Shift + F10), then run ",
       "source(\"run_live_tests.R\") again.", call. = FALSE)
}

readRenviron("~/.Renviron")   # pick up keys added since R started

needed <- c("httr2", "jsonlite", "testthat", "withr", "callr", "processx",
            "xml2", "pdftools", "ggplot2")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
if (utils::packageVersion("httr2") < "1.0.0") install.packages("httr2", repos = "https://cloud.r-project.org")

message("Installing researchAgentR from ./researchAgentR ...")
install.packages("researchAgentR", repos = NULL, type = "source", quiet = TRUE)
library(researchAgentR)
message("researchAgentR ", utils::packageVersion("researchAgentR"), " installed.")

# live_script <- "diag_luna.R" before sourcing this file runs another script in tools/
script <- if (exists("live_script")) live_script else "live_smoke.R"
source(file.path("researchAgentR", "tools", script), echo = FALSE)
