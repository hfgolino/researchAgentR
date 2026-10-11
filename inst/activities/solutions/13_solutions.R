# Solutions: Activity 13 (Data Agent)
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Run Activity 13 first: these solutions use wb_indicator(), world_bank(),
# wb_tool, r_tool, data_agent, work_dir, extraversion and ep_local from it.
# Exercises 1 and 4 need the World Bank API (network, no key); Exercise 2 also
# needs a Groq key; Exercise 3 uses ep_local.

library(researchAgentR)
use_cache("auto")


# ==============================================================================
# Exercise 1: a plot made in the sandbox from downloaded data
# ==============================================================================

gdp <- keep("wb_NY_GDP_PCAP_CD_BRA_IND_ZAF_2010_2023",
            wb_indicator("BRA;IND;ZAF", "NY.GDP.PCAP.CD", "2010:2023"))
write.csv(gdp, file.path(work_dir, "gdp.csv"), row.names = FALSE)

plot_code <- "
d <- read.csv('gdp.csv')
png('gdp.png', width = 800, height = 500)
plot(NA, xlim = range(d$year), ylim = range(d$value, na.rm = TRUE),
     xlab = 'Year', ylab = 'GDP per capita (current US$)')
countries <- unique(d$iso3)
for (k in seq_along(countries)) {
  rows <- d$iso3 == countries[k]
  lines(d$year[rows], d$value[rows], type = 'b', col = k, pch = k)
}
legend('topleft', legend = countries, col = seq_along(countries), pch = seq_along(countries), lty = 1)
invisible(dev.off())
cat('missing values:', sum(is.na(d$value)), '\n')
"
r1 <- run_r_code(plot_code, dir = work_dir)
r1
file.exists(file.path(work_dir, "gdp.png"))
# EXPECTED OUTPUT: status ok, the number of missing values, the files in the
# working folder (gdp.csv, gdp.png and earlier files), and TRUE.


# ==============================================================================
# Exercise 2: two indicators, one correlation, verified in the session
# ==============================================================================

ep2 <- run(data_agent,
  paste("Across Argentina, Brazil, Chile, Colombia, India, Indonesia, Mexico, Nigeria,",
        "South Africa and the United States, what is the Pearson correlation between GDP per",
        "capita (current US$) and life expectancy at birth in 2022?"),
  budget = budget(max_steps = 10, max_cost = 0.04, max_seconds = 300))
ep2
cat(ep2$answer$answer, "\n")

files <- unlist(ep2$answer$data_files)
both <- do.call(rbind, lapply(file.path(work_dir, files), read.csv))
both <- both[both$year == 2022, c("iso3", "indicator", "value")]
wide <- reshape(both, idvar = "iso3", timevar = "indicator", direction = "wide")
wide
cor(wide[["value.NY.GDP.PCAP.CD"]], wide[["value.SP.DYN.LE00.IN"]], use = "complete.obs")
sum(!complete.cases(wide))
# EXPECTED OUTPUT: the agent's answer, the ten countries with both values,
# the correlation computed in your session (it must equal the agent's), and
# the number of countries dropped for missing values (the answer must say it).


# ==============================================================================
# Exercise 3: a leak detector for raw texts
# ==============================================================================

# ep_local$messages holds exactly what was sent to the model, including every
# tool output. Search it for the start of each raw text.
tool_messages <- Filter(function(m) identical(m$role, "tool"), ep_local$messages)
sent <- paste(vapply(tool_messages, function(m) paste(as.character(m$content), collapse = "\n"),
                     character(1)), collapse = "\n")
leaked <- vapply(extraversion$text, function(t) grepl(substr(t, 1, 40), sent, fixed = TRUE),
                 logical(1), USE.NAMES = FALSE)
sum(leaked)
extraversion$text[leaked]
# EXPECTED OUTPUT: the number of raw texts that reached the model (the
# instructions allowed at most 3) and the texts themselves. With participant
# data, any number above 0 is a disclosure.


# ==============================================================================
# Exercise 4: provenance in the tool output
# ==============================================================================

world_bank_traced <- function(country, indicator, years) {
  url <- paste0("https://api.worldbank.org/v2/country/", country, "/indicator/", indicator,
                "?format=json&date=", years)
  paste0(world_bank(country, indicator, years), " Request: ", url,
         " Retrieved: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), ".")
}
wb_tool_traced <- define_tool(
  world_bank_traced, name = "world_bank",
  description = paste(wb_tool$description, "The output includes the request URL and the retrieval time."),
  args = wb_tool$args, timeout = 90, max_output_tokens = 500)
cat(call_tool("world_bank", list(wb_tool_traced),
              arguments = list(country = "CHL", indicator = "SP.POP.TOTL", years = "2020:2023"))$output)
# EXPECTED OUTPUT: the usual summary followed by the request URL and the
# retrieval time. The URL and time are now part of the episode log, so the
# AI-use statement can say exactly which data were retrieved, from where and
# when, and a reader can repeat the request.
