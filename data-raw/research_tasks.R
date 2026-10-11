# Builds data/research_tasks.rda. Install the current source first, then run
# from the package root:
#   R CMD INSTALL .
#   Rscript -e 'library(researchAgentR); source("data-raw/research_tasks.R")'
# (An installed package keeps no source references, so the graders saved in
# the data stay small; devtools::load_all() would store the source with them.)
#
# Rules for this benchmark:
# * Every fixed answer is a fact checked against the article itself. The four
#   DOIs (two in plain text, two as hashes) were read from the PDFs of the
#   published articles.
# * Tasks without a fixed answer are graded by code against a live API at
#   grading time (NIH RePORTER), so no answer is invented here.
# * Held-out answers appear only as hashes, here and in the data, so a
#   browsing agent that finds this repository cannot read them. Each hash is
#   hash_answer(<DOI read from the article's PDF>, type = "doi"); anyone with
#   the article can check it, for example
#   hash_answer("<the DOI printed on the article>", type = "doi"), or with
#   `printf "<doi in lowercase>" | md5sum` in a terminal.

# The live NIH grader is internal (it depends on another module at grading time).
grader_nih_fy <- utils::getFromNamespace(".grader_nih_fy", "researchAgentR")

title_aigenie <- paste("Generative psychometrics via AI-GENIE: Automatic item generation and",
                       "validation with network-integrated evaluation")
title_dynega <- paste("Modeling Latent Topics in Social Media using Dynamic Exploratory Graph",
                      "Analysis: The Case of the Right-wing and Left-wing Trolls in the 2016 US Elections")
title_entropy <- paste("Entropy Fit Indices: New Fit Measures for Assessing the Structure and",
                       "Dimensionality of Multiple Latent Variables")
title_ega_sim <- paste("Investigating the Performance of Exploratory Graph Analysis and Traditional",
                       "Techniques to Identify the Number of Latent Factors: A Simulation and Tutorial")

doi_task <- function(title, journal, year) {
  sprintf("Return the DOI of the article titled \"%s\" (%s, %d). Reply with the DOI only.",
          title, journal, year)
}
nih_task <- function(topic) {
  sprintf(paste("Find one research project funded by the US National Institutes of Health in",
                "fiscal year 2025 whose title or abstract mentions \"%s\". Return its NIH project",
                "number (activity code, institute code and serial number, for example in the",
                "form of the letter R, two digits, two letters and six digits)."), topic)
}

research_tasks <- task_set(
  id = c("doi_dynega", "doi_entropy", "journal_entropy", "nih_ema_2025",
         "doi_aigenie", "doi_ega_sim", "nih_irt_2025"),
  instruction = c(
    doi_task(title_dynega, "Psychometrika", 2022),
    doi_task(title_entropy, "Multivariate Behavioral Research", 2020),
    sprintf("Which journal published the 2020 article titled \"%s\"? Reply with the journal name only.",
            title_entropy),
    nih_task("ecological momentary assessment"),
    doi_task(title_aigenie, "Behavior Research Methods", 2026),
    doi_task(title_ega_sim, "Psychological Methods", 2020),
    nih_task("item response theory")
  ),
  grader = list(
    grader_doi(), grader_doi(), grader_exact(),
    grader_nih_fy(2025, "ecological momentary assessment"),
    grader_doi(), grader_doi(),
    grader_nih_fy(2025, "item response theory")
  ),
  answer = list(
    "10.1007/s11336-021-09820-y",
    "10.1080/00273171.2020.1779642",
    "Multivariate Behavioral Research",
    NULL,
    "md5:245c6ad9798323b7345293a29e1c7d5f",   # Behavior Research Methods (2026) article
    "md5:7031ea26d741d220d0b71a5c2ef3ade6",   # Psychological Methods (2020) article
    NULL
  ),
  split = c("dev", "dev", "dev", "dev", "heldout", "heldout", "heldout"),
  type = c("doi", "doi", "journal", "nih_live", "doi", "doi", "nih_live"),
  source = c("PDF of the article", "PDF of the article", "PDF of the article",
             "NIH RePORTER API at grading time", "PDF of the article", "PDF of the article",
             "NIH RePORTER API at grading time")
)

usethis_available <- requireNamespace("usethis", quietly = TRUE)
if (usethis_available) {
  usethis::use_data(research_tasks, overwrite = TRUE)
} else {
  save(research_tasks, file = "data/research_tasks.rda", compress = "bzip2")
}
