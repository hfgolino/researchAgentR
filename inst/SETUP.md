# Software Setup Guide: LLM Agents for Research Using R

Updated November 23, 2026 · Hudson Golino, University of Virginia

Complete the five steps below before the first session on December 2, 2026. They take about one hour, and everything that is required is free.

## What you need

Seven items are required and all of them are free. Two more are recommended. Python is not needed.

| Item | Needed? | Cost | Time |
| --- | --- | --- | --- |
| R, version 4.3 or later (current release: 4.6.2) | Required | Free | 5 minutes |
| RStudio Desktop | Required | Free (open source) | 5 minutes |
| R packages: the course package `researchAgentR` and ten others | Required | Free | 10 minutes |
| Groq API key (runs the course's language models) | Required | Free plan | 5 minutes |
| OpenAlex API key (literature search) | Required | Free | 3 minutes |
| Tavily API key (web search) | Required | Free, no credit card | 3 minutes |
| Your e-mail address, saved next to the keys | Required | Free | 1 minute |
| Groq Developer plan (pay as you go) | Recommended | Under US$1 for the whole course in the instructor's test | 5 minutes |
| Ollama with one small embedding model (274 MB) | Recommended | Free | 10 minutes |
| OpenAI and Anthropic keys, other keys | Optional | Prepaid credit; US$5 is far more than the optional steps use | 10 minutes each |

A laptop with Windows or macOS and a stable internet connection is enough. No graphics card is needed: the models run on the providers' servers.

## Step 1. Install R and RStudio

Install R first, then RStudio. If both are already installed, type `R.version.string` in the RStudio console: version 4.3.0 or later is fine, and an older version should be updated.

**R (current release: 4.6.2)**

1. Go to [cran.r-project.org](https://cran.r-project.org/).
2. Windows: click **Download R for Windows**, then **base**, then **Download R-4.6.2 for Windows** (88 MB, for Windows 10 or later). Run the installer and accept the default choices.
3. macOS: click **Download R for macOS** and pick the installer for your chip. Apple menu, **About This Mac** shows it: "Chip: Apple M..." is Apple silicon, "Processor: Intel" is Intel.
    - Apple silicon: `R-4.6.2-arm64.pkg` (needs macOS 14 Sonoma or later).
    - Intel: `R-4.6.2-x86_64.pkg` (needs macOS 11 Big Sur or later).
4. Open the downloaded file and follow the installer.

On an Apple silicon Mac with macOS 13 or earlier the current installer does not run. The same page links to a folder of earlier builds (`big-sur-arm64`): any R from 4.3 on is enough for the course.

**RStudio Desktop (current release: 2026.11)**

1. Go to the [RStudio download page](https://docs.posit.co/ide/user/download.html) and download the installer for your system.
2. Windows: run the `.exe` file. macOS: open the `.dmg` file and drag RStudio to the Applications folder.
3. Open RStudio. The console (bottom left) shows the R version in its first line.

The current RStudio lists Windows 11 and macOS 15 or later. On an older system, install an earlier release from the [list of supported versions](https://docs.posit.co/supported-versions/rstudio.html). The course does not depend on any recent RStudio feature.

**A folder for the course**

In RStudio choose **File**, **New Project**, **New Directory**, **New Project**, and name it, for example, `llm-agents-course`. Open this project every time you work on the course, so that scripts, results and saved runs stay in one place.

## Step 2. Install the R packages

Paste the lines below into the RStudio console, with the course project open. They install ten packages from CRAN and the course package, `researchAgentR`, from GitHub.

```r
needed <- c("remotes", "usethis", "httr2", "jsonlite", "callr", "xml2", "pdftools",
            "ggplot2", "processx", "pwr")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

remotes::install_github("hfgolino/researchAgentR", upgrade = "never")

library(researchAgentR)
packageVersion("researchAgentR")
```

The last line should print 0.1.5 or a later version.

- If R asks "Do you want to install from sources the package which needs compilation?", answer **no**.
- If Windows asks whether to create a personal library, answer **yes**.
- No compiler is needed (no Rtools on Windows, no Xcode on macOS): `researchAgentR` is written in R only.

Then copy the course scripts into your project folder:

```r
file.copy(system.file("activities", package = "researchAgentR"), ".", recursive = TRUE)
```

This prints `TRUE` and creates a folder `activities` with the 20 numbered scripts of the course and a `solutions` folder.

Optional: `install.packages("mirai")` lets one activity of Day 2 run several agents at the same time. Without it they run one after another.

## Step 3. Get your API keys

Three free keys are required: Groq, OpenAlex and Tavily. An API key is a password that lets R use a service in your name. Copy each key as soon as it is shown and keep it in a private note until Step 4. Never paste a key into a script, an e-mail or a chat.

| Service | Used for | Sign up | Free allowance |
| --- | --- | --- | --- |
| Groq | The language models of every activity (`openai/gpt-oss-120b` and `openai/gpt-oss-20b`) | [console.groq.com](https://console.groq.com/) | Free plan with limits per minute and per day (see "Free plans, limits and optional credit") |
| OpenAlex | Searching the scholarly literature (Days 1, 2 and 3) | [openalex.org/signup](https://openalex.org/signup) | US$1 of requests per day, which is 1,000 searches; lookups by DOI are free |
| Tavily | Web search for agents (Days 2 and 3) | [app.tavily.com](https://app.tavily.com/) | 1,000 credits per month, no credit card; one search costs 1 credit |

**Groq**

1. Go to [console.groq.com](https://console.groq.com/) and create an account.
2. Open [console.groq.com/keys](https://console.groq.com/keys) and click **Create API Key**. Give it a name, for example `agents-course`.
3. Copy the key. It starts with `gsk_`.

**OpenAlex**

1. Create an account at [openalex.org/signup](https://openalex.org/signup). It takes about 30 seconds.
2. Copy your key from [openalex.org/settings/api-key](https://openalex.org/settings/api-key).

Create this account a few days before the course. A new free account spends its first US$1 at no more than US$0.10 every 5 minutes (about 100 searches), and the daily budget starts again at midnight UTC.

**Tavily**

1. Create an account at [app.tavily.com](https://app.tavily.com/).
2. Copy your API key from the dashboard that opens after signing in. It starts with `tvly-`.

**Your e-mail address**

No sign-up is needed. Crossref and Unpaywall, two free services the course uses to check references, answer faster and more reliably when a request carries a contact e-mail. Step 4 saves yours.

## Step 4. Save the keys in .Renviron

The keys go into one private file, `.Renviron`, which R reads every time it starts. The course scripts then find them without ever showing them.

1. In the RStudio console, run:

    ```r
    usethis::edit_r_environ()
    ```

2. A file opens in RStudio. Add these four lines, with your own keys and address:

    ```
    GROQ_API_KEY=gsk_your_key_here
    OPENALEX_API_KEY=your_key_here
    TAVILY_API_KEY=tvly-your_key_here
    RESEARCHAGENTR_EMAIL=you@university.edu
    ```

3. Check the format: one key per line, no quotation marks, no spaces around `=`. Press Enter after the last line.
4. Save the file, then restart R: **Session**, **Restart R**.
5. Confirm that R sees a key, without printing it:

    ```r
    nzchar(Sys.getenv("GROQ_API_KEY"))
    ```

    The answer should be `TRUE`. `FALSE` means the line is missing or misspelled, or R was not restarted.

The file is in your home folder (on Windows, the Documents folder). Do not share it, and do not copy it into a folder that syncs to other people.

## Step 5. Check your setup

One function checks R, the packages, the keys and the services. Run:

```r
library(researchAgentR)
verify_setup(test_apis = TRUE)
```

It prints a table with the columns `check`, `status` and `detail`. Keys are shown masked (for example `gsk_******abcd`). The test sends a one-line chat to Groq and looks up one record at OpenAlex and at Crossref: free, and done in a few seconds.

| Row | Status it should show |
| --- | --- |
| `R version` | `OK` |
| `package httr2`, `package jsonlite`, `package callr`, `package xml2`, `package pdftools`, `package ggplot2`, `package processx` | `OK` |
| `key groq`, `key openalex`, `key tavily` | `set` |
| `e-mail for Crossref/Unpaywall` | `set` |
| `API groq` and `API groq chat` | `OK` |
| `API openalex` and `API crossref` | `OK` |
| `Ollama` | `running` if you installed it; otherwise "not running", which is fine |
| Keys you did not get (`key openai`, `key anthropic` and others) | `not set`, which is fine |

A row that says `FAILED` gives the reason next to it. See "If something goes wrong" below.

**Test the web search** (uses 1 of your 1,000 Tavily credits):

```r
web_search("exploratory graph analysis", n = 3)[, c("title", "url")]
```

Three rows with a title and a web address mean that the Tavily key works.

**Run the setup activity** (recommended, about 30 minutes): open `activities/00_Setup_researchAgentR.R` in RStudio and run it line by line with Ctrl+Enter (Cmd+Return on macOS). It repeats the checks above and explains how models are named, where prices come from, and how results are saved and replayed. Lines for services you did not sign up for print "No key found for ...": that is expected.

## Free plans, limits and optional credit

Nothing has to be bought. The one upgrade worth making is Groq's pay-as-you-go plan: one complete pass through all 20 activities cost US$0.24 at list prices in the instructor's test of November 22, 2026.

**What the Groq Free plan allows**

Groq's documentation lists the same limits for both course models: 30 requests per minute, 1,000 requests per day, 8,000 tokens per minute and 200,000 tokens per day. Your own limits are shown at [console.groq.com/settings/limits](https://console.groq.com/settings/limits).

An agent sends its whole conversation again at every step, so one step often uses 3,000 to 7,000 tokens. When the limit per minute is reached, the course package waits and tries again by itself: a run that takes seconds on the paid plan can take several minutes on the Free plan. The table shows the tokens of one pass in the instructor's test.

| Part of the course | Tokens on `gpt-oss-120b` | Tokens on `gpt-oss-20b` |
| --- | --- | --- |
| Day 1 (activities 00 to 04) | 304,055 | 26,442 |
| Day 2 (activities 05 to 08b) | 237,852 | 208,291 |
| Day 3 (activities 09 to 11) | 99,937 | 430,503 |
| Self-study (activities 12 to 18) | 121,939 | 266,121 |

Groq does not count cached tokens toward its limits, so the real margin is somewhat larger than these numbers suggest. Even so, following a whole class day live is likely to reach the daily limit of the Free plan.

**Recommended: the Groq Developer plan (pay as you go)**

1. In the Groq console open **Settings**, **Billing** ([console.groq.com/settings/billing](https://console.groq.com/settings/billing)) and add a payment method. The upgrade takes effect at once.
2. Set a spending limit there, for example US$5, and a usage alert.
3. After the course you can go back to the Free plan from the same page.

There is no charge for upgrading and no credit to buy in advance. Groq bills after use, and only once usage has reached at least US$0.50. List prices are US$0.15 per million input tokens and US$0.60 per million output tokens for `gpt-oss-120b`, and half of that for `gpt-oss-20b`.

**If you stay on the Free plan**

You can still take part. Run the shorter steps yourself during the sessions, watch the longer ones on the instructor's screen, and run them after class. Every finished run is saved on your computer, so repeating a script does not use your limits twice.

**OpenAlex and Tavily**

The free allowances are far above what the course uses. OpenAlex gives 1,000 searches a day. Tavily gives 1,000 searches a month, and the instructor's complete pass made fewer than 20.

## Recommended and optional extras

Only the first item is used in class. Skip the rest unless you want to try the steps that the scripts mark as optional.

**Recommended: Ollama, for searching your own documents privately (free)**

Ollama runs small models on your own laptop, so your texts never leave it. Two class activities (Day 2 and Day 3) use it to search documents by meaning. The model needed is small (274 MB) and runs on any recent laptop.

1. Download and install Ollama from [ollama.com/download](https://ollama.com/download). On a Mac it needs macOS 14 Sonoma or later.
2. Open the Ollama app once, so that it runs in the background.
3. Open a terminal (Terminal on macOS, PowerShell on Windows; not the R console) and run:

    ```
    ollama pull nomic-embed-text
    ```

4. Back in R, `ollama_status()` should list `nomic-embed-text`.

A local chat model is not needed. On a laptop it is too slow to drive an agent, and the course shows a recorded run instead. If you are curious, `ollama pull qwen3:4b` downloads one (2.5 GB).

**Optional: paid models from OpenAI and Anthropic**

A few steps compare the free models with a paid one. They are marked "optional" in the scripts and nothing else depends on them. Both providers sell prepaid credit; US$5 is far more than these steps use.

| Service | Line in `.Renviron` | Where to get the key |
| --- | --- | --- |
| OpenAI | `OPENAI_API_KEY=sk-...` | [platform.openai.com/api-keys](https://platform.openai.com/api-keys) |
| Anthropic (Claude) | `ANTHROPIC_API_KEY=sk-ant-...` | [platform.claude.com](https://platform.claude.com/); buy credit under Settings, Billing |

**Optional: keys for the self-study activities**

| Service | Line in `.Renviron` | Used in | Cost |
| --- | --- | --- | --- |
| TypeSafe (the Jev classifier) | `TYPESAFE_API_KEY=...` | Activity 12, screening abstracts | Paid; about US$0.002 in the instructor's run |
| Jina | `JINA_API_KEY=jina_...` | Activity 15, comparing embedding models | Free key; see [jina.ai](https://jina.ai/) for the current allowance |
| Voyage | `VOYAGE_API_KEY=pa-...` | Activity 15, comparing embedding models | See [dash.voyageai.com](https://dash.voyageai.com/) for the current allowance |
| Semantic Scholar | `SEMANTIC_SCHOLAR_API_KEY=...` | Activity 03, one more literature database | Free; keys are approved by hand, so apply early or skip it |

After adding a line to `.Renviron`, save the file and restart R, as in Step 4.

## If something goes wrong

Most problems are a key that R cannot see or a network that blocks a service. Find the message in the table.

| What you see | Likely cause | What to do |
| --- | --- | --- |
| `nzchar(Sys.getenv("GROQ_API_KEY"))` gives `FALSE` | The line is missing or misspelled, has quotation marks or spaces, or R was not restarted | Run `usethis::edit_r_environ()` again, correct the line, save, restart R |
| "No key found for groq. Add the line ..." | Same as above | Same as above |
| The keys are in the file but R still does not see them (Windows) | The file was saved as `.Renviron.txt` | Open it with `usethis::edit_r_environ()` and save it from RStudio, not from Notepad |
| `API groq` says "FAILED: HTTP 401 ... Check the API key" | The key is wrong, was revoked, or lost a character when copied | Create a new key in the Groq console and replace the line |
| "FAILED: HTTP 429 ... Rate limit or quota reached" | A limit of the free plan was reached | Wait a minute and run it again; see "Free plans, limits and optional credit" |
| OpenAlex answers with a waiting time in the first hour of use | A new free account is paced at first | Wait the time shown; this stops after the first US$1 of use |
| "Could not reach https://api.groq.com ... a VPN, proxy or firewall can also block the address" | No internet, or a university VPN or firewall blocks the service | Disconnect the VPN, or try another network (home network, phone hotspot) |
| `install_github()` fails with an HTTP error | GitHub cannot be reached, or the repository name was mistyped | Check the spelling `hfgolino/researchAgentR`; try again later or on another network |
| "Nothing answered at http://localhost:11434 ... Ollama is not running" | The Ollama app is closed | Open the Ollama app, then try again. Ollama is optional |
| The R installer does not open on an Apple silicon Mac | macOS is older than 14 | Use an earlier R build (Step 1) |
| `pdftools` does not install (Linux only) | A system library is missing | Install `libpoppler-cpp-dev` (Ubuntu, Debian) or `poppler-cpp-devel` (Fedora), then install the package again |

Still stuck? Copy the complete output of `verify_setup(test_apis = TRUE)` and of `sessionInfo()` and send both to the instructor before the course. The keys in that output are masked, so it is safe to share.

## Checklist before the first session

- [ ] R 4.3 or later and RStudio open without errors
- [ ] A course project folder exists and contains the `activities` folder
- [ ] `packageVersion("researchAgentR")` prints 0.1.5 or later
- [ ] `.Renviron` holds the Groq, OpenAlex and Tavily keys and your e-mail address
- [ ] `verify_setup(test_apis = TRUE)` shows `OK` for `API groq`, `API groq chat`, `API openalex` and `API crossref`
- [ ] `web_search()` returned three results
- [ ] Recommended: Groq Developer plan with a spending limit
- [ ] Recommended: Ollama is running and `ollama_status()` lists `nomic-embed-text`

## Sources

Versions, limits and prices were read from the pages below on November 23, 2026. They change: check the provider's page when a number matters to you.

- R: [R for Windows](https://ftp.fau.de/cran/bin/windows/base/) (a CRAN mirror) and [R for macOS](https://mac.r-project.org/bin/macosx/)
- RStudio: [Download RStudio](https://docs.posit.co/ide/user/download.html) and [supported versions](https://docs.posit.co/supported-versions/rstudio.html)
- Groq: [rate limits](https://console.groq.com/docs/rate-limits), [billing questions](https://console.groq.com/docs/billing-faqs) and the [gpt-oss-120b model page](https://console.groq.com/docs/model/openai/gpt-oss-120b)
- OpenAlex: [authentication and free budget](https://help.openalex.org/api/authentication/) and the [developer reference](https://developers.openalex.org/api-reference/authentication)
- Tavily: [API credits](https://docs.tavily.com/documentation/api-credits) and [pricing](https://www.tavily.com/pricing)
- Ollama: [macOS requirements](https://docs.ollama.com/macos) and the [nomic-embed-text model page](https://registry.ollama.ai/library/nomic-embed-text)
- Anthropic: [paying for API usage](https://support.claude.com/en/articles/8977456-how-do-i-pay-for-my-api-usage)
- Token counts and costs: the instructor's complete run of all 20 activities on November 22, 2026, with `researchAgentR` 0.1.1
