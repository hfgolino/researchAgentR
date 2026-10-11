# Agent verification checklist

Run these checks before an agent result is used in research. An agent result is
a claim to verify, not a finding. Record the outcome of each check; a check
that cannot be completed is reported, not skipped.

Episode or task: [id]  Checked by: [name]  Date: [date]

## 1. The trace

- [ ] The full trajectory was inspected with `view_episode()`, not only the
      final answer.
- [ ] Every tool call that supports the answer is present, with its arguments
      and result.
- [ ] Tool errors were read; the answer does not rest on a failed call.
- [ ] Tool results were treated as untrusted data: no instruction found in a
      web page or document changed what the agent did.

## 2. Budgets and stop reason

- [ ] Budgets (steps, tokens, cost, time) were set before the run.
- [ ] The stop reason is `final_answer`. Any other stop reason (`max_steps`,
      `max_cost`, `api_error`, ...) means the result is incomplete.
- [ ] Truncated tool outputs were checked: nothing needed was cut.

## 3. References and claims

- [ ] Every reference was checked with `verify_citations()`: the DOI resolves,
      title, authors and year match, and the work is not retracted.
- [ ] References that failed verification were removed or corrected by hand,
      and the number is reported.
- [ ] Every factual claim was checked with `check_claims()` and is backed by a
      quoted passage from a source that was actually retrieved.
- [ ] A random sample of quotes was compared with the source by a person.

## 4. Contamination

- [ ] For evaluation tasks: the agent did not retrieve the answer key or a
      published solution while browsing (search the trajectory for it).
- [ ] The task is not likely to appear verbatim in the model's training data;
      if it might, this is stated.

## 5. Reliability

- [ ] The task was run in repeated trials (k of at least 3).
- [ ] pass^k (the chance that all k trials succeed) is reported, not only the
      best run.
- [ ] Results were compared with a simple baseline (a fixed cheap model, a
      fixed strong model, or a single agent with the same budget).

## 6. Cost

- [ ] Cost per episode is recorded from the ledger, including failed attempts
      and retries.
- [ ] Cost per successful task is reported.
- [ ] Offline development costs are kept in a separate ledger.

## 7. Data and privacy

- [ ] No confidential, identifiable or participant data were sent to a cloud
      provider, or their terms allow it and the ethics approval covers it.
- [ ] No API keys appear in scripts, logs or shared files (keys live in
      `~/.Renviron`).
- [ ] Code written by the agent ran only in the sandbox (`callr`, timeout,
      temporary folder, no network by default).

## 8. Documentation

- [ ] Episode logs (JSON) are saved with the project.
- [ ] An AI-use statement was drafted with `ai_use_statement()` and every
      placeholder was completed by a person.

Decision: [use as is / use after corrections / do not use]
Reason: [one or two sentences]
