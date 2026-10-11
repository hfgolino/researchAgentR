# AI-use statement (template)

Complete every field in [square brackets]. Fields marked *from the logs* can be
filled with `researchAgentR::ai_use_statement()`; the others can only be
completed by the authors. State only what the logs or the authors can support.

## Short statement for a methods section

Large language model (LLM) agents were used for the following purpose:
[purpose, for example screening 412 titles and abstracts against four
inclusion criteria]. The agents were run with the researchAgentR package
(version [x.y.z], *from the logs*) in [R version, *from the logs*] between
[first date] and [last date] (*from the logs*). The logs contain [n] episodes
with [n] model calls to [model IDs with providers, for example
groq:openai/gpt-oss-120b] and [n] tool calls to [tools and the services they
query, for example OpenAlex and Crossref] (*from the logs*). The logged calls
used [n] billed tokens at an estimated cost of [amount] US dollars at list
price (*from the logs*). [Verification recorded in the logs, for example 3
calls to verify_citations covering 48 references, of which 2 did not resolve
and were removed.] [Human review: who checked what, how many items, and with
what agreement, for example Cohen's kappa on a random sample of 50 records.]
[Role of AI: which outputs were used, how, and which decisions remained with
the authors.] The authors reviewed all agent outputs used in this work and take
full responsibility for its content. The episode logs (JSON) are available at
[repository or supplementary materials].

## Checklist of what to report

| Item | Source | Entry |
|---|---|---|
| Purpose of AI use | Authors | [ ] |
| Model IDs and providers | Logs | [ ] |
| Dates of use | Logs | [ ] |
| Software and versions | Logs | [ ] |
| Number of episodes, model calls, tool calls | Logs | [ ] |
| Tools and external services queried | Logs | [ ] |
| Tokens and cost at list price | Logs | [ ] |
| Cache replays (results reused, not regenerated) | Logs | [ ] |
| Budgets and stop reasons | Logs | [ ] |
| Verification steps recorded | Logs | [ ] |
| Verification performed outside the logs | Authors | [ ] |
| Human review and agreement with humans | Authors | [ ] |
| Repeated trials and reliability (pass^k) | Evaluation logs | [ ] |
| Data sent to cloud providers (confidential or participant data?) | Authors | [ ] |
| Responsibility statement | Authors | [ ] |
| Where the logs are available | Authors | [ ] |

## Notes

- Report model IDs exactly as logged (for example `groq:openai/gpt-oss-20b`),
  not a product name.
- Costs at list price are a comparable "shadow cost"; a free tier or a local
  model may have incurred no actual charge.
- Follow the AI-use policy of the target journal or funder, which may require
  more than this template covers.
