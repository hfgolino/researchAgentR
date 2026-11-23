# Activities: LLM Agents for Research Using R

Numbered R scripts, one or two per session. Copy them to a working folder with
`file.copy(system.file("activities", package = "researchAgentR"), ".", recursive = TRUE)`.

## Format of every script (same as the llmClassificR activities)

```
# Activity NN: <Title>
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026

# ==============================================================================
# GOAL: one or two sentences.
# OVERVIEW: what the activity builds and why (a few lines), then
# researchAgentR functions used here:
#   fun_a(), fun_b()        what they do
# TIME: about N minutes.  COST: free tier (Groq, OpenAlex) unless stated.
# ==============================================================================

# SETUP  (library, version check, use_cache("auto"))
# STEP 1: <name>   code, then "# EXPECTED OUTPUT:" lines describing what prints
# STEP 2: ...
# TRY IT: short variations
# EXERCISES (solutions in solutions/NN_solutions.R)
# WHAT CAN GO WRONG
# DISCUSSION POINTS
```

Rules:

- English, no em dashes, no "we/our" in explanatory prose, never "delve".
- `# EXPECTED OUTPUT:` lines describe the shape and meaning of the output
  (columns, which stop reason, what a correct answer contains). They never
  contain invented numbers or invented model text. Exact values are filled in
  after the instructor's recorded live run.
- Every script calls `use_cache("auto")` in SETUP: the first live run saves
  every API response, so later runs (and the class demo) replay them.
- Keys are never typed in scripts (they live in `~/.Renviron`).
- Default models are free (Groq) or local (Ollama). Paid providers appear as
  clearly labeled optional lines.
- Check scripts with `Rscript tools/check_activities.R` (argument names match
  the package, and every exported function is demonstrated somewhere).

## Coverage map

| Script | Session | Main functions |
|---|---|---|
| 00_Setup_researchAgentR.R | before class | setup_guide, set_*_key, verify_setup, ollama_status, providers, register_provider, model_registry, registry_set, parse_model, default_model, set_default_model, list_provider_models, use_cache, cache_dir, keep, setup_python_env, activate_python_env |
| 01_Lens_Watch_an_Agent.R | Day 1 | chat, ask, builtin_tool, count_tokens, view_episode, view_episodes, episode_steps, episodes_df, save_episode, read_episode, plot_episode, reliability_curve |
| 02_Loop_First_Tool_and_Agent.R | Day 1 | define_tool, arg_string, arg_number, arg_integer, arg_boolean, arg_enum, arg_array, arg_object, tool_schema, call_tool, agent, run, budget, ctx_full |
| 03_Literature_Scout_and_Verification.R | Day 1 | search_openalex, get_work, get_references, get_citations, search_crossref, search_semantic_scholar, search_europepmc, search_arxiv, find_open_access, normalize_doi, dedupe_records, prisma_counts, tool_openalex, tool_crossref, tool_semantic_scholar, tool_europepmc, tool_arxiv, tool_unpaywall, tool_snowball, verify_citations, tool_verify_citations, check_claims |
| 04_Graded_Task_Set.R | Day 1 | task_set, grader_exact, grader_contains, grader_doi, grader_code, hash_answer, run_eval, eval_summary, pass_at_k, pass_hat_k, research_tasks |
| 05_Context_Management_and_Compression.R | Day 2 | context_report, filter_output, offload, recall, offload_tools, select_relevant, compress_text, ctx_window, ctx_compact, ctx_notes, notes_tools, subagent, use_skills, list_skills |
| 06_Measurement_Finder.R | Day 2 | agent(output_schema), web_search, tool_web_search, read_page, tool_read_page, html_to_text, filter_domains, verify_citations, check_claims |
| 07_Design_Advisor_and_Funding_Scout.R | Day 2 | run_r_code, tool_run_r, search_nih_projects, tool_nih_reporter, search_nsf_awards, tool_nsf_awards, search_grants_gov, grants_gov_opportunity, tool_grants_gov |
| 08_Multi_Agent_Manuscript_Review.R | Day 2 | as_tool, team, fan_out, sub_episodes, critique_loop, checkpoint, checkpoint_log, compare_single_vs_team, route, review_checks, review_manuscript, review_report, default_style_rules |
| 08b_MCP_Clients.R | Day 2 | mcp_connect, mcp_list_tools, mcp_call, mcp_as_tools, mcp_close, mcp_stdio, remote_mcp_tool |
| 09_Evaluation_Harness.R | Day 3 | run_eval, grader_judge, calibrate_judge, eval_summary, pass_at_k, pass_hat_k, paired_bootstrap, task_compare, contamination_check, plot_frontier |
| 10_Make_Every_Token_Count.R | Day 3 | cascade, baselines, cost_ledger, plot_frontier, keep, use_cache, model_registry |
| 11_Small_Models_and_Ask_My_Library.R | Day 3 | run_eval, eval_summary, task_compare (small open-weight model + harness), read_documents, chunk_documents, embed_texts, ingest_library, vector_store, store_add, store_search, store_save, store_load, retrieve_tool, notes_write, notes_read, cosine_similarity |
| 12_Screening_Assistant.R | self-study | get_works, classify_texts, tool_classify, select_relevant, route, calibrate_judge |
| 13_Data_Agent.R | self-study | define_tool (an open-data API), run_r_code, tool_run_r |
| 14_Capstone_Team_Brief.R | self-study | team, as_tool, tool_get_work, verify_citations, check_claims, ai_use_statement, course_template, cost_ledger, view_episodes |
| 15_Embeddings_for_Agents.R | self-study | embed_texts, cosine_similarity, route (embedding and llm), minilm_embeddings, label_descriptions, set_jina_key, set_voyage_key |
| 16_Create_Your_Own_Skills.R | self-study | create_skill, validate_skill, test_skill, use_skills, list_skills, plain_punctuation |
| 17_Learn_Your_Writing_Style.R | self-study | read_documents, style_profile, learn_style, style_to_skill, write_in_style, style_distance |
| 18_Review_Your_Own_Paper.R | self-study | review_checks, review_manuscript, review_report, apply_revisions |
