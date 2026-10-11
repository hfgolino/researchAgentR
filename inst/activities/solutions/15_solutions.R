# Solutions: Activity 15, Embeddings for Agents
# LLM Agents for Research Using R - Dr. Hudson Golino
# University of Virginia
# Updated: November 2026
#
# Exercises 1, 2, 3 and 5 run OFFLINE with the shipped MiniLM embeddings and
# the model registry: the numbers in the comments were computed offline with
# exactly this code. Exercise 4 needs Ollama with the all-minilm model.

library(researchAgentR)
stopifnot(packageVersion("researchAgentR") >= "0.1.0")
use_cache("auto")

data(minilm_embeddings)
data(extraversion)

# The same computation as route(method = "embedding"), on given vectors
softmax_route <- function(similarity, temperature = 0.05) {
  z <- similarity / temperature
  z <- z - apply(z, 1, max)
  p <- exp(z)
  p / rowSums(p)
}


# ==============================================================================
# EXERCISE 1: ROUTE THE BIG FIVE DESCRIPTIONS (OFFLINE)
# ==============================================================================

data(personality_descriptions)
P  <- minilm_embeddings$texts$personality_descriptions   # 110 x 384
LB <- minilm_embeddings$labels$bigfive                   # 5 x 384 (spelling "Openess" kept)
traits <- rownames(LB)
truth <- personality_descriptions$Personality

sim_b <- cosine_similarity(P, LB)
routed_b <- colnames(sim_b)[max.col(sim_b)]
confusion_b <- table(route = factor(routed_b, traits), trait = factor(truth, traits))
confusion_b
mean(routed_b == truth)
# Computed offline: 84 of 110 correct (accuracy 0.764).

rowSums(confusion_b)                        # texts each route attracts (22 per trait in truth)
round(diag(confusion_b) / colSums(confusion_b), 2)   # recall per trait
# Computed offline: Agreeableness attracts 33 texts and Openess 32, while
# Extraversion receives 13 and Neuroticism only 10. Recall: Openess and
# Agreeableness 1.00, Conscientiousness 0.77, Extraversion 0.59,
# Neuroticism 0.45.

weight_b <- apply(softmax_route(sim_b), 1, max)
conf_b <- weight_b >= 0.6
c(confident = sum(conf_b), correct_if_confident = sum((routed_b == truth)[conf_b]),
  uncertain = sum(!conf_b), correct_if_uncertain = sum((routed_b == truth)[!conf_b]))
# Computed offline: 65 confident with 58 correct (89%); 45 uncertain with 26
# correct (58%). The weight still separates easy from hard inputs, but the
# Big Five task is harder than the extraversion facets: almost half of the
# inputs would be escalated by a cascade.


# ==============================================================================
# EXERCISE 2: FEW-SHOT ROUTES (CENTROIDS) VERSUS DESCRIPTIONS (OFFLINE)
# ==============================================================================

E <- minilm_embeddings$texts$extraversion
L <- minilm_embeddings$labels$extraversion
facet <- extraversion$subtopic
facets <- rownames(L)

# The first 5 texts of each facet (in data order) describe the route
train <- unlist(lapply(facets, function(k) which(facet == k)[1:5]))
test  <- setdiff(seq_len(nrow(E)), train)
centroids <- t(sapply(facets, function(k) colMeans(E[train[facet[train] == k], ])))
dim(centroids)   # 4 x 384; cosine_similarity() does not need unit length

by_centroid    <- facets[max.col(cosine_similarity(E[test, ], centroids))]
by_description <- facets[max.col(cosine_similarity(E[test, ], L))]
c(centroid = mean(by_centroid == facet[test]),
  description = mean(by_description == facet[test]))
table(route = factor(by_centroid, facets), facet = factor(facet[test], facets))
# Computed offline: 94 of 100 with centroids and 90 of 100 with the written
# descriptions, on the same 100 texts. The examples come from the same
# distribution as the inputs (same generator, same style, same length), while
# a description is short, abstract and written differently. The cost: five
# labelled examples per route, which must be kept out of the evaluation (the
# test set excludes them). With 100 test texts, 4 texts is a small difference.


# ==============================================================================
# EXERCISE 3: OUT-OF-SCOPE INPUTS (OFFLINE)
# ==============================================================================

data(neuroticism)
out_of_scope <- neuroticism$subtopic %in% c("Sad", "Angry")
N_out <- minilm_embeddings$texts$neuroticism[out_of_scope, ]
nrow(N_out)                                   # 60 texts

sim_out <- cosine_similarity(N_out, L)
weight_out <- apply(softmax_route(sim_out), 1, max)
sum(weight_out >= 0.6)
table(colnames(sim_out)[max.col(sim_out)])
# Computed offline: 42 of the 60 out-of-scope texts get a "confident" weight,
# and every one of them gets a route (Positive 25, Energetic 18, Friendly 9,
# Assertive 8). Softmax weights are RELATIVE: they say which route is best,
# not whether any route fits.

# A floor on the RAW similarity: the 5th percentile of the best similarity of
# the in-scope (extraversion) texts.
best_in  <- apply(cosine_similarity(E, L), 1, max)
best_out <- apply(sim_out, 1, max)
floor_sim <- unname(quantile(best_in, 0.05))
round(floor_sim, 3)
c(out_of_scope_rejected = sum(best_out < floor_sim),
  in_scope_wrongly_rejected = sum(best_in < floor_sim))
# Computed offline: floor 0.305; 47 of the 60 out-of-scope texts are rejected,
# and 6 of the 120 in-scope texts are wrongly rejected (5%, by construction).
# The floor belongs to this model and this kind of text (Step 6b): set it
# again after changing the embedding model.


# ==============================================================================
# EXERCISE 4: SAME MODEL, DIFFERENT SOFTWARE? (OLLAMA)
# ==============================================================================

# ollama pull all-minilm   (in a terminal, once)
E_ollama <- keep("act15_ex4_allminilm",
                 embed_texts(extraversion$text[1:10], model = "ollama:all-minilm"))
dim(E_ollama)
same_text <- diag(cosine_similarity(E_ollama, E[1:10, ]))
round(same_text, 3)
round(cosine_similarity(E_ollama[1, ], E[2:10, ]), 3)   # text 1 versus the OTHER texts
# Expected: 10 x 384. If Ollama's all-minilm is the same network as the
# shipped sentence-transformers/all-MiniLM-L6-v2, the diagonal is close to 1
# (small differences come from the software, its numeric precision and the
# rounding of the shipped vectors to 4 decimals) and clearly above text 1's
# similarity with the other texts. If the diagonal is far from 1, the two
# are different models, whatever their names.
#
# Even when the diagonal is close to 1, do NOT mix the two in one store:
# the store records one model string, and a later change in either piece of
# software would go unnoticed. Re-embed everything with one model.
#
# OPTIONAL (Python behind R): the "hf:" path computes the shipped vectors
# with the same software that made them:
#   activate_python_env()
#   E_hf <- embed_texts(extraversion$text[1:10], model = "hf:sentence-transformers/all-MiniLM-L6-v2")
#   round(diag(cosine_similarity(E_hf, E[1:10, ])), 4)


# ==============================================================================
# EXERCISE 5: WHAT DOES ROUTING COST AT SCALE? (OFFLINE)
# ==============================================================================

reg <- model_registry("groq")
prices <- reg[reg$model %in% c("openai/gpt-oss-20b", "openai/gpt-oss-120b"),
              c("model", "input", "output")]
prices
n_month <- 10000
tokens_in <- 250    # same assumptions as the worked example in Step 8
tokens_out <- 200
llm_cost <- setNames(n_month * (tokens_in * prices$input + tokens_out * prices$output) / 1e6,
                     paste0("llm_", sub("openai/", "", prices$model)))
monthly <- c(llm_cost,
             embedding_openai_3_small = n_month * 30 * 0.02 / 1e6,   # 30 tokens per request
             embedding_local = 0)
round(monthly, 4)
# Computed offline (registry prices checked 2026-10-11; input/output per
# million tokens: gpt-oss-120b $0.15/$0.60, gpt-oss-20b $0.075/$0.30):
#   llm_gpt-oss-120b 1.575, llm_gpt-oss-20b 0.7875,
#   embedding_openai_3_small 0.006, embedding_local 0 dollars per month.

minutes <- n_month / 30
c(minutes = minutes, hours = round(minutes / 60, 1))
# Computed offline: 333.3 minutes, about 5.6 hours of requests at 30 per
# minute, if the daily request limit allowed 10,000 requests at all (check
# console.groq.com/settings/limits). The local embedding router embeds the
# same 10,000 requests in a few batches, and the requests never leave the
# computer.
