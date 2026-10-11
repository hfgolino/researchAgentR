# Generative psychometrics via AI-GENIE: Automatic item generation and validation with network-integrated evaluation

Adapted for teaching from Russell-Lasalandra, Christensen and Golino (2026), Behavior Research Methods, 58, 217, https://doi.org/10.3758/s13428-026-03082-1, licensed under CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/). Changes: excerpted, and deliberate errors were inserted for a teaching exercise. Do not cite this version; cite the original article.

Lara L. Russell-Lasalandra, Alexander P. Christensen, Hudson Golino

# Abstract

The rapid advancement of artificial intelligence (AI), particularly large language models (LLMs), has introduced powerful tools for various research domains, including psychological scale development. This study presents a methodology for efficiently generating and selecting high-quality, non-redundant items for psychological assessments using LLMs and network psychometrics. Our approach, termed Automatic Item Generation and Validation with Network-Integrated Evaluation (AI-GENIE), reduces reliance on expert intervention by integrating generative AI with the latest network psychometric techniques. The efficacy of AI-GENIE was evaluated through Monte Carlo simulations using the Mixtral, Gemma 2, Llama 3, GPT-3.5, and GPT-4o models to generate item pools that mimic Big Five personality assessments. Additionally, items from AI-GENIE were empirically tested with five nationally representative U.S. samples (N = 4,694 total), demonstrating that AI-GENIE-generated scales achieve structural validity—that is, evidence based on internal structure (dimensionality and item stability)—comparable to traditional expert-developed measures. The results demonstrated improvements in item selection efficiency, with overall average increases of 8.68–20.03 in normalized mutual information in the final item pool across all models. We also present a simulation study on the emerging construct of AI anxiety to demonstrate AI-GENIE’s utility for underrepresented constructs. Results from newly released models (DeepSeek, GPT-OSS 20B, GPT-OSS 120B) are presented in the Appendix. The findings prove that AI-GENIE streamlines the scale development and structural validation process.

# Introduction (excerpt)

The development of psychological scales is a resource-intensive and challenging process, particularly during the crucial stages of item generation and structural validation. Traditional methods require extensive human intervention, making the process time-consuming and costly. In response to these challenges, this paper introduces Automatic Item Generation and Validation with Network-Integrated Evaluation (AI-GENIE), a novel methodology for item development and structural validation in silico, leveraging the capabilities of large language models (LLMs) and advanced network psychometric techniques. This new approach streamlines, simplifies, and makes scale development more cost-effective while maintaining the quality and structural validity of the generated items. AI-GENIE begins by prompting an LLM to generate an initial pool of items; these items are then evaluated and refined using a multi-step psychometric pipeline that includes redundancy detection and stability-based filtering. The present AI-GENIE methodology has been integrated into an R package called AIGENIE available on the R Universe. For more details on the package and its usage, see Russell-Lasalandra and Golino (2026).

Consider the task of measuring the conscientiousness trait from the popular Big Five model of personality (John, 1999). Traditionally, researchers would manually craft items like “I keep my work organized” or “I meet deadlines reliably” based on their theoretical understanding and iterative empirical testing. Then, researchers have to repeat the process for the four other personality traits. This painstaking, construct-by-construct development is not an edge case. It is the norm. AI-GENIE partially automates this process by prompting LLMs to generate dozens of candidate items for conscientiousness, then evaluates and filters these using network psychometrics to arrive at a concise, high-quality scale.

Scale development is fundamental to psychological measurement, and the number of researchers developing scales has increased in recent years. A search of PsycTests reveals that 761 scales were published to the site in the first eight months of 2023 alone (this figure is much higher than when Götz et al. (2023) cited the same figure from 2022). Despite its ubiquity, scale development is a challenging, costly, and time-intensive undertaking (Boateng et al., 2018). One especially arduous, yet critical, step in the scale development process is item generation. This step can be difficult for several reasons: (1) major deficiencies in item quality cannot be corrected after items are written, (2) the initial pool should be so comprehensive that some items are ultimately shown to be unrelated to the target construct, and (3) the initial pool should include up to five times as many items as desired in the final scale (Clark & Watson, 2016; Schinka & Velicer, 2012). Given the challenges of item generation and the sheer writing power of modern LLMs—which can delve into almost any topic—researchers have begun to test the efficacy of AI-generated items for psychological assessments (Hommel et al., 2022; Kjell & Kjell, 2023; Maertens et al., 2024). These efforts reflect a broad and rapidly emerging research program, which we term generative psychometrics: the use of generative AI and text embeddings to author, refine, and evaluate the building blocks of psychological measurement.

[...]

Yet for all of this progress, a critical gap remains. Within this emerging field, the central challenge is not merely generating items but efficiently selecting and structurally validating those that are non-redundant and faithful to the intended construct. Götz et al. (2023), for example, used GPT 2 (Radford et al., 2019) to draft items for a Big Five inventory and a novel “wanderlust” construct. While the resulting scales met standard quality benchmarks, the process of identifying viable items was resource-intensive: a committee of four expert psychologists independently reviewed thousands of generated items, ultimately retaining fewer than one hundred (a yield below 0.03%). Although the best AI-authored items were of expert quality, identifying them required nearly as much effort as traditional human authorship (DeVellis, 2017). Despite several recent propositions to develop and select items via LLMs (see Laverghetta et al. (2024), our methodology is, to our knowledge, the first to generate, assess, and structurally validate the quality of AI-generated items for psychometric scales within a single integrated pipeline.

Critically, while we demonstrate AI-GENIE using AI-generated items, the pipeline itself is deterministic and source-agnostic: given the same item pool and embedding model, AI-GENIE will produce identical results regardless of whether items were authored by humans, AI, or both. This reproducibility and flexibility mean researchers can apply our psychometric refinement methodology to any item pool that is sufficiently large, making AI-GENIE a general-purpose tool for systematic item selection that extends beyond AI-based generation. Having established what AI-GENIE does, it is equally important to be precise about what it is designed to do.

Throughout this paper, we use the term validation to refer specifically to what Christensen et al. (2020) call the “structural phase” of validation, i.e., assessing item redundancy, dimensionality, and internal consistency, which corresponds to evidence based on internal structure as outlined in the Standards for Educational and Psychological Testing (Aera et al., 1999; Rios & Wells, 2014). This scope does not extend to other sources of validity evidence (e.g., convergent, discriminant, or criterion-related). However, AI-GENIE does systematically check the structural validity of candidate items entirely automatically.

# Methods (excerpt): Simulation design, item generation, and prompt engineering techniques

Five large language models were used to generate items for the Big Five traits: agreeableness, conscientiousness, extraversion, neuroticism, and openness to experience (John, 1999). Three of these models, Gemma 2 (Team et al., 2024), Llama 3 (Meta, 2024), and Mixtral 8x7b (Jiang et al., 2024), are open-source models available on the Groq API (groq, 2024). The other two models, GPT-3.5 (Openai, 2024b) and GPT-4o (Openai, 2024c), come from OpenAI.

Results from an additional simulation that includes DeepSeek (Deepseek et al., 2024) and OpenAI’s open-source GPT OSS 20b and 120b (Openai, 2025), released after our main simulation was completed, are reported in the Appendix to illustrate AI-GENIE’s robustness across the evolving model ecosystem.

Each of the five models was given the same few-shot prompt that asked the model to generate eight novel items for a given Big Five personality trait. We kept inferencing the model until a sample of at least 60 unique items for a given trait was obtained. The temperature of the model was either set to low (0.5), medium (1; default), or high (1.5). One hundred samples of at least 60 items were collected for each combination of item type (trait), model, and temperature setting. Thus, a total of 7500 samples of about 60 items were collected across the entire Monte Carlo simulation.

Prompt quality plays a significant role in the consistency and viability of LLM output (Jin et al., 2022; White et al., 2023). The current paper used four established prompt engineering techniques to elicit high-quality items: (1) clear and precise prompting, providing a detailed task description and output format; (2) few-shot prompting, supplying examples of desirable output to improve consistency (Brown et al., 2020); (3) role prompting, assigning the model the identity of “an expert psychometrician and test developer specializing in personality assessment” (Zheng & Pei, 2023); and (4) adaptive prompting, instructing the model not to repeat items already generated (Lightman et al., 2023). A detailed examination of how these prompt engineering strategies shape item quality within the AI-GENIE framework is provided in Russell-Lasalandra and Golino (2026).

As part of the few-shot prompting approach, the model was given a list of trait attributes to ensure that generated items targeted several aspects of each Big Five trait. The attributes for each trait were:

1. Openness: creative, perceptual, curious, and philosophical
2. Conscientiousness: organized, responsible, disciplined, and prudent
3. Neuroticism: anxious, depressed, insecure, and emotional
4. Agreeableness: cooperative, compassionate, trustworthy, and humble
5. Extraversion: friendly, positive, assertive, and energetic

The model was prompted to generate two items per attribute per iteration. Since LLMs are limited in the amount of text they can produce in a single generation, the prompt was presented multiple times until a minimum of 60 non-duplicate items were collected per sample. The adaptive prompting approach mitigated repetition by providing the model with a cumulative list of items already generated. A total of nearly half a million items were generated across all conditions. The average item count per sample varied minimally between experimental conditions (see Table 7 in the Appendix), so sample size should have a negligible impact on the observed differences in model performance.

# Empirical validation (results excerpt)

To demonstrate the validity of the surveys produced by AI-GENIE, we created five new Big Five personality surveys using Gemma 2, GPT-3.5, GPT-4o, Llama 3, and Mixtral. The same prompt and instructions were used for each model (the full prompt is provided in the Appendix). Each model generated at least 40 items using a temperature of 1 and the EBICglasso method was used to validate the items in AI-GENIE. The resulting surveys had items ranging from 28 (Gemma 2) to 35 (GPT-4o) with at least four items per Big Five trait.

## Participants

Five nationally representative samples in the United States were recruited from Prolific. Each sample consisted of approximately 1000 people (final Ns ranged from 989 to 997 after exclusions) who completed one of the five Big Five surveys created and validated by AI-GENIE. Participants were compensated at $12/hour, with the median duration to completion for each sample ranging from 3-4 minutes. Participants were excluded from analysis if they demonstrated straight-line responding (zero variance in their responses) or had responded with a single response (e.g., “Agree”) for 95% or more of the items. All surveys were administered independently over Qualtrics and, based on exclusion criteria on Prolific, people were only allowed to complete one of the five surveys. Participants provided consent, completed a survey, and concluded with a set of demographics questions. Full descriptive statistics for each sample are provided in Table 5 of the Appendix. This study and subsequent surveys were all approved by the [masked] University’s institutional review board.

## Materials

All surveys were completed using a five-point Likert scale ranging from ‘1’ (Strongly Disagree) to ‘5’ (Strongly Agree) with a midpoint of ‘3’ (Neither Disagree nor Agree). People were provided with a response option of ‘Not Applicable’ in case any of the items did not apply to them. All survey items are provided on our OSF page.

## Analyses

To provide an evaluation of structural validation, we performed two procedures: UVA and bootstrap EGA. One goal of AI-GENIE is to reduce redundancies. UVA assessed the extent to which there were moderate (≥ 0.25) and large (≥ 0.30) redundancies. For the robustness of the structure, bootEGA provided item stability estimates with the goal of achieving ≥ 0.75 item stability. Finally, to evaluate the extent to which the empirical results aligned with the embedding item placements, we computed NMI between the embedding structure and the empirically derived structure. To provide traditional psychometric benchmarks, we also conducted confirmatory factor analyses (CFA) using weighted least-squares mean and variance adjusted estimation (WLSMV; Muthén et al., 1997) and computed McDonald’s (ω) as a measure of internal consistency for each factor (see Table 6 in the Appendix). These values were comparable or better than what is typically observed for personality inventories under CFA (Hopwood & Donnellan, 2010).

## Results

The empirical results are presented in Table 4 and Fig. 8. The number of items varied from 28–35 in the post-processing of AI-GENIE. The number of moderate and large redundancies varied across the LLMs, with all having at least two moderate redundancies. The two models that had the fewest moderate redundancies, GPT-4o and Llama 3, also had the most large redundancies, with 2 and 3, respectively. In terms of sum total redundancies, Gemma 2 led all models with the fewest (3) followed by GPT-4o (4) and Mixtral (4).

The structures identified by EGA (top row of Fig. 8) all show five distinct communities corresponding to the Big Five dimensions: agreeableness, conscientiousness, extraversion, neuroticism, and openness to experience. There was a perfect match for three of the five models: Gemma 2, GPT-3.5, and GPT-4o (Table 4). Mixtral (NMI = 95) and Llama 3 (NMI = 92) had one (items_32) and two (items_26 and items_30) items that were placed differently than the theoretical item placements. Only GPT-3.5 and GPT-4o were perfectly placed with AI-GENIE’s placements, followed by Gemma 2 and Llama 3 (both NMIs = 94) and Mixtral (NMI = 89). A source of difference is likely to lie in the embeddings’ meanings and the participants’ interpretations of the items. These results show congruence with the theoretical and AI-GENIE item placements.

The robustness of these structures was evaluated by Bootstrap EGA using resampling with replacement. The item stabilities are reported in the bottom row of Fig. 8. Both Gemma 2 and GPT-4o had high item stabilities, with items replicating in the original EGA’s dimension at least 90% of the time. Both GPT-3.5 and Mixtral had only one problematic item (< 0.75). Llama 3 had several items with low stability (items_27, items_28, items_29), which are all openness to experience items. A key part of their low stability is likely that the other two openness to experience items (items_26 and items_30) were pulled into neuroticism. Item stabilities were higher for GPT-4o items than for Llama 3 items, t(63) = 2.31, p = .002.

Gemma 2 and GPT-4o performed best in terms of minimizing redundancies, maximizing similarity to the embedding structure, and maximizing the item stabilities within those dimensions. GPT-3.5 and Mixtral also performed well, with only one problematic item each with respect to item stabilities. Llama 3, consistent with the simulations, was the worst of the models tested in terms of all three facets (redundancy, embedding similarity, and structural integrity). CFA model fit indices and McDonald’s ω internal consistency estimates for each LLM-generated scale are reported in Table 6 of the Appendix. The AI-GENIE-generated scales showed fit comparable to the BFI under the same modeling conditions, consistent with what is typically observed for personality inventories under CFA (Hopwood & Donnellan, 2010).

# References

Aera, A., et al. (1999). Standards for educational and psychological testing. New York: American Educational Research Association.

Boateng, G. O., Neilands, T. B., Frongillo, E. A., Melgar-Quiñonez, H. R., & Young, S. L. (2018). Best practices for developing and validating scales for health, social, and behavioral research: A primer. Frontiers in Public Health, 6, 149. https://doi.org/10.3389/fpubh.2018.00149

Brown, T., Mann, B., Ryder, N., Subbiah, M., Kaplan, J. D., Dhariwal, P., et al. (2020). Language models are few-shot learners. Advances in Neural Information Processing Systems, 33, 1877–1901.

Christensen, A. P., Golino, H., & Silvia, P. J. (2020). A psychometric network perspective on the validity and validation of personality trait questionnaires. European Journal of Personality, 34(6), 1095–1108.

DeepSeek-AI, :, Bi, X., Chen, D., Chen, G., Chen, S., . . . Zou, Y. (2024). DeepSeek LLM: Scaling open-source language models with longtermism. Retrieved from arXiv:2401.02954.

DeVellis, R. F. (2017). Scale development: Theory and applications (4th ed.). Thousand Oaks, CA: SAGE Publications.

Götz, F. M., Maertens, R., Loomba, S., & Linden, S. van der. (2023). Let the algorithm speak: How to use neural networks for automatic item generation in psychological scale development. Psychological Methods. https://doi.org/10.1037/met0009954.

groq. (2024). Bringing speed to mission with the groq LPU inference engine. Retrieved from https://wow.groq.com/wp-content/uploads/2024/05/2024_GroqThoughts_PublicSectorOverview.pdf

Hommel, B. E., Wollang, F.-J.M., Kotova, V., Zacher, H., & Schmukle, S. C. (2022). Transformer-based deep neural language modeling for construct-specific automatic item generation. Psychometrika, 87(2), 749–772.

Hopwood, C. J., & Donnellan, M. B. (2010). How should the internal structure of personality inventories be evaluated? Personality and Social Psychology Review, 14(3), 332–346.

Jiang, A. Q., Sablayrolles, A., Roux, A., Mensch, A., Savary, B., Bamford, C., et al. (2024). Mixtral of experts. arXiv:2401.04088.

Jin, W., Cheng, Y., Shen, Y., Chen, W., & Ren, X. (2022). A good prompt is worth millions of parameters: Low-resource prompt-based learning for vision-language models. Retrieved from arXiv:2110.08484.

John, O. P., & Srivastava, S. (1999). The big five trait taxonomy: History, measurement, and theoretical perspectives. In L. A. Pervin & O. P. John (Eds.), Handbook of personality: Theory and research (2nd ed., pp. 102–138). Guilford Press.

Kjell, O., Kjell, K., & Schwartz, H. A. (2023). Beyond rating scales: With care for validation large language models are poised to change psychological assessment.

Laverghetta Jr, A., Luchini, S., Linell, A., Reiter-Palmon, R., & Beaty, R. (2024). The creative psychometric item generator: A framework for item generation and validation using large language models. arXix. https://doi.org/10.48550/arXiv.2409.00202.

Lightman, H., Kosaraju, V., Burda, Y., Edwards, H., Baker, B., Lee, T., . . . Cobbe, K. (2023). Let’s verify step by step. Retrieved from arXiv:2305.20050.

Maertens, R., Götz, F. M., Golino, H. F., Roozenbeek, J., Schneider, C. R., Kyrychenko, Y., et al. (2024). The Misinformation Susceptibility Test (MIST): A psychometrically validated measure of news veracity discernment. Behavior Research Methods, 56(3), 1863–1899. https://doi.org/10.3758/s13428-023-02124-2.

Meta. (2024). Introducing meta llama 3: The most capable openly available llm to date. Retrieved from https://ai.meta.com/blog/meta-llama-3/.

Muthén, B., Toit, S. H. C. du, & Spisic, D. (1997). Robust inference using weighted least squares and quadratic estimating equations in latent variable modeling with categorical and continuous outcomes. Psychometrika.

OpenAI. (2024b). OpenAI GPT-3.5 turbo API. Retrieved from https://platform.openai.com/docs/models/gpt-3-5-turbo.

OpenAI. (2024c). OpenAI GPT-4o API. Retrieved from https://platform.openai.com/docs/models/gpt-4o.

OpenAI. (2025). Gpt-oss-120b & gpt-oss-20b model card. https://openai.com/index/gpt-oss-model-card/.

Radford, A., Wu, J., Child, R., Luan, D., Amodei, D., Sutskever, I., et al. (2019). Language models are unsupervised multitask learners. OpenAI Blog, 1(8), 9.

Rios, J., & Wells, C. (2014). Validity evidence based on internal structure. Psicothema, 26(1), 108–116.

Russell-Lasalandra, L. L., & Golino, H. (2026). Prompt engineering for scale development in generative psychometrics. https://osf.io/preprints/psyarxiv/znqkm_v2.

Schinka, J. A., Velicer, W. F., & Weiner, I. B. (2012). Handbook of psychology: Research methods in psychology (2nd ed.). Wiley. Retrieved from https://www.wiley.com/en-gb/Handbook+of+Psychology%2C+Volume+2%2C+Research+Methods+in+Psychology%2C+2nd+Edition-p9780470890646

Team, G., Mesnard, T., Hardin, C., Dadashi, R., Bhupatiraju, S., Pathak, S., . . . Kenealy, K. (2024). Gemma: Open models based on gemini research and technology. Retrieved from arXiv:2403.08295.

White, J., Fu, Q., Hays, S., Sandborn, M., Olea, C., Gilbert, H., . . . Schmidt, D. C. (2023). A prompt pattern catalog to enhance prompt engineering with ChatGPT. Retrieved from arXiv:2302.11382.

Zheng, M., Pei, J., & Jurgens, D. (2023). Is "a helpful assistant" the best role for large language models? A systematic evaluation of social roles in system prompts. Retrieved from arXiv:2311.10054.
