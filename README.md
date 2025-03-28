# Solea_Biphasic
This repo contains codes for the paper _"Biphasic versus monophasic growth curve equation, an application to common sole (Solea solea, L.) in the northern and central Adriatic Sea"_  https://doi.org/10.1016/j.fishres.2023.106694

- Author: Francesco Masnadi
- Supervisors: Giuseppe Scarcella; Pierluigi Carbonara

**Summary**

Traditionally, growth patterns are described as constant throughout life using von Bertalanffy's equation. However, a change in growth due to a reallocation of energy during an individual’s lifespan is to be expected. Following this hypothesis, back-calculation measurements obtained from SoleMon survey data were used to fit and compare monophasic and biphasic growth curves for common sole in the northern and central Adriatic Sea. Moreover, individual variability in growth was considered through nonlinear mixed-effect models in which the individual parameters were considered as a random effect. The analyses performed in this study revealed systematic age-specific discrepancies in the monophasic curve and demonstrated that the fit of the biphasic curve was superior (Δ AIC: 329; Δ BIC: 310), confirming the theory that growth in size would decrease as a consequence of reproductive effort. Finally, since common sole is routinely assessed using models that rely on growth to derive assessment estimates and related management reference points, a stock assessment simulation was performed to compare the two growth alternatives. The results showed how the biphasic alternative was preferable to the conventional alternative and how the use of the monophasic pattern would result in an overly optimistic view of stock status (+40% in SSB/SSBtarget and −35% in F/Ftarget compared to the biphasic pattern), thereby increasing the risk of overfishing.
A detailed implemenation of the growth analyses using nonlinear mixed effects models (SAEM Algorithm) described in the paper is available at [solea_biphasicVBGP.pdf](https://github.com/framasnadi/Solea_Biphasic/files/8370168/solea_biphasicVBGP.pdf)

```prova_R_backCalc.R``` : Image processing and age-related measurements were conducted by adhering to the workflow recommendations of the open-source R package RfishBC (Ogle, 2022. RFishBC. R package Version 0.2.4.9000, https://doi.org/10.5281/zenodo.6058214).

```solea_5pVBGP.Rmd``` : R Markdown script to estimate growth curves for common sole using nonlinear mixed effects models (SAEM Algorithm)
