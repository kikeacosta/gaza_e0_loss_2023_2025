# gaza_e0_loss_2023_2025

**Reproducible materials for the publication** *War, So Much War... Death, How Much Death: Two Years of
Demographic Collapse in Gaza* (Perspectives Demogràfiques, No. 42, May 2026). This repository contains the
code and the input data needed to reproduce the estimates and the two figures of that publication.

## The publication

| Language | Title | PDF | DOI |
|---|---|---|---|
| English | *War, So Much War... Death, How Much Death: Two Years of Demographic Collapse in Gaza* | https://ced.cat/PD/PerspectivesDemografiques_042_ENG.pdf | https://doi.org/10.46710/ced.pd.eng.42 |
| Español | *Cuánta, cuánta guerra... Cuánta, cuánta muerte: dos años de colapso demográfico en Gaza* | https://ced.cat/PD/PerspectivesDemografiques_042_ESP.pdf | https://doi.org/10.46710/ced.pd.esp.42 |
| Català | *Quanta, quanta guerra... Quanta, quanta mort: dos anys de col·lapse demogràfic a Gaza* | https://ced.cat/PD/PerspectivesDemografiques_042_CAT.pdf | https://doi.org/10.46710/ced.pd.cat.42 |

Local copies of the three PDFs are in [`publication/`](publication/)
(`PerspectivesDemografiques_042_ENG.pdf`, `_ESP.pdf`, `_CAT.pdf`).

> **EN** — This repository holds the reproducible materials of *Perspectives Demogràfiques* No. 42 (May 2026),
> published by the Centre d'Estudis Demogràfics (CED-CERCA). Running `run_all.R` regenerates the estimates and
> Figures 1 and 2 of the paper.
>
> **ES** — Este repositorio contiene los materiales reproducibles del número 42 de *Perspectives Demogràfiques*
> (mayo de 2026), publicado por el Centre d'Estudis Demogràfics (CED-CERCA). Al ejecutar `run_all.R` se
> regeneran las estimaciones y las Figuras 1 y 2 del texto.
>
> **CAT** — Aquest repositori conté els materials reproduïbles del número 42 de *Perspectives Demogràfiques*
> (maig de 2026), publicat pel Centre d'Estudis Demogràfics (CED-CERCA). En executar `run_all.R` es
> regeneren les estimacions i les Figures 1 i 2 del text.

## Authors and affiliations

| Author | Affiliation |
|---|---|
| Enrique Acosta | Ramón y Cajal Research Fellow, Centre d'Estudis Demogràfics (CED-CERCA), Universitat Autònoma de Barcelona |
| Diego Alburez-Gutierrez | Max Planck Institute for Demographic Research (MPIDR) |
| Irena Chen | Max Planck Institute for Demographic Research (MPIDR) |
| Ana C. Gómez-Ugarte | Max Planck Institute for Demographic Research (MPIDR) |

Correspondence: Enrique Acosta, eacosta@ced.uab.cat

**Citation.** Acosta, E., Alburez-Gutierrez, D., Chen, I., Gómez-Ugarte, A. C. (2026). "War, So Much War...
Death, How Much Death: Two Years of Demographic Collapse in Gaza". *Perspectives Demogràfiques*, 42: 1-4
(ISSN 2696-4228). https://doi.org/10.46710/ced.pd.eng.42

## What the code reproduces

Two "war years" (7 Oct 2023 - 6 Oct 2025) are compared with a no-conflict counterfactual. The code reproduces:

- **Figure 1**: contribution of each age and sex group to the relative mortality increase from the conflict,
  with the 12.7x (first war year) and 7.4x (second war year) mortality multipliers.
- **Figure 2**: life expectancy at birth (e0) in each war year against the no-conflict counterfactual, by sex,
  and the resulting loss (Total: 29.5 and 40.4 years against 76.7 and 76.8 expected).
- **Total conflict deaths** over the two war years: about 115,700 (95% interval 103,900-128,600).

The 10.6% population decline quoted in the text comes from PCBS (2025) and is not computed here.

## Run

Open `gaza_e0_loss_2023_2025.Rproj` and run

```r
source("run_all.R")
```

It takes 1-2 minutes. Developed with R 4.6.1 (tidyverse 2.0.0, ungroup 1.4.4, rstan 2.32.7, readxl 1.5.0);
missing CRAN packages are installed by `code/00_setup.R`. The last step (`07_check_against_paper.R`) stops
with an error if the e0 values or the conflict-death totals differ from the published ones. No random numbers
are drawn after the posterior samples, so results are the same on every run.

## Pipeline

| Step | Script | Reads | Writes |
|---|---|---|---|
| setup | `code/00_setup.R` | | packages, life-table helpers, Stan-CSV extractors |
| 1 | `01_adjust_exposures_war_years.R` | Guillot/PCBS exposures, baseline forecasts, posterior samples, combatant deaths | `data_inter/adjusted_exposures.rds` |
| 2 | `02_extract_conflict_deaths.R` | posterior samples, combatant deaths | `data_inter/conflict_deaths_samples_sex_age_2waryears.rds` |
| 3 | `03_btselem_gaza_prewar_deaths.R` | B'Tselem xlsx | `data_inter/btselem_gaza_2000-2023_age_sex.rds` |
| 4 | `04_mort_rates_e0_yll.R` | steps 1-2, baseline forecasts | `data_inter/ests_persp_demograficas2.rds` (e0) |
| 5 | `05_pscore_decomposition.R` | steps 1-3, baseline forecasts | `data_inter/ests_persp_demograficas.rds` (relative mortality increase) |
| 6 | `06_paper_figures.R` | steps 4-5 | `figures/persp_dem/` |
| 7 | `07_check_against_paper.R` | steps 2 and 4 | checks only |

`figures/persp_dem/01_psc_cnt*` is Figure 1 and `02_lss_horiz*` is Figure 2, in English, Spanish (`_esp`)
and Catalan (`_cat`). Steps 4 and 5 also write exploratory figures to `figures/`, and step 5 copies some summary
tables to the system clipboard (`copy_this()`).

## Input data (`data_input/`)

| File | Content |
|---|---|
| `posterior_samples/moh_23_24_samples_{1-4}.csv`, `moh_24_25_samples_{1-4}.csv` | Posterior draws (4 MCMC chains each) of the Bayesian model that corrects under-reporting in the Gaza Ministry of Health death records, for the first and second war year. The model follows Gómez-Ugarte et al. (2025), *Population Health Metrics* 23(1), 55, https://doi.org/10.1186/s12963-025-00422-9, extended to October 2025. The model is not re-fitted by this code. |
| `Dx_cmb.rds` | Combatant deaths by sex and age, moved from expected to conflict deaths |
| `pi_x_moh_2024.rds` | Age grouping used when extracting the posterior draws |
| `pcbs_exposures_gaza_guillot.rds` | Gaza population by sex and age (PCBS, Guillot adjustment) |
| `data_plus_forecasts_v2.rds` | Lee-Carter baseline (no-conflict) mortality forecasts for Gaza |
| `btselem_pse_killed_by_isr_{cvs,fcs,unk}.xlsx` | B'Tselem records of Palestinians killed by Israeli forces, used for the pre-war age and sex pattern of deaths |

`data_inter/` holds the intermediate results written by the pipeline.
