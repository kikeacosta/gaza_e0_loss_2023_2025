# gaza_e0_loss_2023_2025

**Reproducible materials for the publication** *War, So Much War... Death, How Much Death: Two Years of
Demographic Collapse in Gaza* (Perspectives Demogràfiques, No. 42, May 2026). This repository contains the
code and the frozen input data needed to reproduce the estimates and the two figures of that publication.

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

Two "war years" (7 Oct 2023 - 6 Oct 2025) are compared with a no-conflict counterfactual:
relative mortality increase (Figure 1) and loss of life expectancy at birth (Figure 2). The figures also carry
the published 12.7x and 7.4x mortality multipliers and the e0 values. The 10.6% population decline quoted in the
text comes from PCBS (2025) and is not computed here.

## Run

Open `gaza_e0_loss_2023_2025.Rproj` and run

```r
source("run_all.R")
```

About 1-2 minutes. Developed and checked with R 4.6.1 (tidyverse 2.0.0, ungroup 1.4.4, rstan 2.32.7,
readxl 1.5.0); missing CRAN packages are installed by `code/00_setup.R`. The last step
(`07_check_against_paper.R`) stops with an error if the e0 values or the conflict-death totals differ
from the published ones.

## Pipeline

| Step | Script | Reads | Writes |
|---|---|---|---|
| setup | `code/00_setup.R` | | packages, life-table helpers, Stan-CSV extractors |
| 1 | `01_adjust_exposures_war_years.R` | Guillot/PCBS exposures, baseline forecasts, posterior samples, combatant deaths | `data_inter/adjusted_exposures.rds` |
| 2 | `02_extract_conflict_deaths.R` | posterior samples, combatant deaths | `data_inter/conflict_deaths_samples_sex_age_2waryears.rds` |
| 3 | `03_btselem_gaza_prewar_deaths.R` | B'Tselem xlsx | `data_inter/btselem_gaza_2000-2023_age_sex.rds` |
| 4 | `04_mort_rates_e0_yll.R` | steps 1-2, baseline forecasts | `data_inter/ests_persp_demograficas2.rds` (e0), `figures/gaza_update/` |
| 5 | `05_pscore_decomposition.R` | steps 1-3, baseline forecasts | `data_inter/ests_persp_demograficas.rds` (P-scores), `figures/*.png` |
| 6 | `06_paper_figures.R` | steps 4-5 | `figures/persp_dem/` |
| 7 | `07_check_against_paper.R` | step 4, step 2 | checks only |

`figures/persp_dem/01_psc_cnt*` is Figure 1 and `02_lss_horiz*` is Figure 2 (English, `_cat`, `_esp`).
`code/archive/` holds code that is **not** run (see below).

## Frozen inputs (`data_input/`)

| File | What it is |
|---|---|
| `posterior_samples/moh_23_24_samples_{1-4}.csv`, `moh_24_25_samples_{1-4}.csv` | Stan posterior draws (4 chains each) of the Bayesian under-reporting model on MoH data, war year 1 and war year 2 |
| `Dx_cmb.rds` | Combatant deaths by sex and age, moved from expected to conflict deaths |
| `pi_x_moh_2024.rds` | Age grouping used when extracting draws |
| `pcbs_exposures_gaza_guillot.rds` | Gaza population (PCBS, Guillot adjustment) |
| `data_plus_forecasts_v2.rds` | Lee-Carter baseline (counterfactual) mortality forecasts |
| `btselem_pse_killed_by_isr_{cvs,fcs,unk}.xlsx` | B'Tselem records of Palestinians killed by Israeli forces (pre-war age/sex pattern) |

Copied unchanged from the working copy used for the paper
(`I:\My Drive\gits_drive\uncertainty_quantification`, folders `kike/` and `data/`).
The Stan model that generates the posterior samples is **not** re-run here; its code is in the
`uncertainty_quantification` repository (`R/model/diff_reporting/`).

## Verification (reproduction vs. the original paper run)

Compared against the original outputs in `kike/data_inter/` and `kike/figures/persp_dem/`:

- `ests_persp_demograficas2.rds` (all e0 results): equal within floating-point tolerance (`all.equal` TRUE).
- All six paper figures (EN/CAT/ESP, Fig. 1 and 2): text labels identical to the originals.
- e0 (Total): 29.5 and 40.4 years vs. 76.7 / 76.8 expected; losses 47.2 and 36.5 years; conflict deaths
  115,657 (95% interval 103,886-128,602). All match the paper.
- Not bit-identical, for harmless reasons: (a) `adjusted_exposures.rds` differs by < 0.001 persons per cell
  (numerical noise in the `ungroup`/pclm fit); (b) `conflict_deaths_samples_sex_age_2waryears.rds` contains
  exactly the same draws as the original but with a different iteration index (a relabelling; per-draw totals
  and every downstream result are unchanged).

## Deviations from the original scripts

Script bodies are the original analysis code; only file paths changed, plus:

- **Step 1 is the 16 Dec 2025 script** (`05_adjusted_exposures_war_years.R` from the GitHub repo). It is the
  version that regenerates the exposures used in the paper. A later revision of that script (Drive copies
  `06_adjusted_exposures_war_years.R` / `a01_adjusting_exposures_23_25.R`, 22 Dec 2025) is in
  `code/archive/` and is **not** used: it yields ~5% lower exposures (and round-number age-0 values), and
  gives e0 = 28.5 / 39.2 instead of the published 29.5 / 40.4. If the paper should have used it, the
  published numbers would change.
- `05_pscore_decomposition.R`: the last section (a draft e0 calculation depending on `mx_cnf`, an object that
  exists only in step 4) was moved to `code/archive/`; it errors when run top to bottom and writes nothing
  the paper uses.
- `05_pscore_decomposition.R` calls `copy_this()`, which writes tables to the system clipboard (as in the
  original).
- No random draws downstream of the posterior samples (`seed` in `00_setup.R` is unused).

## GitHub vs. Drive

The `uncertainty_quantification` repo on GitHub `main` (last commit 2025-12-16) is out of date for this
paper: its 2023-24 posterior-sample files, `adjusted_exposures.rds` and `data_plus_forecasts_v2.rds` differ
from the versions used here, and it lacks the Dec 2025 - May 2026 scripts (e0 loss, P-score decomposition,
paper figures). Those exist only on Drive (`kike/code/`), which is where this project was built from.
