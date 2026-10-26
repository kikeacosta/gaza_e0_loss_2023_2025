# Reproduces the Gaza life-expectancy-loss estimates and figures
# (Acosta, Alburez-Gutierrez, Chen, Gomez-Ugarte, 2026, Perspectives Demografiques 42)
# Run from the project root (open gaza_e0_loss_2023_2025.Rproj, or setwd() to it).
# The pipeline is deterministic given the frozen posterior samples in
# data_input/posterior_samples/ -- no random numbers are drawn downstream.

stopifnot("run from the project root" = file.exists("code/00_setup.R"))

scripts <- c(
  "code/01_adjust_exposures_war_years.R",     # -> data_inter/adjusted_exposures.rds
  "code/02_extract_conflict_deaths.R",        # -> data_inter/conflict_deaths_samples_sex_age_2waryears.rds
  "code/03_btselem_gaza_prewar_deaths.R",     # -> data_inter/btselem_gaza_2000-2023_age_sex.rds
  "code/04_mort_rates_e0_yll.R",              # -> data_inter/ests_persp_demograficas2.rds (e0 losses)
  "code/05_pscore_decomposition.R",           # -> data_inter/ests_persp_demograficas.rds  (P-scores)
  "code/06_paper_figures.R",                  # -> figures/persp_dem/*  (Figures 1 and 2)
  "code/07_check_against_paper.R"             # stops if e0 / death totals differ from the paper
)

for (s in scripts) {
  message("\n===== ", s, " =====")
  source(s, echo = FALSE)
}
