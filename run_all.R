# Reproduces the estimates and figures of
# Acosta, Alburez-Gutierrez, Chen, Gomez-Ugarte (2026), Perspectives Demografiques, 42.
# Run from the project root (open gaza_e0_loss_2023_2025.Rproj, or setwd() to it).
# No random numbers are drawn after the posterior samples in data_input/posterior_samples/,
# so the results are the same on every run.

stopifnot("run from the project root" = file.exists("code/00_setup.R"))

scripts <- c(
  "code/01_adjust_exposures_war_years.R",     # -> data_inter/adjusted_exposures.rds
  "code/02_extract_conflict_deaths.R",        # -> data_inter/conflict_deaths_samples_sex_age_2waryears.rds
  "code/03_e0_loss_pyll.R",                   # -> data_inter/ests_persp_demograficas2.rds, output/*.csv
  "code/04_relative_mortality_increase.R",    # -> data_inter/ests_persp_demograficas.rds
  "code/05_paper_figures.R",                  # -> figures/persp_dem/*  (Figures 1 and 2)
  "code/06_check_against_paper.R"             # stops if the results differ from the paper
)

for (s in scripts) {
  message("\n===== ", s, " =====")
  source(s, echo = FALSE)
}
