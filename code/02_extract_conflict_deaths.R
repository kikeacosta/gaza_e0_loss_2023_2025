# Posterior draws of conflict deaths by war year, sex and single age group.
# Combatant deaths of 7 October 2023 are moved from expected to conflict deaths.
rm(list = ls())
source("code/00_setup.R")

results_dir <- "data_input/posterior_samples/"

# age groups used in the model
x <-
  readRDS("data_input/pi_x_moh_2024.rds") %>%
  pull(age) %>%
  unique()

file_names2324 <- paste0("moh_23_24_samples_", 1:4)
file_names2425 <- paste0("moh_24_25_samples_", 1:4)

dxs_out <-
  bind_rows(
    gimeconfdeaths(file_names2324) %>% mutate(year = "2023-2024"),
    gimeconfdeaths(file_names2425) %>% mutate(year = "2024-2025")
  )

# combatant deaths on 7 October 2023 (first war year only)
dx_cmb <-
  readRDS("data_input/Dx_cmb.rds") %>%
  rename(dx_cmb = Dx_cmb_mean) %>%
  mutate(year = "2023-2024")

dxs_out2 <-
  dxs_out %>%
  left_join(dx_cmb) %>%
  replace_na(list(dx_cmb = 0)) %>%
  mutate(dx2 = dx + dx_cmb) %>%
  select(it, year, sex, age, dx = dx2)

# both sexes combined
dxs_out3 <-
  dxs_out2 %>%
  bind_rows(
    dxs_out2 %>%
      reframe(dx = sum(dx), .by = c(it, year, age)) %>%
      mutate(sex = "t")
  )

write_rds(dxs_out3, "data_inter/conflict_deaths_samples_sex_age_2waryears.rds")
