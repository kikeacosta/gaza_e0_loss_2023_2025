# Exposures (population at risk) for the two war years.
# Mid-year population is taken from the PCBS/Guillot series and reduced by the
# conflict deaths that occurred before the reference date (April of each war year).
rm(list = ls())
library(tidyverse)
library(ungroup)
library(rstan)

# baseline (no-conflict) forecasts and PCBS populations
pop <- read_rds("data_input/data_plus_forecasts_v2.rds")

# Guillot population estimates
pop_guill <- readRDS("data_input/pcbs_exposures_gaza_guillot.rds")

# mean conflict deaths by sex and age in each war year ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (posterior samples of the under-reporting model; combatant deaths are added to year 1)
results_dir <- "data_input/posterior_samples/"

# transforms the array of posterior draws (draws x sex x age group) into tidy data
to_tidy <- function(mt) {
  out <- tibble()
  for (k in 1:18) {
    tst1 <-
      mt[, , k] %>%
      as_tibble() %>%
      mutate(
        age = case_when(
          k == 1 ~ 0,
          k == 2 ~ 1,
          k %in% 3:17 ~ (k - 2) * 5,
          k == 18 ~ 80
        ),
        iter = 1:n()
      ) %>%
      rename(f = V1, m = V2)

    out <-
      out %>%
      bind_rows(tst1)
  }

  out %>%
    gather(m, f, key = sex, value = dx) %>%
    arrange(iter, sex, age)
}

# war year 1: 2023-2024
file_names_y1 <- paste0("moh_23_24_samples_", 1:4)
model_out_y1 <- read_stan_csv(paste0(results_dir, file_names_y1, ".csv"))
Dx_conf_all_y1 <- rstan::extract(model_out_y1, pars = c("D_x_gen"))$D_x_gen
Dx_cnf_y1 <- to_tidy(Dx_conf_all_y1)

# combatant deaths
Dx_cmb <- readRDS("data_input/Dx_cmb.rds")

dxs_y1 <-
  Dx_cnf_y1 %>%
  left_join(Dx_cmb, by = c("sex", "age")) %>%
  mutate(dx = dx + Dx_cmb_mean) %>%
  reframe(dx = sum(dx), .by = c(iter, sex, age)) %>%
  reframe(dx = mean(dx), .by = c(sex, age)) %>%
  mutate(year = "2023-2024")

# war year 2: 2024-2025
file_names_y2 <- paste0("moh_24_25_samples_", 1:4)
model_out_y2 <- read_stan_csv(paste0(results_dir, file_names_y2, ".csv"))
Dx_conf_all_y2 <- rstan::extract(model_out_y2, pars = c("D_x_gen"))$D_x_gen
Dx_cnf2_y2 <- to_tidy(Dx_conf_all_y2)

dxs_y2 <-
  Dx_cnf2_y2 %>%
  reframe(dx = mean(dx), .by = c(sex, age)) %>%
  mutate(year = "2024-2025")

dxs <- rbind(dxs_y1, dxs_y2)

# ungrouping conflict deaths into single ages ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ung_age_dts <- function(chunk) {
  dt_in <-
    tibble(
      age = chunk$age,
      dx = chunk$dx,
      # increasing the size in each group for statistical power;
      # it is shrunk again after ungrouping
      dx_mt = dx * 1e5
    ) %>%
    mutate(dx_mt = ifelse(dx_mt == 0, 1, dx_mt))
  # last age interval
  nl <- 21

  # penalized composite link model
  dxs <- pclm(x = dt_in$age, y = dt_in$dx_mt, nlast = nl)$fitted
  fit <- tibble(age = 0:100, dx = dxs / 1e5)

  chunk %>%
    select(year, sex) %>%
    unique() %>%
    left_join(fit, by = character())
}

dxs_ungrp <-
  dxs %>%
  group_by(year, sex) %>%
  do(ung_age_dts(chunk = .data)) %>%
  ungroup()

# exposures in war year 1 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# population in April 2024
pop_apr24 <-
  pop_guill %>%
  filter(year %in% 2023:2024, source == "pcbs_gaza", region == "Gaza Strip") %>%
  select(year, sex, age, pop) %>%
  spread(year, pop) %>%
  mutate(pop = 3 / 12 * `2023` + 9 / 12 * `2024`, year = "2023-2024") %>%
  select(year, sex, age, pop)

# Remove 80% of the conflict deaths of the first war year (the share that occurred
# before April 2024) from the population estimates
pop_y1 <-
  pop_apr24 %>%
  left_join(
    dxs %>% filter(year == "2023-2024"),
    by = c("year", "sex", "age")
  ) %>%
  mutate(pop_new = pop - 0.8 * dx) %>%
  select(year, sex, age, pop, pop_new)

# exposures in war year 2 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# population in April 2025
pop_apr25 <-
  pop_guill %>%
  filter(year %in% 2024:2025, source == "pcbs_gaza", region == "Gaza Strip") %>%
  select(year, sex, age, pop) %>%
  spread(year, pop) %>%
  mutate(pop = 3 / 12 * `2024` + 9 / 12 * `2025`, year = "2024-2025") %>%
  select(year, sex, age, pop)

# deaths of year 1 to be removed from the population in year 2:
# age the conflict deaths of year 1 by one year, and group them again
dx_cnf_y1_in_y2 <-
  dxs_ungrp %>%
  filter(year == "2023-2024") %>%
  mutate(age = age + 1) %>%
  replace_na(list(dx = 0)) %>%
  mutate(
    age = case_when(
      age == 0 ~ 0,
      age %in% 1:4 ~ 1,
      age >= 80 ~ 80,
      TRUE ~ age - age %% 5
    )
  ) %>%
  reframe(dx = sum(dx), .by = c(sex, age))

# Remove (i) the conflict deaths of war year 1 and (ii) 35% of the conflict deaths of
# war year 2 (the share that occurred before April 2025)
pop_y2 <-
  pop_apr25 %>%
  left_join(dx_cnf_y1_in_y2) %>%
  replace_na(list(dx = 0)) %>%
  mutate(pop_new = pop - dx) %>%
  rename(dx_y1 = dx) %>%
  left_join(
    dxs %>% filter(year == "2024-2025"),
    by = c("year", "sex", "age")
  ) %>%
  mutate(pop_new = pop_new - 0.35 * dx) %>%
  select(year, sex, age, pop, pop_new)

# adjusted exposures, by sex and for both sexes combined ====
pop_war_years <-
  rbind(pop_y1, pop_y2) %>%
  filter(sex != "t") %>%
  select(-pop) %>%
  rename(pop = pop_new) %>%
  rbind(
    rbind(pop_y1, pop_y2) %>%
      filter(sex != "t") %>%
      select(-pop) %>%
      rename(pop = pop_new) %>%
      reframe(pop = sum(pop), .by = c(year, age)) %>%
      mutate(sex = "t")
  )

saveRDS(pop_war_years, file = "data_inter/adjusted_exposures.rds")
