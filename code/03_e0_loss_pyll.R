# Life expectancy at birth with and without conflict mortality (Figure 2), conflict death
# totals and potential years of life lost (PYLL), by war year and sex.
rm(list = ls())
source("code/00_setup.R")
dir.create("output", showWarnings = FALSE)

dxs <- read_rds("data_inter/conflict_deaths_samples_sex_age_2waryears.rds")
pop <- read_rds("data_inter/adjusted_exposures.rds")
bsn <- read_rds("data_input/data_plus_forecasts_v2.rds")

# expected (no-conflict) mortality rates ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lee-Carter projections into 2023, 2024 and 2025, weighted to the war years
# (7 Oct - 6 Oct)
bsn2 <-
  bsn %>%
  filter(source == "lc_pcbs_2019", region == "Gaza Strip", year >= 2023) %>%
  select(year, sex, age, bsn = mx_noc) %>%
  spread(year, bsn) %>%
  mutate(
    `2023-2024` = 3 / 12 * `2023` + 9 / 12 * `2024`,
    `2024-2025` = 3 / 12 * `2024` + 9 / 12 * `2025`
  ) %>%
  select(sex, age, `2023-2024`, `2024-2025`) %>%
  gather(`2023-2024`, `2024-2025`, key = year, value = mx_noc) %>%
  mutate(mx_noc = mx_noc / 1e5) %>%
  left_join(pop)

# conflict mortality rates, by posterior draw
mx_cnf <-
  dxs %>%
  left_join(pop) %>%
  mutate(mx = dx / pop)

# all-cause mortality = expected + conflict
dt <-
  mx_cnf %>%
  rename(mx_cnf = mx, dx_cnf = dx) %>%
  left_join(bsn2) %>%
  mutate(dx_noc = mx_noc * pop) %>%
  mutate(dx = dx_noc + dx_cnf, mx = mx_noc + mx_cnf)

# life expectancy at birth ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dt_lt <-
  dt %>%
  select(it, year, sex, age, mx, mx_noc) %>%
  gather(mx, mx_noc, key = cause, value = mx) %>%
  mutate(cause = recode(cause, "mx" = "estimated", "mx_noc" = "expected"))

dt_lt2 <-
  dt_lt %>%
  group_by(it, cause, year, sex) %>%
  do(lifetable(dt_in = .data)) %>%
  ungroup() %>%
  filter(age == 0)

exs_cnf <-
  dt_lt2 %>%
  summarise(ex_m = mean(ex), .by = c(cause, year, sex))

lss_cnf <-
  dt_lt2 %>%
  select(-mx, -age) %>%
  spread(cause, ex) %>%
  mutate(lss = expected - estimated) %>%
  summarise(lss_m = mean(lss), .by = c(year, sex))

# estimated: e0 with conflict mortality; expected: e0 without; lss: loss in years;
# lss_prp: loss as a proportion of the expected e0
dt_plot <-
  exs_cnf %>%
  spread(cause, ex_m) %>%
  left_join(lss_cnf) %>%
  mutate(
    lss_prp = lss_m / expected,
    sex = recode(sex, "m" = "Males", "f" = "Females", "t" = "Total")
  )

write_rds(dt_plot, "data_inter/ests_persp_demograficas2.rds")

# conflict deaths by war year ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# total conflict deaths: mean and 95% interval over posterior draws
deaths_by_draw <-
  mx_cnf %>%
  filter(sex != "t") %>%
  summarise(dx = sum(dx), .by = c(it, year))

deaths_summary <-
  deaths_by_draw %>%
  bind_rows(
    deaths_by_draw %>%
      summarise(dx = sum(dx), .by = it) %>%
      mutate(year = "Total")
  ) %>%
  summarise(
    deaths = mean(dx),
    deaths_l = quantile(dx, .025),
    deaths_u = quantile(dx, .975),
    .by = year
  ) %>%
  arrange(year)

write_csv(deaths_summary, "output/conflict_deaths_summary.csv")

# potential years of life lost (PYLL) ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PYLL = deaths x remaining life expectancy of the no-conflict life table;
# average age at death = mean age of conflict deaths
ex_expected <-
  dt_lt %>%
  group_by(it, cause, year, sex) %>%
  do(lifetable(dt_in = .data)) %>%
  ungroup() %>%
  filter(cause == "expected") %>%
  select(it, year, sex, age, ex)

pyll <-
  mx_cnf %>%
  left_join(ex_expected) %>%
  mutate(pyll = dx * ex, adts = dx * age)

summarise_pyll <- function(dat, by) {
  dat %>%
    summarise(
      pyll = sum(pyll),
      dts = sum(dx),
      adts = sum(adts),
      .by = c(it, all_of(by))
    ) %>%
    summarise(
      pyll = mean(pyll),
      dts = mean(dts),
      adts = mean(adts),
      .by = all_of(by)
    ) %>%
    mutate(
      pyll_per_death = pyll / dts,
      mean_age_at_death = adts / dts
    ) %>%
    select(-adts) %>%
    arrange(across(all_of(by)))
}

pyll_summary <-
  bind_rows(
    summarise_pyll(pyll, by = c("year", "sex")),
    summarise_pyll(pyll, by = "sex") %>% mutate(year = "2023-2025")
  )

write_csv(pyll_summary, "output/pyll_summary.csv")
