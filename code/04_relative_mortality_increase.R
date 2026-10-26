# Relative mortality increase from the conflict (Figure 1): conflict deaths relative to
# the deaths expected in the absence of conflict, by war year, sex and age.
rm(list = ls())
source("code/00_setup.R")

dxs <- read_rds("data_inter/conflict_deaths_samples_sex_age_2waryears.rds")
pop <- read_rds("data_inter/adjusted_exposures.rds")
bsn <- read_rds("data_input/data_plus_forecasts_v2.rds")

# expected (no-conflict) deaths ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lee-Carter projections into 2023, 2024 and 2025, weighted to the war years
# (7 Oct - 6 Oct), times the exposures
bsn2 <-
  bsn %>%
  filter(
    source == "lc_pcbs_2019",
    region == "Gaza Strip",
    sex != "t",
    year >= 2023
  ) %>%
  select(year, sex, age, bsn = mx_noc) %>%
  spread(year, bsn) %>%
  mutate(
    `2023-2024` = 3 / 12 * `2023` + 9 / 12 * `2024`,
    `2024-2025` = 3 / 12 * `2024` + 9 / 12 * `2025`
  ) %>%
  select(sex, age, `2023-2024`, `2024-2025`) %>%
  gather(`2023-2024`, `2024-2025`, key = year, value = mx_noc) %>%
  mutate(mx_noc = mx_noc / 1e5) %>%
  left_join(pop) %>%
  mutate(bsn = mx_noc * pop) %>%
  select(-mx_noc, -pop)

# conflict deaths (mean over draws) and expected deaths ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dt <-
  dxs %>%
  filter(sex != "t") %>%
  summarise(cnf = mean(dx), .by = c(year, sex, age)) %>%
  left_join(bsn2)

# psc: age-specific relative mortality (conflict deaths / expected deaths)
# tst: conflict deaths relative to all expected deaths in the war year (the
#      overall multiplier is the sum of tst over ages)
# tst_prop: contribution of each age and sex to the overall multiplier
# sex2: age-sex groups of Figure 1
dt4 <-
  dt %>%
  mutate(psc = cnf / bsn) %>%
  group_by(year) %>%
  mutate(bsn_t = sum(bsn), tst = cnf / bsn_t, tst_prop = tst / sum(tst)) %>%
  ungroup() %>%
  mutate(
    year = factor(year, levels = c("2023-2024", "2024-2025")),
    sex2 = case_when(
      sex == "f" & age %in% 15:59 ~ "Females 15-59",
      sex == "m" & age %in% 15:59 ~ "Males 15-59",
      age >= 60 ~ "Old adults 60+",
      age < 15 ~ "Children 0-14"
    ),
    sex2 = factor(
      sex2,
      levels = c(
        "Children 0-14",
        "Females 15-59",
        "Males 15-59",
        "Old adults 60+"
      )
    )
  )

write_rds(dt4, "data_inter/ests_persp_demograficas.rds")
