# Checks the reproduced results against the numbers published in the paper
# (Perspectives Demografiques 42: Figures 1-2 and text). Stops if any value differs.
library(tidyverse)

# Figure 2: life expectancy at birth ====
e0 <- read_rds("data_inter/ests_persp_demograficas2.rds")

published_e0 <- tibble(
  year      = rep(c("2023-2024", "2024-2025"), each = 3),
  sex       = rep(c("Females", "Males", "Total"), 2),
  estimated = c(34.6, 26.2, 29.5, 52.8, 32.9, 40.4),   # e0 during the war year
  expected  = c(78.3, 75.0, 76.7, 78.3, 75.1, 76.8)    # e0 without conflict mortality
)

chk <-
  e0 %>%
  transmute(year, sex, estimated = round(estimated, 1), expected = round(expected, 1)) %>%
  left_join(published_e0, by = c("year", "sex"), suffix = c("", "_paper"))
print(chk)
stopifnot(all(chk$estimated == chk$estimated_paper), all(chk$expected == chk$expected_paper))

# Figure 1: relative mortality increase (12.7 and 7.4 times) and the share of the
# increase borne by children, women and older adults (57% in year 1, over 40% in year 2)
dt4 <- read_rds("data_inter/ests_persp_demograficas.rds")

fig1 <-
  dt4 %>%
  summarise(times_expected = sum(tst), .by = year) %>%
  left_join(
    dt4 %>%
      summarise(share = sum(tst_prop), .by = c(year, sex2)) %>%
      filter(sex2 != "Males 15-59") %>%
      summarise(share_not_males_15_59 = sum(share), .by = year),
    by = "year"
  )
print(fig1)
stopifnot(
  round(fig1$times_expected, 1) == c(12.7, 7.4),
  round(100 * fig1$share_not_males_15_59) == c(57, 42)
)

# conflict deaths: more than 115,000 (104,000 to 129,000) ====
deaths <- read_csv("output/conflict_deaths_summary.csv", show_col_types = FALSE) %>%
  filter(year == "Total")
print(deaths)
stopifnot(deaths$deaths > 115000, round(deaths$deaths_l, -3) == 104000, round(deaths$deaths_u, -3) == 129000)

# years of potential life lost per death (54 women, 49 men) and mean age at death
# (26 women, 28 men) ====
pyll <- read_csv("output/pyll_summary.csv", show_col_types = FALSE) %>%
  filter(year == "2023-2025", sex %in% c("f", "m")) %>%
  arrange(sex)
print(pyll)
stopifnot(
  round(pyll$pyll_per_death) == c(54, 49),
  round(pyll$mean_age_at_death) == c(26, 28)
)

message("OK: the results match the numbers published in the paper.")
