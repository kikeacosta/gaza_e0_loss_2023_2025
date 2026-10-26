# Checks the reproduced estimates against the numbers published in the paper
# (Perspectives Demografiques 42, Figure 2 and text). Stops if any value differs.
library(tidyverse)

e0 <- read_rds("data_inter/ests_persp_demograficas2.rds")
print(e0 %>% mutate(across(c(estimated, expected), ~ round(.x, 1))))

published <- tibble(
  year      = rep(c("2023-2024", "2024-2025"), each = 3),
  sex       = rep(c("Females", "Males", "Total"), 2),
  estimated = c(34.6, 26.2, 29.5, 52.8, 32.9, 40.4),   # e0 during the war year
  expected  = c(78.3, 75.0, 76.7, 78.3, 75.1, 76.8)    # e0 without conflict mortality
)

chk <- e0 %>%
  transmute(year, sex, estimated = round(estimated, 1), expected = round(expected, 1)) %>%
  left_join(published, by = c("year", "sex"), suffix = c("", "_paper"))
stopifnot(all(chk$estimated == chk$estimated_paper), all(chk$expected == chk$expected_paper))

dx <- read_rds("data_inter/conflict_deaths_samples_sex_age_2waryears.rds") %>%
  filter(sex == "t") %>%
  summarise(dx = sum(dx), .by = c(it, year))
tot <- dx %>% summarise(dx = sum(dx), .by = it) %>% pull(dx)
cat(sprintf("\nConflict deaths, both war years: %.0f (95%% interval %.0f-%.0f); paper: >115,000 (104,000-129,000)\n",
            mean(tot), quantile(tot, .025), quantile(tot, .975)))
stopifnot(mean(tot) > 115000, round(quantile(tot, .025), -3) == 104000, round(quantile(tot, .975), -3) == 129000)

message("OK: e0 values and conflict-death totals match the paper.")
