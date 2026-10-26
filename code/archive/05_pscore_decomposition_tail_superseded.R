# NOT RUN. Tail of the original 'a05_pscore decomp.R' (draft e0 calculation).
# It depends on objects (mx_cnf) that only exist in 04_mort_rates_e0_yll.R, which is the version used for the paper.

# life expectancy ====
# ~~~~~~~~~~~~~~~~~~~~
dt <-
  mx_cnf %>%
  rename(mx_cnf = mx, dx_cnf = dx) %>%
  left_join(bsn2) %>%
  mutate(dx_noc = mx_noc * pop) %>%
  mutate(dx = dx_noc + dx_cnf, mx = mx_noc + mx_cnf)

dt_lt <-
  dt %>%
  select(it, year, sex, age, mx, mx_noc) %>%
  gather(mx, mx_noc, key = cause, value = mx) %>%
  mutate(cause = recode(cause, "mx" = "estimated", "mx_noc" = "expected"))

dt_lt2 <-
  dt_lt %>%
  group_by(it, cause, year, sex) %>%
  do(lifetable_full(dt_in = .data)) %>%
  ungroup() %>%
  filter(it == 1)

df_wide <-
  dt_lt2 %>%
  mutate(
    cause = case_match(
      cause,
      "estimated" ~ "1",
      "expected" ~ "2",
      .default = cause
    )
  ) %>%
  pivot_wider(
    names_from = cause, # The column containing 'estimated'/'observed'
    values_from = c(mx, lx, Lx, Tx, ex), # The columns you want to spread
    names_glue = "{.value}{cause}" # Optional: renames to 'estimated_mx', etc.
  )

df_wide2 <-
  df_wide %>%
  mutate(
    d_ex = ex1 - ex2,
    d_x = ifelse(
      age != 80,
      (lx1 / 1e5) *
        (Lx2 / lx2 - Lx1 / lx1) +
        (lead(Tx2) / 1e5) * (lx1 / lx2 - lead(lx1) / lead(lx2)),
      (lx1 / 1e5) * (Tx2 / lx2 - Tx1 / lx1)
    )
  )

df_wide2 %>%
  filter(age == 0)

df_wide2 %>%
  summarise(d_ex = sum(d_x), .by = c(year, sex))

males_ch <-
  df_wide %>%
  filter(sex == "m") %>%
  select(it, year, age, mx_m1 = mx1, mx_m2 = mx2)

df_wide3 <-
  df_wide2 %>%
  filter(sex == "t") %>%
  left_join(males_ch) %>%
  mutate(d_x_m = (mx_m1 - mx_m2) / (mx1 - mx2), d_x_f = d_x - d_x_m)

df_wide3 %>%
  summarise(d_ex = sum(d_x), d_ex_m = sum(d_x_m), .by = c(year)) %>%
  mutate
