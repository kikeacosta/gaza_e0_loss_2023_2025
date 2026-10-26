rm(list = ls())
source("code/00_setup.R")

dxs <- read_rds("data_inter/conflict_deaths_samples_sex_age_2waryears.rds")
pop <- read_rds("data_inter/adjusted_exposures.rds")
bsn <- read_rds("data_input/data_plus_forecasts_v2.rds")

unique(bsn$source)
unique(bsn$sex)
unique(pop$sex)
unique(dxs$sex)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  left_join(pop) %>%
  mutate(dx1 = mx_noc * pop)

bsn2 %>%
  ggplot() +
  geom_line(aes(age, mx_noc, col = sex, group = sex)) +
  scale_y_log10() +
  facet_grid(~year) +
  theme_bw()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mx_cnf <-
  dxs %>%
  left_join(pop) %>%
  mutate(mx = dx / pop)


mx_cnf %>%
  filter(it == 1) %>%
  group_by(year, sex) %>%
  mutate(cx = dx / sum(dx)) %>%
  ggplot() +
  geom_line(aes(age, cx, col = sex, group = sex)) +
  scale_y_log10() +
  facet_grid(~year) +
  theme_bw()

ggsave("figures/update_gaza_2year_age_str.png")

mx_cnf %>%
  filter(it == 1) %>%
  group_by(year, sex) %>%
  mutate(cx = dx / sum(dx)) %>%
  ggplot() +
  geom_line(aes(age, cx, col = year, group = year)) +
  scale_y_log10() +
  facet_grid(~sex) +
  theme_bw()

age_strs <-
  mx_cnf %>%
  filter(sex != "t") %>%
  group_by(it, year, sex) %>%
  mutate(cx = dx / sum(dx)) %>%
  ungroup()

age_strs_av <-
  age_strs %>%
  summarise(dx = mean(dx), cx = mean(cx), .by = c(year, sex, age)) %>%
  mutate(sex = recode(sex, "m" = "Males", "f" = "Females", "t" = "Total"))

age_strs_av %>%
  mutate(cx = ifelse(sex == "Females", -cx, cx)) %>%
  mutate(
    year = factor(year, levels = c("2023-2024", "2024-2025")),
    age_ad = ifelse(age == 80, 85, (age + lead(age)) / 2)
  ) %>%
  ggplot() +
  geom_line(aes(
    age_ad,
    cx,
    col = sex,
    lty = year,
    group = interaction(year, sex)
  )) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_text(x = 80, y = 0, label = "Females", hjust = 2, col = "#ae2012") +
  geom_text(x = 80, y = 0, label = "Males", hjust = -1, col = "#03071e") +
  scale_color_manual(values = c("#ae2012", "#03071e"), guide = "none") +
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  scale_y_continuous(
    breaks = seq(-.15, .15, .05),
    labels = paste0(abs(round(seq(-.15, .15, .05), 2)) * 100, "%")
  ) +
  labs(x = "Age", y = "Within-sex death share", lty = "War-year") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("figures/gaza_update/update_gaza_2year_age_str.png", w = 5, h = 3)

dt <-
  mx_cnf %>%
  rename(mx_cnf = mx, dx_cnf = dx) %>%
  left_join(bsn2) %>%
  mutate(dx_noc = mx_noc * pop) %>%
  mutate(dx = dx_noc + dx_cnf, mx = mx_noc + mx_cnf)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# p-scores ====
# ~~~~~~~~~~~~~
dt_psc <-
  dt %>%
  select(it, year, sex, age, mx, mx_noc) %>%
  mutate(psc = mx / mx_noc)

dt_psc_m <-
  dt_psc %>%
  summarise(
    psc_m = mean(psc),
    psc_l = quantile(psc, .025),
    psc_u = quantile(psc, .975),
    .by = c(year, sex, age)
  ) %>%
  mutate(sex = recode(sex, "m" = "Males", "f" = "Females", "t" = "Total"))

cols <- c("#ae2012", "#03071e", "#00a6fb")

dt_psc_m %>%
  mutate(
    year = factor(year, levels = c("2023-2024", "2024-2025")),
    age_ad = ifelse(age == 80, 85, (age + lead(age)) / 2)
  ) %>%
  ggplot() +
  geom_segment(
    aes(x = age_ad, y = psc_l, xend = age_ad, yend = psc_u, col = sex),
    alpha = 0.5
  ) +
  geom_point(aes(age_ad, psc_m, col = sex, group = interaction(sex))) +
  geom_line(
    aes(age_ad, psc_m, col = sex, group = interaction(sex)),
    alpha = 0.1
  ) +
  geom_hline(yintercept = 1, lty = "dashed") +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 150)) +
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  scale_color_manual(values = cols) +
  facet_grid(~year) +
  labs(
    x = "Age",
    y = "Mortality Relative Risks (times)",
    col = "Sex",
    fill = "Sex",
    lty = "Mortality"
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave("figures/gaza_update/pscs.png", dpi = 400, w = 6.5, h = 3)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# life expectancy ====
# ~~~~~~~~~~~~~~~~~~~~
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
  summarise(
    ex_m = mean(ex),
    ex_l = quantile(ex, .025),
    ex_u = quantile(ex, .975),
    .by = c(cause, year, sex)
  )

dt_lt3 <-
  dt_lt2 %>%
  select(-mx, -age) %>%
  spread(cause, ex) %>%
  mutate(lss = expected - estimated)

lss_cnf <-
  dt_lt3 %>%
  summarise(
    lss_m = mean(lss),
    lss_l = quantile(lss, .025),
    lss_u = quantile(lss, .975),
    .by = c(year, sex)
  )

dt_plot <-
  exs_cnf %>%
  select(-ex_l, -ex_u) %>%
  spread(cause, ex_m) %>%
  left_join(
    lss_cnf %>%
      select(year, sex, lss_m)
  ) %>%
  mutate(
    lss_prp = lss_m / expected,
    sex = recode(sex, "m" = "Males", "f" = "Females", "t" = "Total")
  )

write_rds(dt_plot, "data_inter/ests_persp_demograficas2.rds")

cols <- c("#df2935", "#3772ff", "#bb29df")
cols <- c("#ff595e", "#1982c4", "#6a4c93")

dt_plot %>%
  ggplot() +
  geom_bar(
    aes(x = sex, y = estimated, fill = sex, col = sex),
    stat = "identity",
    alpha = 0.8,
    lwd = .2
  ) +
  geom_bar(
    aes(x = sex, y = expected, fill = sex, col = sex),
    stat = "identity",
    alpha = 0.25,
    lwd = .2
  ) +
  geom_segment(
    aes(x = sex, y = expected, xend = sex, yend = estimated + .5, col = sex),
    position = position_nudge(x = 0.3), # Shifts the whole segment right
    arrow = arrow(length = unit(0.08, "npc"), type = "closed", angle = 20),
    linewidth = 1.5
  ) +
  geom_text(
    aes(x = sex, y = estimated, label = round(estimated, 1), col = sex),
    hjust = 1,
    vjust = -0.5,
    size = 2.8,
    fontface = "bold"
  ) +
  geom_text(
    aes(x = sex, y = expected, label = round(expected, 1), col = sex),
    hjust = 1,
    vjust = 1.3,
    size = 2.8,
    alpha = 0.7,
    fontface = "bold"
  ) +
  geom_text(
    aes(
      x = sex,
      y = (estimated + expected) / 2,
      label = paste0("- ", round(lss_m, 1), "\n(-", round(100 * lss_prp), "%)"),
      col = sex
    ),
    hjust = .6,
    size = 2.8,
    fontface = "bold"
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  labs(y = "Age", x = "Sex") +
  facet_grid(~year) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    # strip.position = "bottom",
    strip.text = element_text(face = "bold", size = 8),
    # axis.title = element_blank(),
    # axis.text = element_blank(),
    axis.ticks.x = element_blank()
  )
ggsave("figures/gaza_update/lss_vert.png", dpi = 400, w = 4, h = 4)

cols <- c("#6a4c93", "#1982c4", "#ff595e")
dt_plot %>%
  mutate(sex = factor(sex, levels = rev(c("Females", "Males", "Total")))) |>
  ggplot() +
  facet_grid(~year) +
  geom_bar(
    aes(x = estimated, y = sex, fill = sex, col = sex),
    stat = "identity",
    alpha = 0.8,
    lwd = .2
  ) +
  geom_bar(
    aes(x = expected, y = sex, fill = sex, col = sex),
    stat = "identity",
    alpha = 0.25,
    lwd = .2
  ) +
  geom_segment(
    aes(x = expected, y = sex, xend = estimated + 0.8, yend = sex, col = sex),
    position = position_nudge(y = 0), # Shifts the whole segment right
    arrow = arrow(length = unit(0.08, "npc"), type = "closed", angle = 20),
    linewidth = 1.5
  ) +
  geom_text(
    aes(x = estimated, y = sex, label = round(estimated, 1), col = sex),
    position = position_nudge(y = -0.3),
    hjust = -0.3,
    size = 2.5,
    fontface = "bold"
  ) +
  geom_text(
    aes(x = expected, y = sex, label = round(expected, 1), col = sex),
    position = position_nudge(y = -0.3),
    hjust = 1.1,
    size = 2.5,
    alpha = 0.7,
    fontface = "bold"
  ) +
  geom_text(
    aes(
      x = (estimated + expected) / 2,
      y = sex,
      label = paste0("- ", round(lss_m, 1), " (-", round(100 * lss_prp), "%)"),
      col = sex
    ),
    position = position_nudge(y = 0.25),
    # vjust = .6,
    size = 2.9,
    fontface = "bold"
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  # scale_y_reverse()+
  labs(y = "Sex", x = "Age") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    # strip.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(face = "bold", size = 9),
    axis.ticks.y = element_blank(),
    # Adjusting the major grid lines (the ones with labels)
    panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
    # Making minor grid lines even more faint or removing them
    panel.grid.minor = element_line(color = "gray96", linewidth = 0.15)
  )
ggsave("figures/gaza_update/lss_horiz.png", dpi = 400, w = 6, h = 2.1)
# ggsave("figures/persp_dem/02_lss_horiz.pdf", dpi = 400, w = 6, h = 2.1)

# table of detailed death counts by age and sex ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

t1 <-
  mx_cnf %>%
  summarise(dx = mean(dx), .by = c(year, sex, age)) %>%
  arrange(year, sex, age)

t2 <-
  t1 %>%
  bind_rows(
    t1 %>%
      summarise(dx = sum(dx), .by = c(year, sex))
  ) %>%
  mutate(age = age %>% as.character()) %>%
  replace_na(list(age = "Total")) %>%
  arrange(as.numeric(age))

t3 <-
  t2 %>%
  bind_rows(
    t2 %>%
      summarise(dx = sum(dx), .by = c(sex, age)) %>%
      mutate(year = "Total")
  )

t4 <-
  t3 %>%
  pivot_wider(
    names_from = c(sex, year),
    values_from = dx,
    names_sep = "_"
  ) %>%
  arrange(as.numeric(age))

copy_this(t4)


# table of summary death counts by age group and sex ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ts1 <-
  mx_cnf %>%
  filter(sex != "t") %>%
  mutate(
    fem = ifelse(sex == "f", dx, 0),
    cfo = ifelse(sex == "f" | age <= 17 | age >= 65, dx, 0),
  ) %>%
  summarise(dx = sum(dx), fem = sum(fem), cfo = sum(cfo), .by = c(it, year))

ts2 <-
  ts1 %>%
  summarise(dx = sum(dx), fem = sum(fem), cfo = sum(cfo), .by = c(it)) %>%
  mutate(year = "Total")

ts3 <-
  ts1 %>%
  bind_rows(ts2) %>%
  mutate(fem_p = fem / dx, cfo_p = cfo / dx) %>%
  summarise(
    tot = mean(dx) / 1e3,
    tot_l = quantile(dx, .025) / 1e3,
    tot_u = quantile(dx, .975) / 1e3,
    fem = round(mean(fem_p), 2),
    cfo = round(mean(cfo_p), 2),
    .by = c(year)
  ) %>%
  arrange(year) %>%
  mutate(
    tot = paste0(
      round(tot, 2),
      " (",
      round(tot_l, 2),
      "-",
      round(tot_u, 2),
      ")"
    )
  ) %>%
  select(-tot_l, -tot_u)

copy_this(ts3)


# Lost of potential years of life (PYLL) ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
tst <-
  dt_lt %>%
  group_by(it, cause, year, sex) %>%
  do(lifetable(dt_in = .data)) %>%
  ungroup()

tst2 <-
  tst %>%
  filter(cause == "expected") |>
  select(it, year, sex, age, ex)

unique(tst2$year)
unique(tst$cause)
unique(mx_cnf$sex)

mx_cnf |>
  filter(sex != "t") |>
  summarise(dx = sum(dx), .by = c(it)) |>
  summarise(
    dx_m = mean(dx),
    dx_l = quantile(dx, .025),
    dx_u = quantile(dx, .975)
  )

pyll <-
  mx_cnf |>
  left_join(tst2) |>
  mutate(pyll = dx * ex, adts = dx * age)

pyll |>
  summarise(
    pyll = sum(pyll),
    dts = sum(dx),
    adts = sum(adts),
    .by = c(it, year, sex)
  ) |>
  summarise(
    pyll = mean(pyll),
    pyll_l = quantile(pyll, .025),
    pyll_u = quantile(pyll, .975),
    dts = mean(dts),
    adts = mean(adts),
    .by = c(year, sex)
  ) |>
  mutate(
    pyll_pc = pyll / dts,
    adts_pc = adts / dts,
    trc = pyll / (pyll + adts)
  ) |>
  arrange(sex)

pyll |>
  summarise(
    pyll = sum(pyll),
    dts = sum(dx),
    adts = sum(adts),
    .by = c(it, sex)
  ) |>
  summarise(
    pyll = mean(pyll),
    pyll_l = quantile(pyll, .025),
    pyll_u = quantile(pyll, .975),
    dts = mean(dts),
    adts = mean(adts),
    .by = c(sex)
  ) |>
  mutate(
    pyll_pc = pyll / dts,
    adts_pc = adts / dts,
    trc = pyll / (pyll + adts)
  ) |>
  arrange(sex)


ex_tst <-
  mx_cnf |>
  filter(it == 1) |>
  group_by(it, year, sex) %>%
  do(lifetable(dt_in = .data)) %>%
  ungroup() |>
  filter(age == 0) |>
  summarise(
    ex = mean(ex),
    dx_l = quantile(ex, .025),
    ex_u = quantile(ex, .975),
    .by = c(year, sex)
  )
