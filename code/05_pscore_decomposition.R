rm(list = ls())
source("code/00_setup.R")

dxs <- read_rds("data_inter/conflict_deaths_samples_sex_age_2waryears.rds")
pop <- read_rds("data_inter/adjusted_exposures.rds")
bsn <- read_rds("data_input/data_plus_forecasts_v2.rds")
blm <- read_rds("data_inter/btselem_gaza_2000-2023_age_sex.rds")

unique(bsn$source)
unique(bsn$year)
unique(pop$sex)
unique(dxs$sex)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# expected mortality ====
# ~~~~~~~~~~~~~~~~~~~~~~~
# from Lee Carter projections, 2012-2019 into 2023, 2024, and 2025
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# conflict mortality before Oct 7th, 2023 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# from B'Tselem in Gaza

blm2 <-
  blm %>%
  rename(cnf = dx) %>%
  mutate(year = "2000-2023") %>%
  drop_na()


# conflict mortality since Oct 7th, 2023 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yrs_lvs <- c("2023-2024", "2024-2025", "2023-2025", "2000-2023")
# yrs_lvs <- c("2000-2023", "2023-2025", "2023-2024", "2024-2025")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dxs2 <-
  dxs %>%
  filter(sex != "t") %>%
  summarise(
    cnf = mean(dx),
    cnf_l = quantile(dx, 0.025),
    cnf_u = quantile(dx, 0.975),
    .by = c(year, sex, age)
  )

dxs3 <-
  dxs2 %>%
  bind_rows(
    dxs %>%
      filter(sex != "t") %>%
      summarise(dx = sum(dx), .by = c(it, sex, age)) %>%
      summarise(
        cnf = mean(dx),
        cnf_l = quantile(dx, 0.025),
        cnf_u = quantile(dx, 0.975),
        .by = c(sex, age)
      ) %>%
      mutate(year = "2023-2025"),
    blm %>%
      mutate(year = "2000-2023") %>%
      rename(cnf = dx)
  ) %>%
  mutate(year = factor(year, levels = yrs_lvs)) %>%
  group_by(year) %>%
  mutate(
    age_ad = ifelse(age == 80, 85, (age + lead(age)) / 2),
    sex = recode(sex, "m" = "Males", "f" = "Females")
  )

cols <- c("#ae2012", "#03071e")
dxs3 %>%
  ggplot() +
  geom_segment(
    aes(x = age_ad, y = cnf_l, xend = age_ad, yend = cnf_u, col = sex),
    alpha = 0.5
  ) +
  geom_point(aes(age_ad, cnf, col = sex, group = interaction(sex))) +
  scale_y_continuous(
    breaks = seq(0, 12000, 2000),
    limits = c(0, 12000),
    labels = label_comma()
  ) +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  scale_color_manual(values = cols) +
  facet_wrap(~year, ncol = 2) +
  labs(
    x = "Age",
    y = "Conflict death counts",
    col = "Sex",
    fill = "Sex",
    lty = "Mortality"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    ,
    panel.border = element_blank(),
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("figures/00_dxs.png", dpi = 400, w = 8, h = 4)

dxs3 %>%
  ungroup() %>%
  summarise(cnf = sum(cnf), .by = year)

dxs3 %>%
  drop_na(age) %>%
  ggplot() +
  geom_bar(
    aes(fill = sex, y = cnf, x = factor(age)),
    position = "stack",
    stat = "identity"
  ) +
  geom_text(
    data = dxs3 %>%
      ungroup() %>%
      summarise(cnf = sum(cnf), .by = year),
    aes(
      x = "80",
      y = 10000,
      label = paste0(label_comma()(round(cnf)), " total\nconflict deaths")
    ),
    lineheight = 0.8,
    hjust = 1
  ) +
  scale_y_continuous(breaks = seq(0, 14000, 2000), labels = label_comma()) +
  # scale_x_continuous(breaks = seq(0, 80, 5))+
  scale_fill_manual(values = cols) +
  facet_wrap(~year, ncol = 2) +
  labs(
    x = "Age",
    y = "Conflict death counts",
    col = "Sex",
    fill = "Sex",
    lty = "Mortality"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    ,
    panel.border = element_blank(),
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("figures/00_dxs_bars.png", dpi = 400, w = 8, h = 4)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# merging ====
# ~~~~~~~~~~~~

dt <-
  dxs %>%
  filter(sex != "t") %>%
  summarise(cnf = mean(dx), .by = c(year, sex, age)) %>%
  left_join(bsn2)

dt2 <-
  dt %>%
  bind_rows(
    dt %>%
      filter(year %in% c("2023-2024", "2024-2025")) %>%
      summarise(bsn = sum(bsn), cnf = sum(cnf), .by = c(sex, age)) %>%
      mutate(year = "2023-2025")
  )

dt3 <-
  dt2 %>%
  bind_rows(
    blm2 %>%
      left_join(
        dt2 %>%
          filter(year == "2023-2025") %>%
          select(-year, -cnf)
      )
  )
dt4 <-
  dt3 %>%
  mutate(psc = cnf / bsn) %>%
  group_by(year) %>%
  mutate(bsn_t = sum(bsn), tst = cnf / bsn_t, tst_prop = tst / sum(tst)) %>%
  ungroup() %>%
  mutate(
    year = factor(
      year,
      levels = c("2023-2024", "2024-2025", "2023-2025", "2000-2023")
    ),
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

dt4 %>%
  summarise(
    psc = sum(cnf) / sum(bsn),
    tst = sum(tst),
    tst_prop = sum(tst_prop),
    .by = c(year)
  )

write_rds(dt4, "data_inter/ests_persp_demograficas.rds")

# age-sex distribution ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~
dt4 %>%
  group_by(year) %>%
  mutate(
    cx = cnf / sum(cnf),
    age_ad = ifelse(age == 80, 85, (age + lead(age)) / 2),
    sex = recode(sex, "m" = "Males", "f" = "Females")
  ) %>%
  ggplot() +
  geom_point(aes(age_ad, cx, col = sex, group = interaction(sex))) +
  geom_line(aes(age_ad, cx, col = sex, group = interaction(sex)), alpha = 0.3) +
  geom_text(
    data = dxs3 %>%
      ungroup() %>%
      summarise(cnf = sum(cnf), .by = year),
    aes(
      x = 80,
      y = .2,
      label = paste0(label_comma()(round(cnf)), " total\nconflict deaths")
    ),
    lineheight = 0.8,
    hjust = 1
  ) +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  scale_y_continuous(labels = label_percent()) +
  scale_color_manual(values = cols) +
  facet_wrap(~year, ncol = 2) +
  labs(
    x = "Age",
    y = "Conflict death distribution",
    col = "Sex",
    fill = "Sex",
    lty = "Mortality"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    ,
    panel.border = element_blank(),
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("figures/01_cxs.png", dpi = 400, w = 8, h = 4)


# age-specific p-scores ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

dt4 %>%
  mutate(
    age_ad = ifelse(age == 80, 85, (age + lead(age)) / 2),
    # psc_per = psc*100,
    sex = recode(sex, "m" = "Males", "f" = "Females")
  ) %>%
  ggplot() +
  geom_point(aes(age_ad, psc, col = sex, group = interaction(sex))) +
  geom_line(
    aes(age_ad, psc, col = sex, group = interaction(sex)),
    alpha = 0.3
  ) +
  # geom_text(data = ttt, aes(x = 80, y = 120,
  #                           label = txt),
  #           size = 3.3, fontface = "bold", hjust = 1, lineheight = 0.8,
  #           show.legend = FALSE)+
  scale_y_continuous(breaks = seq(0, 150, 30), labels = label_percent()) +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  scale_color_manual(values = cols) +
  facet_wrap(~year, ncol = 2) +
  labs(
    x = "Age",
    y = "Relative mortality increase\nfrom conflict",
    col = "Sex",
    fill = "Sex",
    lty = "Mortality"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    ,
    panel.border = element_blank(),
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("figures/02_pscs.png", dpi = 400, w = 8, h = 4)


# age-specific contributions to overall p-score ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# age-specific contributions to relative mortality increases
# interpretation: the total mortality increase resulting from each
# age- and sex-specific excess conflict mortality
# e.g., the conflict mortality at ages 25-29 had the same magnitude of the
# expected mortality for all ages in that year

dt4 %>%
  mutate(sex = recode(sex, "m" = "Males", "f" = "Females")) %>%
  ggplot() +
  geom_bar(
    aes(fill = sex, y = tst, x = factor(age)),
    position = "stack",
    stat = "identity"
  ) +
  geom_text(
    data = dt4 %>%
      summarise(tst = sum(tst), .by = c(year)),
    aes(x = "65", y = 1.2, label = paste0(100 * round(tst, 2), "%"))
  ) +
  scale_fill_manual(values = c("#ae2012", "#03071e")) +
  coord_flip() +
  # facet_grid(~year)+
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 1.6, 0.2)) +
  facet_wrap(~year, ncol = 2) +
  labs(
    x = "Age",
    fill = "Sex",
    y = "Contribution to relative excess mortality"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    ,
    panel.border = element_blank(),
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("figures/04_cnts.png", dpi = 400, w = 8, h = 4)

ttt <-
  dt4 %>%
  summarise(tst = sum(tst), .by = c(year)) %>%
  mutate(
    yrs_bsn = ifelse(year == "2000-2023", "2023-2025", as.character(year)),
    txt = ifelse(
      year == "2000-2023",
      paste0(round(tst, 1), "-times the ", yrs_bsn, "\nexpected mortality"),
      paste0(round(tst, 1), "-times the\nexpected mortality")
    )
  )

smm <-
  dt4 %>%
  summarise(cnt = 100 * sum(tst_prop), .by = c(year, sex2)) %>%
  mutate(
    y_pos = case_when(
      sex2 == "Children 0-14" ~ .15,
      sex2 == "Females 15-59" ~ .12,
      sex2 == "Males 15-59" ~ .09,
      sex2 == "Old adults 60+" ~ .06
    ),
    x_pos = "80"
  )

cols4 <- c("#EE9B00", "#ae2012", "#03071e", "#0A9396", "#005F73")
cols4 <- c("#EE9B00", "#ae2012", "#005F73", "#0A9396")

dt4 %>%
  ggplot() +
  geom_bar(
    aes(fill = sex2, y = tst_prop, x = factor(age)),
    position = "stack",
    stat = "identity"
  ) +
  geom_text(
    data = smm,
    aes(x_pos, y_pos, label = paste0(round(cnt), "%"), col = sex2),
    size = 3.3,
    fontface = "bold",
    hjust = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = ttt,
    aes(x = "80", y = .20, label = txt),
    size = 3.3,
    fontface = "bold",
    hjust = 1,
    lineheight = 0.8,
    show.legend = FALSE
  ) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = cols4) +
  scale_color_manual(values = cols4) +
  facet_wrap(~year, ncol = 2) +
  labs(
    x = "Age",
    fill = "Group",
    y = "Contribution to the relative\nmortality increase from conflict"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    ,
    panel.border = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 9)
  )

ggsave("figures/03_psc_cnt.png", w = 8, h = 4)

colores_ced <- c(
  "Children 0-14" = "#95a5a6",
  "Females 15-59" = "#9b1b30",
  "Males 15-59" = "#2c3e50",
  "Old adults 60+" = "#566573"
)

# cols4 <- c("#EE9B00", "#ae2012", "#005F73", "#0A9396")
cols <- c("#ff595e", "#1982c4", "#6a4c93")
cols4 <- c("#9E86C1", "#ff595e", "#1982c4", "#896BB3")
dt4 %>%
  filter(year %in% c("2023-2024", "2024-2025")) |>
  ggplot() +
  geom_bar(
    aes(fill = sex2, y = tst_prop, x = factor(age)),
    position = "stack",
    stat = "identity"
  ) +
  geom_text(
    data = smm %>%
      filter(year %in% c("2023-2024", "2024-2025")),
    aes(x_pos, y_pos / 2, label = paste0(round(cnt), "%"), col = sex2),
    size = 3.3,
    fontface = "bold",
    hjust = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = ttt %>%
      filter(year %in% c("2023-2024", "2024-2025")),
    aes(x = "80", y = .10, label = txt),
    size = 3.3,
    fontface = "bold",
    hjust = 1,
    lineheight = 0.8,
    show.legend = FALSE
  ) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = cols4) +
  scale_color_manual(values = cols4) +
  facet_wrap(~year, ncol = 2) +
  labs(
    x = "Age",
    fill = "Group",
    y = "Contribution to the relative\nmortality increase from conflict"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    ,
    panel.border = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 9)
  )

ggsave("figures/03_psc_cnt_tst.png", w = 8, h = 3)
# ggsave("figures/persp_dem/01_psc_cnt.pdf", w = 8, h = 3)

dt4 %>%
  mutate(cvl = ifelse(sex == "m" & age %in% 20:64, "n", "y")) %>%
  summarise(cnt_cvl = sum(tst_prop), .by = c(year, cvl)) %>%
  spread(cvl, cnt_cvl)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# table of detailed death counts by age and sex ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# first build summary variables within each iteration, then compute
# central values and uncertainty

t1 <-
  dxs %>%
  bind_rows(
    dxs %>%
      summarise(dx = sum(dx), .by = c(it, year, sex))
  ) %>%
  mutate(age = age %>% as.character()) %>%
  replace_na(list(age = "Total")) %>%
  arrange(as.numeric(age))

t2 <-
  t1 %>%
  bind_rows(
    t1 %>%
      summarise(dx = sum(dx), .by = c(it, sex, age)) %>%
      mutate(year = "Total")
  )

t3 <-
  t2 %>%
  summarise(
    dx_m = mean(dx),
    dx_l = quantile(dx, 0.025),
    dx_u = quantile(dx, 0.975),
    .by = c(year, sex, age)
  ) %>%
  arrange(year, sex, age)

t4 <-
  t3 %>%
  mutate(
    dx = paste0(round(dx_m), "\n(", round(dx_l), "-", round(dx_u), ")")
  ) %>%
  select(year, sex, age, dx) %>%
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
  dxs %>%
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


# (original tail of this script -- a draft e0 calculation depending on objects from 04 -- moved to
#  code/archive/05_pscore_decomposition_tail_superseded.R; it wrote no outputs used by the paper)
