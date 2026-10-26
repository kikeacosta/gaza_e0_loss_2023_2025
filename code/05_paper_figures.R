rm(list = ls())
source("code/00_setup.R")

dt4 <- read_rds("data_inter/ests_persp_demograficas.rds")
dt_plot <- read_rds("data_inter/ests_persp_demograficas2.rds")


# English ====
# ~~~~~~~~~~~~~

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
    y = "Contribution to the relative\nmortality increase from war"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    axis.title = element_text(face = "bold", size = 8.5),
    panel.border = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 9)
  )

ggsave("figures/persp_dem/01_psc_cnt.pdf", w = 8, h = 3)
ggsave("figures/persp_dem/01_psc_cnt.png", dpi = 600, w = 8, h = 3)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
ggsave("figures/persp_dem/02_lss_horiz.pdf", w = 6, h = 2.1)
ggsave("figures/persp_dem/02_lss_horiz.png", dpi = 600, w = 6, h = 2.1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Catalá ====
# ~~~~~~~~~~~~~

unique(dt4$sex2)
dt4_cat <-
  dt4 |>
  mutate(
    sex2 = recode(
      sex2,
      "Children 0-14" = "Infants 0-14",
      "Females 15-59" = "Dones 15-59",
      "Males 15-59" = "Homes 15-59",
      "Old adults 60+" = "Gent gran 60+"
    )
  )


ttt <-
  dt4_cat %>%
  summarise(tst = sum(tst), .by = c(year)) %>%
  mutate(
    yrs_bsn = ifelse(year == "2000-2023", "2023-2025", as.character(year)),
    txt = ifelse(
      year == "2000-2023",
      paste0(round(tst, 1), "-vegades la ", yrs_bsn, "\nmortalitat esperada"),
      paste0(round(tst, 1), "-vegades la \nmortalitat esperada")
    )
  )

smm <-
  dt4_cat %>%
  summarise(cnt = 100 * sum(tst_prop), .by = c(year, sex2)) %>%
  mutate(
    y_pos = case_when(
      sex2 == "Infants 0-14" ~ .15,
      sex2 == "Dones 15-59" ~ .12,
      sex2 == "Homes 15-59" ~ .09,
      sex2 == "Gent gran 60+" ~ .06
    ),
    x_pos = "80"
  )


cols4 <- c("#9E86C1", "#ff595e", "#1982c4", "#896BB3")
dt4_cat %>%
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
    x = "Edat",
    fill = "Grup",
    y = "Contribució a l'augment relatiu\nde la mortalitat per guerra"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    axis.title = element_text(face = "bold", size = 8.5),
    panel.border = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 9)
  )

ggsave("figures/persp_dem/01_psc_cnt_cat.pdf", w = 8, h = 3)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cols <- c("#6a4c93", "#1982c4", "#ff595e")
dt_plot %>%
  mutate(
    sex = forcats::fct_recode(
      sex,
      "Dones" = "Females",
      "Homes" = "Males",
      "Total" = "Total"
    ),
    sex = factor(sex, levels = rev(c("Dones", "Homes", "Total"))),
  ) |>
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
  labs(x = "Edat") +
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

ggsave("figures/persp_dem/02_lss_horiz_cat.pdf", dpi = 400, w = 6, h = 2.1)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Español ====
# ~~~~~~~~~~~~~

unique(dt4$sex2)
dt4_cat <-
  dt4 |>
  mutate(
    sex2 = recode(
      sex2,
      "Children 0-14" = "Niños/as 0-14",
      "Females 15-59" = "Mujeres 15-59",
      "Males 15-59" = "Hombres 15-59",
      "Old adults 60+" = "Adultos mayores 60+"
    )
  )


ttt <-
  dt4_cat %>%
  summarise(tst = sum(tst), .by = c(year)) %>%
  mutate(
    yrs_bsn = ifelse(year == "2000-2023", "2023-2025", as.character(year)),
    txt = ifelse(
      year == "2000-2023",
      paste0(round(tst, 1), "-veces la ", yrs_bsn, "\nmortalidad esperada"),
      paste0(round(tst, 1), "-veces la \nmortalidad esperada")
    )
  )

smm <-
  dt4_cat %>%
  summarise(cnt = 100 * sum(tst_prop), .by = c(year, sex2)) %>%
  mutate(
    y_pos = case_when(
      sex2 == "Niños/as 0-14" ~ .15,
      sex2 == "Mujeres 15-59" ~ .12,
      sex2 == "Hombres 15-59" ~ .09,
      sex2 == "Adultos mayores 60+" ~ .06
    ),
    x_pos = "80"
  )


cols4 <- c("#9E86C1", "#ff595e", "#1982c4", "#896BB3")
dt4_cat %>%
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
    x = "Edad",
    fill = "Grupo",
    y = "Contribución al aumento relativo de\nla mortalidad a causa de la guerra"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.border = element_blank(),
    axis.title = element_text(face = "bold", size = 8.5),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 9)
  )

ggsave("figures/persp_dem/01_psc_cnt_esp.pdf", w = 8, h = 3)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cols <- c("#6a4c93", "#1982c4", "#ff595e")
dt_plot %>%
  mutate(
    sex = forcats::fct_recode(
      sex,
      "Mujeres" = "Females",
      "Hombres" = "Males",
      "Total" = "Total"
    ),
    sex = factor(sex, levels = rev(c("Mujeres", "Hombres", "Total"))),
  ) |>
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
  labs(y = "Sexo", x = "Edad") +
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

ggsave("figures/persp_dem/02_lss_horiz_esp.pdf", dpi = 400, w = 6, h = 2.1)
