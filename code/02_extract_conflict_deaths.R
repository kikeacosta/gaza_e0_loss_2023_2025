rm(list=ls())
source("code/00_setup.R")

## set the working directory
results_dir <- "data_input/posterior_samples/"

## get the age groups for the lifetable calculations 
x <- readRDS("data_input/pi_x_moh_2024.rds") %>% 
  pull(age) %>% 
  unique()

file_names2324 <- paste0("moh_23_24_samples_", 1:4)
file_names2425 <- paste0("moh_24_25_samples_", 1:4)

dxs_out <- 
  bind_rows(
    gimeconfdeaths(file_names2324) %>% 
      mutate(year = "2023-2024"),
    gimeconfdeaths(file_names2425) %>% 
      mutate(year = "2024-2025")
  )

dxs_out %>% 
  filter(it == 1) %>% 
  ggplot()+
  geom_line(aes(age, dx, col = sex, group = interaction(sex)))+
  scale_y_log10()+
  facet_grid(cause~year)+
  theme_bw()

dxs_out %>% 
  filter(it == 1) %>% 
  summarise(dx = sum(dx))

# need to extract combatants deaths in 2023 from the ~1000 persons killed in Israel
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Combatant mortality for 2023 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# moving Oct 7. Hamas combatants from expected to conflict ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dx_cmb <- readRDS("data_input/Dx_cmb.rds") 

dx_cmb2 <- 
  dx_cmb %>%
  rename(dx_cmb = Dx_cmb_mean) %>% 
  mutate(year = "2023-2024")

dx_cmb3 <- 
  dx_cmb2 %>% 
  summarise(sum(dx_cmb),
            .by = sex)

## Age-sex specific mortality rates for COMBATANTS (2023 and 2023-2024 only)
dxs_out2 <- 
  dxs_out %>% 
  left_join(dx_cmb2) %>% 
  replace_na(list(dx_cmb = 0)) %>% 
  mutate(dx2 = dx + dx_cmb) %>% 
  select(it, year, sex, age, dx = dx2)

dxs_out3 <- 
  dxs_out2 %>% 
  bind_rows(dxs_out2 %>% 
              reframe(dx = sum(dx), .by = c(it, year, age)) %>%
              mutate(sex = "t"))

write_rds(dxs_out3, "data_inter/conflict_deaths_samples_sex_age_2waryears.rds")

dxs_out3 %>% 
  filter(it == 1) %>% 
  reframe(dx = sum(dx), .by = c(year, sex))
  
