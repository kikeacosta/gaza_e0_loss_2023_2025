rm(list=ls())
source("code/00_setup.R")

ps1 <- read_xlsx("data_input/btselem_pse_killed_by_isr_cvs.xlsx")
ps2 <- read_xlsx("data_input/btselem_pse_killed_by_isr_fcs.xlsx")
ps3 <- read_xlsx("data_input/btselem_pse_killed_by_isr_unk.xlsx")


gza <- 
  bind_rows(ps1, ps2, ps3) %>% 
  select(date = 2,
         age = 3, 
         reg = 7,
         # date = 8,
         sex = 9) %>% 
  mutate(date = ymd(date),
         sex = str_to_lower(sex)) %>% 
  filter(reg == "Gaza Strip",
         date <= "2023-10-07")

gza2 <- 
  gza %>% 
  mutate(age = case_when(age == 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age >= 80 ~ 80, 
                         TRUE ~ age-age%%5)) %>% 
  summarise(dx = n(),
            .by = c(sex, age)) %>% 
  arrange(sex, age)

write_rds(gza2, "data_inter/btselem_gaza_2000-2023_age_sex.rds")
