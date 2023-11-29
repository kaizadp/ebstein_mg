library(tidyverse)
library(readxl)
theme_set(theme_bw())

# load files 
calibration = read_excel("1-data/Ebselen_plate_1.4.xlsx", sheet = "30min post stop_kfp",
                         skip = 1) %>% 
  dplyr::select(`intensity...1`, concentration)

data = read_excel("1-data/Ebselen_plate_1.4.xlsx", sheet = "30min post stop_kfp",
                         skip = 1) %>% 
  dplyr::select(time_for_urea, inhibitor_concentration, cell_concentration, `intensity...7`, skip)


# process
calibration_processed = 
  calibration %>% 
  rename(intensity = `intensity...1`,
         concentration_uM = concentration) %>% 
  mutate(concentration_uM = parse_number(concentration_uM)) %>%
  drop_na()

data_processed = 
  data %>% 
  rename(intensity = `intensity...7`) %>% 
  filter(is.na(skip)) %>% 
  dplyr::select(-skip) %>% 
  mutate(inhibitor_concentration = factor(inhibitor_concentration, 
                                          levels = c("31.25um", "62.5um", "125um"))) %>% 
  drop_na()

data_processed %>% 
  ggplot(aes(x = time_for_urea, y = intensity, color = inhibitor_concentration))+
  geom_point()+
  facet_wrap(~cell_concentration)
