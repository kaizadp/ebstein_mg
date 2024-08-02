# load packages
library(tidyverse)
theme_set(theme_bw())
library(readxl)

# load files
map = read.csv("1-data/msc2_growth_map.csv")
#data = read.csv("1-data/MSC2_growth_R.csv", skip = 27)

path = "1-data/MSC2 growth_3.xlsx"
data <- 
  path %>%    
  excel_sheets() %>%    
  set_names() %>%    
  map_dfr(read_excel, skip = 35, path = path, .id = "Plate", col_types = "text")

# process
map_processed = 
  map %>% 
  pivot_longer(cols = -X, names_to = "number", values_to = "sample_name") %>% 
  mutate(number = parse_number(number),
         well = paste0(X, number)) %>% 
  dplyr::select(well, sample_name) %>% 
  mutate_all(as.character)

data_processed = 
  data %>% 
  filter(!is.na(A1)) %>% 
  dplyr::select(-"...1", -"T° 600") %>% 
  rename(time = Time) %>% 
  rowwise() %>% 
  mutate(time = as.numeric(time),
         time = chron::times(time)) %>% 
  filter(!is.na(time)) %>% 
  arrange(Plate, time) %>% 
 # mutate(time_hr = as.numeric(hms(time), units = "hours")) %>% 
 group_by(Plate) %>% 
#  ungroup() %>% 
  dplyr::mutate(time_diff = time - lag(time),
                time_diff = case_when(is.na(time_diff) ~ time, TRUE ~ time_diff),
                time_hr = as.numeric(hms(time), units = "hours")) %>% 
  ungroup() %>% 
#  mutate(time_hr_cum = cumsum(time_hr)) %>% 
#  mutate_all(as.numeric) %>% 
  filter(!is.na(time_hr)) %>% 
  dplyr::select(-time) %>% 
  pivot_longer(cols = -c(time_hr, Plate), names_to = "well", values_to = "abs_600") %>% 
  left_join(map_processed) %>% 
  drop_na()

# graph
data_processed %>% 
  ggplot(aes(x = time_hr, y = abs_600, group = sample_name, color = sample_name))+
  geom_point()+
  geom_line()+
  facet_wrap(~sample_name + Plate)


