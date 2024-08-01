# load packages
library(tidyverse)
theme_set(theme_bw())


# load files
map = read.csv("1-data/msc2_growth_map.csv")
data = read.csv("1-data/MSC2_growth_R.csv", skip = 27)

# process
map_processed = 
  map %>% 
  pivot_longer(cols = -X, names_to = "number", values_to = "sample") %>% 
  mutate(number = parse_number(number),
         well = paste0(X, number)) %>% 
  dplyr::select(well, sample) %>% 
  filter(!grepl("SKIP", sample)) %>% 
  separate(sample, sep = " ", into = c("sample_name", "rep"), remove = F) %>% 
  mutate_all(as.character)

data_processed = 
  data %>% 
  dplyr::select(-X, -`T..600`) %>% 
  rename(time = Time) %>% 
  mutate(time_hr = as.numeric(hms(time), units = "hours")) %>% 
  mutate_all(as.numeric) %>% 
  filter(!is.na(time_hr)) %>% 
  pivot_longer(cols = -time_hr, names_to = "well", values_to = "abs_600") %>% 
  left_join(map_processed) %>% 
  drop_na()

# graph
data_processed %>% 
  ggplot(aes(x = time_hr, y = abs_600, group = sample, color = sample_name))+
  geom_line()


