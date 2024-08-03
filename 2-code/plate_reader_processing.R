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
  filter(!is.na(time)) %>% 
  mutate(time = as.numeric(time)) %>% 
  arrange(Plate, time) %>% 
  rowwise() %>% 
  # now clean up the time
  # #1: importing from excel f-d up the format
  # convert to the correct time format (h:m:s)
  # #2: the time is split across multiple sheets/plates, 
  # we need to add them all for an overall cumulative time
  # so, calculate the difference between timesteps and then do a cumulative addition
  group_by(Plate) %>% 
  mutate(
         time = chron::times(time),
         time_diff = time - lag(time),
         time_diff = case_when(is.na(time_diff) ~ time, TRUE ~ time_diff),
         time_hr = as.numeric(hms(time_diff), units = "hours")) %>% 
  ungroup() %>% 
  filter(!is.na(time_hr)) %>% 
  mutate(time_hr_cum = cumsum(time_hr)) %>% 
  # now drop all the unnecessary columns
  dplyr::select(-time, -time_diff, -time_hr, -Plate) %>% 
  pivot_longer(cols = -c(time_hr_cum), names_to = "well", values_to = "abs_600") %>% 
  left_join(map_processed) %>% 
  mutate(time_hr_cum = as.numeric(time_hr_cum),
         abs_600 = as.numeric(abs_600))

# graph
data_processed %>% 
  ggplot(aes(x = time_hr_cum, y = abs_600, group = sample_name, color = sample_name))+
  geom_point()+
 # geom_line()+
  facet_wrap(~sample_name)


