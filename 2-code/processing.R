## Use this script to read plate-reader data for Ebstein assay
## Calibrate the data, and plot graphs
## KFP, 2023-05-08

library(readxl)
library(tidyverse)
library(data.table)
theme_set(theme_bw())

# PLATE INFO ----
## the plate was run multiple times, at different times
## i.e., plate 1-7
## determine "time elapsed" for each plate.

plate_key = read.csv("1-data/ebstein_plate_key.csv")
plate_key_processed = 
  plate_key %>% 
  mutate(datetime = paste(date, time),
         datetime = lubridate::mdy_hms(datetime),
         time_elapsed_min = difftime(datetime, min(datetime), units = "mins"),
         time_elapsed_min = as.numeric(time_elapsed_min),
         time_elapsed_min = round(time_elapsed_min))
  

#
# PLATE MAP ----
## load the plate map and use this as the sample key

map = read.csv("1-data/ebstein_plate_map.csv") 
# convert blank cells to NA
map[map == ''] <- NA

map_processed = 
  map %>% 
  rename(letter = X) %>% 
  pivot_longer(-letter, names_to = "number") %>% 
  drop_na() %>% 
  mutate(number = str_remove(number, "X"),
         number = str_pad(number, 2, pad = "0"), # turn 1 into 01, 2 -> 02, etc.
         well_position = paste0(letter, number),
         # set sample type, i.e. "sample" or "standard"
         sample_type = case_when(grepl("01", well_position) ~ "standard",
                                 TRUE ~ "sample" ),
         standard_uM = case_when(sample_type == "standard" ~ parse_number(value))
         )

#
# DATA ----
path <- "1-data/ebstein_plate_assay.xlsx"
# import all tabs from the Excel spreadsheet
data = 
  path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path, skip= 25) # make sure all sheets have data starting on line 26

# convert list into single dataframe
data_df = rbindlist(data, fill=TRUE, idcol = "plate")

processed = 
  data_df %>% 
  mutate_all(as.character) %>% 
  rename(letter = `...1`) %>% 
  dplyr::select(-`...14`) %>% 
  pivot_longer(-c(plate, letter), names_to = "number", values_to = "intensity") %>% 
  mutate(number = str_pad(number, 2, pad = "0"),
         well_position = paste0(letter, number),
         intensity = as.numeric(intensity)) %>% 
  dplyr::select(plate, well_position, intensity) %>% 
  left_join(map_processed %>% dplyr::select(well_position, sample_type, value, standard_uM)) %>% 
  filter(!is.na(sample_type)) %>% 
  left_join(plate_key_processed %>% dplyr::select(plate, time_elapsed_min))


calibrate_data = function(processed){

  standards = 
    processed %>% 
    filter(grepl("01", well_position)) 
  
  standards %>% 
    ggplot(aes(x = standard_uM, y = intensity, color = plate))+
    geom_point()+ 
    geom_line()+
    geom_smooth(method = "lm", se = FALSE)+
    facet_wrap(~ plate)
  
  calibration_coef = 
    standards %>% 
    dplyr::group_by(plate) %>% 
    dplyr::summarize(slope = lm(intensity ~ standard_uM)$coefficients["standard_uM"], 
                     intercept = lm(intensity ~ standard_uM)$coefficients["(Intercept)"])
  
  # y = mx + c
  # abs = m*ppm + c
  # ppm = abs-c/m
  
  # data_processed2 = 
  processed %>% 
    left_join(calibration_coef) %>% 
    mutate(uM_urea_calculated = ((intensity - intercept) / slope),
           uM_urea_calculated = if_else(uM_urea_calculated < 0, 0, uM_urea_calculated))
  
}

samples = 
  processed %>% 
  # do calibrations
  calibrate_data() %>% 
  filter(sample_type == "sample") %>% 
  mutate(uM_inhibitor = parse_number(value))
# ^ this dataframe is the processed, cleaned version, 
# with 3 reps per treatment

# calculate summary (mean) for line graph
samples_summary = 
  samples %>% 
  group_by(plate, time_elapsed_min, uM_inhibitor) %>% 
  dplyr::summarise(mean = mean(uM_urea_calculated)) %>% 
  mutate(time_elapsed_min = as.factor(time_elapsed_min),
         # set the factor levels, 1101 should be last
         time_elapsed_min = fct_relevel(time_elapsed_min, "1101", after = Inf))

#
# GRAPHS ----

## all data, color by time
samples_summary %>% 
  ggplot(aes(x = uM_inhibitor, y = mean, color = time_elapsed_min, fill = time_elapsed_min))+
  geom_line(size = 0.7)+
  geom_point(size = 3, shape = 21, color = "black")+ 
  scale_color_brewer(palette = "YlOrRd")+
  scale_fill_brewer(palette = "YlOrRd")+
 # facet_wrap(~plate)+
 labs(y = "uM urea",
      x = "uM inhibitor added",
      color = "time elapsed (min)",
      fill = "time elapsed (min)")+
  scale_x_log10()

## plot only 62.5 uM data
samples_summary %>% 
  filter(uM_inhibitor == 62.5) %>% 
  ggplot(aes(x = time_elapsed_min, y = mean, 
         #    color = time_elapsed_min
         ))+
  geom_point()+ geom_line(group = 1)+
  scale_color_brewer(palette = "PuOr")+
  annotate("text", label = "stops inhibiting", x = 5.5, y = 100, angle = 45)+
  labs(y = "uM urea",
       title = "for 62.5 uM inhibitor")+
  NULL



