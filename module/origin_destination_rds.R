packages <- c('shiny', 'shinydashboard', 'shinythemes', 
              'sf','tmap',
              'plotly', 'tidyverse', 'ggstatsplot', 
              'tools')

for (p in packages){
  library(p, character.only=T)
}

#------------------------prepare for mpxx_origin/destination)----------------------------#
#read trip data
oct <- readRDS(file = "SG_Bus_Commuting/data/rds/oct_data.rds") #2022-10 total trips
nov <- readRDS(file = "SG_Bus_Commuting/data/rds/nov_data.rds") #2022-11 total trips
dec <- readRDS(file = "SG_Bus_Commuting/data/dec_data.rds") #2022-12 total trips
total <- readRDS(file = "SG_Bus_Commuting/data/rds/total_data.rds")

#read busstop info data
bus_info <- read_csv("SG_Bus_Commuting/data/BusStopURA2014SZ.csv") #bustop_code and pa, region info


#Data Wrangling
origin <- left_join(total,bus_info, by = c('ORIGIN_PT_CODE' = 'BUS_STOP_N')) %>%
  select(1:3,5:7,10,12,14)
destination <- left_join(total,bus_info, by = c('DESTINATION_PT_CODE' = 'BUS_STOP_N')) %>%
  select(1:3,5:7,10,12,14)
write_rds(origin,'SG_Bus_Commuting/data/rds/origin.rds')
write_rds(destination,'SG_Bus_Commuting/data/rds/destination.rds')

#sample dec_origin
origin_dec <- left_join(dec,bus_info, by = c('ORIGIN_PT_CODE' = 'BUS_STOP_N')) %>%
  select(1:3,5:7,10,12,14)
#write_rds(origin_dec,'SG_Bus_Commuting/data/rds/origin_dec.rds')

#sample dec_origin_newhr_subzone
#origin_dec_read <- readRDS(file = "SG_Bus_Commuting/data/rds/origin_dec.rds")

origin_dec_wider <- origin_dec %>%
  group_by(DAY_TYPE,TIME_PER_HOUR,SUBZONE_N) %>%
  summarise(`TOTAL_TRIPS` = sum(`TOTAL_TRIPS`)) %>%
  ungroup() %>%
  pivot_wider(names_from = TIME_PER_HOUR,
              values_from = TOTAL_TRIPS) %>%
  mutate(`Total` = rowSums(.[3:26],na.rm = TRUE))

write_rds(origin_dec_wider,'SG_Bus_Commuting/data/rds/origin_dec_wider.rds')

origin_dec_newhr_sz  
  mutate(`0-6` = rowSums(.[3:4],na.rm = TRUE)+ rowSums(.[15],na.rm = TRUE) + rowSums(.[20:23],na.rm = TRUE)) %>%
  mutate(`7-10` = rowSums(.[24:26],na.rm = TRUE) + rowSums(.[5],na.rm = TRUE)) %>%
  mutate (`11-14` = rowSums(.[6:9],na.rm = TRUE) ) %>%
  mutate(`15-17` = rowSums(.[10:12],na.rm = TRUE)) %>%
  mutate(`18-20` = rowSums(.[13:14],na.rm = TRUE) + rowSums(.[16],na.rm = TRUE)) %>%
  mutate (`21-23`= rowSums(.[17:19],na.rm = TRUE)) %>%
  mutate(`Total` = rowSums(.[3:26],na.rm = TRUE)) %>%
  select(`DAY_TYPE`,`SUBZONE_N`,`0-6`,`7-10`,`11-14`,`15-17`,`18-20`,`21-23`,`Total`)
write_rds(origin_dec_newhr_sz,'SG_Bus_Commuting/data/rds/origin_dec_newhr_sz.rds')




