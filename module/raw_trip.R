pacman::p_load('tidyverse')

nms <- names(read_csv("SG_Bus_Commuting/data/origin_destination_bus_202210.csv"))
nms
#ct <- ifelse(grepl("^(D|O|P)", nms), "text", "numeric")
#ct
oct <- read_csv("SG_Bus_Commuting/data/origin_destination_bus_202210.csv",col_types = cols(
  YEAR_MONTH = col_date(format = "%Y-%m"),
  DAY_TYPE = col_character(),
  TIME_PER_HOUR = col_character(),
  PT_TYPE = col_character(),
  ORIGIN_PT_CODE = col_character(),
  DESTINATION_PT_CODE = col_character(),
  TOTAL_TRIPS = col_integer()
) )
nov <- read_csv("SG_Bus_Commuting/data/origin_destination_bus_202211.csv",col_types = cols(
  YEAR_MONTH = col_date(format = "%Y-%m"),
  DAY_TYPE = col_character(),
  TIME_PER_HOUR = col_character(),
  PT_TYPE = col_character(),
  ORIGIN_PT_CODE = col_character(),
  DESTINATION_PT_CODE = col_character(),
  TOTAL_TRIPS = col_integer()
) )
dec <- read_csv("SG_Bus_Commuting/data/origin_destination_bus_202212.csv",col_types = cols(
  YEAR_MONTH = col_date(format = "%Y-%m"),
  DAY_TYPE = col_character(),
  TIME_PER_HOUR = col_character(),
  PT_TYPE = col_character(),
  ORIGIN_PT_CODE = col_character(),
  DESTINATION_PT_CODE = col_character(),
  TOTAL_TRIPS = col_integer()
) )
rawdata <- bind_rows(oct,nov,dec)

saveRDS(oct,file="SG_Bus_Commuting/data/oct_data.rds")
saveRDS(nov,file="SG_Bus_Commuting/data/nov_data.rds")
saveRDS(dec,file="SG_Bus_Commuting/data/dec_data.rds")
saveRDS(rawdata,file="SG_Bus_Commuting/data/total_data.rds")


#totalrds <- readRDS("data/total_data.rds")

