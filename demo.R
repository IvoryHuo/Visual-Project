packages <- c('shiny', 'shinydashboard', 'shinythemes', 
              'sf','tmap',
              'plotly', 'tidyverse', 'ggstatsplot', 
              'tools')

for (p in packages){
  library(p, character.only=T)
}

#------------------------prepare for mpxx_origin/destination)----------------------------#
#read trip data
origin <- readRDS(file = "SG_Bus_Commuting/data/rds/origin.rds") 
destination <- readRDS(file = "SG_Bus_Commuting/data/rds/destination.rds") 


#read geospatial data
mpbus <- st_read(dsn = "SG_Bus_Commuting/data/geo/busstop",layer = "BusStop")
mpsz <- st_read(dsn = "SG_Bus_Commuting/data/geo/sz",layer = "MP14_SUBZONE_NO_SEA_PL")
mppa <- st_read(dsn = "SG_Bus_Commuting/data/geo/pa",layer = "MP14_PLNG_AREA_NO_SEA_PL")
mpre <- st_read(dsn = "SG_Bus_Commuting/data/geo/region",layer = "MP14_REGION_WEB_PL")

#Data Wrangling
##Join data with map
#subzone level
mpsz_origin <-left_join(mpsz,origin, by = c('SUBZONE_N'='SUBZONE_N')) %>%
  select(3,16:18,21,24)
#30GB...write_rds(mpsz_origin,'SG_Bus_Commuting/data/rds/mpsz_origin.rds')
#mpsz_destination <- left_join(mpsz,destination, by = c('SUBZONE_N'='SUBZONE_N')) %>%
  #sselect(3,16:18,21,24)

# Question to prof: should we provide many levels? will it be too slow? or we just focus on one month?
#region
mpre_origin <- left_join(mpre,origin, by = c('REGION_N'='REGION_N')) %>%
  select(2,10:12,15,18)
#write_rds(mpre_origin,'SG_Bus_Commuting/data/rds/mpre_origin.rds')

#pa
#mppa_origin <- left_join(mpre,origin, by = c('REGION_N'='REGION_N'))


####------try-plot-map——with categorized time dec data------
origin_dec_wider<- read_rds(file='SG_Bus_Commuting/data/rds/origin_dec_wider.rds')

#plot
mpsz_origin_dec <-left_join(mpsz,origin_dec_wider, by = c('SUBZONE_N'='SUBZONE_N')) %>%
  select(3,16,17:42)
summary(mpsz_origin_dec$Total)



tm_shape(mpsz_origin_dec) +
  tm_polygons()


tm_shape(mpsz_origin_dec) +
  tm_fill("Total",
          #n = 12,
          breaks = c(0,2.5*10^5,6.5*10^5,1.2*10^6,1.5*10^6,2*10^7),
          style = "quantile",
          palatte = "Blues",
          legend.hist = TRUE,
          legend.is.portrait = TRUE,
          legend.hist.z = 0.1) +
  tm_borders(alpha = 0.5) +
  tm_layout(main.title = "Distribution of SG Bus Commuting Trips by planning subzone \n subzone as Origin",
            main.title.position = "center",
            main.title.size = 1,
            legend.height = 0.45,
            legend.width = 0.35,
            legend.outside=FALSE,
            legend.position = c("right","bottom"),
            frame = FALSE)

#time:
#slider
#group

#pa or subzone

#origin or destination

#sample chro shiny-workshop elearn new files























 