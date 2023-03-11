#Load R Packages
## Add all your packages here
pacman::p_load('shiny', 'shinydashboard', 'shinythemes', 
              'sf','tmap',
              'plotly', 'tidyverse', 'ggstatsplot', 
              'tools')


# Read rds data file
## compared with csv, rds save space and have a faster speed to read in data
#oct <- readRDS(file = "data/rds/oct_data.rds") #2022-10 total trips
#nov <- readRDS(file = "data/rds/nov_data.rds") #2022-11 total trips
#dec <- readRDS(file = "data/rds/dec_data.rds") #2022-12 total trips
#bus_info <- read_csv("data/BusStopURA2014SZ.csv") #bustop_code and pa, region info
#total <- readRDS(file = "data/rds/total_data.rds") #2022 oct-dec total trips


#huoda original rds file
H_origin_dec_wider <-read_rds(file = "data/rds/origin_dec_wider.rds")
#prachi_original_rds_file


#aish_original_rds_file

#Read geospatial data
## busstop locatioin, subzone, planning area, and region data

#read geospatial data
mpbus <- st_read(dsn = "data/geo/busstop",layer = "BusStop")
mpsz <- st_read(dsn = "data/geo/sz",layer = "MP14_SUBZONE_NO_SEA_PL")
mppa <- st_read(dsn = "data/geo/pa",layer = "MP14_PLNG_AREA_NO_SEA_PL")
mpre <- st_read(dsn = "data/geo/region",layer = "MP14_REGION_WEB_PL")

#Add geospatial dataset below
##huoda mpxx data
H_mpsz_origin_dec <-left_join(mpsz,H_origin_dec_wider, by = c('SUBZONE_N'='SUBZONE_N')) %>%
  select(3,16,17:42)
##aish_mpxx_data


# Set up parameter -see if need to delete
## Add all the paramters here, name it clearly
months <- c("Oct","Nov","Dec")
regions <- c( "Central", "NorthEast", "East", "North", "West")
planning_areas <- c()
cluster_type <- c()
regression_types <- c()

#add a paragraph to introduce the proj
introduction = "Welcome to Singapore Bus Commuting Pattern Interactive Dashboard!" 
#==============================================================================#
                            ###### Shiny UI ######
#==============================================================================#
ui <- navbarPage(
  title = "ShinyIDEA: Interactive SG Bus Commuting Pattern Exploration and Analysis",
  fluid = TRUE,
  theme=shinytheme("flatly"),
  id = "navbarID",
  tabPanel("Introduction",introduction),
  tabPanel("EDA"),
  #prachi and note that the inputId = P_xxx
  navbarMenu("ESDA",
              ##### Shiny UI/Inferential Analysis/Correlation Analysis ##### 
              tabPanel("Number of Commuters",
                       sidebarLayout(
                         sidebarPanel(width = 3,  #inputid should be unique, A/H/P_xx
                                      selectInput(inputId = "H_daytype",
                                                  label = "Weekday or Weekend?",
                                                  choices = c("weekday" = "WEEKDAY",
                                                              "weekend" = "WEEKEND"),
                                                  selected = "weekday",
                                                  multiple = TRUE),
                                      selectInput(inputId = "H_hour",
                                                  label = "Select the hour:",
                                                  choices = c("0am" = "0","1am" = "1",
                                                              "2am" = "2","3am" = "3",
                                                              "4am" = "4","5am" = "5",
                                                              "6am" = "6","7am" = "7",
                                                              "8am" = "8","9am" = "9",
                                                              "10am" = "10","11am" = "11",
                                                              "12pm" = "12","1pm" = "13",
                                                              "2pm" = "14","3pm" = "15",
                                                              "4pm" = "16","5pm" = "17",
                                                              "6pm" = "18","7pm" = "19",
                                                              "8pm" = "20","9pm" = "21",
                                                              "10pm" = "22","11pm" = "23",
                                                              "total" = "Total"
                                                             ),
                                                  selected = "20"),
                                      selectInput(inputId = "H_classification",
                                                  label = "Classification method:",
                                                  choices = list("sd" = "sd", 
                                                                 "equal" = "equal", 
                                                                 "pretty" = "pretty", 
                                                                 "quantile" = "quantile", 
                                                                 "kmeans" = "kmeans", 
                                                                 "hclust" = "hclust", 
                                                                 "bclust" = "bclust", 
                                                                 "fisher" = "fisher", 
                                                                 "jenks" = "jenks"),
                                                  selected = "pretty"),
                                      sliderInput(inputId = "H_classes",
                                                  label = "Number of classes:",
                                                  min = 6,max=12,
                                                  value = c(10)),
                                      selectInput(inputId = "H_color",
                                                  label = "Choose your color scheme:",
                                                  choices = c("blue" = "Blues",
                                                              "red" = "Reds",
                                                              "green" = "Greens",
                                                              "Yellow-Orange-Red" = "YlOrRd"),
                                                  selected = "YlOrRd")
  
                                      
                         ),
                         mainPanel(
                           tmapOutput("H_mapPlot",
                                      width = "100%", 
                                      height = 400)
                         )
                       )),
             tabPanel("Commuter FLow") #note input)d = A_xxx
  ),
  navbarMenu("Cluster Analysis",
             tabPanel("Hierarchical Clustering"),
             tabPanel("Kmeans Clustering"),
             tabPanel("Skater Clustering"),
             tabPanel("ClustGeo Clustering")),
  navbarMenu("Regression Analysis",
             tabPanel("Gravity Model"),
             tabPanel("Linear Mixed Model"),
             tabPanel("Poisson Constrained Model"),
             tabPanel("Poisson Unconstrained Model")),
  tabPanel("Data Table")
)


#==============================================================================#
                         ###### Shiny Server ######
#==============================================================================#
server <- function(input, output){
  ##HD Chroloplot
  output$H_mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(H_mpsz_origin_dec)+
      tm_fill(input$H_hour,
              n = input$H_classes,
              style = input$H_classification,
              palette = input$H_color) +
      tm_borders(lwd = 0.1,  alpha = 1) +
      tm_view(set.zoom.limits = c(11, 14)
      )
  })
}


shinyApp(ui = ui, server = server)



# Reference Link: Add useful links below
## Geospatial clustering: https://geospatial-analytics.netlify.app/take%20home%20exercise/take%20home%20exercise%202/take_home_ex02

