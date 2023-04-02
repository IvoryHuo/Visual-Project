#Load R Packages
pacman::p_load(rgdal, spdep, tmap, sf, 
ggpubr, cluster, factoextra, NbClust,
heatmaply, corrplot,psych,
Hmisc, knitr, kableExtra, ClustGeo, ggiraphExtra,
plotly, ggstatsplot, tools,
scales, extrafont, ggridges, gganimate, viridis, caret, gtsummary, gt,
treemap,
shiny, shinycssloaders, shinydashboard, shinythemes, tidyverse)

# Read rds data file
#huoda original rds file
H_origin_dec_wider <- read_rds(file = "data/rds/origin_dec_wider.rds")
H_cluster_org <-read_rds(file="data/rds/cluster_org.rds")
H_cluster_des <- read_rds(file="data/rds/cluster_des.rds")

#### Aishwarya data files ####
#### Commuter Flow files ####

A_subzone <- st_read(dsn = "data",
                   layer = "MP14_SUBZONE_WEB_PL") %>%
  st_transform(crs = 3414)

A_flowLine <- readRDS(file = "flowLine_with_spatial_time_day_details.rds")

A_top_5_percent <- quantile(A_flowLine$flows, probs = 0.95)

A_flow_5 <- A_flowLine[A_flowLine$flows > A_top_5_percent, ]

A_flow_5$TIME_PER_HOUR <- as.factor(A_flow_5$TIME_PER_HOUR)
A_flow_5$YEAR_MONTH <- as.Date(A_flow_5$YEAR_MONTH)


#### Treemap Plot / Density of Commuters files ####
A_data_TRM <- read_rds("A_eda_data.rds")

#### Prachi data files ####
#### Ridge Plot / Distribution of Commuters files ####

P_ridge_data <- read_rds("P_ridge_data.rds")

P_o_data <- read_rds("P_ridge_odata.rds")
P_o_data$MONTH <- format(as.Date(P_o_data$YEAR_MONTH, format="%d/%m/%Y"),"%m")

P_d_data <- read_rds("P_ridge_ddata.rds")
P_d_data$MONTH <- format(as.Date(P_d_data$YEAR_MONTH, format="%d/%m/%Y"),"%m")


#### Regression Plot / Predicting the Commuter Flow files ####
P_regression_data <- readRDS("P_regression_data.rds")

P_regression_data <- na.omit(P_regression_data)

P_input_list <- c("YEAR_MONTH", "DAY_TYPE", "TIME_PER_HOUR", "dist", "POP_ORI", "POP_DES", "BUSINESS_EDU_ORI", "RECREATION_ORI", "RESIDENTIAL_ORI", "TRANSIT_ORI", "OTHERS_ORI", "BUSINESS_EDU_DES", "RECREATION_DES", "RESIDENTIAL_DES", "TRANSIT_DES", "OTHERS_DES")

#Read geospatial data
## busstop locatioin, subzone, planning area, and region data

#read geospatial data
#mpbus <- st_read(dsn = "data/geo/busstop",layer = "BusStop")
mpsz <- st_read(dsn = "data/geo/sz",layer = "MP14_SUBZONE_NO_SEA_PL")
#mppa <- st_read(dsn = "data/geo/pa",layer = "MP14_PLNG_AREA_NO_SEA_PL")
#mpre <- st_read(dsn = "data/geo/region",layer = "MP14_REGION_WEB_PL")

##huoda geospatial data
H_sg_1 <- mpsz %>%
  select(3,16)


#Add geospatial dataset below

##huoda mpxx data
H_mpsz_origin_dec <-left_join(mpsz,H_origin_dec_wider, by = c('SUBZONE_N'='SUBZONE_N')) %>%
  select(3,16,17:42)


#==============================================================================#
                            ###### Shiny UI ######
#==============================================================================#
ui <- navbarPage(
  title = "Interactive SG Bus Commuting Pattern Exploration & Analysis",
  fluid = TRUE,
  theme=shinytheme("united"),
  id = "navbarID",
  tabPanel("Introduction",
           
           tags$h1("Welcome to Our Interactive Dashboard!\n",  align = "center"),
           tags$h3("Analyzing patterns in the commuter flow in public buses, Singapore\n\n",  align = "center"),
          
           tags$figure(
             align = "center",
             tags$img(src = "Stratio.webp",
                      width = 650,
                      height = 390)
           ),
           
           tags$h4("\n\n Authors: Aishwarya Maloo, Huo Da, Prachi Ashani",  align = "center"),
           tags$h4("\n\n We are thankful to our advisor, Prof. Kam Tin Seong, for his unwavering support and guidance",  align = "center")
           
        ),
  
  
  navbarMenu("EDA",
             #### Prachi / Aishwarya - Shiny UI ####
             
             tabPanel("Distribution of Commuters",
                      
                      #### Distribution Analysis/Ridge Plot ####
                      
                      titlePanel("Commuter Flow Distribution by Origin and Destination, Singapore"),
                      
                      fluidRow(
                        # Sidebar with a slider input for number of bins
                        sidebarLayout(
                          sidebarPanel(
                            
                            # Column names as Radio Buttons
                            radioButtons(inputId = "P_radio1", label = h4("Choose Origin Input"),
                                         
                                         choices = c('Month' = 'MONTH',
                                                     'Day Type' = 'DAY_TYPE',
                                                     'Time' = 'TIME_PER_HOUR',
                                                     'Origin Subzone' = 'SUBZONE_N_ORI',
                                                     'Origin Planning Area' = 'PLN_AREA_N_ORI',
                                                     'Origin Region' = 'REGION_N_ORI'),
                                         
                                         selected = c('Day Type' = 'DAY_TYPE')),
                            
                            actionButton(inputId = "P_submit1", label = "Submit", class = "btn btn-primary"),
                            p("\nPlease select your variables and click the '", actionLink(inputId = "P_submit1", label = "Submit"), "' button.")
                            
                          ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            plotOutput("P_ridgePlot1"),
                            
                          ))
                      ),
                      
                      fluidRow(
                        
                        sidebarLayout(
                          sidebarPanel(
                            
                            # Column names as Radio Buttons
                            radioButtons(inputId = "P_radio2", label = h4("Choose Destination Input"),
                                         
                                         choices = c('Month' = 'MONTH',
                                                     'Day Type' = 'DAY_TYPE',
                                                     'Time' = 'TIME_PER_HOUR',
                                                     'Destination Subzone' = 'SUBZONE_N_DES',
                                                     'Destination Planning Area' = 'PLN_AREA_N_DES',
                                                     'Destination Region' = 'REGION_N_DES'),
                                         
                                         selected = c('Day Type' = 'DAY_TYPE')),
                            
                            actionButton(inputId = "P_submit2", label = "Submit", class = "btn btn-primary"),
                            p("\nPlease select your variables and click the '", actionLink(inputId = "P_submit2", label = "Submit"), "' button.")
                          ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            plotOutput("P_ridgePlot2")
                          ))
                        )
                      
                      ),
             
             tabPanel("Density of Commuters",
                      
                      #### Density Analysis/Treemap Plot ####
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId = "A_tree",
                                       label = "Select input:",
                                       choices = c("Commuter Flow" = "Commuter_Flow",
                                                   "Population - Subzone" = "POP_ORI"),
                                       
                                       selected = c("Commuter Flow" = "Commuter_Flow")),
                          
                          selectInput(inputId = "A_colour_tree",
                                      label = "Choose colour palette:",
                                      choices = c("Blue" = "Blues",
                                                  "Red" = "Reds",
                                                  "Green" = "Greens",
                                                  "Purple" = "Purples",
                                                  "Purple-Red" = "PuRd",
                                                  "Brown-Blue-Green" = "BrBG",
                                                  "Yellow-Orange-Red" = "YlOrRd"),
                                      
                                      selected = c("Yellow-Orange-Red" = "YlOrRd"))
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("A_treemap")
                        )
                    )
             )
             
  ),
  
  
  navbarMenu("ESDA",
              ##### Shiny UI/Inferential Analysis/Correlation Analysis ##### 
              tabPanel("Number of Commuters",
                       sidebarLayout(
                         sidebarPanel(width = 3,  #inputid should be unique, A/H/P_xx
                                      selectInput(inputId = "H_daytype",
                                                  label = "Weekday or Weekend?",
                                                  choices = c("weekday" = "WEEKDAY",
                                                              "weekend/holiday" = "WEEKENDS/HOLIDAY"),
                                                  selected = c("WEEKDAY", "WEEKENDS/HOLIDAY"),
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
             tabPanel("Commuter Flow",
                      
                      #### Aishwarya - Shiny UI ####
                      #### Commuter Flow ####
                      
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(inputId = "A_day",
                                             label = "Select the day Type",
                                             choices = c("Weekdays" = "WEEKDAY",
                                                         "Weekends/Holidays" = "WEEKENDS/HOLIDAY"),
                                             
                                             selected = c("Weekdays" = "WEEKDAY")),
                          
                          checkboxGroupInput(inputId = "A_month",
                                             label = "Select the month(s)",
                                             choices = c("October" = "2022-10-01",
                                                         "November" = "2022-11-01",
                                                         "December" = "2022-12-01"),
                                             
                                             selected = c("October" = "2022-10-01",
                                                          "November" = "2022-11-01",
                                                          "December" = "2022-12-01")),
                          selectInput(inputId = "A_colour",
                                      label = "Select the colour palette",
                                      choices = c("Magma" = "-magma",
                                                  "Viridis" = "-viridis",
                                                  "Plasma" = "-plasma",
                                                  "Inferno" = "-inferno"),
                                      
                                      selected = c("Plasma" = "-plasma")),
                          
                          selectInput(inputId = "A_optics",
                                      label = "Select the view",
                                      choices = c("White" = "white",
                                                  "Classic" = "classic",
                                                  "Gray" = "gray",
                                                  "Natural" = "natural",
                                                  "Cobalt" = "cobalt",
                                                  "Colour Blind" = "col_blind",
                                                  "Albastross" = "albatross",
                                                  "Beaver" = "beaver",
                                                  "Black-White" = "bw",
                                                  "Watercolour" = "watercolor"),
                                      
                                      selected = c("Classic" = "classic")),
                          
                          selectInput(inputId = "A_time",
                                      label = "Select the time of day",
                                      choices = unique(sort(A_flow_5$TIME_PER_HOUR)),
                                      selected = c(10, 11, 12),
                                      multiple = TRUE),
                          
                          actionButton('submit', "Show Map", class = 'btn btn-primary'),
                          p("\nPlease select your variables and click the '", actionLink('submit', "Show Map"), "' button.")
                        ),
                        mainPanel(
                          tmapOutput("A_map"))
                      )
                      ) #note input)d = A_xxx
  ),
  navbarMenu("Cluster Analysis",
             tabPanel("Hierarchical Clustering",
                      sidebarLayout(
                        sidebarPanel(width = 3,  #inputid should be unique, A/H/P_xx
                                     selectInput(inputId = "H_cluster_daytype",
                                                 label = "Weekday or Weekend?",
                                                 choices = c("weekday" = "WEEKDAY",
                                                             "weekend/holiday" = "WEEKENDS/HOLIDAY"),
                                                 selected = c("WEEKDAY"),
                                                 multiple = FALSE),
                                     # selectInput(inputId = "H_cluster_var",
                                     #             label = "Cluster based on variables of",
                                     #             choices = c("population in the area" = "POP",
                                     #                         "commuter from/to the area" = "COMMUTER",
                                     #                         "business&edu level" = "BUSINESS_EDU",
                                     #                         "recreation level" = "RECREATION",
                                     #                         "residentail level" = "RESIDENTIAL",
                                     #                         "transit level" = "TRANSIT",
                                     #                         "others(utility, waterbody etc) level" = "OTHERS"),
                                     #             selected = c("POP","COMMUTER","BUSINESS_EDU","RECREATION","RESIDENTIAL",'TRANSIT','OTHERS'),
                                     #             multiple = TRUE),
                                  
                                     sliderInput(inputId = "H_NoClusters",
                                                 label = "Number of clusters:",
                                                 min = 2,max=15,
                                                 value = c(8)),
                                    
                        ),
                        mainPanel(
                          plotOutput("H_cluster_org",width = "100%", height = 400),
                          plotOutput("H_cluster_des",width = "100%", height = 400)
                          #plotOutput("H_corrplot",width = '50%',height = 100)
                          #plotOutput("H_cluster_elbow",width = '50%',height = 100),
                        )
                      )),
                
             
             
             tabPanel("Geospatial Constrained - SKATER Clustering",
                      sidebarLayout(
                        sidebarPanel(width = 3,  #inputid should be unique, A/H/P_xx
                                     selectInput(inputId = "H_scluster_daytype",
                                                 label = "Weekday or Weekend?",
                                                 choices = c("weekday" = "WEEKDAY",
                                                             "weekend/holiday" = "WEEKENDS/HOLIDAY"),
                                                 selected = c("WEEKDAY"),
                                                 multiple = FALSE),
                                     
                                     sliderInput(inputId = "H_NosClusters",
                                                 label = "Number of clusters:",
                                                 min = 2,max=15,
                                                 value = c(12)),
                                     
                                     radioButtons(inputId = "H_Distance",
                                                  label = "Select distance measured method:",
                                                  choices = c("Euclidean" = "euclidean",
                                                              "Maximum" = "maximum",
                                                              "Manhattan" = "manhattan",
                                                              "Canberra" = "canberra",
                                                              "Binary" = "binary",
                                                              "Minkowski" = "minkowski",
                                                              "Mahalanobis" = "mahalanobis"),
                                                  
                                                  selected = c("Euclidean" = "euclidean"))
                                    
                                     
                        ),
                        mainPanel(
                          plotOutput("H_skater_cluster_org",width = "100%", height = 400),
                          plotOutput("H_skater_cluster_des",width = "100%", height = 400)
                          
                        )
                      )),
             
             ),
  
  navbarMenu("Regression Analysis",
             
             #### Prachi - Shiny UI ####
             #### Regression - Predicting Commuter Flow ####
             
             tabPanel(title = "Unconstrained",
                      titlePanel("Unconstrained Spatial Interaction Model"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(inputId = "P_xvars", label = "Select Input Variables",
                                             choices = c("Month" = P_input_list[1],
                                                         "Day Type" = P_input_list[2], 
                                                         "Time of Day" = P_input_list[3],
                                                         "Subzone Distance" = P_input_list[4],
                                                         "Population at Origin" = P_input_list[5],
                                                         "Population at Destination" = P_input_list[6],
                                                         "Business/Education Area at Origin" = P_input_list[7],
                                                         "Recreation Area at Origin" = P_input_list[8],
                                                         "Residential Area at Origin" = P_input_list[9],
                                                         "Transit Area at Origin" = P_input_list[10],
                                                         "Other Area at Origin" = P_input_list[11],
                                                         "Business/Education Area at Destination" = P_input_list[12],
                                                         "Recreation Area at Destination" = P_input_list[13],
                                                         "Residential Area at Destination" = P_input_list[14],
                                                         "Transit Area at Destination" = P_input_list[15],
                                                         "Other Area at Destination" = P_input_list[16]),
                                             
                                             selected =  c("Month" = P_input_list[1],
                                                           "Day Type" = P_input_list[2], 
                                                           "Time of Day" = P_input_list[3],
                                                           "Subzone Distance" = P_input_list[4],
                                                           "Population at Origin" = P_input_list[5],
                                                           "Population at Destination" = P_input_list[6],
                                                           "Business/Education Area at Origin" = P_input_list[7],
                                                           "Recreation Area at Origin" = P_input_list[8],
                                                           "Residential Area at Origin" = P_input_list[9],
                                                           "Transit Area at Origin" = P_input_list[10],
                                                           "Other Area at Origin" = P_input_list[11],
                                                           "Business/Education Area at Destination" = P_input_list[12],
                                                           "Recreation Area at Destination" = P_input_list[13],
                                                           "Residential Area at Destination" = P_input_list[14],
                                                           "Transit Area at Destination" = P_input_list[15],
                                                           "Other Area at Destination" = P_input_list[16])),
                          
                          actionButton("P_run_regression1", "Run Regression"),
                          p("\nPlease select your variables and click the '", actionLink("P_run_regression1", "Run Regression"), "' button.")
                          
                        ),
                        
                        mainPanel(
                          
                          withSpinner(plotOutput("P_regplot1"), color="#0dc5c1"),
                          br(),
                          verbatimTextOutput("P_txtout1"),
                          br(),
                          gt_output('P_tbl1')
                          
                        )) 
             ),
             
             tabPanel(title = "Origin Constrained",
                      titlePanel("Origin Constrained Spatial Interaction Model"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(inputId = "P_xvars", label = "Select Input Variables",
                                             choices = c("Month" = P_input_list[1],
                                                         "Day Type" = P_input_list[2], 
                                                         "Time of Day" = P_input_list[3],
                                                         "Subzone Distance" = P_input_list[4],
                                                         "Population at Destination" = P_input_list[6],
                                                         "Business/Education Area at Destination" = P_input_list[12],
                                                         "Recreation Area at Destination" = P_input_list[13],
                                                         "Residential Area at Destination" = P_input_list[14],
                                                         "Transit Area at Destination" = P_input_list[15],
                                                         "Other Area at Destination" = P_input_list[16]),
                                             
                                             selected = c("Month" = P_input_list[1],
                                                          "Day Type" = P_input_list[2], 
                                                          "Time of Day" = P_input_list[3],
                                                          "Subzone Distance" = P_input_list[4],
                                                          "Population at Destination" = P_input_list[6],
                                                          "Business/Education Area at Destination" = P_input_list[12],
                                                          "Recreation Area at Destination" = P_input_list[13],
                                                          "Residential Area at Destination" = P_input_list[14],
                                                          "Transit Area at Destination" = P_input_list[15],
                                                          "Other Area at Destination" = P_input_list[16])),
                          
                          actionButton("P_run_regression2", "Run Regression"),
                          p("\nPlease select your variables and click the '", actionLink("P_run_regression2", "Run Regression"), "' button.")
                          
                        ),
                        
                        mainPanel(
                          
                          withSpinner(plotOutput("P_regplot2"), color="#0dc5c1"),
                          br(),
                          verbatimTextOutput("P_txtout2"),
                          br(),
                          gt_output('P_tbl2')
                          
                        ))
             ),
             
             tabPanel(title = "Destination Constrained",
                      
                      titlePanel("Destination Constrained Spatial Interaction Model"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(inputId = "P_xvars", label = "Select Input Variables",
                                             choices = c("Month" = P_input_list[1],
                                                         "Day Type" = P_input_list[2], 
                                                         "Time of Day" = P_input_list[3],
                                                         "Subzone Distance" = P_input_list[4],
                                                         "Population at Origin" = P_input_list[5],
                                                         "Business/Education Area at Origin" = P_input_list[7],
                                                         "Recreation Area at Origin" = P_input_list[8],
                                                         "Residential Area at Origin" = P_input_list[9],
                                                         "Transit Area at Origin" = P_input_list[10],
                                                         "Other Area at Origin" = P_input_list[11]),
                                             
                                             selected = c("Month" = P_input_list[1],
                                                          "Day Type" = P_input_list[2], 
                                                          "Time of Day" = P_input_list[3],
                                                          "Subzone Distance" = P_input_list[4],
                                                          "Population at Origin" = P_input_list[5],
                                                          "Business/Education Area at Origin" = P_input_list[7],
                                                          "Recreation Area at Origin" = P_input_list[8],
                                                          "Residential Area at Origin" = P_input_list[9],
                                                          "Transit Area at Origin" = P_input_list[10],
                                                          "Other Area at Origin" = P_input_list[11])),
                          
                          actionButton("P_run_regression3", "Run Regression"),
                          p("\nPlease select your variables and click the '", actionLink("P_run_regression3", "Run Regression"), "' button.")
                          
                        ),
                        
                        mainPanel(
                          withSpinner(plotOutput("P_regplot3"), color="#0dc5c1"),
                          br(),
                          verbatimTextOutput("P_txtout3"),
                          br(),
                          gt_output('P_tbl3')
                          
                        ))
             ),
             
             tabPanel(title = "Double Constrained",
                      titlePanel("Doubly Constrained Spatial Interaction Model"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(inputId = "P_xvars", label = "Select Input Variables",
                                             choices = c("Month" = P_input_list[1],
                                                         "Day Type" = P_input_list[2], 
                                                         "Time of Day" = P_input_list[3],
                                                         "Subzone Distance" = P_input_list[4],
                                                         "Population at Origin" = P_input_list[5],
                                                         "Population at Destination" = P_input_list[6],
                                                         "Business/Education Area at Origin" = P_input_list[7],
                                                         "Recreation Area at Origin" = P_input_list[8],
                                                         "Residential Area at Origin" = P_input_list[9],
                                                         "Transit Area at Origin" = P_input_list[10],
                                                         "Other Area at Origin" = P_input_list[11],
                                                         "Business/Education Area at Destination" = P_input_list[12],
                                                         "Recreation Area at Destination" = P_input_list[13],
                                                         "Residential Area at Destination" = P_input_list[14],
                                                         "Transit Area at Destination" = P_input_list[15],
                                                         "Other Area at Destination" = P_input_list[16]),
                                             
                                             selected = c("Month Year" = P_input_list[1],
                                                          "Day Type" = P_input_list[2], 
                                                          "Time of Day" = P_input_list[3],
                                                          "Distance" = P_input_list[4],
                                                          "Population at Origin" = P_input_list[5],
                                                          "Population at Destination" = P_input_list[6],
                                                          "Business/Education Area at Origin" = P_input_list[7],
                                                          "Recreation Area at Origin" = P_input_list[8],
                                                          "Residential Area at Origin" = P_input_list[9],
                                                          "Transit Area at Origin" = P_input_list[10],
                                                          "Other Area at Origin" = P_input_list[11],
                                                          "Business/Education Area at Destination" = P_input_list[12],
                                                          "Recreation Area at Destination" = P_input_list[13],
                                                          "Residential Area at Destination" = P_input_list[14],
                                                          "Transit Area at Destination" = P_input_list[15],
                                                          "Other Area at Destination" = P_input_list[16])),
                          
                          actionButton("P_run_regression4", "Run Regression"),
                          p("\nPlease select your variables and click the '", actionLink("P_run_regression4", "Run Regression"), "' button.")
                        ),
                        
                        mainPanel(
                          
                          withSpinner(plotOutput("P_regplot4"), color="#0dc5c1"),
                          br(),
                          verbatimTextOutput("P_txtout4"),
                          br(),
                          gt_output('P_tbl4')
                          
                        ))
             )
  ),
             
  tabPanel(title = "Data Table",

           navbarPage("Panels",

             tabPanel(title = "EDA: Commuter Distribution",
                      DT::dataTableOutput("P_distribution_datatable")
                      ),

             tabPanel(title = "EDA: Commuter/Subzone Density",
                      DT::dataTableOutput("A_density_datatable")
                      ),

             tabPanel(title = "ESDA: Commuter Proportion",
                      DT::dataTableOutput("H_commuterprop_datatable")
                      ),

             tabPanel(title = "ESDA: Commuter Flow",
                      DT::dataTableOutput("A_commuterflow_datatable")
                      ),

             tabPanel(title = "Cluster Analysis",
                      DT::dataTableOutput("H_cluster_datatable")
                      ),

             tabPanel(title = "Regression Analysis",
                      DT::dataTableOutput("P_regression_datatable")
                      )
           )

      )
)


#==============================================================================#
                         ###### Shiny Server ######
#==============================================================================#
server <- function(input, output) {
  
  #### Prachi - Shiny Server ####
  #### Distribution Analysis / Ridge Plot ####
  
  P_odata <- eventReactive (input$P_submit1, {
    
    P_odata <- P_o_data %>% 
      select(c("col" = as.factor(input$P_radio1), "flow" = flow))
    
    return(P_odata)
    
  })
  
  observeEvent(input$P_submit1, {
    
    P_plot1 <- NULL
    
    output$P_ridgePlot1 <- renderPlot({
      
      #print(P_odata())
      
      P_plot1 <- ggplot(data = P_odata(), aes(x = flow, y = col, group = col, fill = after_stat(x))) +
        
        geom_density_ridges_gradient() +
        
        scale_fill_viridis(option = "inferno", direction = 1, begin = 0.4) +
        
        theme_minimal() +
        
        theme(text = element_text(family = "Garamond", face = 'bold'),
              
              plot.margin = margin(t=1, r=1, b=1, l=1),
              plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
              plot.caption = element_text(hjust = 0, face = 'italic'),
              plot.caption.position = 'plot',
              
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              
              axis.line = element_line(color = 'grey'),
              axis.title = element_text(size = 12),
              axis.title.y = element_text(angle = 90, vjust = 1.03),
              axis.text = element_text(size = 10),
              axis.text.x = element_text(),
              
              axis.ticks.y = element_line(color = 'grey'),
              axis.ticks.x = element_line('grey'),
              
              legend.position = "None") +
        
        ggtitle("Commuter flow by Subzone Origin") +
        xlab("Commuter Distribution") +
        ylab("")
      
      P_plot1
      
    })
  })
  
  P_ddata <- eventReactive (input$P_submit2, {
    
    P_ddata <- P_d_data %>% 
      select(c("col" = as.factor(input$P_radio2), "flow" = flow))
    
    return(P_ddata)
    
  })
  
  observeEvent(input$P_submit2, {
    
    P_plot2 <- NULL
    
    output$P_ridgePlot2 <- renderPlot({
      
      #print(P_ddata())
      
      P_plot2 <- ggplot(data = P_ddata(), aes(x = flow, y = col, group = col, fill = after_stat(x))) +
        
        geom_density_ridges_gradient() +
        
        scale_fill_viridis(option = "inferno", direction = 1, begin = 0.4) +
        
        theme_minimal() +
        
        theme(text = element_text(family = "Garamond", face = 'bold'),
              
              plot.margin = margin(t=1, r=1, b=1, l=1),
              plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
              plot.caption = element_text(hjust = 0, face = 'italic'),
              plot.caption.position = 'plot',
              
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              
              axis.line = element_line(color = 'grey'),
              axis.title = element_text(size = 12),
              axis.title.y = element_text(angle = 90, vjust = 1.03),
              axis.text = element_text(size = 10),
              axis.text.x = element_text(),
              
              axis.ticks.y = element_line(color = 'grey'),
              axis.ticks.x = element_line('grey'),
              
              legend.position = "None") +
        
        ggtitle("Commuter flow by Subzone Destination") +
        xlab("Commuter Distribution") +
        ylab("")
      
      P_plot2
      
    })
    
  })
  
  #### Aishwarya - Shiny Server ####
  #### Density Analysis / Treemap Plot
  
  output$A_treemap <- renderPlot({
    
    if(input$A_tree == "Commuter_Flow"){
      treemap(A_data_TRM %>% 
                group_by(SUBZONE_N_ORI)%>% 
                summarise(Commuter_Flow = mean(Commuter_Flow)),
              index = c("SUBZONE_N_ORI"),
              vSize = "Commuter_Flow",
              vColor = "Commuter_Flow",
              type = "manual",
              palette = input$A_colour_tree,
              border.col = c("black", "white"),
              title = "Commuter Flow by Subzones",
              title.legend = "Commuter Flow")}
    
    else {
      treemap(A_data_TRM %>% 
                group_by(SUBZONE_N_ORI)%>% 
                summarise(POP_ORI = mean(POP_ORI)),
              index = c("SUBZONE_N_ORI"),
              vSize = "POP_ORI",
              vColor = "POP_ORI",
              type = "manual",
              palette = input$A_colour_tree,
              border.col = c("black", "white"),
              title = "Population for Subzones",
              title.legend = "Population")
    }
    
  })
  
  #### Commuter Flow ####
  f_data <- eventReactive(input$submit, {
    
    data <- A_flow_5 %>%
      filter(DAY_TYPE == input$A_day) %>%
      filter(YEAR_MONTH == input$A_month) %>% 
      filter(TIME_PER_HOUR == input$A_time)
    
    return(data)
  })
  
  observeEvent(input$submit, {
    
    output$A_map <- renderTmap({
      
      # print(f_data())
      
      tmap_mode("view")
      tmap_style(input$A_optics)
      tmap_options(bg.color = "black")
      
      tm_shape(A_subzone) +
        tm_polygons(alpha = 0.3) +
        tm_shape(f_data()) +
        tm_lines(lwd = "flows",
                 style = "quantile",
                 scale = c(0.1, 1, 3, 5, 10),
                 palette = input$A_colour,
                 col = "flows",
                 data = f_data(), 
                 tmap_options(check.and.fix = TRUE))
    })
    
  })
  
  
  ##HD Chroloplot
  output$H_mapPlot <- renderTmap({
    H_mpsz_origin_dec_plot <- H_mpsz_origin_dec %>%
      filter(DAY_TYPE %in% (input$H_daytype))
    
    tmap_options(check.and.fix = TRUE) +
      tm_shape(H_mpsz_origin_dec_plot)+
      tm_fill(input$H_hour,
              n = input$H_classes,
              style = input$H_classification,
              palette = input$H_color) +
      tm_borders(lwd = 0.1,  alpha = 1) +
      tm_view(set.zoom.limits = c(11, 14)
      )
  })
  
  output$H_cluster_org <- renderPlot({
    #clustering
    cluster.vars <- H_cluster_org %>%
      filter(DAY_TYPE %in% input$H_cluster_daytype) %>%
      select(c(2:9))

    row.names(cluster.vars) <- cluster.vars$SUBZONE_N_ORI
    df_org <- cluster.vars %>%
      select(c(2:8))

    df_org_scaled<-scale(df_org)

    proxmat_org <- dist(df_org_scaled, method = 'euclidean')
    hclust_ward_org <- hclust(proxmat_org, method = 'ward.D')

    #Plot Map
    groups <- as.factor(cutree(hclust_ward_org, k = input$H_NoClusters))
    appeared_sz <- unique(cluster.vars$SUBZONE_N_ORI)
    sg <- H_sg_1 %>%
      filter(SUBZONE_N %in% appeared_sz)
    sg_hcluster <- cbind(sg,as.matrix(groups)) %>%
      rename(`CLUSTER`=`as.matrix.groups.`)
    qtm(sg_hcluster,"CLUSTER") +
      tm_layout(main.title="Subzone as Origin",
                main.title.size = 3,
                main.title.position = "centre",
                legend.height=0.25,
                legend.width=0.35)
    
  })
  
  output$H_cluster_des <- renderPlot({
    #clustering
    cluster.vars <- H_cluster_des %>%
      filter(DAY_TYPE %in% input$H_cluster_daytype) %>%
      select(c(2:9))
    
    row.names(cluster.vars) <- cluster.vars$SUBZONE_N_DES
    df_des <- cluster.vars %>%
      select(c(2:8))
    
    df_des_scaled<-scale(df_des)
    
    proxmat_des <- dist(df_des_scaled, method = 'euclidean')
    hclust_ward_des <- hclust(proxmat_des, method = 'ward.D')
    
    
    #Plot Map
    groups <- as.factor(cutree(hclust_ward_des, k = input$H_NoClusters))
    appeared_sz <- unique(cluster.vars$SUBZONE_N_DES)
    sg <- H_sg_1 %>%
      filter(SUBZONE_N %in% appeared_sz)
    sg_hcluster <- cbind(sg,as.matrix(groups)) %>%
      rename(`CLUSTER`=`as.matrix.groups.`)
    qtm(sg_hcluster,"CLUSTER") +
      tm_layout(main.title="Subzone as Destination",
                main.title.size = 3,
                main.title.position = "centre",
                legend.height=0.25,
                legend.width=0.35)
  })
  
  output$H_skater_cluster_org <- renderPlot({
    #clustering
    cluster.vars <- H_cluster_org %>%
      filter(DAY_TYPE %in% input$H_scluster_daytype) %>% 
      select(c(2:9))

    
    appeared_sz <- unique(cluster.vars$SUBZONE_N_ORI)
    sg <- H_sg_1 %>%
      filter(SUBZONE_N %in% appeared_sz)
    
    row.names(cluster.vars) <- cluster.vars$SUBZONE_N_ORI
    df_org <- cluster.vars %>%
      select(c(2:8))
    
    df_org_scaled<-scale(df_org)
    
    sg_sp <- as_Spatial(sg)
    sg_nb <- poly2nb(sg_sp)
    
    lcosts <- nbcosts(sg_nb, df_org_scaled)
    sg.wm<-nb2listw(sg_nb,lcosts,style = "B")
    sg.mst <-mstree(sg.wm)
    
    #plot
    clust <- spdep::skater(edges = sg.mst[,1:2], 
                           data = df_org_scaled, 
                           method = input$H_Distance, 
                           ncuts = input$H_NosClusters - 1)
    groups_mat <- as.matrix(clust$groups)
    sg_sf_spatialcluster <- cbind(sg, as.factor(groups_mat)) %>%
      rename(`SP_CLUSTER`=`as.factor.groups_mat.`)
    qtm(sg_sf_spatialcluster, "SP_CLUSTER") +
      tm_layout(main.title="Subzone as Origin",
                main.title.size = 3,
                main.title.position = "centre",
                legend.height=0.25,
                legend.width=0.35)
  })
  
  output$H_skater_cluster_des <- renderPlot({
    #clustering
    cluster.vars <- H_cluster_des %>%
      filter(DAY_TYPE %in% input$H_scluster_daytype) %>% 
      select(c(2:9))
    
    
    appeared_sz <- unique(cluster.vars$SUBZONE_N_DES)
    sg <- H_sg_1 %>%
      filter(SUBZONE_N %in% appeared_sz)
    
    row.names(cluster.vars) <- cluster.vars$SUBZONE_N_DES
    df_des <- cluster.vars %>%
      select(c(2:8))
    
    df_des_scaled<-scale(df_des)
    
    sg_sp <- as_Spatial(sg)
    sg_nb <- poly2nb(sg_sp)
    
    lcosts <- nbcosts(sg_nb, df_des_scaled)
    sg.wm<-nb2listw(sg_nb,lcosts,style = "B")
    sg.mst <-mstree(sg.wm)
    
    #plot
    clust <- spdep::skater(edges = sg.mst[,1:2], 
                           data = df_des_scaled, 
                           method = input$H_Distance, 
                           ncuts = input$H_NosClusters - 1)
    groups_mat <- as.matrix(clust$groups)
    sg_sf_spatialcluster <- cbind(sg, as.factor(groups_mat)) %>%
      rename(`SP_CLUSTER`=`as.factor.groups_mat.`)
    qtm(sg_sf_spatialcluster, "SP_CLUSTER") +
      tm_layout(main.title="Subzone as Destination",
                main.title.size = 3,
                main.title.position = "centre",
                legend.height=0.25,
                legend.width=0.35)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #### Prachi - Shiny Server ####
  #### Regression - Predicting Commuter Flow ####
  
  # Define the reactive dataset 
  P_data <- reactive({
    
    select(P_regression_data, c("Commuter_Flow", input$P_xvars))
    
  })
  
  # Define the regression table output
  
  observeEvent(input$P_run_regression1, {
    
    output$P_txtout1 <- renderText({
      # Output the regression coefficients and statistics
      P_mdata = P_data()
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      P_mdata$fitted <- fitted(P_model)
      
      P_summary <- postResample(P_mdata$Commuter_Flow, P_mdata$fitted)
      
      paste('RMSE: ', round(P_summary[1],0), '\t', 'R^2 Score: ', round(P_summary[2],2), '\t', 'MAE: ', round(P_summary[3],0))
      
    })
    
    output$P_tbl1 <- render_gt({
      
      # Output the regression coefficients and statistics
      
      P_mdata = P_data()
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      as_gt(tbl_regression(P_model))
      
    })
  
  # Define the regression output
    
    output$P_regplot1 <- renderPlot({
      
      # Plot the regression results
      
      P_mdata = P_data()
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      P_mdata$fitted <- fitted(P_model)
      
      ggplot(data = P_mdata, aes(x = fitted, y = Commuter_Flow)) +
        
        geom_point(color="black", fill="blue") +
        geom_smooth(method = "glm", method.args = list(family = poisson),
                    fill = "maroon", color = "maroon", linetype = 2)
    })
    
  })
  
  # Define the regression table output
  
  observeEvent(input$P_run_regression2, {
    
    output$P_txtout2 <- renderText({
      
      # Output the regression coefficients and statistics
      
      P_mdata = P_data()
      
      P_mdata$SUBZONE_N_ORI <- P_regression_data$SUBZONE_N_ORI
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      P_mdata$fitted <- fitted(P_model)
      
      P_summary <- postResample(P_mdata$Commuter_Flow, P_mdata$fitted)
      
      paste('RMSE: ', round(P_summary[1],0), '\t', 'R^2 Score: ', round(P_summary[2],2), '\t', 'MAE: ', round(P_summary[3],0))
      
    })
    
    output$P_tbl2 <- render_gt({
      
      # Output the regression coefficients and statistics
      
      P_mdata = P_data()
      
      P_mdata$SUBZONE_N_ORI <- P_regression_data$SUBZONE_N_ORI
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      as_gt(tbl_regression(P_model))
      
    })
    
    # Define the regression output
    output$P_regplot2 <- renderPlot({
      
      # Plot the regression results
      
      P_mdata = P_data()
      
      P_mdata$SUBZONE_N_ORI <- P_regression_data$SUBZONE_N_ORI
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      P_mdata$fitted <- fitted(P_model)
      
      ggplot(data = P_mdata, aes(x = fitted, y = Commuter_Flow)) +
        
        geom_point(color="black", fill="blue") +
        geom_smooth(method = "glm", method.args = list(family = poisson),
                    fill = "maroon", color = "maroon", linetype = 2)
    })
  
  })
  
  # Define the regression table output
  observeEvent(input$P_run_regression3, {
    
    output$P_txtout3 <- renderText({
      
      # Output the regression coefficients and statistics
      
      P_mdata = P_data()
      
      P_mdata$SUBZONE_N_DES <- P_regression_data$SUBZONE_N_DES
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      P_mdata$fitted <- fitted(P_model)
      
      P_summary <- postResample(P_mdata$Commuter_Flow, P_mdata$fitted)
      
      paste('RMSE: ', round(P_summary[1],0), '\t', 'R^2 Score: ', round(P_summary[2],2), '\t', 'MAE: ', round(P_summary[3],0))
      
    })
    
    output$P_tbl3 <- render_gt({
      
      # Output the regression coefficients and statistics
      
      P_mdata = P_data()
      
      P_mdata$SUBZONE_N_DES <- P_regression_data$SUBZONE_N_DES
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      as_gt(tbl_regression(P_model))
      
    })
    
    # Define the regression output
    output$P_regplot3 <- renderPlot({
      
      # Plot the regression results
      
      P_mdata = P_data()
      
      P_mdata$SUBZONE_N_DES <- P_regression_data$SUBZONE_N_DES
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      P_mdata$fitted <- fitted(P_model)
      
      ggplot(data = P_mdata, aes(x = fitted, y = Commuter_Flow)) +
        
        geom_point(color="black", fill="blue") +
        geom_smooth(method = "glm", method.args = list(family = poisson),
                    fill = "maroon", color = "maroon", linetype = 2)
    })
  
  })
  
  # Define the regression table output
  observeEvent(input$P_run_regression4, {
    
    output$P_txtout4 <- renderText({
      
      # Output the regression coefficients and statistics
      
      P_mdata = P_data()
      
      P_mdata$SUBZONE_N_ORI <- P_regression_data$SUBZONE_N_ORI
      P_mdata$SUBZONE_N_DES <- P_regression_data$SUBZONE_N_DES
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      P_mdata$fitted <- fitted(P_model)
      
      P_summary <- postResample(P_mdata$Commuter_Flow, P_mdata$fitted)
      
      paste('RMSE: ', round(P_summary[1],0), '\t', 'R^2 Score: ', round(P_summary[2],2), '\t', 'MAE: ', round(P_summary[3],0))
      
    })
    
    output$P_tbl4 <- renderTable({
      
      # Output the regression coefficients and statistics
      
      P_mdata = P_data()
      
      P_mdata$SUBZONE_N_ORI <- P_regression_data$SUBZONE_N_ORI
      P_mdata$SUBZONE_N_DES <- P_regression_data$SUBZONE_N_DES
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      as_gt(tbl_regression(P_model))
      
    })
    
    # Define the regression output
    output$P_regplot4 <- renderPlot({
      
      # Plot the regression results
      
      P_mdata = P_data()
      
      P_mdata$SUBZONE_N_ORI <- P_regression_data$SUBZONE_N_ORI
      P_mdata$SUBZONE_N_DES <- P_regression_data$SUBZONE_N_DES
      
      P_model <- glm(Commuter_Flow ~ ., data = P_mdata, na.action = na.omit, family = poisson(link = "log"))
      
      P_mdata$fitted <- fitted(P_model)
      
      ggplot(data = P_mdata, aes(x = fitted, y = Commuter_Flow)) +
        
        geom_point(color="black", fill="blue") +
        geom_smooth(method = "glm", method.args = list(family = poisson),
                    fill = "maroon", color = "maroon", linetype = 2)
    })
  
  })
  
  output$P_distribution_datatable <- DT::renderDataTable({
    DT::datatable(P_ridge_data, options = list(pageLength = 10))
  })

  output$A_density_datatable <- DT::renderDataTable({
    DT::datatable(A_data_TRM, options = list(pageLength = 10))
  })

  output$H_commuterprop_datatable <- DT::renderDataTable({
    DT::datatable(H_mpsz_origin_dec, options = list(pageLength = 10))
  })

  output$A_commuterflow_datatable <- DT::renderDataTable({
    DT::datatable(A_flow_5, options = list(pageLength = 10))
  })

  output$H_cluster_datatable <- DT::renderDataTable({
    DT::datatable(H_cluster_org, options = list(pageLength = 10))
  })

  output$P_regression_datatable <- DT::renderDataTable({
    DT::datatable(P_regression_data, options = list(pageLength = 10))
  })
  
}


shinyApp(ui = ui, server = server)