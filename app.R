#Load R Packages
## Add all your packages here
# pacman::p_load('shiny', 'shinydashboard', 'shinythemes', 
#               'sf','tmap',
#               'plotly', 'tidyverse', 'ggstatsplot', 
#               'tools')
pacman::p_load(rgdal, spdep, tmap, sf, 
ggpubr, cluster, factoextra, NbClust,
heatmaply, corrplot, tidyverse,psych,
Hmisc,knitr,kableExtra,ClustGeo,ggiraphExtra,
plotly,ggstatsplot,tools,
shiny,shinydashboard,shinythemes,tidyverse)

# Read rds data file
## compared with csv, rds save space and have a faster speed to read in data
#oct <- readRDS(file = "data/rds/oct_data.rds") #2022-10 total trips
#nov <- readRDS(file = "data/rds/nov_data.rds") #2022-11 total trips
#dec <- readRDS(file = "data/rds/dec_data.rds") #2022-12 total trips
#bus_info <- read_csv("data/BusStopURA2014SZ.csv") #bustop_code and pa, region info
#total <- readRDS(file = "data/rds/total_data.rds") #2022 oct-dec total trips


#huoda original rds file
H_origin_dec_wider <-read_rds(file = "data/rds/origin_dec_wider.rds")
H_cluster_org <-read_rds(file="data/rds/cluster_org.rds")
H_cluster_des <- read_rds(file="data/rds/cluster_des.rds")
#prachi_original_rds_file


#aish_original_rds_file

#Read geospatial data
## busstop locatioin, subzone, planning area, and region data

#read geospatial data
mpbus <- st_read(dsn = "data/geo/busstop",layer = "BusStop")
mpsz <- st_read(dsn = "data/geo/sz",layer = "MP14_SUBZONE_NO_SEA_PL")
mppa <- st_read(dsn = "data/geo/pa",layer = "MP14_PLNG_AREA_NO_SEA_PL")
mpre <- st_read(dsn = "data/geo/region",layer = "MP14_REGION_WEB_PL")
##huoda geospatial data
H_sg_1 <- mpsz %>%
  select(3,16)


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
                                                              "weekend/holiday" = "WEEKENDS/HOLIDAY"),
                                                  selected = c("weekday",'weekend/holiday'),
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
             tabPanel("Hierarchical Clustering",
                      sidebarLayout(
                        sidebarPanel(width = 3,  #inputid should be unique, A/H/P_xx
                                     selectInput(inputId = "H_cluster_daytype",
                                                 label = "Weekday or Weekend?",
                                                 choices = c("weekday" = "WEEKDAY",
                                                             "weekend/holiday" = "WEEKENDS/HOLIDAY"),
                                                 selected = c("weekday","weend/holiday"),
                                                 multiple = TRUE),
                                     selectInput(inputId = "H_cluster_var",
                                                 label = "Cluster based on variables of",
                                                 choices = c("population in the area" = "POP",
                                                             "commuter from/to the area" = "COMMUTER",
                                                             "business&edu level" = "BUSINESS_EDU",
                                                             "recreation level" = "RECREATION",
                                                             "residentail level" = "RESIDENTIAL",
                                                             "transit level" = "TRANSIT",
                                                             "others(utility, waterbody etc) level" = "OTHERS"),
                                                 selected = c("POP","COMMUTER","BUSINESS_EDU","RECREATION","RESIDENTIAL",'TRANSIT','OTHERS'),
                                                 multiple = TRUE),
                                  
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
  
  # output$H_corrplot <- renderPlot({
  #   cluster_vars.cor = cor(H_cluster_org[,c(input$H_cluster_va)])
  #   corrplot.mixed(cluster_vars.cor,
  #                  lower = "ellipse", 
  #                  upper = "number",
  #                  tl.pos = "lt",
  #                  diag = "l",
  #                  tl.col = "black")
  # })
  
  
  
  # output$H_cluster_elbow({
  #   cluster.vars <- cluster_org %>%
  #     filter(DAY_TYPE==input$H_cluster_daytype) %>%
  #     select(c(input$H_cluster_var))
  #   
  #   row.names(cluster.vars) <- cluster.vars$SUBZONE_N_ORI
  #   df_org <- cluster.vars %>%
  #     select(c(input$H_cluster_var))
  #   
  #   df_org_scaled<-scale(df_org)
  #   
  #   set.seed(123)
  #   fviz_nbclust(df_org_scaled, hcut, method = "wss", k.max = 15) + 
  #     theme_minimal() + 
  #     ggtitle("Elbow Method to choose number of clusters")
  # })
  # 
  output$H_cluster_org <- renderPlot({
    #clustering
    cluster.vars <- H_cluster_org %>%
      filter(DAY_TYPE %in% input$H_cluster_daytype) %>%
      select(c(input$H_cluster_var))

    row.names(cluster.vars) <- cluster.vars$SUBZONE_N_ORI
    df_org <- cluster.vars %>%
      select(c(input$H_cluster_var))

    df_org_scaled<-scale(df_org)

    proxmat_org <- dist(df_org_scaled, method = 'euclidean')
    hclust_ward_org <- hclust(proxmat_org, method = 'ward.D')
    plot(hclust_ward_org, cex = 0.1)
    rect.hclust(hclust_ward_org,k=input$H_NoClusters,border=c(1:input$H_NoClusters))
    legend("topright",legend = paste("Cluster",1:input$H_NoClusters),fill = 1:input$H_NoClusters,border = "white",text.font = 10)

    #Plot Map
    p1 <-cutree(hclust_ward_org,input$H_NoClusters)
    sz_label <-as.vector(cluster.vars$"SUBZONE_N_ORI")
    names(p1) <- sz_label
    plot(H_sg_1,border = 'grey',col = p1,
         main = 'Parition of Subzone by Hierachical Clustering(Subzone as Origin)')
    legend("bottomright",legend = paste("c",1:input$H_NoClusters),fill = 1:input$H_NoClusters,border = 'white')
  })
  
  output$H_cluster_des <- renderPlot({
    #clustering
    cluster.vars <- H_cluster_des %>%
      filter(DAY_TYPE %in% input$H_cluster_daytype) %>%
      select(c(input$H_cluster_var))
    
    row.names(cluster.vars) <- cluster.vars$SUBZONE_N_des
    df_des <- cluster.vars %>%
      select(c(input$H_cluster_var))
    
    df_des_scaled<-scale(df_des)
    
    proxmat_des <- dist(df_des_scaled, method = 'euclidean')
    hclust_ward_des <- hclust(proxmat_des, method = 'ward.D')
    plot(hclust_ward_des, cex = 0.1)
    rect.hclust(hclust_ward_des,k=input$H_NoClusters,border=c(1:input$H_NoClusters))
    legend("topright",legend = paste("Cluster",1:input$H_NoClusters),fill = 1:input$H_NoClusters,border = "white",text.font = 10)
    
    #Plot Map
    p1 <-cutree(hclust_ward_des,input$H_NoClusters)
    sz_label <-as.vector(cluster.vars$"SUBZONE_N_DES")
    names(p1) <- sz_label
    plot(H_sg_1,border = 'grey',col = p1,
         main = 'Parition of Subzone by Hierachical Clustering (Subzone as Destination)')
    legend("bottomright",legend = paste("c",1:input$H_NoClusters),fill = 1:input$H_NoClusters,border = 'white')
  })
  
}


shinyApp(ui = ui, server = server)



# Reference Link: Add useful links below
## Geospatial clustering: https://geospatial-analytics.netlify.app/take%20home%20exercise/take%20home%20exercise%202/take_home_ex02

