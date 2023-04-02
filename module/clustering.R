pacman::p_load(rgdal, spdep, tmap, sf, 
               ggpubr, cluster, factoextra, NbClust,
               heatmaply, corrplot, tidyverse,psych,
               Hmisc,knitr,kableExtra,ClustGeo,ggiraphExtra)
sg_1 <- st_read(dsn = "data/geo/sz",layer = "MP14_SUBZONE_NO_SEA_PL") %>%
  select(3,16)

regression_data <- read_rds(file = "data/rds/regression_data.rds")

cluster_org <- regression_data %>%
  select(c(1:4,6,8,10:14)) %>%
  group_by(DAY_TYPE,SUBZONE_N_ORI) %>%
  summarise(POP = mean(POP_ORI),
            COMMUTER = sum(Commuter_Flow),
            BUSINESS_EDU = mean(BUSINESS_EDU_ORI),
            OTHERS = mean(OTHERS_ORI),
            RECREATION = mean(RECREATION_ORI),
            RESIDENTIAL = mean(RESIDENTIAL_ORI),
            TRANSIT = mean(TRANSIT_ORI)) %>%
  ungroup() %>%
  drop_na(SUBZONE_N_ORI) %>%
  replace_na(list(x=0))
cluster_org[is.na(cluster_org)] <- 0
write_rds(cluster_org,'data/rds/cluster_org.rds')


cluster_des <- regression_data %>%
  select(c(1:3,5,7:8,15:19)) %>%
  group_by(DAY_TYPE,SUBZONE_N_DES) %>%
  summarise(POP = mean(POP_DES),
            COMMUTER = sum(Commuter_Flow),
            BUSINESS_EDU = mean(BUSINESS_EDU_DES),
            OTHERS = mean(OTHERS_DES),
            RECREATION = mean(RECREATION_DES),
            RESIDENTIAL = mean(RESIDENTIAL_DES),
            TRANSIT = mean(TRANSIT_DES)) %>%
  ungroup() %>%
  drop_na(SUBZONE_N_DES) %>%
  replace_na(list(x=0))
cluster_des[is.na(cluster_des)] <- 0
write_rds(cluster_des,'data/rds/cluster_des.rds')


  
#multivariate analysis
cluster_vars.cor = cor(cluster_org[,c(3:9)])
corrplot.mixed(cluster_vars.cor,
               lower = "ellipse", 
               upper = "number",
               tl.pos = "lt",
               diag = "l",
               tl.col = "black")

#clustering
cluster.vars <- cluster_org %>%
  filter(DAY_TYPE=='WEEKDAY') %>%
  select(c(2:9))

row.names(cluster.vars) <- cluster.vars$SUBZONE_N_ORI
df_org <- cluster.vars %>%
  select(c(2:8))

#scale
df_org_scaled<-scale(df_org)

#Hierarchical Clustering
#number of clusters
set.seed(123)
fviz_nbclust(df_org_scaled, hcut, method = "wss", k.max = 15) + 
  theme_minimal() + 
  ggtitle("Elbow Method")

#clustering
proxmat_org <- dist(df_org_scaled, method = 'euclidean')
hclust_ward_org <- hclust(proxmat_org, method = 'ward.D')
plot(hclust_ward_org, cex = 0.1) 
rect.hclust(hclust_ward_org,k=8,border=c(1:8)) 
legend("topright",legend = paste("c",1:8),fill = 1:8,border = "white",text.font = 10)


#Plot Map
p1 <-cutree(hclust_ward_org,8)
sz_label <-as.vector(cluster.vars$"SUBZONE_N_ORI")
names(p1) <- sz_label
plot(sg_1,border = 'grey',col = p1, 
     main = 'Parition of Subzone by Hierachical Clustering')
legend("bottomright",legend = paste("c",1:8),fill = 1:8,border = 'white')


#Geo spatial Clustering
#remove geospatial data with no neighbor
appeared_sz <- unique(cluster_org$SUBZONE_N_ORI)
appeared_sz

sg <- inner_join(sg_1,cluster_org,by = c('SUBZONE_N'='SUBZONE_N_ORI'))
cluster_org
sg_11 <- sg_1[c(-36,-37,-38,-276,-298),]
sg_sp <- as_Spatial(sg_11)
sg_nb <- poly2nb(sg_sp)
sg_nb

df_org_spatial <- df_org_scaled[c(-36,-37,-38,-276,-298),]
df_org_spatial

lcosts <- nbcosts(sg_nb, df_org_spatial)

















