pacman::p_load(rgdal, spdep, tmap, sf, 
               ggpubr, cluster, factoextra, NbClust,
               heatmaply, corrplot, tidyverse,psych,
               Hmisc,knitr,kableExtra,ClustGeo,ggiraphExtra)
sg_1 <- st_read(dsn = "data/geo/sz",layer = "MP14_SUBZONE_NO_SEA_PL") %>%
  select(3,16)
cluster_org <- read_rds(file = "data/rds/cluster_org.rds")

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

appeared_sz <- unique(cluster.vars$SUBZONE_N_ORI)
sg <- sg_1 %>%
  filter(SUBZONE_N %in% appeared_sz)


row.names(cluster.vars) <- cluster.vars$SUBZONE_N_ORI
df_org <- cluster.vars %>%
  select(c(2:8))

#scale
df_org_scaled<-scale(df_org)

sg_sp <- as_Spatial(sg)
sg_nb <- poly2nb(sg_sp)
sg_nb

lcosts <- nbcosts(sg_nb, df_org_scaled)
sg.wm<-nb2listw(sg_nb,lcosts,style = "B")
#summary(sg.wm)
sg.mst <-mstree(sg.wm)
head(sg.mst)

#plot
clust <- spdep::skater(edges = sg.mst[,1:2], 
                        data = df_org_scaled, 
                        method = "manhattan", 
                        ncuts = 10)

ccs <- clust$groups
table(ccs)

groups_mat <- as.matrix(clust$groups)
sg_sf_spatialcluster <- cbind(sg, as.factor(groups_mat)) %>%
  rename(`SP_CLUSTER`=`as.factor.groups_mat.`)
qtm(sg_sf_spatialcluster, "SP_CLUSTER")




