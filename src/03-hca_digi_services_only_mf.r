#Hierarchical Clustering Analysis
library(cluster)
library(dplyr)
library(stringr)
library(gtsummary)
library(StatMatch)
library(factoextra)
library("reshape2")
library("purrr")
# let's start with a dendrogram
library("dendextend")

mydate = "20220830"

#load in enriched data
data = read.csv('data/data_cuts_log/data_enriched_clustering_Cutdigi.csv')
data = data[,-c(1)]
#drop the last column indicating has used any online services again
data = data[,-6]
#remove nas from the data
#data = na.omit(data)


### Re-align levels

data <- data %>% rename(last_booking_attempt = Q75, last_appointment = Q83,satisfaction=Q28)

data$last_booking_attempt <- factor(data$last_booking_attempt,levels = c("In the past 3 months",
                                                                         "Between 3 and 6 months ago",
                                                                         "Between 6 and 12 months ago",
                                                                         "More than 12 months ago",
                                                                         "Don’t know",
                                                                         "I haven’t tried to make an appointment since being registered with my current GP practice"))
data$last_appointment <- factor(data$last_appointment,levels = c("In the past 3 months",
                                                                         "Between 3 and 6 months ago",
                                                                         "Between 6 and 12 months ago",
                                                                         "More than 12 months ago",
                                                                         "I haven’t had an appointment since being registered with my current GP practice"))

data$satisfaction <- factor(data$satisfaction, levels = c("Very good","Fairly good","Neither good nor poor","Fairly poor","Very poor"))

data$info_offline <- factor(data$info_offline , levels=c("yes","no","not-applicable"))

data$info_online <- factor(data$info_online , levels=c("yes","no","not-applicable"))

data$booking_mode_triedonline <- factor(data$booking_mode_triedonline , levels=c("yes","other","not-applicable"))

data$CCG_Name <- str_replace_all(data$CCG_Name, fixed(" "), "") # fix spaces
data$STP_Code_ONS <- str_replace_all(data$STP_Code_ONS, fixed(" "), "") # fix spaces


### Some new variable with reduction of levels

data <- data %>% mutate(religion3 = case_when(religion == "Christian" ~ "Christian",
                                        religion == "No religion" ~ "No religion",
                                        TRUE ~ "Other"),
                        religion3 = ifelse(is.na(religion),religion,religion3))

data %>% group_by(religion3,religion) %>% summarise(n())
data %>% group_by(religion3) %>% summarise(n())

################ 3 CCGs in CM ############


data %>% filter(STP_Code_ONS == "E54000008") %>% .$CCG_Name %>% unique()

clusterid = "digisOLSD4ccg"
data_stpa <- data %>% filter(CCG_Name %in% c("NHSWARRINGTONCCG","NHSWIRRALCCG","NHSSTHELENSCCG","NHSHALTONCCG")  ) # Extract matching rows with str_detect

clusterid = "digisOLSD" # looks at digitally engaged, clusters on both sociodemographics and online use aspects
data_stpa <- data %>% filter(CCG_Name %in% c("NHSWARRINGTONCCG","NHSWIRRALCCG","NHSSTHELENSCCG")  ) # Extract matching rows with str_detect

# data %>% filter(CCG_Name %in% c("NHSWARRINGTONCCG","NHSWIRRALCCG","NHSSTHELENSCCG","NHSHALTONCCG")) %>% group_by(CCG_Name,used_online_service) %>% summarise(n())

#### Show how used online differs from non-used online . Further down we only segment within those that did use any online ####
# Clustering vars, summarised
tbsummary_yn <- data_stpa %>%
  select("GPOL_appts", "GPOL_repeat", "GPOL_records", "GPOL_OC", "last_booking_attempt", "last_appointment", "gp_website", "info_online", "info_offline", "booking_mode_triedonline",
         "age","has_LTC","LTC_hypertension","smoker","sex","LTC_mentalhealth","LTC_breathing","religion3","LTC_diabetes","has_mobilityissues","guardian","used_online_service") %>%
  tbl_summary(by = 'used_online_service') %>%
  add_overall()
tbsummary_yn
gt::gtsave(as_gt(tbsummary_yn), file = file.path(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"_summarypre0_cp.html")))


# Contextual vars (1)
tbsummary_ynex1 <- data_stpa  %>% select(used_online_service,patient_imd_decile,ethnicity,carer,work,has_shielded,has_isolation,sexual_orientation) %>%
  tbl_summary(by = 'used_online_service') %>% add_overall()
tbsummary_ynex1
gt::gtsave(as_gt(tbsummary_ynex1), file = file.path(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"_summarypre1_cp.html")))


# Contextual vars (2)
tbsummary_ynex2 <- data_stpa %>% select(used_online_service,has_LTC,starts_with("LTC"),satisfaction,"POMI_region_name", "POMI_perc_patenabled", "POMI_perc_patenabledpresc", "POMI_perc_patenabledappts", "POMI_perc_patenabledDCR", "POMI_system_supplier",
                                                                    "GRP_LABEL","int_avg_download","int_max_download","int_avg_datausage") %>%
  tbl_summary(by = 'used_online_service') %>% add_overall()
tbsummary_ynex2
gt::gtsave(as_gt(tbsummary_ynex2), file = file.path(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"_summarypre2_cp.html")))



#### Sample of interest and clustering ###

data_stp <- data_stpa %>% filter(used_online_service == 1)

data_stp %>% group_by(GPOL_appts,GPOL_repeat,GPOL_records,GPOL_OC) %>% summarise(n()) %>% View()

#feel free to add/remove variables as needed
data_cm <- data_stp[, c("id","GPOL_appts", "GPOL_repeat", "GPOL_records", "GPOL_OC", "last_booking_attempt", "last_appointment", "gp_website", "info_online", "info_offline", "booking_mode_triedonline",
                        "age","has_LTC","LTC_hypertension","smoker","sex","LTC_mentalhealth","LTC_breathing","religion3","LTC_diabetes","has_mobilityissues","guardian")]
data_cm <- na.omit(data_cm)

#data_cm <- data_cm[1:10000,]
data_cm_id <- data_cm$id
data_cm <- data_cm[,-c(1)]
#create the distance matrix for warrington
dist_cm <- gower.dist(data_cm)
dist_cm <- as.dist(dist_cm)
#dist_warrington <- na.omit(dist_warrington) #remove nas if needed

#run hierarchical clustering algorithm HCA and plot dendrogram
hc_cm<-hclust(dist_cm, method = "complete")

# Elbow
fviz_nbclust(data_cm, FUN = hcut, method = "wss",diss=dist_cm,k.max=15)

ggsave(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"_elbow.png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"_elbow.svg"),width = 20, height = 20, dpi=300,units ="cm")

# Silhouette
fviz_nbclust(data_cm, FUN = hcut, method = "silhouette",diss=dist_cm,k.max=15)

ggsave(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"_silhouette.png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"_silhouette.svg"),width = 20, height = 20, dpi=300,units ="cm")

know=10

# choose k, number of clusters 
cluster<-cutree(hc_cm, k=know)


dendro <- as.dendrogram(hc_cm)
dendro.col <- dendro %>%
  set("branches_k_color", k = know, value =   c("darkslategray", "darkslategray4", "darkslategray3", "gold3", "darkcyan", "cyan3", "gold3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 10")

ggsave(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_dendogram.png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_dendogram.svg"),width = 20, height = 20, dpi=300,units ="cm")


# Radial plot looks less cluttered (and cooler)
ggplot(ggd1, labels = T) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

ggsave(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_dendogram_cp.png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_dendogram_cp.svg"),width = 20, height = 20, dpi=300,units ="cm")


#add the cluster column to the clustering dataframe
# add cluster to original data 
df_cm<-cbind(data_cm, as.factor(cluster))
names(df_cm)[names(df_cm) == 'as.factor(cluster)'] <- 'cluster'

# add the cluster column to the original dataframe
lkp_id_clust <- data.frame(id=data_cm_id,cluster=cluster)
data_stp_cl <- data_stp %>% left_join(lkp_id_clust,by=c("id"))

# Clustering vars, summarised
tbsummary <- df_cm %>% tbl_summary(by = 'cluster') %>% add_overall()
tbsummary
gt::gtsave(as_gt(tbsummary), file = file.path(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_summary0_cp.html")))


# Contextual vars (1)
tbsummary_ex1 <- data_stp_cl %>% filter(!is.na(cluster)) %>% select(cluster,used_online_service,patient_imd_decile,ethnicity,carer,work,has_shielded,has_isolation,sexual_orientation) %>% tbl_summary(by = 'cluster') %>% add_overall()
tbsummary_ex1
gt::gtsave(as_gt(tbsummary_ex1), file = file.path(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_summary1_cp.html")))


# Contextual vars (2)
tbsummary_ex2 <- data_stp_cl %>% filter(!is.na(cluster)) %>% select(cluster,used_online_service,has_LTC,starts_with("LTC"),satisfaction,"POMI_region_name", "POMI_perc_patenabled", "POMI_perc_patenabledpresc", "POMI_perc_patenabledappts", "POMI_perc_patenabledDCR", "POMI_system_supplier",
                                                                    "GRP_LABEL","int_avg_download","int_max_download","int_avg_datausage") %>% tbl_summary(by = 'cluster') %>% add_overall()
tbsummary_ex2
gt::gtsave(as_gt(tbsummary_ex2), file = file.path(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_summary2_cp.html")))


### Heatmap
library("reshape2")
# Time for the heatmap
# the 1st step here is to have 1 variable per row
# factors have to be converted to characters in order not to be dropped

df_cm_id <- df_cm %>% bind_cols(lkp_id_clust[,1]) %>% rename("id"=`...23`)
cust.long <- melt(data.frame(lapply(df_cm_id, as.character), stringsAsFactors=FALSE), 
                  id = c("id", "cluster"), factorsAsStrings=T)
cust.long.q <- cust.long %>%
  group_by(cluster, variable, value) %>%
  mutate(count = n_distinct(id)) %>%
  distinct(cluster, variable, value, count)

cust.long.all <- cust.long %>% ungroup() %>%
  group_by(variable,value) %>%
  summarise(cluster="All",count = n_distinct(id))

cust.long.q <- cust.long.q %>% rbind(cust.long.all)

## vector with all levels
v_levels = c()
n_levels = c()
v_colnames <- colnames(df_cm %>% select(-cluster))
cust.long.q <- cust.long.q %>% mutate(value_l = paste0(variable,value))

for (coli in v_colnames){
  
  levels_now <- df_cm %>% select(coli) %>% unique() %>% unlist() %>% as.character()
  levels_now <- paste0(coli,levels_now)
  v_levels <- c(v_levels,levels_now)
  n_levels <- c(n_levels,length(levels_now))
}

# heatmap.c will be suitable in case you want to go for absolute counts - but it doesn't tell much to my taste
heatmap.c <- ggplot(cust.long.q, aes(x = cluster, y =factor(value_l, levels = v_levels, ordered = T))) +
  
  geom_tile(aes(fill = count))+
  scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")
heatmap.c

# calculating the percent of each factor level in the absolute count of cluster members
cust.long.p <- cust.long.q %>%
  group_by(cluster, variable) %>%
  mutate(perc = count / sum(count)) %>%
  arrange(cluster)
heatmap.p <- ggplot(cust.long.p, aes(x = cluster, y = factor(value_l, levels = v_levels, ordered = T))) +
  
  geom_tile(aes(fill = perc*100), alpha = 0.85)+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  #scale_fill_gradient2(low = "gray96", high = "turquoise4")+
  scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")+
  theme_minimal()+
  labs(fill="(%)")

for(lin in cumsum(n_levels)){
  
  heatmap.p <- heatmap.p + geom_hline(yintercept = lin+0.5)
  
}

heatmap.p
ggsave(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_heatmapp.png"),width = 40, height = 30, dpi=300,units ="cm")
ggsave(paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_heatmapp.svg"),width = 40, height = 30, dpi=300,units ="cm")

write.csv(cust.long.p,paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_heatmapp.csv"))
write.csv(cust.long.q,paste0("clustering_mf\\outputs\\",mydate,"_",clusterid,"k",know,"_heatmapq.csv"))







################# STP #####

data_stp <- data %>% filter(STP_Code_ONS == "E54000008") # Extract matching rows with str_detect

#remove variables which showed to add no value to the clustering and separating between whether or not patients used GP online services
#as there are quite a few LTC variables, we will cluster with has_LTC and look at the breakdown of the clusters with the individual LTC's
#we will also add patient_imd_decile post clustering to see the effects of the other variables

#feel free to add/remove variables as needed
data_cm <- data_stp[, c("id","GPOL_appts", "GPOL_repeat", "GPOL_records", "GPOL_OC", "last_booking_attempt", "last_appointment", "gp_website", "info_online", "info_offline", "booking_mode_triedonline")]

data_cm <- na.omit(data_cm)


data_cm <- data_cm[1:10000,]
data_cm_id <- data_cm$id
data_cm <- data_cm[,-c(1)]
#create the distance matrix for warrington
dist_cm <- gower.dist(data_cm[1:10000,])
dist_cm <- as.dist(dist_cm)
#dist_warrington <- na.omit(dist_warrington) #remove nas if needed

#run hierarchical clustering algorithm HCA and plot dendrogram
hc_cm<-hclust(dist_cm, method = "complete")


# choose k, number of clusters 
cluster<-cutree(hc_cm, k=8)


#add the cluster column to the clustering dataframe
# add cluster to original data 
df_cm<-cbind(data_cm, as.factor(cluster))
names(df_cm)[names(df_cm) == 'as.factor(cluster)'] <- 'cluster'

# add the cluster column to the original dataframe
lkp_id_clust <- data.frame(id=data_cm_id,cluster=cluster)
data_stp_cl <- data_stp %>% left_join(lkp_id_clust,by=c("id"))

# Clustering vars, summarised
tbsummary <- df_cm %>% tbl_summary(by = 'cluster') %>% add_overall()
tbsummary

# Contextual vars (1)
tbsummary_ex1 <- data_stp_cl %>% filter(!is.na(cluster)) %>% select(cluster,used_online_service,has_LTC,age,smoker,patient_imd_decile,religion,guardian,carer,work,has_mobilityissues,has_shielded) %>% tbl_summary(by = 'cluster') %>% add_overall()
tbsummary_ex1

# Contextual vars (2)
tbsummary_ex2 <- data_stp_cl %>% filter(!is.na(cluster)) %>% select(cluster,used_online_service,has_LTC,starts_with("LTC"),satisfaction,"POMI_region_name", "POMI_perc_patenabled", "POMI_perc_patenabledpresc", "POMI_perc_patenabledappts", "POMI_perc_patenabledDCR", "POMI_system_supplier",
                                                                    "GRP_LABEL","int_avg_download","int_max_download","int_avg_datausage") %>% tbl_summary(by = 'cluster') %>% add_overall()
tbsummary_ex2


