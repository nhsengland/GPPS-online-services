# R.version()

###
## Loading libraries##
###

library(openxlsx) # to open xlsx files
library(haven) # to open SPSS files
library(readr) #
library(tidyverse) # general toolkit
library(foreign) # another way to open SPSS files
library(kableExtra) # formatted tables
library(sjPlot)
library(gtsummary)
library(ggeffects)
library(lmtest)  # for coeftest
library(sandwich) # for vcovHC
library(modelsummary) # for modelsummary (logit...)
library(gt) # for tab_header
library(caret)
library(pROC)
'%!in%' <- function(x,y)!('%in%'(x,y))


### Call other scripts
source("log scripts/fun_table_robust.R")



###
##READING DATA CUT FILES##
###

flag_read= TRUE

if (flag_read){
  
  #data_cut_factors <- read.csv('data\\data_cuts_log\\data_identifiers_online__log_factors_1_enrich.csv')
  load('data\\data_cuts_log\\data_identifiers_online__log_factors_3_enrich.RData')
  
}

#check for nas again after simplifying
sapply(data_cut_factors, function(x) sum(x=="" | is.na(x)))
sapply(data_cut_factors, function(x) class(x))


#remove NA
#data_cut_factors <- na.omit(data_cut_factors)



#data_cut_factors <- data_cut_factors %>% mutate_at(vars(sex:GRP_LABEL,POMI_system_supplier,POMI_region_name),factor)

#### Re-level where appropriate
data_cut_factors$carer <- relevel(data_cut_factors$carer,ref="No")
data_cut_factors$sex <- relevel(data_cut_factors$sex,ref="Female")
data_cut_factors$ethnicity <- relevel(data_cut_factors$ethnicity,ref="White")
data_cut_factors$age <- relevel(data_cut_factors$age,ref="55 to 64")
data_cut_factors$work <- relevel(data_cut_factors$work,ref="Full-time")
data_cut_factors$guardian <- relevel(data_cut_factors$guardian,ref="No")
data_cut_factors$deaf <- relevel(data_cut_factors$deaf,ref="No")
data_cut_factors$smoker <- relevel(data_cut_factors$smoker,ref="Never smoked")
data_cut_factors$sexual_orientation <- relevel(data_cut_factors$sexual_orientation,ref="Heterosexual or straight")
data_cut_factors$religion <- relevel(data_cut_factors$religion,ref="Christian")
data_cut_factors$has_LTC <- relevel(data_cut_factors$has_LTC,ref="No")
data_cut_factors$POMI_system_supplier <- relevel(as.factor(data_cut_factors$POMI_system_supplier),ref="EMIS")
data_cut_factors$GRP_LABEL <- relevel(data_cut_factors$GRP_LABEL,ref="e-Mainstream")

data_cut_factors$POMI_region_name <- relevel(as.factor(data_cut_factors$POMI_region_name),ref="LONDON COMMISSIONING REGION")
data_cut_factors$ethnicity %>% levels()


### Change pop size to per 1000
data_cut_factors <- data_cut_factors %>% mutate(practice_pop_size = practice_pop_size/1000)


mydate = "20220704"


data_cut_factors <-  data_cut_factors %>% mutate(`Internet User Classification`=GRP_LABEL)

data_cut_factors$`Internet User Classification` <- factor(data_cut_factors$`Internet User Classification`,
                                                             level=c('e-Cultural Creators',
                                                                      'e-Professionals',
                                                                      'e-Veterans',
                                                                      'Youthful Urban Fringe',
                                                                      'e-Rational Utilitarians',
                                                                      'e-Mainstream',
                                                                      'Passive and Uncommitted Users',
                                                                      'Digital Seniors',
                                                                      'Settled Offline Communities',
                                                                      'e-Withdrawn'))

data_cut_factors$IUC <- data_cut_factors$`Internet User Classification`

######################################################
###  Logistic regression - any online - IUC ####
#####################################################

outcomestring = "anyOL"
outcomevar <- "used_online_service"


fmla <- paste(outcomevar, "~", "IUC")

glm_IUC <- glm(fmla,
               data = data_cut_factors,
               family=binomial)
summary(glm_IUC)

plot_model(glm_IUC,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           transform = "plogis",
           title="HAS used at least one of the online services"
) #+ scale_y_log10(limits = c(0.2, 3))

ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_plogis_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_plogis_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")


plot_model(glm_IUC,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           title="HAS used at least one of the online services"
) #+ scale_y_log10(limits = c(0.2, 3))

ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_OR_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_OR_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")



plot_model(glm_IUC,
           type = "eff",
           terms = c("IUC"),
           robust=TRUE,
           vcov.type="HC1",digits=1) + 
  labs(title='Probability of having used any online service',y="% used any online service",x="Internet User Classification") +
  theme(axis.text.x = element_text(angle = -90, hjust=0))
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_mProb_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_mProb_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")


(gt_anyonlineservice <- data_cut_factors %>% select("IUC",outcomevar) %>%
    tbl_summary(by=outcomevar) %>%
    add_p() %>%
    #add_overall() %>%
    modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ "**Used any online service**"))

gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\IUC\\",mydate,"_gt_",outcomestring,"_IUC_cp.html")))


(gt_anyonlineservice <- data_cut_factors %>% select("IUC","used_online_service") %>%
    tbl_summary(by=used_online_service,
                percent="row") %>%
    add_p() %>%
    #add_overall() %>%
    modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ "**Used any online service**"))

gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\IUC\\",mydate,"_gt_",outcomestring,"_IUC_rp.html")))




######################################################
###  Logistic regression - book appt online - IUC ####
#####################################################

outcomestring = "GPOL_appts"
outcomevar <- "GPOL_appts"
outcometitle <- "booking appointments online"

fmla <- paste(outcomevar, "~", "IUC")

glm_IUC <- glm(fmla,
               data = data_cut_factors,
               family=binomial)
summary(glm_IUC)

plot_model(glm_IUC,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           transform = "plogis",
           title=paste0("HAS used ",outcometitle)
) #+ scale_y_log10(limits = c(0.2, 3))

ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_plogis_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_plogis_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")


plot_model(glm_IUC,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           title=paste0("HAS used ",outcometitle)
) #+ scale_y_log10(limits = c(0.2, 3))

ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_OR_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_OR_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")



plot_model(glm_IUC,
           type = "eff",
           terms = c("IUC"),
           robust=TRUE,
           vcov.type="HC1",digits=1) + 
  labs(title=paste0('Probability of having used ',outcometitle),y=paste0("% used ",outcometitle),x="Internet User Classification") +
  theme(axis.text.x = element_text(angle = -90, hjust=0))
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_mProb_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_mProb_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")


(gt_anyonlineservice <- data_cut_factors %>% select("IUC",outcomevar) %>%
    tbl_summary(by=outcomevar) %>%
    add_p() %>%
    #add_overall() %>%
    modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("**Used ",outcometitle,"**")))

gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\IUC\\",mydate,"_gt_",outcomestring,"_IUC_cp.html")))


(gt_anyonlineservice <- data_cut_factors %>% select("IUC",outcomevar) %>%
    tbl_summary(by=outcomevar,
                percent="row") %>%
    add_p() %>%
    #add_overall() %>%
    modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("**Used ",outcometitle,"**")))

gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\IUC\\",mydate,"_gt_",outcomestring,"_IUC_rp.html")))



######################################################
###  Logistic regression - repeat presc online - IUC ####
#####################################################

outcomestring = "GPOL_repeat"
outcomevar <- "GPOL_repeat"
outcometitle <- "ordering repeat prescriptions online"

fmla <- paste(outcomevar, "~", "IUC")

glm_IUC <- glm(fmla,
               data = data_cut_factors,
               family=binomial)
summary(glm_IUC)

plot_model(glm_IUC,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           transform = "plogis",
           title=paste0("HAS used ",outcometitle)
) #+ scale_y_log10(limits = c(0.2, 3))

ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_plogis_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_plogis_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")


plot_model(glm_IUC,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           title=paste0("HAS used ",outcometitle)
) #+ scale_y_log10(limits = c(0.2, 3))

ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_OR_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_OR_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")



plot_model(glm_IUC,
           type = "eff",
           terms = c("IUC"),
           robust=TRUE,
           vcov.type="HC1",digits=1) + 
  labs(title=paste0('Probability of having used ',outcometitle),y=paste0("% used ",outcometitle),x="Internet User Classification") +
  theme(axis.text.x = element_text(angle = -90, hjust=0))
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_mProb_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_mProb_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")


(gt_anyonlineservice <- data_cut_factors %>% select("IUC",outcomevar) %>%
    tbl_summary(by=outcomevar) %>%
    add_p() %>%
    #add_overall() %>%
    modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("**Used ",outcometitle,"**")))

gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\IUC\\",mydate,"_gt_",outcomestring,"_IUC_cp.html")))


(gt_anyonlineservice <- data_cut_factors %>% select("IUC",outcomevar) %>%
    tbl_summary(by=outcomevar,
                percent="row") %>%
    add_p() %>%
    #add_overall() %>%
    modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("**Used ",outcometitle,"**")))

gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\IUC\\",mydate,"_gt_",outcomestring,"_IUC_rp.html")))



######################################################
###  Logistic regression - repeat presc online - IUC ####
#####################################################

outcomestring = "GPOL_records"
outcomevar <- "GPOL_records"
outcometitle <- "accessing medical records online"

fmla <- paste(outcomevar, "~", "IUC")

glm_IUC <- glm(fmla,
               data = data_cut_factors,
               family=binomial)
summary(glm_IUC)

plot_model(glm_IUC,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           transform = "plogis",
           title=paste0("HAS used ",outcometitle)
) #+ scale_y_log10(limits = c(0.2, 3))

ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_plogis_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_plogis_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")


plot_model(glm_IUC,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           title=paste0("HAS used ",outcometitle)
) #+ scale_y_log10(limits = c(0.2, 3))

ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_OR_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_OR_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")



plot_model(glm_IUC,
           type = "eff",
           terms = c("IUC"),
           robust=TRUE,
           vcov.type="HC1",digits=1) + 
  labs(title=paste0('Probability of having used ',outcometitle),y=paste0("% used ",outcometitle),x="Internet User Classification") +
  theme(axis.text.x = element_text(angle = -90, hjust=0))
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_mProb_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_mProb_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")


(gt_anyonlineservice <- data_cut_factors %>% select("IUC",outcomevar) %>%
    tbl_summary(by=outcomevar) %>%
    add_p() %>%
    #add_overall() %>%
    modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("**Used ",outcometitle,"**")))

gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\IUC\\",mydate,"_gt_",outcomestring,"_IUC_cp.html")))


(gt_anyonlineservice <- data_cut_factors %>% select("IUC",outcomevar) %>%
    tbl_summary(by=outcomevar,
                percent="row") %>%
    add_p() %>%
    #add_overall() %>%
    modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("**Used ",outcometitle,"**")))

gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\IUC\\",mydate,"_gt_",outcomestring,"_IUC_rp.html")))




######################################################
###  Logistic regression - online consultation - IUC ####
#####################################################

outcomestring = "GPOL_OC"
outcomevar <- "GPOL_OC"
outcometitle <- "having online consultation"

fmla <- paste(outcomevar, "~", "IUC")

glm_IUC <- glm(fmla,
               data = data_cut_factors,
               family=binomial)
summary(glm_IUC)

plot_model(glm_IUC,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           transform = "plogis",
           title=paste0("HAS used ",outcometitle)
) #+ scale_y_log10(limits = c(0.2, 3))

ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_plogis_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_plogis_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")


plot_model(glm_IUC,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           title=paste0("HAS used ",outcometitle)
) #+ scale_y_log10(limits = c(0.2, 3))

ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_OR_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_OR_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")



plot_model(glm_IUC,
           type = "eff",
           terms = c("IUC"),
           robust=TRUE,
           vcov.type="HC1",digits=1) + 
  labs(title=paste0('Probability of having used ',outcometitle),y=paste0("% used ",outcometitle),x="Internet User Classification") +
  theme(axis.text.x = element_text(angle = -90, hjust=0))
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_mProb_",outcomestring,".png"),width = 20, height = 20, dpi=300,units ="cm")
ggsave(paste0("log scripts\\outputs\\IUC\\",mydate,"_IUC_mProb_",outcomestring,".svg"),width = 20, height = 20, dpi=300,units ="cm")


(gt_anyonlineservice <- data_cut_factors %>% select("IUC",outcomevar) %>%
    tbl_summary(by=outcomevar) %>%
    add_p() %>%
    #add_overall() %>%
    modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("**Used ",outcometitle,"**")))

gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\IUC\\",mydate,"_gt_",outcomestring,"_IUC_cp.html")))


(gt_anyonlineservice <- data_cut_factors %>% select("IUC",outcomevar) %>%
    tbl_summary(by=outcomevar,
                percent="row") %>%
    add_p() %>%
    #add_overall() %>%
    modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("**Used ",outcometitle,"**")))

gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\IUC\\",mydate,"_gt_",outcomestring,"_IUC_rp.html")))


