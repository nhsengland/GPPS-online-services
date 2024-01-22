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
data_cut_factors <- na.omit(data_cut_factors)



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


mydate = "202200803"

sink(paste0("log-",mydate,"selection.txt"))

plot_model_edit <- function(theglm,thevars,mytitle="HAS used at least one of the online services"){
  
  plot_model(theglm,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
             v.line.color="red",
             terms = thevars,
             robust=T,vcov.type="HC1",show.intercept=T,digits=3,
             title=paste0(mytitle," - ",varsarea)
  ) + scale_y_log10(limits = c(0.2, 3))+
    theme(text = element_text(size = 18))
  myheight = ifelse(varsarea=="all",40,30)
  ggsave(paste0("log scripts\\outputs\\",myfolder,"\\",outcomevar,"_",mydate,"glm_treeplot_",varsarea,"_",model_d,".svg"),width = 40, height = myheight, dpi=300,units ="cm")
  ggsave(paste0("log scripts\\outputs\\",myfolder,"\\",outcomevar,"_",mydate,"glm_treeplot_",varsarea,"_",model_d,".png"),width = 40, height = myheight, dpi=300,units ="cm")
  
}


descriptive_function <- function(data_cut_factors,outcomevar) {
  
  # LTCs
  (gt_anyonlineservice <- data_cut_factors %>% select("has_mobilityissues":"LTC_anyother",outcomevar) %>%
     mutate_all(~as.numeric(as.character(.))) %>%
     tbl_summary(by=outcomevar) %>%
     add_p() %>%
     #add_overall() %>%
     modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("** Used ",outcomevar, "online service**")))
  
  
  gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\",myfolder,"\\gt_",outcomevar,"_CC_exp.html")))
  
  # Internet utilisation
  (gt_anyonlineservice <- data_cut_factors %>% select("GRP_LABEL",outcomevar,"int_avg_download","int_avg_datausage","int_perc_NGA","used_online_service") %>%
      tbl_summary(by=outcomevar) %>%
      add_p() %>%
      #add_overall() %>%
      modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("** Used ",outcomevar, "online service**")))
  
  gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\",myfolder,"\\gt_",outcomevar,"_IUC_exp.html")))
  
  
  
  # SD
  (gt_anyonlineservice <- data_cut_factors %>% select("sex":"religion","has_shielded","patient_imd_decile","rurality_new",outcomevar) %>%
      tbl_summary(by=outcomevar) %>%
      add_p() %>%
      #add_overall() %>%
      modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ paste0("** Used ",outcomevar, "online service**")))
  

  gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\",myfolder,"\\gt_",outcomevar,"_SD_exp.html")))
  
  
}

######################## HAS USED ANY ONLINE SERVICE ##################################

###
#### Descriptive ####
###

flag_descriptive=FALSE
if (flag_descriptive){
  
  # LTCs
  (gt_anyonlineservice <- data_cut_factors %>% select("has_mobilityissues":"LTC_anyother","used_online_service") %>%
     mutate_all(~as.numeric(as.character(.))) %>%
     tbl_summary(by=used_online_service) %>%
     add_p() %>%
     #add_overall() %>%
     modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ "**Used any online service**"))
  
  
  gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\",mydate,"\\gt_anyOL_LTC_exp.html")))
  # Internet utilisation
  (gt_anyonlineservice <- data_cut_factors %>% select("GRP_LABEL","used_online_service","int_avg_download","int_avg_datausage","int_perc_NGA","used_online_service") %>%
      tbl_summary(by=used_online_service) %>%
      add_p() %>%
      #add_overall() %>%
      modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ "**Used any online service**"))
  
  gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\",mydate,"\\gt_anyOL_IUC_exp.html")))
  
  
  # SD
  (gt_anyonlineservice <- data_cut_factors %>% select("sex":"religion","has_shielded","patient_imd_decile","rurality_new","used_online_service") %>%
      tbl_summary(by=used_online_service) %>%
      add_p() %>%
      #add_overall() %>%
      modify_header(label="**Characteristics**") %>% modify_spanning_header(c("stat_1", "stat_2") ~ "**Used any online service**"))
  
  gt::gtsave(as_gt(gt_anyonlineservice), file = file.path(paste0("log scripts\\outputs\\",mydate,"\\gt_anyOL_SD_exp.html")))
  
}
  
# Use function from gt package to save table as neat html
#gt::gtsave(as_gt(gt_anyonlineservice), file = file.path("log scripts\\outputs\\gt_anyonlineservice_exp.html"))




###
###  Logistic regression ####
###

model_d = "traincompall"

#outcomevar = "Q114_5"
outcomevar = "used_online_service"
comp = "compall"
model_d = paste0("train",comp)
myfolder = paste0(outcomevar,"_",mydate,"_",comp)

if (!dir.exists(paste0("log scripts\\outputs\\",myfolder))){
  dir.create(paste0("log scripts\\outputs\\",myfolder))
}



exclusion_vars = c("id","Q31_17","deaf","gender_identity_same",
                   "Q75","Q83","GPOL_appts","GPOL_repeat","GPOL_records","GPOL_OC",
                   "int_max_download","int_avg_datausage","int_unable_less10mb","GRP_CD","GRP_LABEL",
                   "Practice_Code","STP_Code_ONS",
                   "POMI_perc_patenabledpresc","POMI_perc_patenabledappts","POMI_perc_patenabledDCR","POMI_Sys_Appts_Enbld","POMI_Sys_DetCodeRec_Enbld","POMI_Sys_Presc_Enbld")

# for forward
exclusion_vars = c("id","Q31_17",
                   "Q75","Q83","GPOL_appts","GPOL_repeat","GPOL_records","GPOL_OC",
                   "int_max_download","int_avg_datausage","int_unable_less10mb","GRP_CD","GRP_LABEL",
                   "Practice_Code","STP_Code_ONS",
                   "POMI_perc_patenabledpresc","POMI_perc_patenabledappts","POMI_perc_patenabledDCR","POMI_Sys_Appts_Enbld","POMI_Sys_DetCodeRec_Enbld","POMI_Sys_Presc_Enbld")


explanatoryvar = colnames(data_cut_factors) %>% as.tibble()
explanatoryvar = explanatoryvar %>% filter(value != outcomevar & value %!in% c("used_online_service","Q114_5",exclusion_vars))
explanatoryvar = explanatoryvar %>% filter(substr(value,1,3)!="LTC")
print(explanatoryvar)

fmla <-
  as.formula(paste(outcomevar, paste(explanatoryvar$value, collapse = " + "
  ), sep = "~"))
print(fmla)

# Cohort: those in practices with at least an econsultation code instance in 20/21, those with either an econsult or GP consultation recorded.
data_model = data_cut_factors

set.seed(999)
a <- createDataPartition(data_model$used_online_service, list=FALSE,p=0.7)
training <- data_model[a,]
test <- data_model[-a,]


postglm <- glm(fmla,
               data = training,
               family=binomial)


###
#### Tidy tabulations ####

print("standard errors")
print(summary(postglm))

create_ORrob(postglm,
             paste0("log scripts\\outputs\\",myfolder,"\\"),
             paste0(outcomevar,"_",mydate,"_",model_d))


# Tidy plot of model (sjPlot)
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
plot_model(postglm,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           v.line.color="red",
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           title="HAS used at least one of the online services"
           ) + scale_y_log10(limits = c(0.2, 3)) +
  theme(text = element_text(size = 18))
ggsave(paste0("log scripts\\outputs\\",myfolder,"\\",outcomevar,"_",mydate,"_",model_d,"_glm_treeplot.svg"),
       width = 40, height = 40, dpi=300,units ="cm")



####
### Variable considerations ####

# Significance of each factor (not level)
flag_omnibus = FALSE
if (flag_omnibus){
  myanova <- anova(postglm,test="LRT")
  print(myanova)
  saveRDS(myanova, file = paste0("log scripts\\outputs\\",myfolder,"\\","anova.rds"))
  aa<-readRDS(file = paste0("log scripts\\outputs\\",myfolder,"\\","anova.rds"))
}
#chisq.test(postglm)

#### The Null model fitted and compared
flag_null = FALSE
postglm0 <- update(postglm, . ~ 1)
if (flag_null){
  summary(postglm0)
  rcompanion::nagelkerke(postglm) # meth_b - 2nd as nested of 1st
}

## Variable importance / drops
### Significance tests for 'dropping 1 factor' (omnibus)
# http://rstudio-pubs-static.s3.amazonaws.com/2899_a9129debf6bd47d2a0501de9c0dc583d.html
flag_drop1 = FALSE
if (flag_drop1){
  mydrop1 <- drop1(postglm,test="LRT")
  saveRDS(mydrop1, file = paste0("log scripts\\outputs\\",myfolder,"\\","drop1.rds"))
}

# Backward Step-wise to confirm importance of variables
# http://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf
postglm.bw = step(postglm) # default is backwards
summary(postglm.bw)
# this removed 'gender_identity_same' and 'deaf' from list sex + gender_identity_same + ethnicity + age + work + carer + guardian + smoker + sexual_orientation +   religion + has_LTC + rurality_new + patient_imd_decile

# Forward Step-wise to confirm importance of variables
flag_forward = TRUE
if (flag_forward){
  postglm.fw = step(postglm0,scope=list(lower=formula(postglm0),upper=formula(postglm)), direction="forward")
}
# this led to variables, in the following order, added: 
# used_online_service ~ has_LTC + age + patient_imd_decile + smoker +  
# sex + religion + work + guardian + sexual_orientation + carer +  rurality_new + ethnicity


###
#### evaluation on test ####

modtrain_resp<-predict(postglm, newdata=training, type="response")
modtest_resp<-predict(postglm, newdata=test, type="response")

test<-cbind(test, modtest_resp)

# plot the ROC

# train data:
#windows()
plot(roc(training$used_online_service,modtrain_resp),print.auc=TRUE)
ggsave(paste0("log scripts\\outputs\\",myfolder,"\\",outcomevar,"_",mydate,"_",model_d,"_glm_roc.svg"),
       width = 40, height = 40, dpi=300,units ="cm")


# Test data:
#windows();
plot(roc(test$used_online_service,modtest_resp),print.auc=TRUE)


#### Confusion Matrix at 0.5 threshold ####
# Will capture useful statistics such as sensitivity and specificity
# Train data:
cmtrain <-DescTools::Conf(x=as.numeric(ifelse(modtrain_resp>0.5,1,0)),
                ref=training$used_online_service,
                pos=1)
saveRDS(cmtrain, file = paste0("log scripts\\outputs\\",myfolder,"\\","cmtrain.rds"))


# Test data:
(cmtest <- DescTools::Conf(x=as.numeric(ifelse(modtest_resp>0.5,1,0)),
                ref=as.numeric(as.character(test$used_online_service)),
                pos=1))
saveRDS(cmtest, file = paste0("log scripts\\outputs\\",myfolder,"\\","cmtest.rds"))

#### Brier score #####
# Remember, the closer the Brier score is to 0, the better the model.
#Brier score on train data:
DescTools::BrierScore(resp=as.numeric(as.character(training$used_online_service)),pred=modtrain_resp) # will yield the same result!
#Brier score on test data:
DescTools::BrierScore(resp=as.numeric(as.character(test$used_online_service)),pred=modtest_resp)


#### Full data model ####

model_d = "fullcompall"

#print(postglm.bw)
fullglm <- glm(fmla,
              data = data_model,
              family=binomial)


summary(fullglm)

# Table
create_ORrob(postglm,
             paste0("log scripts\\outputs\\",myfolder,"\\"),
             paste0(outcomevar,"_",mydate,"_",model_d))

# Tidy plot of model (sjPlot)
coef_n <- fullglm$coefficients %>% names
coef_n <- as.data.frame(coef_n)



varsarea <- "SD"
varsnow <- coef_n %>% filter(substr(coef_n,1,3) %in% c("sex","eth","age","wor","car","gua","rel"))
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

plot_model_edit(fullglm,varsnow$coef_n)

varsarea <- "CC"
varsnow <- coef_n %>% filter(substr(coef_n,1,3) %in% c("has","smo"))
plot_model_edit(fullglm,varsnow$coef_n)

varsarea <- "other"
varsnow <- coef_n %>% filter(substr(coef_n,1,3) %!in% c("has","smo","sex","eth","age","wor","car","gua","rel"))
plot_model_edit(fullglm,varsnow$coef_n)




###
#### marginal effects ####
# https://strengejacke.github.io/ggeffects/articles/introduction_marginal_effects.html
# https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html
plot_model(postglm, type = "pred", terms = c("age","sex [Female,Male]"))
ggsave(paste0("log scripts\\outputs\\",myfolder,"\\","glm_margpred_agesex.svg"),width = 40, height = 30, dpi=300,units ="cm")

plot_model(postglm, type = "eff", terms = c("age","sex [Female,Male]"))
ggsave(paste0("log scripts\\outputs\\",myfolder,"\\","glm_margeff_agesex.svg"),width = 40, height = 30, dpi=300,units ="cm")

plot_model(postglm, type = "pred", terms = c("age","ethnicity"))
ggsave(paste0("log scripts\\outputs\\",myfolder,"\\","glm_margeff_ageethn.svg"),width = 40, height = 30, dpi=300,units ="cm")
# adding deprivation making ethnicity black in itself not s.s...

plot_model(postglm, type = "eff", terms = c("ethnicity"))

plot_model(postglm, type = "eff", terms = c("patient_imd_decile"))
plot_model(postglm, type = "eff", terms = c("patient_imd_decile","has_LTC"))
ggsave(paste0("log scripts\\outputs\\",myfolder,"\\","glm_margeff_imdltc.svg"),width = 40, height = 30, dpi=300,units ="cm")


ggpredict(postglm,"age")
ggeffect(postglm,"age")


###
###  Logistic regression - any online - LTCs vars #########
###

comp = "compallvLTC"
model_d = paste0("train",comp)
model_d_test = paste0("test",comp)
myfolder = paste0(outcomevar,"_",mydate,"_",comp)


if (!dir.exists(paste0("log scripts\\outputs\\",myfolder))){
  dir.create(paste0("log scripts\\outputs\\",myfolder))
}

# As above but instead of has_LTC uses the various LTC levels
outcomevar = "used_online_service"

exclusion_vars = c("id","Q31_17","deaf","gender_identity_same","has_LTC",
                   "Q75","Q83","GPOL_appts","GPOL_repeat","GPOL_records","GPOL_OC",
                   "Practice_Code","STP_Code_ONS",
                   "int_max_download","int_avg_datausage","int_unable_less10mb","GRP_CD","GRP_LABEL",
                   "POMI_perc_patenabledpresc","POMI_perc_patenabledappts","POMI_perc_patenabledDCR","POMI_Sys_Appts_Enbld","POMI_Sys_DetCodeRec_Enbld","POMI_Sys_Presc_Enbld")


explanatoryvar = colnames(data_cut_factors) %>% as.tibble()
explanatoryvar = explanatoryvar %>% filter(value != outcomevar & value %!in% c("used_online_service","Q114_5",exclusion_vars))
print(explanatoryvar)

fmla <-
  as.formula(paste(outcomevar, paste(explanatoryvar$value, collapse = " + "
  ), sep = "~"))
print(fmla)

data_model = data_cut_factors

set.seed(999)
a <- createDataPartition(data_model$used_online_service, list=FALSE,p=0.7)
training <- data_model[a,]
test <- data_model[-a,]


postglm <- glm(fmla,
               data = training,
               family=binomial)


###
#### Tidy tabulations ####

create_ORrob(postglm,
             paste0("log scripts\\outputs\\",myfolder,"\\"),
             paste0(outcomevar,"_",mydate,"_",model_d))

# Tidy plot of model (sjPlot)
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
plot_model(postglm,show.values=T, show.p=TRUE, ci.lvl=.95, value.offset = 0.5,
           v.line.color="red",
           robust=T,vcov.type="HC1",show.intercept=T,digits=3,
           title="HAS used at least one of the online services"
) + scale_y_log10(limits = c(0.2, 3)) +
  theme(text = element_text(size = 18))
ggsave(paste0("log scripts\\outputs\\",myfolder,"\\",outcomevar,"_",mydate,"_",model_d,"_glm_treeplot.svg"),
       width = 40, height = 40, dpi=300,units ="cm")

###
### Variable considerations ####

# Significance of each factor (not level)
flag_omnibus = TRUE
if (flag_omnibus){
  myanova <- anova(postglm,test="LRT")
  print(myanova)
  saveRDS(myanova, file = paste0("log scripts\\outputs\\",myfolder,"\\","anova.rds"))
  aa<-readRDS(file = paste0("log scripts\\outputs\\",myfolder,"\\","anova.rds"))
}
#chisq.test(postglm)

#### The Null model fitted and compared
flag_null = TRUE
if (flag_null){
  postglm0 <- update(postglm, . ~ 1)
  summary(postglm0)
  (rcompanion::nagelkerke(postglm)) # meth_b - 2nd as nested of 1st
  saveRDS(rcompanion::nagelkerke(postglm), file = paste0("log scripts\\outputs\\",myfolder,"\\","nagelkerke.rds"))
}

## Variable importance / drops
### Significance tests for 'dropping 1 factor' (omnibus)
# http://rstudio-pubs-static.s3.amazonaws.com/2899_a9129debf6bd47d2a0501de9c0dc583d.html
flag_drop1 = TRUE
if (flag_drop1){
  mydrop1 <- drop1(postglm,test="LRT")
  saveRDS(mydrop1, file = paste0("log scripts\\outputs\\",myfolder,"\\","drop1.rds"))
}

# Backward Step-wise to confirm importance of variables
# http://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf
flag_bw = TRUE
if (flag_bw){
  postglm.bw = step(postglm) # default is backwards
  summary(postglm.bw)
}

# Forward Step-wise to confirm importance of variables
flag_forward = TRUE
if (flag_forward){
  postglm.fw = step(postglm0,scope=list(lower=formula(postglm0),upper=formula(postglm)), direction="forward")
}

# close connection
sink()

###
#### evaluation on test ####

modtrain_resp<-predict(postglm, newdata=training, type="response")
modtest_resp<-predict(postglm, newdata=test, type="response")

test<-cbind(test, modtest_resp)

# plot the ROC

# train data:
#windows()
plot(roc(training$used_online_service,modtrain_resp),print.auc=TRUE)
ggsave(paste0("log scripts\\outputs\\",myfolder,"\\",outcomevar,"_",mydate,"glm_ROC_",model_d,".svg"),width = 40, height = 30, dpi=300,units ="cm")

# Test data:
#windows();
plot(roc(test$used_online_service,modtest_resp),print.auc=TRUE)
ggsave(paste0("log scripts\\outputs\\",myfolder,"\\",outcomevar,"_",mydate,"glm_ROC_",model_d_test,".svg"),width = 40, height = 30, dpi=300,units ="cm")


#### Confusion Matrix at 0.5 threshold ####
# Will capture useful statistics such as sensitivity and specificity
# Train data:
cmtrain <-DescTools::Conf(x=as.numeric(ifelse(modtrain_resp>0.5,1,0)),
                          ref=training$used_online_service,
                          pos=1)
saveRDS(cmtrain, file = paste0("log scripts\\outputs\\",myfolder,"\\","cmtrain.rds"))


# Test data:
(cmtest <- DescTools::Conf(x=as.numeric(ifelse(modtest_resp>0.5,1,0)),
                           ref=as.numeric(as.character(test$used_online_service)),
                           pos=1))
saveRDS(cmtest, file = paste0("log scripts\\outputs\\",myfolder,"\\","cmtest.rds"))

#### Brier score #####
# Remember, the closer the Brier score is to 0, the better the model.
#Brier score on train data:
briers <- c(0,0)
briers[1]<-DescTools::BrierScore(resp=as.numeric(as.character(training$used_online_service)),pred=modtrain_resp) # will yield the same result!
#Brier score on test data:
briers[2]<-DescTools::BrierScore(resp=as.numeric(as.character(test$used_online_service)),pred=modtest_resp)
saveRDS(briers, file = paste0("log scripts\\outputs\\",myfolder,"\\","briers.rds"))

#### Full data model ###

model_d = paste0("full",comp)


print(postglm.bw)

fullglm <- glm(fmla,
               data = data_model,
               family=binomial)


summary(fullglm)

# Table
create_ORrob(fullglm,paste0("log scripts\\outputs\\",myfolder,"\\"),paste0(outcomevar,"_",mydate,"_",model_d))

# Tidy plot of model (sjPlot)
coef_n <- fullglm$coefficients %>% names
coef_n <- as.data.frame(coef_n)

varsarea <- "SD"
varsnow <- coef_n %>% filter(substr(coef_n,1,3) %in% c("sex","eth","age","wor","car","gua","rel"))
plot_model_edit(fullglm,varsnow$coef_n)


varsarea <- "CC"
varsnow <- coef_n %>% filter(substr(coef_n,1,3) %in% c("has","smo","LTC"))
plot_model_edit(fullglm,varsnow$coef_n)


varsarea <- "other"
varsnow <- coef_n %>% filter(substr(coef_n,1,3) %!in% c("has","smo","sex","eth","age","wor","car","gua","rel","LTC"))
plot_model_edit(fullglm,varsnow$coef_n)

