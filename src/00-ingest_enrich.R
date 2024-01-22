# R.version()

######################
## Loading libraries##
######################

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
'%!in%' <- function(x,y)!('%in%'(x,y))
library(stringr)



###############################
## Loading file, initial prep
#####

# establish name of temporary file
temp = "data/GPPS Y15 Person Dataset for NHSX_CONFIDENTIAL_1.sav"

# Load SPSS file (haven)
# https://community.rstudio.com/t/how-do-i-convert-a-sav-file-to-a-csv-file-in-r/73366
data_gpps <- read_sav(temp)
class(data_gpps)
#data_gpps_q141_1 <- read_sav(temp, col_select="Q114_1") # load given columns only
#data_gpps_df <- data.frame(data_gpps) # converted to dataframe

# Save as csv - loss of mapping / keys
#write_csv(x=data_gpps,path="data/GPPS Y15 Person Dataset for NHSX_CONFIDENTIAL.csv")


#Alternative way to load SPSS Files #1 - view haven labels info
#https://www.pipinghotdata.com/posts/2020-12-23-leveraging-labelled-data-in-r/
#data <- haven::read_sav(temp) # 83 columns, brings in a 'CCG_N0' and a 'STP_N0'
#class(data) # "data.frame"

# create data dictionary - a dataframe with details of each variable the attached haven and value labels of the data
#dictionary <- labelled::generate_dictionary(data)
#view(dictionary)




# Alternative way to load SPSS files #2
data <- foreign::read.spss(temp, to.data.frame=TRUE) # 83 columns, brings in a 'CCG_N0' and a 'STP_N0'
class(data) # "data.frame"
#data_spss <- foreign::read.spss(temp) # 83 columns, brings in a 'CCG_N0' and a 'STP_N0'
#class(data_spss) # list

# Explore data_gpps format. Example methods
data_gpps %>% .[,1:20] %>% glimpse()


# Fetch attributes from haven objects
data_gpps$Q114_1 %>% class()
data_gpps$Q114_1 %>% attr('label') #singular gives the question wording
data_gpps$Q114_1 %>% attr('labels') #plural gives options and coding of options
data_gpps$Q114_1 %>% table



# https://rstudio-pubs-static.s3.amazonaws.com/466787_84982191b86e4591bfcee776e498a6d4.html
# use the all might `map` function in the purrr packages
variables_with_labels = map(data_gpps, function(x) attr(x, "class") == "haven_labelled") %>% 
  #unlist() %>% 
  names()

data_gpps_factored = data_gpps %>%
  mutate_at( vars(variables_with_labels), as_factor)
# class still as [1] "tbl_df"     "tbl"        "data.frame"


# View label correspondence
data_gpps %>% sjPlot::view_df() #viewing label correspondence for SAV file
data %>% sjPlot::view_df() #viewing label correspondence for read spss file
data_gpps_factored %>% sjPlot::view_df() #same as first one but labels are factors for plotting



####################
# Initial summaries with factored
####################s
#count the number of na's in each variable and column
sapply(data_gpps_factored, function(x) sum(x=="" | is.na(x)))
apply(data_gpps_factored, 1, function(x) sum(x=="" | is.na(x)))


#kable summary table of all variables
#kab <- summary(data_gpps_factored) %>% kbl() %>% kable_styling()
#kab

data_bu <- data

data <- data %>% mutate(id=row_number())


# Fix lagging spaces in some string vars
data$La_id_ONS <- str_replace_all(data$La_id_ONS, fixed(" "), "")
data$Practice_Code <- str_replace_all(data$Practice_Code, fixed(" "), "")
# 188 NAs in imd1


data_enrich <- data


#######################################
# Load IUC 2018 data - and join
######################################

data_IUC <- read.csv("data/datasets-auxiliary/iuc2018.csv")


data_enrich <- data_enrich %>% left_join(data_IUC %>% select(LSOA11_CD,GRP_CD,GRP_LABEL),by=c("LSOA"="LSOA11_CD"))



#######################################
# Load internet speed data, coverage data and OA to LSOA data
######################################

data_coverage <-  read.csv("data/datasets-auxiliary/201809_fixed_oa11_coverage_r01.csv")

data_performance <-  read.csv("data/datasets-auxiliary/201805_fixed_oa11_performance_r02.csv")

data_internet <- data_performance %>% left_join(data_coverage,by="oa11")

data_oa_lsoa <- read.xlsx("data/datasets-auxiliary/OA11_LAD21_LSOA11_MSOA11_LEP21_EN_v3.xlsx",sheet="OA11_LAD21_LSOA_MSOA_LEP21_ENv3")
data_oa_lsoa <- data_oa_lsoa %>% select(OA11CD,LSOA11CD)

data_internet <- data_internet %>% left_join(data_oa_lsoa,by=c("oa11"="OA11CD"))

data_internet_lsoa_metrics <- data_internet %>%
  group_by(LSOA11CD) %>%
  summarise(
    int_avg_download = sum(`Average.download.speed..Mbit.s.`*`All.Premises`,na.rm=T)/sum(`All.Premises`*!is.na(`Average.download.speed..Mbit.s.`*`All.Premises`),na.rm=T),
    int_max_download = max(`Average.download.speed..Mbit.s.`,na.rm=T),
    int_avg_datausage = sum(`Average.data.usage..GB.`*`All.Premises`,na.rm=T)/sum(`All.Premises`*!is.na(`Average.data.usage..GB.`*`All.Premises`),na.rm=T),
    int_unable_less10mb = sum(`X..of.premises.unable.to.receive.10Mbit.s`*`All.Premises`,na.rm=T)/sum(`All.Premises`*!is.na(`X..of.premises.unable.to.receive.10Mbit.s`*`All.Premises`),na.rm=T),
    int_perc_NGA = sum(`X..of.premises.with.NGA`*`All.Premises`,na.rm=T)/sum(`All.Premises`*!is.na(`X..of.premises.with.NGA`*`All.Premises`),na.rm=T)
  )

data_enrich <- data_enrich %>% left_join(data_internet_lsoa_metrics, by=c("LSOA"="LSOA11CD"))





#######################################
# Load POMI data
######################################

data_pomi <- read.csv("data/datasets-auxiliary/POMI_APR2020_to_MAR2021_v2.csv")

data_pomi_sep <- data_pomi %>% filter(report_period_end=="30SEP2020") %>% pivot_wider(names_from=field,values_from=value)

data_pomi_metrics <- data_pomi_sep %>% mutate(perc_patenabled=Total_Pat_Enbld/patient_list_size*100,
                                              perc_patenabledpresc=Pat_Presc_Enbld/patient_list_size*100,
                                              perc_patenabledappts=Pat_Appts_Enbld/patient_list_size*100,
                                              perc_patenabledDCR=Pat_DetCodeRec_Enbld/patient_list_size*100)


data_pomi_metrics <- data_pomi_metrics %>% select(practice_code,region_name,perc_patenabled,perc_patenabledpresc,perc_patenabledappts,perc_patenabledDCR,system_supplier,Sys_Appts_Enbld,Sys_DetCodeRec_Enbld,Sys_Presc_Enbld)

colnames(data_pomi_metrics) = paste0("POMI_",colnames(data_pomi_metrics))

data_enrich <- data_enrich %>% left_join(data_pomi_metrics, by=c("Practice_Code"="POMI_practice_code"))



#################################################################
# FOR 'ANALYSIS FILE' - ONLY SIMPLIFYING PATIENT IDENTIFIERS LEAVING INPUTS AS FACTORS
#################################################################

context_vars = c("GRP_CD",
                 "GRP_LABEL",
                 "int_avg_download",
                 "int_max_download",
                 "int_avg_datausage",
                 "int_unable_less10mb",
                 "int_perc_NGA",
                 "POMI_region_name",
                 "POMI_perc_patenabled",
                 "POMI_perc_patenabledpresc",
                 "POMI_perc_patenabledappts",
                 "POMI_perc_patenabledDCR",
                 "POMI_system_supplier",
                 "POMI_Sys_Appts_Enbld",
                 "POMI_Sys_DetCodeRec_Enbld",
                 "POMI_Sys_Presc_Enbld",
                 "CCG_Name",
                 "CCG_Code")


data_cut_factors <- data_enrich %>% select(
                                   c("id","Q114_1","Q114_2","Q114_3","Q114_4","Q114_5","Q91_1","Q91_2","Q91_3",starts_with("Q31_"), "Q112", "Q113", "Q49", "Q48", "Q50", "Q56", "Q53", "Q54", "Q55", "Q57", "Q58", "Q30_recoded","Q110_1","patient_imd_decile"),
                                   c("Q75","Q83","rurality_new","STP_Code_ONS","Practice_Code","practice_pop_size"),
                                   context_vars)



#drop the rows which are 16-17 or under 16 as we do not want to include them in our data
data_cut_factors <- data_cut_factors[!(data_cut_factors$Q48 == "Under 16" |data_cut_factors$Q48 == "16 to 17"), ]

# make outcome variable affirmative
data_cut_factors <- data_cut_factors %>% mutate(used_online_service = 1- as.numeric(as.character(Q114_5)))

#simplify Q49 ethnic goup 
data_cut_factors <- data_cut_factors %>% mutate(Q49 = case_when(Q49 == "English, Welsh, Scottish, Northern Irish or British" ~ "White",
                                                                Q49 == "Irish" ~ "White",
                                                                Q49 == "Gypsy or Irish Traveller" ~ "White",
                                                                Q49 == "Any other White background" ~ "White",
                                                                Q49 == "White and Black Caribbean" ~ "Mixed or Multiple ethnic background",
                                                                Q49 == "White and Black African" ~ "Mixed or Multiple ethnic background",
                                                                Q49 == "White and Asian" ~ "Mixed or Multiple ethnic background",
                                                                Q49 == "Any other Mixed or Multiple ethnic background" ~ "Mixed or Multiple ethnic background",
                                                                Q49 == "Indian" ~ "Asian", 
                                                                Q49 == "Pakistani" ~ "Asian",
                                                                Q49 == "Bangladeshi" ~ "Asian",
                                                                Q49 == "Chinese" ~ "Asian",
                                                                Q49 == "Any other Asian background" ~ "Asian",
                                                                Q49 == "African" ~ "Black",
                                                                Q49 == "Caribbean"  ~ "Black",
                                                                Q49 == "Any other Black, Black British, Caribbean or African background" ~ "Black"))
#simplify Q56 Carers question
data_cut_factors <- data_cut_factors %>% mutate(Q56 = case_when(Q56 == "No" ~ "No",
                                                                Q56 == "Yes, 1 to 9 hours a week" ~ "Yes",
                                                                Q56 == "Yes, 10 to 19 hours a week" ~ "Yes",
                                                                Q56 == "Yes, 20 to 34 hours a week" ~ "Yes",
                                                                Q56 == "Yes, 35 to 49 hours a week" ~ "Yes",
                                                                Q56 == "Yes, 50 or more hours a week" ~ "Yes"))

#simplify Q50 work - length of response levels question
data_cut_factors <- data_cut_factors %>% mutate(Q50 = case_when(Q50 == "In full-time paid work (30 hours or more each week)" ~ "Full-time",
                                                                Q50 == "In part-time paid work (under 30 hours each week)" ~ "Part-time",
                                                                Q50 == "In full-time education at school, college or university" ~ "Education",
                                                                Q50 == "Unemployed" ~ "Unemployed",
                                                                Q50 == "Permanently sick or disabled" ~ "Permanently sick or disabled",
                                                                Q50 == "Fully retired from work" ~ "Retired",
                                                                Q50 == "Looking after the family or home" ~ "Looking after the family or home",
                                                                Q50 == "Doing something else" ~ "Other"))


#simplify Q113 gender identity question (very low numbers so put No and prefer not to say together)
data_cut_factors <- data_cut_factors %>% mutate(Q113 = case_when(Q113 == "Yes" ~ "Yes",
                                                                 Q113 == "No" ~ "No or undisclosed",
                                                                 Q113 == "Prefer not to say" ~ "No or undisclosed"))



# simplify suppliers
data_cut_factors <- data_cut_factors %>% mutate(POMI_system_supplier = case_when(POMI_system_supplier == "EMIS" ~ "EMIS",
                                                                                 POMI_system_supplier == "TPP" ~ "TPP",
                                                                                 POMI_system_supplier %in% c("EMIS (I)","VISION (I)","VISION","MICROTEST") ~ "Other"))


# simply specific LTCs


data_cut_factors %>% group_by(Q31_1,Q30_recoded) %>% summarise(n()) %>% View()

# simplify names for interpretability
data_cut_factors <- data_cut_factors %>%
  rename(carer="Q56",
         gender_identity_same = "Q113",
         sex="Q112",
         ethnicity="Q49",
         age="Q48",
         work="Q50",
         guardian="Q53",
         deaf="Q54",
         smoker="Q55",
         sexual_orientation="Q57",
         religion="Q58",
         has_LTC = "Q30_recoded",
         has_shielded = "Q110_1",
         has_mobilityissues = "Q91_1",
         has_falls = "Q91_2",
         has_isolation  ="Q91_3",
         LTC_alzheimers = "Q31_1",
         LTC_arthritis = "Q31_3",
         LTC_autism = "Q31_21",
         LTC_blindness = "Q31_5",
         LTC_breathing = "Q31_4",
         LTC_cancer = "Q31_6",
         LTC_deafness = "Q31_7",
         LTC_diabetes = "Q31_8",
         LTC_heart = "Q31_2",
         LTC_hypertension = "Q31_10",
         LTC_kidneyliver = "Q31_11",
         LTC_learningdisability = "Q31_19",
         LTC_mentalhealth = "Q31_14",
         LTC_neurological = "Q31_15",
         LTC_stroke = "Q31_20",
         LTC_anyother = "Q31_16",
         GPOL_appts = "Q114_1",
         GPOL_repeat = "Q114_2",
         GPOL_records = "Q114_3",
         GPOL_OC = "Q114_4"
           
  )

aa <- data_cut_factors

data_cut_factors <- aa


# Renaming for religion (simplify christian level name)
data_cut_factors <- data_cut_factors %>%
  mutate(religion = ifelse(religion == "Christian (including Church of England, Catholic, Protestant, and other Christian denominations)",
                           "Christian",
                           as.character(religion)))

data_cut_factors$religion <- factor(as.character(data_cut_factors$religion))


# Pad more on LTCs to reduce NAs. i.e. if Q30_recorded answer is "No" and the LTC_xx" is NA, give it a 0
data_cut_factors <- data_cut_factors %>% mutate(LTC_pad=as.factor(ifelse(has_LTC=="No","0",NA)))
data_cut_factors <- data_cut_factors %>% mutate(across(matches("LTC_"),~factor(ifelse(is.na(.),LTC_pad,.)-1,levels=c(0,1))))
data_cut_factors <- data_cut_factors %>% select(-LTC_pad)
#data_cut_factors %>% group_by(LTC_arthritis,has_LTC) %>% summarise(n()) %>% View("post") # View effect / sense check
#aa %>% group_by(LTC_arthritis,has_LTC) %>% summarise(n()) %>% View("post") # View effect

#check for nas again after simplifying
sapply(data_cut_factors, function(x) sum(x=="" | is.na(x)))
sapply(data_cut_factors, function(x) class(x))

aa <- data_cut_factors

factor_vars = c("age","work")

###########################
##SAVING DATA CUT FILES##
###########################
#saving data_cut
#write.csv(data_cut, 'data\\data_cuts_log\\data_identifiers_online_1.csv')
#saving data_cut_factors
write.csv(data_cut_factors, 'data\\data_cuts_log\\data_enriched_clustering_Cut.csv')
data_cut_factors <- data_cut_factors %>% mutate_at(vars(has_mobilityissues:patient_imd_decile,GRP_LABEL,GRP_CD,used_online_service),factor)
save(data_cut_factors,file='data/data_cuts_log/data_enriched_ccg_clustering.Rdata')












