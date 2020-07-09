
### Script: Defining cases and controls for psychotic experiences and tobacco smoking variables
### Author: Judit Garcia Gonzalez, following JRIColeman scripts for the Mental Health Questionnaire data
### Date: 14/11/2018
### R version used: 3.5.1 (2018-07-02) -- "Feather Spray"

# install.packages("ukbtools")
# install.packages("gmodels")

library(ukbtools)
library(lubridate) # For time length calculation

# my_ukbXXXXX_data = ukb_df("ukbXXXXX", path="__PATH_TO_DATA__")
# my_ukbXXXXX_field = ukb_df_field("ukbXXXXX", path="__PATH_TO_DATA__")

# save(my_ukbXXXXX_data, file="my_ukbXXXXX_data.rda")
# save(my_ukbXXXXX_field, file="my_ukbXXXXX_field.rda")

load("__PATH_TO_DATA__/my_ukbXXXXX_data.rda")
load("__PATH_TO_DATA__/my_ukbXXXXX_field.rda")

###################################################################################################################
########################  CREATE SMALL MENTAL HEALTH DATASET WITH FIELDS OF INTEREST  #############################
###################################################################################################################

MH_dataset = my_ukbXXXXX_data[,c("eid",	
                                 "age_when_attended_assessment_centre_f21003_0_0",
                                 "ethnic_background_f21000_0_0",
                                 "sex_f31_0_0",
                                 "smoking_status_f20116_0_0", 									 
                                 "pack_years_of_smoking_f20161_0_0",   
                                 "maternal_smoking_around_birth_f1787_0_0")]

### Calculate age at MHQ as age at baseline + (diff in days between baseline and MHQ) Example taked from https://rdrr.io/cran/lubridate/man/time_length.html 
MH_dataset$Age.At.MHQ = as.numeric(with(my_ukbXXXXX_data, ifelse(is.na(date_of_completing_mental_health_questionnaire_f20400_0_0), 
                                                                 NA,
                                                                 as.numeric(age_when_attended_assessment_centre_f21003_0_0) + 
                                                                   (trunc(time_length(interval(ymd(date_of_attending_assessment_centre_f53_0_0), ymd(date_of_completing_mental_health_questionnaire_f20400_0_0)), "year"))))))
summary(MH_dataset$Age.At.MHQ)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 45.7    57.5    64.5    63.5    69.6    80.7  345152 


###################################################################################################################
########################                           SMOKING VARIABLES                  #############################
###################################################################################################################

############################ Quick check of fields

MH_dataset$pack_years_of_smoking_f20161_0_0 = as.numeric(as.character(MH_dataset$pack_years_of_smoking_f20161_0_0))

summary(MH_dataset$pack_years_of_smoking_f20161_0_0)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    10.0    19.0    23.4    32.0   336.0  351590

summary(MH_dataset$smoking_status_f20116_0_0)
#  Prefer not to answer      Never      Previous       Current        NA's 
#  2059                     273542        173072        52979         891 

summary(MH_dataset$maternal_smoking_around_birth_f1787_0_0)
# Prefer not to answer          Do not know                   No             Yes                 NA's
# 250                                 61136               306266          126632                 8259

############################ Transform data to replace Prefer not to answer / Do not know by NAs

MH_dataset$smoking_status_f20116_0_0 = with(MH_dataset, ifelse(is.na(smoking_status_f20116_0_0) | 
                                                                 smoking_status_f20116_0_0 == "Prefer not to answer", 
                                                               NA, smoking_status_f20116_0_0))                      

table(MH_dataset$smoking_status_f20116_0_0)
# 2      3      4 
# 273540 173071  52979  

MH_dataset$maternal_smoking_around_birth_f1787_0_0 = with(MH_dataset, ifelse(is.na(maternal_smoking_around_birth_f1787_0_0) | 
                                                                               maternal_smoking_around_birth_f1787_0_0 == "Prefer not to answer" | 
                                                                               maternal_smoking_around_birth_f1787_0_0 == "Do not know", 
                                                                             NA, maternal_smoking_around_birth_f1787_0_0))    
table(MH_dataset$maternal_smoking_around_birth_f1787_0_0)
#      3      4 
# 306265 126632 

###################################################################################################################
########################                           PSYCHOSIS VARIABLES                 ############################
###################################################################################################################

##### 1. MENTAL HEALTH QUESTIONNAIRE - SELF REPORTED PHYSICIAN DIAGNOSED DISORDERS  #####

# Data coding: http://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=1401

MH_dataset$SRSchizophrenia = with(my_ukbXXXXX_data, ifelse(is.na(ever_sought_or_received_professional_help_for_mental_distress_f20499_0_0), NA, 
                                                           ifelse((!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_1) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_1 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_2) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_2 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_3) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_3 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_4) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_4 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_5) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_5 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_6) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_6 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_7) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_7 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_8) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_8 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_9) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_9 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_10) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_10 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_11) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_11 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_12) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_12 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_13) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_13 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_14) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_14 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_15) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_15 == "Schizophrenia") |
                                                                    (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_16) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_16 == "Schizophrenia"), 
                                                                  1, 0)))

table(MH_dataset$SRSchizophrenia) 
# 0           1 
# 157200    157 

MH_dataset$SRPsychosisOther = with(my_ukbXXXXX_data, ifelse(is.na(ever_sought_or_received_professional_help_for_mental_distress_f20499_0_0), NA, 
                                                            ifelse((!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_1) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_1 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_2) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_2 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_3) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_3 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_4) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_4 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_5) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_5 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_6) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_6 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_7) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_7 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_8) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_8 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_9) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_9 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_10) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_10 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_11) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_11 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_12) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_12 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_13) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_13 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_14) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_14 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_15) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_15 == "Any other type of psychosis or psychotic illness") |
                                                                     (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_16) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_16 == "Any other type of psychosis or psychotic illness"), 
                                                                   1, 0)))

table(MH_dataset$SRPsychosisOther)
# 0           1
# 156753    604 

MH_dataset$SRPsychosisAny = with(MH_dataset, ifelse(is.na(my_ukbXXXXX_data$ever_sought_or_received_professional_help_for_mental_distress_f20499_0_0), NA,
                                                    ifelse((!is.na(SRSchizophrenia) & SRSchizophrenia == 1) | (!is.na(SRPsychosisOther) & SRPsychosisOther == 1), 1, 0)))

table(MH_dataset$SRPsychosisAny)
# 0         1
# 156634    723 

##### 2. MENTAL HEALTH QUESTIONNAIRE - SELF REPORTED UNUSUAL EXPERIENCIES  #####

###Unusual.Experience Ever Hallucinations
MH_dataset$Unusual.Experience.Ever.Hallucinations = with(my_ukbXXXXX_data, ifelse((is.na(ever_seen_an_unreal_vision_f20471_0_0) | ever_seen_an_unreal_vision_f20471_0_0 == "Prefer not to answer" | ever_seen_an_unreal_vision_f20471_0_0 == "Do not know" ) &
                                                                                    (is.na(ever_heard_an_unreal_voice_f20463_0_0) | ever_heard_an_unreal_voice_f20463_0_0 == "Prefer not to answer" | ever_heard_an_unreal_voice_f20463_0_0 == "Do not know"), 
                                                                                  NA,
                                                                                  ifelse((!is.na(ever_seen_an_unreal_vision_f20471_0_0) & ever_seen_an_unreal_vision_f20471_0_0 == "Yes") |
                                                                                           (!is.na(ever_heard_an_unreal_voice_f20463_0_0) & ever_heard_an_unreal_voice_f20463_0_0 == "Yes"), 1, 0)))
table(MH_dataset$Unusual.Experience.Ever.Hallucinations)
#       0      1
#  150336   6689 

###Unusual.Experience.Ever.Delusions
MH_dataset$Unusual.Experience.Ever.Delusions = with(my_ukbXXXXX_data, ifelse((is.na(ever_believed_in_unreal_communications_or_signs_f20474_0_0) | ever_believed_in_unreal_communications_or_signs_f20474_0_0 == "Prefer not to answer" | ever_believed_in_unreal_communications_or_signs_f20474_0_0 == "Do not know") &
                                                                               (is.na(ever_believed_in_an_unreal_conspiracy_against_self_f20468_0_0) | ever_believed_in_an_unreal_conspiracy_against_self_f20468_0_0  == "Prefer not to answer" | ever_believed_in_an_unreal_conspiracy_against_self_f20468_0_0  == "Do not know"), 
                                                                             NA,
                                                                             ifelse((!is.na(ever_believed_in_unreal_communications_or_signs_f20474_0_0) & ever_believed_in_unreal_communications_or_signs_f20474_0_0 == "Yes") |
                                                                                      (!is.na(ever_believed_in_an_unreal_conspiracy_against_self_f20468_0_0) & ever_believed_in_an_unreal_conspiracy_against_self_f20468_0_0 == "Yes"), 1, 0)))
table(MH_dataset$Unusual.Experience.Ever.Delusions)
#      0      1
# 155182    2067 

###Unusual.Experience.Ever
MH_dataset$Unusual.Experience.Ever = with(MH_dataset, ifelse((!is.na(Unusual.Experience.Ever.Hallucinations) & Unusual.Experience.Ever.Hallucinations == 1) | 
                                                               (!is.na(Unusual.Experience.Ever.Delusions) & Unusual.Experience.Ever.Delusions == 1), 
                                                             1, 
                                                             ifelse(!is.na(SRPsychosisAny) & SRPsychosisAny == 0, 0, NA)))

table(MH_dataset$Unusual.Experience.Ever)
#      0      1
# 149289   7803 

