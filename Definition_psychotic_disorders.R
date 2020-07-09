
####################################################################################################################
    #########################  INCLUSION CRITERIA -- PEOPLE WITH PSYCHOTIC DISORDERS ##########################
####################################################################################################################

# Author: Judit Garcia Gonzalez

load("__PATH_TO_DATA__/my_ukbXXXXX_data.rda")
load("__PATH_TO_DATA__/my_ukbXXXXX_field.rda")

####################################################################################################################
                    #####                  HES Inpatient Data                     #####
####################################################################################################################

main_hesin = read.table(file="__PATH_TO_DATA__/ukbXXXXX.hesin.tsv", h=T, sep = "\t", fill = T)
secondary_hesin = read.table(file="__PATH_TO_DATA__/ukbXXXXX.hesin_diag10.tsv", h=T, sep = "\t", fill = T)

# --- F20-F29 Schizophrenia, schizotypal and delusional disorders
ICD10 = c('F200','F201','F202','F203','F204','F205','F206','F208','F209', #F20 Schizophrenia
          'F21', # Schizotypal disorder
          'F220','F228','F229', # F22 Persistent delusional disorders
          'F230','F231','F232','F233','F238','F239', # F23 Acute and transient psychotic disorders
          'F24', # F24 Induced delusional disorder
          'F250','F251','F252','F258','F259', # F25 Schizoaffective disorders
          'F28','F29', #Unspecified/other nonorganic psychotic disorders
          
          # --- F30-F39 Mood [affective] disorders
          'F300','F301','F302','F308','F309', # F30 Manic episode
          'F310','F311','F312','F313','F314','F315','F316','F317','F318','F319') # F31 Bipolar affective disorder 

HES_scz_main = apply(main_hesin, 1, function(row) ICD10 %in% row)
HES_scz_secondary = apply(secondary_hesin, 1, function(row) ICD10 %in% row)

HES_main_secondary = numeric()

for(indiv in 1:dim(my_ukbXXXXX_data)[1]){
  HES_main_secondary[indiv] = ifelse(any(HES_scz_main[(((indiv-1)*length(ICD10))+1):(indiv*length(ICD10))] | 
                                           HES_scz_secondary[(((indiv-1)*length(ICD10))+1):(indiv*length(ICD10))]), 
                                     1, 0)
}

my_ukbXXXXX_data$exclude_HES[c(HES_main_secondary==1)] = 1
table(my_ukbXXXXX_data$exclude_HES)
# N = 1894

####################################################################################################################
                  #####  Self-reported diagnosis at the assessment centre interview   #####
####################################################################################################################

##### Excluded - Full cohort available - Self-reported diagnoses in oral interview #####

psy.cond.oral.int = c('1289', # 1289	schizophrenia	(N = 618)
                      '1291') # 1291	mania/bipolar disorder/manic depression	(N = 1509)

OralInterview_boolean = apply(my_ukbXXXXX_data[,grep("noncancer_illness_code_selfreported_f20002_0_", colnames(my_ukbXXXXX_data))], 1, function(row) psy.cond.oral.int %in% row)

OralInterview_allpsy = numeric()

# Give 1 to individuals with a diagnosis of psychotic disorders.
for(indiv in 1:dim(my_ukbXXXXX_data)[1]){
  OralInterview_allpsy[indiv] = ifelse(any(OralInterview_boolean[(((indiv-1)*length(psy.cond.oral.int))+1):(indiv*length(psy.cond.oral.int))]), 
                                       1, 0)
}

table(OralInterview_allpsy)
#       0      1  
#  500545   1995  

my_ukbXXXXX_data$exclude_SRbaseline[OralInterview_allpsy==1] = 1

####################################################################################################################
                  #####  Self-reported diagnosis during Mental Health Questionnaire   #####
####################################################################################################################

my_ukbXXXXX_data$exclude_MHQ_mania = with(my_ukbXXXXX_data, ifelse(is.na(ever_sought_or_received_professional_help_for_mental_distress_f20499_0_0), NA, 
                                                                   ifelse((!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_1) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_1 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_2) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_2 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_3) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_3 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_4) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_4 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_5) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_5 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_6) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_6 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_7) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_7 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_8) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_8 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_9) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_9 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_10) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_10 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_11) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_11 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_12) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_12 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_13) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_13 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_14) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_14 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_15) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_15 == "Mania, hypomania, bipolar or manic-depression") |
                                                                            (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_16) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_16 == "Mania, hypomania, bipolar or manic-depression"), 
                                                                          1, 0)))

table(my_ukbXXXXX_data$exclude_MHQ_mania)
#      0      1 
# 156520    837 


my_ukbXXXXX_data$exclude_MHQ_schizophrenia = with(my_ukbXXXXX_data, ifelse(is.na(ever_sought_or_received_professional_help_for_mental_distress_f20499_0_0), NA, 
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

table(my_ukbXXXXX_data$exclude_MHQ_schizophrenia) 
# 0           1 
# 157200    157 

my_ukbXXXXX_data$exclude_MHQ_PsychosisOther = with(my_ukbXXXXX_data, ifelse(is.na(ever_sought_or_received_professional_help_for_mental_distress_f20499_0_0), NA, 
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

table(my_ukbXXXXX_data$exclude_MHQ_PsychosisOther)
# 0           1
# 156753    604 


####################################################################################################################
                  #####                       Death records                   #####
####################################################################################################################

# Main cause of death 
my_ukbXXXXX_data$exclude_main_death = with(my_ukbXXXXX_data, ifelse(!is.na(underlying_primary_cause_of_death_icd10_f40001_0_0) & (underlying_primary_cause_of_death_icd10_f40001_0_0 %in% ICD10), 1, 0))
table(my_ukbXXXXX_data$exclude_main_death)
# N = 2 people

# Secondary cause of death (There are 13 columns for first assessment)
death_boolean = apply(my_ukbXXXXX_data[,grep("contributory_secondary_causes_of_death_icd10_f40002_0_", colnames(my_ukbXXXXX_data))], 1, function(row) ICD10 %in% row)
boolean_psy_death = numeric()

# Give 1 to individuals with a diagnosis of ANY psychiatric disorders.
for(indiv in 1:dim(my_ukbXXXXX_data)[1]){
  boolean_psy_death[indiv] = ifelse(any(death_boolean[(((indiv-1)*length(ICD10))+1):(indiv*length(ICD10))]), 
                                    1, 0)
}

table(boolean_psy_death)
# N = 13 people

my_ukbXXXXX_data$exclude_secondary_death[boolean_psy_death==1] = 1

####################################################################################################################
               #####                  Pleople under antipsychotics medication                 #####
####################################################################################################################


antipsychotics = c("1140868170","1140928916","1141152848","1140867444","1140879658","1140868120","1141153490",
                   "1140867304","1141152860","1140867168","1141195974","1140867244","1140867152","1140909800",
                   "1140867420","1140879746","1141177762","1140867456","1140867952","1140867150","1141167976",
                   "1140882100","1140867342","1140863416","1141202024","1140882098","1140867184","1140867092",
                   "1140882320","1140910358","1140867208","1140909802","1140867134","1140867306","1140867210",
                   "1140867398","1140867078","1140867218","1141201792","1141200458","1140867136","1140879750",
                   "1140867180","1140867546","1140928260","1140927956")

antipsychotics_boolean = apply(my_ukbXXXXX_data[,grep("treatmentmedication_code_f20003_0_", colnames(my_ukbXXXXX_data))], 1, function(row) antipsychotics %in% row)
# antipsychotics_boolean --> indicates whether 1 individual has in any of the columns of treatment medication a drug that is %in% antipsychotics category (TRUE/FALSE)

Drug_antipsychotics = numeric()

for(indiv in 1:dim(my_ukbXXXXX_data)[1]){ 
  Drug_antipsychotics[indiv] = ifelse(
    any(antipsychotics_boolean[(((indiv-1)*length(antipsychotics))+1) : (indiv*length(antipsychotics))]),
    1,0)}

# Explaining the loop: If any row in antipsychotics_boolean col has a TRUE, it assigns a 1 to the indiv, otherwise it assings a 0

table(Drug_antipsychotics)
# 0             1   
# 499584      2956

my_ukbXXXXX_data$antipsychotics[Drug_antipsychotics==1] = 1


####################################################################################################################
            #####                  Put them together                 #####
####################################################################################################################

my_ukbXXXXX_data$with_psychotic_disorder = with(my_ukbXXXXX_data, ifelse((!is.na(exclude_HES) & exclude_HES == 1) |  # 1894 individiduals
                                                                        (!is.na(exclude_SRbaseline) & exclude_SRbaseline == 1) |  # 1995 individuals
                                                                        (!is.na(exclude_MHQ_mania) & exclude_MHQ_mania == 1) | # 837
                                                                        (!is.na(exclude_MHQ_schizophrenia) & exclude_MHQ_schizophrenia == 1) | # 197 
                                                                        (!is.na(exclude_MHQ_PsychosisOther) & exclude_MHQ_PsychosisOther == 1) | # 604
                                                                        (!is.na(exclude_main_death) & exclude_main_death == 1) | # 2 individuals
                                                                        (!is.na(antipsychotics) & antipsychotics == 1) | # 2956 individuals
                                                                        (!is.na(exclude_secondary_death) & exclude_secondary_death == 1), # 13 individuals
                                                                        1, 0)) 

table(my_ukbXXXXX_data$with_psychotic_disorder) 
# 0           1
# 495813   6727

# Export dataset

