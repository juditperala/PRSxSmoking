
### Script: Defining cases and controls for DEPRESSION and ADHD variables
### Author: Judit Garcia Gonzalez, following JRIColeman scripts for the Mental Health Questionnaire data
### Date: 14/11/2018
### R version used: 3.5.1 (2018-07-02) -- "Feather Spray"

###################################################################################################################
                          ########################  Depression  #############################
###################################################################################################################

##### ---- 1.1. Full cohort - Baseline interview with depression or post-natal depression ---- #####

psy.cond.oral.int = c('1286',      # 1286 depression (N = 31579)
                      '1531')      # 1531 post-natal depression (N = 554)
OralInterview_boolean = apply(my_ukb25907_data[,grep("noncancer_illness_code_selfreported_f20002_0_", colnames(my_ukb25907_data))], 1, function(row) psy.cond.oral.int %in% row)
OralInterview_allpsy = numeric()
# Give 1 to individuals with a diagnosis of ANY psychiatric disorders.
for(indiv in 1:dim(my_ukb25907_data)[1]){
  OralInterview_allpsy[indiv] = ifelse(any(OralInterview_boolean[(((indiv-1)*length(psy.cond.oral.int))+1):(indiv*length(psy.cond.oral.int))]), 
                                       1, 0)
}
table(OralInterview_allpsy)
#       0      1  
#  474068  28472 
my_ukb25907_data$dep_SRbaseline[OralInterview_allpsy==1] = 1
table(my_ukb25907_data$dep_SRbaseline)
# 1
# 28472 


##### ---- 1.2. Full cohort - Antidepressant treatment ---- #####

antidepressants = c("1140879616","1140921600", "1140879540", "1140867878","1140916282","1140909806",
                    "1140867876","1140882236","1141152732","1141180212","1140879634","1140867888",
                    "1140867726","1140879620", "1140867818", "1140879630","1141200564","1141190158",  
                    "1140867756", "1140867884","1140867948", "1140867624","1141151946","1140879628",  
                    "1140867690","1140867640","1140867920", "1140867850","1141152736","1141151978",
                    "1140867934","1140867758","1140867914","1140867820","1141151982","1141201834",
                    "1140882244","1140879556","1140867852","1140867860","1140879544","1141200570",
                    "1140917460","1140867938","1140867856","1140867922","1140910820","1140882312",
                    "1140867944","1140867784","1140867812","1140867668","1140867940")

antidepressant_boolean = apply(my_ukb25907_data[,grep("treatmentmedication_code_f20003_0_", colnames(my_ukb25907_data))], 1, function(row) antidepressants %in% row)
# antidepressant_boolean --> indicates whether 1 individual has in any of the 46 columns of treatment medication a drug that is %in% antidepressant category (TRUE/FALSE)
#                       --> Formed by 53 rows and 502543 columns

Drug_antidepressant = numeric()

for(indiv in 1:dim(my_ukb25907_data)[1]){ 
  Drug_antidepressant[indiv] = ifelse(
    any(antidepressant_boolean[(((indiv-1)*length(antidepressants))+1) : (indiv*length(antidepressants))]),
    1,0)}

# Explaining the loop: If any row in those 53 has a TRUE, it assigns a 1 to the indiv, otherwise it assings a 0

table(Drug_antidepressant)
# 0             1   
# 465755    36785 

my_ukb25907_data$antidepressant[Drug_antidepressant==1] = 1

##### ---- 1.3. Full cohort -  HES Inpatient Diagnoses ---- #####

# F32 and F33  
ICD10_dep = c('F320','F321','F322','F323','F328','F329', # F32 Depressive episode
              'F330','F331','F332','F333','F334','F338','F339') # F33 Recurrent depressive disorder 

HES_main_dep = apply(main_hesin, 1, function(row) ICD10_dep %in% row)
HES_secondary_dep = apply(secondary_hesin, 1, function(row) ICD10_dep %in% row)

HES_main_secondary = numeric()

for(indiv in 1:dim(my_ukb25907_data)[1]){
  HES_main_secondary[indiv] = ifelse(any(HES_main_dep[(((indiv-1)*length(ICD10_dep))+1):(indiv*length(ICD10_dep))] | 
                                           HES_secondary_dep[(((indiv-1)*length(ICD10_dep))+1):(indiv*length(ICD10_dep))]), 
                                     1, 0)
}

my_ukb25907_data$exclude_MDD_HES[c(HES_main_secondary==1)] = 1

table(my_ukb25907_data$exclude_MDD_HES)
# 1
# 4086


##### ---- 1.4. MHQ self reported - Depression ---- #####

my_ukb25907_data$MHQ_SRDepression = with(my_ukb25907_data, ifelse(is.na(ever_sought_or_received_professional_help_for_mental_distress_f20499_0_0), NA, 
                                                                  ifelse((!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_1) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_1 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_2) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_2 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_3) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_3 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_4) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_4 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_5) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_5 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_6) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_6 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_7) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_7 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_8) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_8 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_9) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_9 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_10) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_10 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_11) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_11 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_12) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_12 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_13) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_13 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_14) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_14 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_15) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_15 == "Depression") |
                                                                           (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_16) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_16 == "Depression"), 
                                                                         1, 0)))
table(my_ukb25907_data$MHQ_SRDepression)
#       0      1 
#  123935  33422


##### ---- 1.5. MHQ screen - Depressed ever ---- #####  

# No information for CIDI MDD
my_ukb25907_data$CIDI.MDD.No.Info = with(my_ukb25907_data,
                                         ifelse(
                                           (is.na(ever_had_prolonged_feelings_of_sadness_or_depression_f20446_0_0) | ever_had_prolonged_feelings_of_sadness_or_depression_f20446_0_0 == "Prefer not to answer") &
                                             (is.na(ever_had_prolonged_loss_of_interest_in_normal_activities_f20441_0_0) | ever_had_prolonged_loss_of_interest_in_normal_activities_f20441_0_0 == "Prefer not to answer"), 
                                           1, 0))

# CIDI.MDD.Screen --> People with at least one core symptom of depression
my_ukb25907_data$CIDI.MDD.Screen = with(my_ukb25907_data,
                                        ifelse(((!is.na(ever_had_prolonged_feelings_of_sadness_or_depression_f20446_0_0) & ever_had_prolonged_feelings_of_sadness_or_depression_f20446_0_0 == "Yes") |
                                                  (!is.na(ever_had_prolonged_loss_of_interest_in_normal_activities_f20441_0_0) & ever_had_prolonged_loss_of_interest_in_normal_activities_f20441_0_0 == "Yes")) &
                                                 
                                                 ((!is.na(fraction_of_day_affected_during_worst_episode_of_depression_f20436_0_0) & fraction_of_day_affected_during_worst_episode_of_depression_f20436_0_0 == "Most of the day") |
                                                    (!is.na(fraction_of_day_affected_during_worst_episode_of_depression_f20436_0_0) & fraction_of_day_affected_during_worst_episode_of_depression_f20436_0_0 == "All day long")) &
                                                 
                                                 ((!is.na(frequency_of_depressed_days_during_worst_episode_of_depression_f20439_0_0) & frequency_of_depressed_days_during_worst_episode_of_depression_f20439_0_0 == "Almost every day") |
                                                    (!is.na(frequency_of_depressed_days_during_worst_episode_of_depression_f20439_0_0) & frequency_of_depressed_days_during_worst_episode_of_depression_f20439_0_0 == "Every day")) &
                                                 
                                                 ((!is.na(impact_on_normal_roles_during_worst_period_of_depression_f20440_0_0) & impact_on_normal_roles_during_worst_period_of_depression_f20440_0_0 == "Somewhat") |
                                                    (!is.na(impact_on_normal_roles_during_worst_period_of_depression_f20440_0_0) & impact_on_normal_roles_during_worst_period_of_depression_f20440_0_0 == "A lot")),
                                               1, 0))
table(my_ukb25907_data$CIDI.MDD.Screen)
#      0      1 
# 457784  44756   

# CIDI.MDD.Response --> Obtain people with depressive symptoms
my_ukb25907_data$CIDI.MDD.Response1 = with(my_ukb25907_data, ifelse(!is.na(ever_had_prolonged_feelings_of_sadness_or_depression_f20446_0_0) & ever_had_prolonged_feelings_of_sadness_or_depression_f20446_0_0 == "Yes", 1, 0))
my_ukb25907_data$CIDI.MDD.Response2 = with(my_ukb25907_data, ifelse(!is.na(ever_had_prolonged_loss_of_interest_in_normal_activities_f20441_0_0) & ever_had_prolonged_loss_of_interest_in_normal_activities_f20441_0_0 == "Yes", 1, 0))
my_ukb25907_data$CIDI.MDD.Response3 = with(my_ukb25907_data, ifelse(!is.na(feelings_of_tiredness_during_worst_episode_of_depression_f20449_0_0) & feelings_of_tiredness_during_worst_episode_of_depression_f20449_0_0 == "Yes", 1, 0))
my_ukb25907_data$CIDI.MDD.Response4 = with(my_ukb25907_data, ifelse(!is.na(weight_change_during_worst_episode_of_depression_f20536_0_0) & (weight_change_during_worst_episode_of_depression_f20536_0_0 == "Gained weight" |
                                                                                                                                             weight_change_during_worst_episode_of_depression_f20536_0_0 == "Lost weight" |
                                                                                                                                             weight_change_during_worst_episode_of_depression_f20536_0_0 == "Both gained and lost some weight during the episode"), 1, 0))
my_ukb25907_data$CIDI.MDD.Response5 = with(my_ukb25907_data, ifelse(!is.na(did_your_sleep_change_f20532_0_0) & did_your_sleep_change_f20532_0_0 == "Yes", 1, 0))
my_ukb25907_data$CIDI.MDD.Response6 = with(my_ukb25907_data, ifelse(!is.na(difficulty_concentrating_during_worst_depression_f20435_0_0) & difficulty_concentrating_during_worst_depression_f20435_0_0 == "Yes", 1, 0))
my_ukb25907_data$CIDI.MDD.Response7 = with(my_ukb25907_data, ifelse(!is.na(feelings_of_worthlessness_during_worst_period_of_depression_f20450_0_0) & feelings_of_worthlessness_during_worst_period_of_depression_f20450_0_0 == "Yes", 1, 0))
my_ukb25907_data$CIDI.MDD.Response8 = with(my_ukb25907_data, ifelse(!is.na(thoughts_of_death_during_worst_depression_f20437_0_0) & thoughts_of_death_during_worst_depression_f20437_0_0 == "Yes", 1, 0))

my_ukb25907_data$CIDI.MDD.Response = 0
my_ukb25907_data$CIDI.MDD.Response = rowSums(my_ukb25907_data[,c("CIDI.MDD.Response1","CIDI.MDD.Response2","CIDI.MDD.Response3","CIDI.MDD.Response4","CIDI.MDD.Response5","CIDI.MDD.Response6","CIDI.MDD.Response7","CIDI.MDD.Response8")], na.rm=TRUE)
table(my_ukb25907_data$CIDI.MDD.Response)
#      0      1      2      3      4      5      6      7      8 
# 413502   2711   5789   8865  12523  15422  17359  16097  10275 

#####Depression Diagnoses 

### Depressed ever is equivalent to screening for caseness on the CIDI
my_ukb25907_data$Depressed.Ever = with(my_ukb25907_data, ifelse(CIDI.MDD.No.Info == 1, NA,
                                                                ifelse((CIDI.MDD.Screen == 1) & (CIDI.MDD.Response > 4), 
                                                                       1, 0)))
table(my_ukb25907_data$Depressed.Ever)
#      0      1 
# 119757  37430   


##### ---- 1.6. Create variable ---- #####  

my_ukb25907_data$depression = with(
  my_ukb25907_data,
  ifelse(
    (dep_SRbaseline == 1 & (!is.na(dep_SRbaseline))) | 
      (antidepressant == 1 & (!is.na(antidepressant))) | 
      (exclude_MDD_HES == 1 & (!is.na(exclude_MDD_HES))) | 
      (MHQ_SRDepression == 1 & (!is.na(MHQ_SRDepression))) |
      (Depressed.Ever == 1 & (!is.na(Depressed.Ever))), 
    1, ifelse(
      (!is.na(dep_SRbaseline)) &
        (!is.na(antidepressant)) &
        (!is.na(exclude_MDD_HES)) &
        (!is.na(MHQ_SRDepression)) &
        (!is.na(Depressed.Ever)), NA, 0)))


table(my_ukb25907_data$depression)
# 0           1
# 413564  88976 



###################################################################################################################
                      ########################          ADHD           ##########################
###################################################################################################################

##### ---- 2.1. MHQ self reported  ---- #####  

my_ukb25907_data$ADHD = with(my_ukb25907_data, ifelse(is.na(ever_sought_or_received_professional_help_for_mental_distress_f20499_0_0), NA, 
                                                      ifelse((!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_1) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_1 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_2) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_2 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_3) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_3 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_4) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_4 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_5) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_5 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_6) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_6 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_7) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_7 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_8) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_8 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_9) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_9 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_10) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_10 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_11) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_11 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_12) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_12 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_13) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_13 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_14) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_14 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_15) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_15 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)") |
                                                               (!is.na(mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_16) & mental_health_problems_ever_diagnosed_by_a_professional_f20544_0_16 == "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)"), 
                                                             1, 0)))

table(my_ukb25907_data$ADHD)
#      0      1 
# 157224    133 

##### ---- 2.2. Create variable  ---- #####  

my_ukb25907_data$adhd = with(
  my_ukb25907_data,
  ifelse(
    (ADHD == 1 & (!is.na(ADHD))), 1, ifelse((!is.na(ADHD)), NA, 0)))

table(my_ukb25907_data$adhd)
