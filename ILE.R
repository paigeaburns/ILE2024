#ILE

#data prep
setwd("/Users/Paige/Desktop/ILE")
ile <- read.csv("ukr_adults_ILE(1).csv", header = TRUE)

#count unique cases
library(dplyr)
  n_distinct(ile$case_id)

  #there are 111631 unique case IDs in this dataset, out of 150512 total observations
  
#if I subset out just my population (prisoners)
  
ile2 <- subset(ile, ile$Prisoner =="Yes")
  
  #there are 7378 observations in which the individual was a prisoner

#now looking at unique cases amongst prisoners
n_distinct(ile2$case_id)

  #now there are 5741 unique cases among prisoners

#Deliverable - Tables
#First, remove participants I will not be looking at 
#subset data to include only unique case IDs
ile_df <- ile %>% distinct(case_id, .keep_all = TRUE)

#exclude the people with treatment in process, exclude those that transferred (we don't know what happened to them)
table(ile_df$final_outcome_group)

ile_df <- ile_df[ile_df$final_outcome_group !="Treatment in process" & ile_df$final_outcome_group !="Transfer", ]

table(ile_df$final_outcome_group)

#My final dataset has 90,240 individuals

#Table 1 - Characteristics of Individuals diagnosed with TB 
#rename prisoner outcomes to make life easier
ile_df$Prisoner <- ifelse(ile_df$Prisoner == "Yes", "Prisoner", "General Public")

#check work - get N of both group
table(ile_df$Prisoner)
#factor prisoners to ensure GP will be ref group later
ile_df$Prisoner <- factor(ile_df$Prisoner, levels = c("General Public", "Prisoner"))

#characteristics - factor so reference group is level 1
#get column % 
table(ile_df$Sex)
ile_df$Sex <- factor(ile_df$Sex, levels = c("Male", "Female"))
  sex_table <- table(ile_df$Sex, ile_df$Prisoner)
    sex_table
      round(prop.table(sex_table, 2), 3)
        chisq.test(sex_table, correct = FALSE)
      
table(ile_df$hiv_def)
ile_df$hiv_def <- factor(ile_df$hiv_def, levels = c("Negative", "Positive", "Unknown"))
  hiv_table<- table(ile_df$hiv_def, ile_df$Prisoner)
      hiv_table
        round(prop.table(hiv_table, 2), 3)
          chisq.test(hiv_table, correct = FALSE)
        
table(ile_df$Localization)
ile_df$Localization <- factor(ile_df$Localization, levels = c("Pulmonary", "Extra-pulmonary", "Both"))
  loc_table <- table(ile_df$Localization, ile_df$Prisoner)
    loc_table
      round(prop.table(loc_table, 2), 3)
        chisq.test(loc_table, correct = FALSE)
      
table(ile_df$Cavitation)
ile_df$Cavitation <- factor(ile_df$Cavitation, levels = c("No", "Yes"))
  cav_table <- table(ile_df$Cavitation, ile_df$Prisoner)
    cav_table
      round(prop.table(cav_table, 2), 3)
        chisq.test(cav_table, correct = FALSE)
      
table(ile_df$new_prev)
ile_df$new_prev <- factor(ile_df$new_prev, levels = c("New", "Previously treated"))
  hist_table <- table(ile_df$new_prev, ile_df$Prisoner)
    hist_table
      round(prop.table(hist_table, 2), 3)
        chisq.test(hist_table, correct = FALSE)
      
table(ile_df$Alcohol.abuse)
ile_df$Alcohol.abuse <- ifelse(ile_df$Alcohol.abuse == "Yes", "Yes", "No")
table(ile_df$Alcohol.abuse)
ile_df$Alcohol.abuse <- factor(ile_df$Alcohol.abuse, levels = c("No", "Yes"))
  alc_table <- table(ile_df$Alcohol.abuse, ile_df$Prisoner)
    alc_table
      round(prop.table(alc_table, 2), 3)
         chisq.test(alc_table, correct = FALSE)
      
table(ile_df$Injecting.drug.user)
ile_df$Injecting.drug.user <- ifelse(ile_df$Injecting.drug.user == "Yes", "Yes", "No")
table(ile_df$Injecting.drug.user)
ile_df$Injecting.drug.user <- factor(ile_df$Injecting.drug.user, levels = c("No", "Yes"))
  drug_table <- table(ile_df$Injecting.drug.user, ile_df$Prisoner)
    drug_table
      round(prop.table(drug_table, 2), 3)
        chisq.test(drug_table, correct = FALSE)
      
table(ile_df$Homeless)
ile_df$Homeless <- ifelse(ile_df$Homeless == "Yes", "Unhoused", "Housed")
table(ile_df$Homeless)
ile_df$Homeless <- factor(ile_df$Homeless, levels = c("Housed", "Unhoused"))
  house_table <- table(ile_df$Homeless, ile_df$Prisoner)
    house_table
      round(prop.table(house_table, 2), 3)
        chisq.test(house_table, correct = FALSE)

table(ile_df$DST.result.R)
sum(is.na(ile_df$DST.result.R))
#I want the NA to be no test - not missing
ile_df$DST.result.R[is.na(ile_df$DST.result.R)] <- "No test"
ile_df$DST.result.R <- factor(ile_df$DST.result.R, levels = c("Sensitive", "Resistant", "Contaminated", "No test"))
 rif_table <- table(ile_df$DST.result.R, ile_df$Prisoner)
  rif_table
    round(prop.table(rif_table, 2),3)
      chisq.test(rif_table, correct = FALSE)

#Table 2: Characteristics of Individuals Diagnosed with RRTB, repeat of table 1
rr <- ile_df[ile_df$DST.result.R == "Resistant", ]
table(rr$Prisoner)

table(rr$Sex)
rr$Sex <- factor(rr$Sex, levels = c("Male", "Female"))
  sex_table <- table(rr$Sex, rr$Prisoner)
    sex_table
      round(prop.table(sex_table, 2), 3)
        chisq.test(sex_table, correct = FALSE)
      
table(rr$hiv_def)
rr$hiv_def <- factor(rr$hiv_def, levels = c("Negative", "Positive", "Unknown"))
  hiv_table<- table(rr$hiv_def, rr$Prisoner)
      hiv_table
        round(prop.table(hiv_table, 2), 3)
          chisq.test(hiv_table, correct = FALSE)
        
table(rr$Localization)
rr$Localization <- factor(rr$Localization, levels = c("Pulmonary", "Extra-pulmonary", "Both"))
  loc_table <- table(rr$Localization, rr$Prisoner)
    loc_table
      round(prop.table(loc_table, 2), 3)
        chisq.test(loc_table, correct = FALSE)
      
table(rr$Cavitation)
rr$Cavitation <- factor(rr$Cavitation, levels = c("No", "Yes"))
  cav_table <- table(rr$Cavitation, rr$Prisoner)
    cav_table
      round(prop.table(cav_table, 2), 3)
        chisq.test(cav_table, correct = FALSE)
      
table(rr$new_prev)
rr$new_prev <- factor(rr$new_prev, levels = c("New", "Previously treated"))
  hist_table <- table(rr$new_prev, rr$Prisoner)
    hist_table
      round(prop.table(hist_table, 2), 3)
        chisq.test(hist_table, correct = FALSE)
      
table(rr$Alcohol.abuse)
rr$Alcohol.abuse <- ifelse(rr$Alcohol.abuse == "Yes", "Yes", "No")
table(rr$Alcohol.abuse)
rr$Alcohol.abuse <- factor(rr$Alcohol.abuse, levels = c("No", "Yes"))
  alc_table <- table(rr$Alcohol.abuse, rr$Prisoner)
    alc_table
      round(prop.table(alc_table, 2), 3)
        chisq.test(alc_table, correct = FALSE)
      
table(rr$Injecting.drug.user)
rr$Injecting.drug.user <- ifelse(rr$Injecting.drug.user == "Yes", "Yes", "No")
table(rr$Injecting.drug.user)
rr$Injecting.drug.user <- factor(rr$Injecting.drug.user, levels = c("No", "Yes"))
  drug_table <- table(rr$Injecting.drug.user, rr$Prisoner)
    drug_table
      round(prop.table(drug_table, 2), 3)
        chisq.test(drug_table, correct = FALSE)
      
table(rr$Homeless)
rr$Homeless <- factor(rr$Homeless, levels = c("Housed", "Unhoused"))
  house_table <- table(rr$Homeless, rr$Prisoner)
    house_table
      round(prop.table(house_table, 2), 3)
        chisq.test(house_table, correct = FALSE)
        
#Prep the outcome variable
        #Cure/Treatment Completion 
        #Failure (after fake failures removed)
        #Treatment discontinuation
        #Death/Palliative from TB (I am assuming the palliative care is due to TB - a limitation in my study)
        #made up of individuals with Died.B20, Died.TB and Failure.palliative in final_outcome
        #Other - died other cause 
table(ile_df$final_outcome_group)       
ile_df$ile_outcome <- ifelse(ile_df$final_outcome_group == "Cure or Treatment Completion", "Cured/Treatment Completed", 
                             ifelse(ile_df$final_outcome_group == "Failure" & ile_df$fake.failure == 0, "True failure",
                                    ifelse(ile_df$final_outcome == "Died.other.cause", "Died - other cause", 
                                           ifelse(ile_df$final_outcome == "Died.TB" | ile_df$final_outcome == "Died.B20"
                                                  | ile_df$final_outcome == "Failure.palliative", "TB death/Palliative Care",
                                                  ifelse(ile_df$final_outcome_group == "Treatment discontinuation", "Treatment discontinued",
                                                         NA))))) 
table(ile_df$ile_outcome)

ile_df$ile_outcome <- factor(ile_df$ile_outcome, levels = c("Cured/Treatment Completed", "True failure", 
                                                            "TB death/Palliative Care", "Died - other cause", "Treatment discontinued"))
#Analysis table
#call library to run multinom and make nice tables
library(nnet)
library(tidyverse)
library(broom)
library(kableExtra)
library(knitr)
library(gtsummary)


global <- multinom(ile_outcome ~ Prisoner, data = ile_df)
  summary(global)
    tidy(global, conf.int = TRUE)
    
fit_full <- multinom(ile_outcome ~ Prisoner + Sex + hiv_def + Localization + Cavitation
                     + new_prev + Alcohol.abuse + Injecting.drug.user + Homeless, data = ile_df)
  summary(fit_full)
  tidy(fit_full, conf.int = TRUE) %>% 
    kable() %>% 
    kable_styling("basic", full_width = FALSE)
  
tbl_regression(fit_full, exp = TRUE)

#outcome with RR
rr$ile_outcome <- ifelse(rr$final_outcome_group == "Cure or Treatment Completion", "Cured/Treatment Completed", 
                             ifelse(rr$final_outcome_group == "Failure" & rr$fake.failure == 0, "True failure",
                                    ifelse(rr$final_outcome == "Died.other.cause", "Died - other cause", 
                                           ifelse(rr$final_outcome == "Died.TB" | rr$final_outcome == "Died.B20"
                                                  | rr$final_outcome == "Failure.palliative", "TB death/Palliative Care",
                                                  ifelse(rr$final_outcome_group == "Treatment discontinuation", "Treatment discontinued",
                                                         NA))))) 
table(rr$ile_outcome)

global_rr <- multinom(ile_outcome ~ Prisoner, data = rr)
  summary(global_rr)
    tidy(global_rr, conf.int = TRUE)
    
fit_full_rr <- multinom(ile_outcome ~ Prisoner + Sex + hiv_def + Localization + Cavitation
                         + new_prev + Alcohol.abuse + Injecting.drug.user + Homeless, data = rr)
    summary(fit_full_rr)
    
      tidy(fit_full_rr, conf.int = TRUE) %>% 
        kable() %>% 
          kable_styling("basic", full_width = FALSE)
    
tbl_regression(fit_full_rr, exp = TRUE)






