# this is the driver file to extract the cohort. 
# all related functions are defined in run.R, cohorts_vaccine_infection.R, and cohorts_flags.R

library(ggplot2)
library(scales)
library(lubridate)
library(ks)
library(srcr)

setwd("/Users/fanz/Desktop/123/Vaccine-Effectiveness-Trial-Emulation/")

#############################################################################################
## ------ Step 1: load functions & libraries --------
source("site/run.R")
source("code/cohorts_new2_infection.R")
#source("code/cohorts_flags.R")



## ------ Step 2: define the cohort start and end date --------
## please choose one of the study cohort to start
study_cohort = "omicron_age5"
# study_cohort = "omicron_age12"
# study_cohort = "delta_age12"

## the max and min values of age are different among three study cohorts 
if (study_cohort == "delta_age12"){
  study_date_start= min_date =  "2021-07-01" # delta, 12 - 21.99
  study_date_end = max_date = "2021-11-30"
  min_age = 12
  max_age = 21
}else{
  study_date_start = min_date = "2022-01-01" # omicron, 5 - 11.99
  study_date_end = max_date = "2022-11-30"
  if (study_cohort == "omicron_age5"){
    min_age = 5
    max_age = 12
  }else{
    min_age = 12
    max_age = 21
  }
}

## create a new list to store the results
rslt <- list()

## ------ Step 3: start the cohort extraction --------
### all positive patients omicron
# `get_covid_positives_impute` function is the 
rslt$positive_all<-get_covid_positives_impute(max_date = max_date) 
#rslt$positive_all <- results_tbl("vaccine_positives_imputed") ###!!!!!

### all patients  (positive + negative)
rslt$full_cohort <- make_cohort(positive_cohort = rslt$positive_all, 
                                study_start = study_date_start,
                                study_end = study_date_end, 
                                any_visit = TRUE, 
                                max_date = max_date)
### age group
rslt$birth_date_cohort <- rslt$full_cohort %>%
  inner_join(cdm_tbl("person") %>% 
               dplyr::select(person_id, birth_date), by="person_id")


rslt$age_cohort <- rslt$birth_date_cohort  %>% 
  mutate(age_study_start = (as.Date(study_date_start)-birth_date)/365.25 )%>%
  filter(age_study_start>=min_age & age_study_start <max_age) #make sure the min age <= age-study-start <= max day


# above this point, all of the positives & negative (cases & control) have been extracted & combined
#############################################################################################
### add immunization info for each age group in column imm_index_date
### since we need to input the vaccination date for unvaccinated patients which differs across age group
immu <- find_all_immunizations() #extract corresponding immunization records
immu_rslt <- immu%>%inner_join(rslt$age_cohort %>% select(person_id), by  = "person_id") #join the extracted immunization records back to the previous table by matching their id
immu_rslt_clean <- immu_rslt %>% filter(immunization_date >= "2021-03-17") # based on Vitaly's definition of Date: ages 18+ eligible'
# immu_meta <- compute_imm_metadata(immu_rslt) # meta data, dont need to run at this time point

# compute the first vaccination
immu_first <- compute_first_imm(immu_rslt_clean)

# compute the second and third vaccination
immu_second_third <- compute_second_third_imm(imm_tbl_narrowed = immu_rslt_clean, 
                                              imm_metadata = immu_first) 

# cohort_entry_date in previous tables was for covid test date, renamed as covid_visit_date in imput_immu_index function
rslt$cohort_immu <- imput_immu_index(cohort = rslt$age_cohort, 
                                     immu_first = immu_first, 
                                     immu_second_third = immu_second_third,
                                     study_start_date = study_date_start, 
                                     study_end_date = study_date_end)
rslt$cohort_immu_remove_na <- rslt$cohort_immu %>% 
  filter(!is.na(imm_index_date))

rslt$immu_cleanup <- rslt$cohort_immu_remove_na%>%rename(cohort_entry_date = imm_index_date)%>%
  rename(cohort_entry_visit_concept_id_flag = imm_visit_concept_id_flag)%>%
  rename(cohort_entry_visit_type_flag = imm_visit_type_flag)
rslt$immu_cleanup  %>% select(person_id) %>% distinct() %>% count()

rslt$history <-require_prior_encounter(rslt$immu_cleanup) #have a prior visit 18m to 7d prior to the index date

### misclassified Vaccination & Gold Standard 
## assume in here we could extract the realistic vaccinated status as gold standard
rslt$immu_out <- rslt$history %>% mutate(gold_imm = if_else(is.na(imm_date_first), 0, 1))
### Add demographics
rslt$cohort_demo <-rslt$immu_out %>% 
  #cat_obesity()%>%
  join_cohort_demo(max_date = max_date)

rslt$cohort_prior_visits<-rslt$cohort_demo%>%
  get_prior_visits()
  #%>%get_drug_history()

#add number of covid tests 
rslt$final <-compute_negative_tests()


#############################################################################################
### ----------------- Output table ------------------
final <- rslt$final %>% as.data.frame()
final %>% output_tbl(paste0(study_cohort,"_final_table"), index = "person_id")




