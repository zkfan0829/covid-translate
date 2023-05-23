# this is the driver file to extract the cohort. 
# all related functions are defined in run.R, cohorts_vaccine_infection.R, and cohorts_flags.R

#############################################################################################
## ------ Step 1: load functions & libraries --------
source("site/run.R")
source("code/cohorts_vaccine_infection.R")
source("code/cohorts_flags.R")

library(ggplot2)
library(scales)
library(lubridate)
library(ks)

## ------ Step 2: define the cohort start and end date --------
## please choose one of the study cohort to start
study_cohort = "omicron_age5"
# study_cohort = "omicron_age12"
# study_cohort = "delta_age12"

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

rslt <- list()

## ------ Step 3: start the cohort extraction --------
### all positive patients omicron
rslt$positive_all<-get_covid_positives_imputed(max_date = max_date) 
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
  filter(age_study_start>=min_age & age_study_start <max_age)


# above this point, all of the positives & negative (cases & control) have been extracted & combined
#############################################################################################
### add immunization info for each age group in column imm_index_date
### since we need to input the vaccination date for unvaccinated patients which differs across age group
immu <- find_all_immunizations()
immu_rslt <- immu%>%inner_join(rslt$age_cohort %>% select(person_id), by  = "person_id")
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
rslt$cohort_immu_remove_na <- rslt$cohort_immu%>%filter(!is.na(imm_index_date))
rslt$immu_cleanup <- rslt$cohort_immu_remove_na%>%rename(cohort_entry_date = imm_index_date)%>%
  rename(cohort_entry_visit_concept_id_flag = imm_visit_concept_id_flag)%>%
  rename(cohort_entry_visit_type_flag = imm_visit_type_flag)
rslt$immu_cleanup  %>% select(person_id) %>% distinct() %>% count()


### Add demographics
rslt$history<-require_prior_encounter(rslt$immu_cleanup) 

rslt$cohort_demo<-rslt$history %>% 
  cat_obesity()%>%
  join_cohort_demo(max_date = max_date)

rslt$cohort_prior_visits<-rslt$cohort_demo%>%
  get_prior_visits()%>%
  get_drug_history()


### Add pmca
pmca_lookup <- 
  produce_pmca_lookup(cohort=rslt$cohort_prior_visits) %>% 
  compute_new(indexes=list('person_id','body_system',
                           'condition_concept_id','visit_occurrence_id'))

pmca_summary <- 
  compute_pmca_summary(pmca_lookup) %>% 
  compute_new(indexes=list('person_id','body_system'))

pmca_cats_lib <- 
  compute_pmca_cats_lib(pmca_summary)


rslt$cohort_pmca <- rslt$cohort_prior_visits %>% left_join(
  pmca_cats_lib%>% 
    distinct(person_id, chronic, complex_chronic),
  by="person_id"
) %>%
  mutate(pmca_index=case_when(
    complex_chronic==1~"2",
    chronic==1~"1",
    TRUE~"0"
  )) %>%
  dplyr::select(-chronic, -complex_chronic) %>%
  compute_new


### Add medical history
rslt$cohort_medical <- get_medical_history(rslt$cohort_pmca)


## add other outcomes
rslt$cohort_icu <- make_icu_flag(rslt$cohort_medical)
rslt$cohort_hosp <- make_hosp_flag(rslt$cohort_icu)
rslt$cohort_pasc <- get_pasc_dx(rslt$cohort_hosp, max_date = max_date)
rslt$cohort_misc <- get_misc_dx(rslt$cohort_pasc, max_date = max_date)


## compute severity (only positive patients due to covid test/diagnosis)
cohort <-  rslt$full_cohort  %>% filter(test_result=="positive")%>% 
  filter(!(event_type %in% c("pasc_dx","misc_dx","b94_8_dx")))

rslt$cohort_visits<-cohort %>%
  rename(observation_date=cohort_entry_date) %>% 
  dplyr::select(person_id, observation_date, test_result, event_type, site) %>%
  get_visits() %>%
  compute_new(indexes=list('person_id',
                           'visit_type',
                           'observation_date',
                           'visit_occurrence_id'))


rslt$flag_1 <- compute_flag1(visit_cohort = rslt$cohort_visits) %>% 
  compute_new(indexes=list('person_id',
                           'condition_concept_id',
                           'visit_occurrence_id'))

rslt$flag_2 <- compute_flag2(visit_cohort = rslt$cohort_visits) %>%
  compute_new(indexes=list('person_id',
                           'concept_id',
                           'visit_occurrence_id'))

rslt$flag_3 <- compute_flag3(visit_cohort = rslt$cohort_visits) %>%
  compute_new(indexes=list('person_id',
                           'concept_id',
                           'visit_occurrence_id'))


rslt$visit_level_severity <- compute_visit_level_severity(visit_cohort = rslt$cohort_visits,
                                                          flag_1_tbl=rslt$flag_1,
                                                          flag_2_tbl=rslt$flag_2,
                                                          flag_3_tbl=rslt$flag_3) %>%
  compute_new(indexes=list('person_id'))


rslt$person_level_severity <- compute_person_level_severity(visit_severity_tbl = rslt$visit_level_severity)%>%
  compute_new(indexes=list('person_id'))


rslt$final_output <- left_join(rslt$cohort_misc,
                               rslt$person_level_severity %>% select(-site))
# change all positive patients who have NA person_severity index to Asymptomatic
final <- rslt$final_output %>% as.data.frame()
index = which(final$test_result == "positive")
final[index,]$person_severity_index[is.na(final[index,]$person_severity_index)] = "Asymptomatic"


#############################################################################################
### ----------------- Output table ------------------
final %>% output_tbl(paste(study_cohort,"_final_table"), index = "person_id")





