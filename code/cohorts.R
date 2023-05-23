#'
#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.
#'

#' produce cohort of COVID positives
#' 
#' @param observation_derivation_recover Precomputed table containing COVID-relevant observations
#' @param require_covid_inpatient_ed boolean indicating whether to require COVID diagnoses to
#' occur at inpatient or ED visits for cohort inclusion
#' @param max_date Last date of cohort entry--update if switching to newer RECOVER submission.
#' 
#' @return Cohort of COVID positive (PCR, antigen, qualifying serology, specific dx, or pasc dx, excluding MIS-C) patients
#' columns: 
#' person_id | cohort_entry_date | site | event_type | event_loc
#'
#'
#'
#'
#'

get_first_PCR_infected <- function(observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                             require_covid_inpatient_ed=FALSE, max_date="2022-09-01"){
  
  obs_der_tbl<-observation_derivation_recover%>% filter(observation_date<max_date)
  
  # first PCR positive date
  pcr_positive<-obs_der_tbl %>%
    filter(observation_concept_id==2000001530L, value_as_concept_id %in% c(9191L, 2000001526L))
  
  first_positive<-pcr_positive %>% 
    group_by(person_id) %>% 
    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup() %>% 
    rename(cohort_entry_date=observation_date) %>% 
    get_test_type_and_loc() %>% 
    distinct(person_id, cohort_entry_date, site, event_type, event_loc) %>%
    compute_new(indices=c("person_id", "visit_occurrence_id"))  # n=166682
  
  # First PASC date
  pasc_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001520L)
  
  first_pasc <- pasc_dx%>% 
    get_test_type_and_loc() %>% 
    distinct(person_id, observation_date, site, event_type, event_loc) %>%
    group_by(person_id) %>% 
    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup() %>% 
    compute_new(indices="person_id")  # n=1759
  
  
  # pasc (- 4 weeks) earlier than PCR: PCR may not the first infection
  pasc_earlier <- first_pasc %>% inner_join(first_positive, by = "person_id") %>% 
    filter(cohort_entry_date>(observation_date-weeks(4)))  #n=118
  
  b94_dx=(get_b94_dx() ) %>% 
    rename(observation_date = cohort_entry_date)%>%
    compute_new(index="person_id")
  
  first_b94 <- b94_dx%>% 
    distinct(person_id, observation_date) %>%
    group_by(person_id) %>% 
    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup() %>% 
    compute_new(indices="person_id")  # n=1425
  
  b94_earlier <- first_b94 %>% inner_join(first_positive, by = "person_id") %>% 
    filter(cohort_entry_date>(observation_date-weeks(4)))  #n=118

  
  
  misc_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==703578L)
  
  first_misc <- misc_dx%>% 
    get_test_type_and_loc() %>% 
    distinct(person_id, observation_date, site, event_type, event_loc) %>%
    group_by(person_id) %>% 
    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup() %>% 
    compute_new(indices="person_id")   # n=1717
  
  # misc  (- 4 weeks)  earlier than PCR: PCR may not the first infection
  misc_earlier <- first_misc %>% inner_join(first_positive, by = "person_id") %>% 
    filter(cohort_entry_date>(observation_date-weeks(4)))   # n = 286
  
  
  first_infection<- first_positive %>% anti_join(pasc_earlier, by = "person_id") %>%
    anti_join(misc_earlier, by = "person_id") %>% anti_join(b94_earlier, by = "person_id") %>% 
    #left_join(first_pasc %>% select (person_id, observation_date), by = "person_id")%>% 
    #rename(pasc_date = observation_date) %>%
    #select(person_id, cohort_entry_date, site, event_type, event_loc, pasc_date) %>%
    #left_join(first_misc %>% select (person_id, observation_date), by = "person_id")%>%
    #rename(misc_date = observation_date) %>%
    #select(person_id, cohort_entry_date, site, event_type, event_loc, pasc_date, misc_date) %>%
    #left_join(first_b94 %>% select (person_id, observation_date), by = "person_id")%>%
    #rename(b94_date = observation_date) %>%
    select(person_id, cohort_entry_date, site, event_type, event_loc)      # n = 166299
  return(first_infection)
}

require_two_apart_prior_encounter<-function(cohort){
  # require 2 encounters with 3 months apart within 3 years prior to cohort entry
  keep_ids<-cohort %>% 
    inner_join(cdm_tbl("visit_occurrence") %>% distinct(person_id, visit_start_date), by="person_id") %>%
    filter(visit_start_date<cohort_entry_date-days(7), visit_start_date>cohort_entry_date-months(36)) 
  
  max_visit <- keep_ids %>%
    group_by(person_id) %>%
    slice_max(visit_start_date, with_ties=FALSE) %>%
    ungroup %>% rename(max_date = visit_start_date) %>% select(person_id, max_date)
  
  min_visit <- keep_ids %>%
    group_by(person_id) %>%
    slice_min(visit_start_date, with_ties=FALSE) %>%
    ungroup %>% rename(min_date = visit_start_date) %>% select(person_id, min_date)
  
  keep_ids <- min_visit %>% inner_join(max_visit, by = "person_id") %>%
    filter(max_date > min_date + months(3))
  
  cohort %>% inner_join(keep_ids, by="person_id")%>% select(-min_date, max_date) %>% 
    compute_new(index="person_id")
}

require_follow_up_encounter<- function(cohort) {
  keep_ids<-cohort %>% 
    inner_join(cdm_tbl("visit_occurrence") %>% distinct(person_id, visit_start_date), by="person_id") %>%
    filter(visit_start_date>cohort_entry_date+days(28)) %>%
    distinct(person_id) %>%
    compute_new(index="person_id")
  
  cohort %>% inner_join(keep_ids, by="person_id") %>% compute_new(index="person_id")
}


##########################################################################################


get_covid_positives<-function(observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                              require_covid_inpatient_ed=FALSE, max_date="2022-09-01"){
  
  obs_der_tbl<-observation_derivation_recover%>% filter(observation_date<max_date)
  pcr_positive<-obs_der_tbl %>%
    filter(observation_concept_id==2000001530L, value_as_concept_id %in% c(9191L, 2000001526L))
  antigen_positive<-obs_der_tbl %>%
    filter(observation_concept_id==2000001529L, value_as_concept_id %in% c(9191L, 2000001526L))
  serology_positive<-get_serology_positives()
  covid_dx_specific<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001525L)
  covid_dx_complication<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001523L)
  covid_dx_history<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001522L)
  
  covid_dx_temp<-covid_dx_specific %>%
    dplyr::union(covid_dx_complication) %>%
    dplyr::union(covid_dx_history) %>%
    compute_new(indices=c("person_id", "visit_occurrence_id", "observation_id"))
  
  if (require_covid_inpatient_ed==TRUE){
    covid_dx<-covid_dx_temp %>% 
      inner_join(cdm_tbl("visit_occurrence") %>% 
                   select(visit_occurrence_id, visit_concept_id), by="visit_occurrence_id") %>%
      filter(visit_concept_id %in% c(9201L, 2000000048L, 9203L)) %>%
      select(-visit_concept_id) %>%
      compute_new(index="person_id")
  }
  else{
    covid_dx=covid_dx_temp
  }

  
  
  pasc_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001520L)
  
  misc_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==703578L)
  
#  b94_dx<-get_b94_dx()
  
  positive_non_misc_cohort<-(pcr_positive %>%
                               dplyr::union(antigen_positive) %>%
                               dplyr::union(serology_positive) %>% 
                               dplyr::union(covid_dx)) %>%
    anti_join(misc_dx, by="person_id") %>%
    get_test_type_and_loc() %>%
    compute_new(indices=list(c("person_id", "visit_occurrence_id", "observation_id")))
    
  positive_first<-positive_non_misc_cohort %>% 
    group_by(person_id) %>% 
    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup() %>% 
    rename(cohort_entry_date=observation_date) %>% 
    distinct(person_id, cohort_entry_date, site, event_type, event_loc) %>%
    compute_new(indices=c("person_id", "visit_occurrence_id"))
  
  b94_dx=(get_b94_dx() %>% anti_join(positive_first, by="person_id")) %>% 
    anti_join(misc_dx, by="person_id") %>% 
    compute_new(index="person_id")
  
  pasc_positive_first<- (pasc_dx %>% anti_join(misc_dx, by="person_id")) %>%
    anti_join(positive_first, by="person_id") %>% 
    get_test_type_and_loc() %>% 
    mutate(cohort_entry_date=observation_date-weeks(4)) %>%
    distinct(person_id, cohort_entry_date, site, event_type, event_loc) %>%
    dplyr::union(b94_dx) %>% 
    group_by(person_id) %>% 
    slice_min(cohort_entry_date, with_ties=FALSE) %>%
    ungroup() %>% 
    compute_new(indices="person_id")
    
  positive_cohort<-positive_first %>%
    dplyr::union(pasc_positive_first) %>%
    compute_new(index="person_id")
  

  
  return(positive_cohort)
}



get_covid_negatives<-function(observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                              max_date="2022-09-01"){
  obs_der_tbl<-observation_derivation_recover%>% filter(observation_date<max_date)
  pcr_positive<-obs_der_tbl %>%
    filter(observation_concept_id==2000001530L, value_as_concept_id %in% c(9191L, 2000001526L))
  antigen_positive<-obs_der_tbl %>%
    filter(observation_concept_id==2000001529L, value_as_concept_id %in% c(9191L, 2000001526L))
  serology_positive<-get_serology_positives()
  covid_dx_specific<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001525L)
  covid_dx_complication<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001523L)
  covid_dx_history<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001522L)
  
  covid_dx<-covid_dx_specific %>%
    dplyr::union(covid_dx_complication) %>%
    dplyr::union(covid_dx_history) %>%
    compute_new(indices=c("person_id", "visit_occurrence_id", "observation_id"))
  
  pasc_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001520L)
  
  misc_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==703578L)
  
  b94_dx=get_b94_dx()
  
  positive_any<-pcr_positive %>% select(person_id) %>% 
    dplyr::union(antigen_positive %>%select(person_id)) %>% 
    dplyr::union(serology_positive %>%select(person_id)) %>% 
    dplyr::union(covid_dx %>%select(person_id)) %>% 
    dplyr::union(pasc_dx %>%select(person_id)) %>% 
    dplyr::union(misc_dx %>%select(person_id)) %>% 
    dplyr::union(b94_dx %>% select(person_id))
  
  
  
  
  pcr_negative<-obs_der_tbl %>%
    filter(observation_concept_id==2000001530L, value_as_concept_id %in% c(9189L))
  antigen_negative<-obs_der_tbl %>%
    filter(observation_concept_id==2000001529L, value_as_concept_id %in% c(9189L))
  serology_negative<-obs_der_tbl %>%
    filter(observation_concept_id==2000001528L, value_as_concept_id %in% c(9189L)) %>%
    mutate(observation_date=observation_date-weeks(4))
  
  negatives<-pcr_negative %>% 
    dplyr::union(antigen_negative) %>%
    dplyr::union(serology_negative)
  
  
  negatives_filtered<-negatives %>%
    anti_join(positive_any, by="person_id") %>% 
    compute_new(indices=c("person_id", "visit_occurrence_id", "observation_id"))
  
  negatives_first<-negatives_filtered %>%
    group_by(person_id) %>%
    slice_sample(observation_date, n=1) %>%
    #    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup %>%
    mutate(covid_status="negative") %>%
    mutate(cohort_entry_date=observation_date) %>%
    mutate(cohort_entry_date=as.Date(cohort_entry_date)) %>% 
    select(-observation_date) %>% 
    get_test_type_and_loc %>% 
    distinct(person_id, cohort_entry_date, site, event_type, event_loc) %>%
    compute_new(index="person_id")
  
  return(negatives_first)
  
  
}


#'SEROLOGY TESTING LOGIC: “To define the serology-positive cohort, 
#'we first examined the frequency and type of serology testing performed at PEDSnet institutions 
#'for children and adolescents <21 years of age at the time of the health encounter, 
#'from March 1, 2020, to April 20, 2022. The serology tests included were IgM, 
#'IgG anti- N antibodies, IgG anti-S or receptor binding domain (RBD) antibodies, 
#'and IgG and IgA undifferentiated antibodies. We then identified children who tested positive 
#'for SARS-CoV-2 by serology only and did not have a positive SARS-CoV-2 PCR test. 
#'EHR documentation of COVID-19 vaccination has not been fully linked and harmonized 
#'with other EHR data within our network. Thus, to assure that serology tests results 
#'were related to a past SARS-CoV-2 infection rather than vaccination, we applied the 
#'timing of age-specific vaccine approvals by the US FDA and excluded children with 
#'positive IgG anti-S/RBD, IgA or undifferentiated IgG tests after the vaccination 
#'eligible periods.  We used the following vaccine approval dates: 
#'December 12, 2020: BNT162b2 (Pfizer-BioNtech) in children > 16 years of age; 
#'May 12, 2021: BNT162b2 (Pfizer-BioNtech) for children 12-15 years of age, 
#'and November 2, 2021: BNT162b2 (Pfizer-BioNtech) for children 5-11 years of age. 
#'The adenovirus vaccine (Ad26.COV2.S) marketed by Janssen was approved in the U.S
#' in February 28, 2021 for patients > 18 years of age. 
#' However, the overall uptake of this vaccine has been low, especially in the 18–25-year age group; 
#' thus, serology age-cut offs for this vaccine were not included. 
#' Cohort entry for the positive serology group was defined as the date of first positive 
#' IgM, IgA or IgG COVID-19 antibodies after applying the filters described above. 
#' For the PCR-positive group, cohort entry was defined as the date of a first positive 
#' SARS-CoV-2 PCR test, irrespective serology testing.

#'
#'
#'
#'
#`

### 
get_serology_positives<-function(observation_derivation_recover=cdm_tbl("observation_derivation_recover"), max_date="2022-09-01"){
  obs_der_tbl<-observation_derivation_recover%>% filter(observation_date<max_date)
  
  ## Positive/Negative Serology Results
  serology_tbl<-obs_der_tbl %>% 
    filter(observation_concept_id==2000001528L)%>%
    mutate(test_result=case_when(
      value_as_concept_id %in% c(9191L, 2000001526L)~"positive",
      value_as_concept_id==9189L~"negative",
      TRUE~"unknown"
    )) %>%
    filter(test_result!="unknown") %>%
    compute_new(indices=c("person_id", "visit_occurrence_id", "observation_source_concept_id"))
  
  
  measurement_tbl<-cdm_tbl('measurement_labs') %>%
    filter(measurement_date >= as.Date('2020-03-01')) %>%
    mutate(vsv_lower = tolower(value_source_value), unit_lower=tolower(unit_source_value))
  
  serology_meas<-serology_tbl %>% 
    select(-value_source_value, -unit_source_value, -unit_concept_id) %>%
    inner_join(measurement_tbl %>% 
                 select(measurement_id, measurement_date, vsv_lower, measurement_source_value, measurement_concept_id),
               by=c("observation_source_concept_id"="measurement_id")) %>%
    compute_new(indices=c("person_id", "visit_occurrence_id", "observation_source_concept_id"))
  
  serology<-serology_meas %>% left_join(select(cdm_tbl('person'),person_id,birth_date),by='person_id') %>%
    mutate(measurement_age = (measurement_date - birth_date)/365.25) %>%
    filter(measurement_age >= 0, measurement_age < 21) %>%
    mutate(age_range = case_when(measurement_age < 5 ~ "<5",
                                 measurement_age >= 5 & measurement_age < 12 ~ "5-11",
                                 measurement_age >= 12 & measurement_age < 16 ~ "12-15",
                                 measurement_age >= 16 ~ "16+"))%>% 
    mutate(sv_lower = tolower(measurement_source_value)) %>%
    mutate(measurement_concept = case_when(str_detect(sv_lower, 'nucleocapsid') ~ 2000001501,
                                           str_detect(sv_lower, 'spike') ~ 2000001502,
                                           TRUE ~ measurement_concept_id))%>% 
    mutate(serology_type = case_when(measurement_concept == 2000001501L ~ 'IgG N protein',
                                     measurement_concept %in% c(723474L, 706177L, 706181L) ~ 'IgG undifferentiated',
                                     measurement_concept %in% c(723475L, 706178L) ~ 'IgM',
                                     measurement_concept == 723473L ~ 'IgA',
                                     measurement_concept %in% c(586515L,586522L,723480L) ~ 'Ab undifferentiated',
                                     measurement_concept %in% c(36032309L, 36031956L, 36031969L) ~ 'stimulated gamma interferon release',
                                     measurement_concept %in% c(2000001502L, 1619027L, 36031734L) ~ 'S protein or RBD')) %>%
    filter(serology_type != 'stimulated gamma interferon release')
  
  serology_filtered<-serology %>%
    mutate(include_flag=case_when(
      serology_type %in% c('IgG N protein', 'IgM')~1,
      age_range=='<5'~1,
      age_range=='16+' & measurement_date<as.Date('2020-12-12')~1,
      age_range=='12-15' & measurement_date<as.Date('2021-05-12') ~1,
      age_range=='5-11' & measurement_date<as.Date('2021-11-02')~1,
      TRUE~0
    )) %>%
    filter(include_flag==1) %>% distinct(observation_id) %>% compute_new(index="observation_id")
  
  
  serology_tbl %>% 
    filter(test_result=="positive") %>%
    select(-test_result) %>% 
    inner_join(serology_filtered, by="observation_id") %>%
    mutate(observation_date=observation_date-weeks(4)) %>% 
    compute_new(indices=c("person_id", "visit_occurrence_id", "observation_source_concept_id")) %>%
    return()
}







get_test_type_and_loc<-function(cohort){
  cohort %>%
    mutate(event_type=case_when(
      observation_concept_id==2000001530L~"pcr",
      observation_concept_id==2000001529L~"antigen",
      observation_concept_id==2000001528L~"serology",
      observation_concept_id==2000001527L & value_as_concept_id==2000001525L~"covid_dx_specific",
      observation_concept_id==2000001527L & value_as_concept_id==2000001523L~"covid_dx_complication",
      observation_concept_id==2000001527L & value_as_concept_id==2000001522L~"covid_dx_history",
      observation_concept_id==2000001527L & value_as_concept_id==2000001520L~"pasc_dx",
      observation_concept_id==2000001527L & value_as_concept_id==703578L~"misc_dx"
    )) %>%
    left_join(cdm_tbl("visit_occurrence") %>% 
                select(visit_occurrence_id, visit_concept_id), by="visit_occurrence_id") %>%
    mutate(event_loc=case_when(
      visit_concept_id %in% c(9201L,2000000088L,2000000048L) ~ 'Inpatient',
      visit_concept_id %in% c(9202L,581399L) ~ 'Outpatient Office',
      visit_concept_id %in% c(9203L) ~ 'ED',
      visit_concept_id %in% c(2000000469L,44814711L) ~ 'Outpatient: Test Only',
      TRUE ~ 'Other/Unknown'
    )) %>%
    select(-observation_concept_id, -value_as_concept_id, -visit_occurrence_id, -visit_concept_id) %>%
    compute_new(index="person_id")
}

join_cohort_demo<-function(cohort, max_date="2022-09-01"){
  cohort_demo<- cohort %>%
    mutate(cohort_entry_date=as.Date(cohort_entry_date)) %>% 
    inner_join(cdm_tbl("person") %>% dplyr::select(person_id, birth_date,
                                                   gender_concept_id, race_concept_id,
                                                   ethnicity_concept_id), by="person_id") %>%
    mutate(entry_age=floor((cohort_entry_date-birth_date)/365.25)) %>%
    filter(entry_age<21) %>%
    mutate(obs_age_cat = case_when(entry_age < 1 ~ '<1',
                                   entry_age <  5 ~ '1-4',
                                   entry_age < 12 ~ '5-11',
                                   entry_age < 16 ~ '12-15',
                                   entry_age < 21 ~ '16-20')) %>% 
    mutate(sex_cat = case_when(gender_concept_id == 8507L ~ 'Male',
                               gender_concept_id == 8532L ~ 'Female',
                               TRUE ~ 'Other/unknown'),
           eth_cat = case_when(ethnicity_concept_id == 38003563L ~ 'Hispanic',
                               race_concept_id == 8516L ~ 'Black/AA',
                               race_concept_id %in% c(8515L, 8557L) ~
                                 'Asian/PI',
                               #                               race_concept_id == 8657L ~ 'Native American',
                               race_concept_id == 8527L ~ 'White',
                               race_concept_id == 44814659L ~ 'Multiple',
                               TRUE ~ 'Other/Unknown')) %>%
    dplyr::select(-gender_concept_id, -race_concept_id, -ethnicity_concept_id) %>%
    filter(cohort_entry_date<as.Date(max_date), cohort_entry_date>=as.Date("2020-03-01")) %>%
    mutate(cohort_entry_month=paste(month(cohort_entry_date), year(cohort_entry_date), sep=",")) %>%
    mutate(cohort_entry_period=case_when(
      year(cohort_entry_date)=="2020" & month(cohort_entry_date) %in% c(3, 4, 5, 6)~"mar_jun_20",
      year(cohort_entry_date)=="2020" & month(cohort_entry_date) %in% c(7, 8, 9, 10)~"jul_oct_20",
      (year(cohort_entry_date)=="2020" & month(cohort_entry_date) %in% c(11, 12))|
        (year(cohort_entry_date)=="2021" & month(cohort_entry_date) %in% c(1, 2))~"nov_feb_21",
      year(cohort_entry_date)=="2021" & month(cohort_entry_date) %in% c(3, 4, 5, 6)~"mar_jun_21",
      year(cohort_entry_date)=="2021" & month(cohort_entry_date) %in% c(7, 8, 9, 10)~"jul_oct_21",
      (year(cohort_entry_date)=="2021" & month(cohort_entry_date) %in% c(11, 12))|
        (year(cohort_entry_date)=="2022" & month(cohort_entry_date) %in% c(1, 2))~"nov_feb_22",
      year(cohort_entry_date)=="2022" & month(cohort_entry_date) %in% c(3, 4, 5, 6)~"mar_jun_22",
      year(cohort_entry_date)=="2022" & month(cohort_entry_date) %in% c(7, 8)~"jul_aug_22"
    ))  %>%
    mutate(follow_days=as.numeric(as.Date(max_date)-cohort_entry_date)) %>%
    mutate(follow_months=floor(follow_days/(365.25/12))) %>%
    compute_new(index="person_id")
  return(cohort_demo)
}

get_b94_dx<-function(
    b94_src_val_exclude=results_tbl("b94_src_val_exclude")
    ){
  b94_code_dx<-vocabulary_tbl("concept") %>% 
    filter(grepl("B94.8", concept_code), vocabulary_id==("ICD10CM")) %>% 
    select(concept_id) %>%
    inner_join(cdm_tbl("condition_occurrence"), by=c("concept_id"="condition_source_concept_id")) %>%
    anti_join(b94_src_val_exclude, by="condition_source_value") %>%
    left_join(cdm_tbl("visit_occurrence") %>% 
                select(visit_occurrence_id, visit_concept_id, visit_start_date), by="visit_occurrence_id") %>%
    mutate(event_loc=case_when(
      visit_concept_id %in% c(9201L,2000000088L,2000000048L) ~ 'Inpatient',
      visit_concept_id %in% c(9202L,581399L) ~ 'Outpatient Office',
      visit_concept_id %in% c(9203L) ~ 'ED',
      visit_concept_id %in% c(2000000469L,44814711L) ~ 'Outpatient: Test Only',
      TRUE ~ 'Other/Unknown'
    )) %>%
    mutate(event_type="b94_8_dx") %>% 
    mutate(cohort_entry_date=visit_start_date-weeks(4)) %>% 
    distinct(person_id, cohort_entry_date, site, event_type, event_loc) %>%
    compute_new(index="person_id")
  
  
  
    
}


require_prior_encounter<-function(cohort){
  keep_ids<-cohort %>% 
    inner_join(cdm_tbl("visit_occurrence") %>% distinct(person_id, visit_start_date), by="person_id") %>%
    filter(visit_start_date<cohort_entry_date-days(7), visit_start_date>cohort_entry_date-months(18)) %>%
    distinct(person_id) %>%
    compute_new(index="person_id")
  
  cohort %>% inner_join(keep_ids, by="person_id") %>% compute_new(index="person_id")
}


#'need to update to get b94.8 also
#'
get_pasc_dx_flag<-function(cohort, max_date="2022-09-01"){
  u09_dx<-cdm_tbl("observation_derivation_recover") %>% 
    filter(observation_date<max_date) %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001520L) %>%
    distinct(person_id) %>%
    mutate(pasc_flag=1) %>%
    compute_new(index="person_id")
  
  b94_dx<-get_b94_dx() %>%
    distinct(person_id) %>%
    mutate(pasc_flag=1) %>%
    compute_new(index="person_id")
  
  pasc_dx<-u09_dx %>%
    dplyr::union(b94_dx) %>%
    compute_new(index="person_id")
  
  cohort %>% 
    left_join(pasc_dx, by="person_id") %>%
    mutate(pasc_flag=case_when(
      is.na(pasc_flag)~0,
      TRUE~pasc_flag
    )) %>%
    compute_new(index="person_id")
  
  
}


require_follow_up_code<-function(cohort, cond_codeset=load_codeset("cluster_master_pasc")){
  cond_visit_tbl<-cdm_tbl("condition_occurrence") %>% 
    inner_join(cdm_tbl("visit_occurrence") %>% select(visit_occurrence_id, visit_start_date), by="visit_occurrence_id")
  
  cohort_cond_occurrence<-cohort %>% 
    inner_join(cond_visit_tbl, by="person_id") %>%
    inner_join(cond_codeset, by=c("condition_source_concept_id"="concept_id")) %>%
    filter(visit_start_date>=cohort_entry_date+days(28), visit_start_date<=cohort_entry_date+days(179)) %>%
    compute_new(index="person_id")
  
  code_occurrence_ids<-cohort_cond_occurrence %>% distinct(person_id) %>%
    mutate(code_occurrence_flag=1)
  
  cohort %>% 
    left_join(code_occurrence_ids, by="person_id") %>%
    mutate(code_occurrence_flag=case_when(
      is.na(code_occurrence_flag)~0,
      TRUE~code_occurrence_flag
    )) %>%
    filter(pasc_flag==1 | code_occurrence_flag==1) %>%
    compute_new(index="person_id")
    
}


get_cluster_visits<-function(cohort, cond_codeset=load_codeset("cluster_master_pasc")){
  cond_visit_tbl<-cdm_tbl("condition_occurrence") %>% 
    inner_join(cdm_tbl("visit_occurrence") %>% select(visit_occurrence_id, visit_start_date), by="visit_occurrence_id")
  
  cohort_clust_occurrence<-cohort %>% 
    inner_join(cond_visit_tbl %>% select(-site), by="person_id") %>%
    inner_join(cond_codeset, by=c("condition_source_concept_id"="concept_id")) %>%
    filter(visit_start_date>=cohort_entry_date+days(28), visit_start_date<=cohort_entry_date+days(179)) %>%
    group_by(person_id, cluster) %>%
    summarize(n_visits=n_distinct(visit_start_date)) %>%
    filter(n_visits>0) %>% 
    ungroup %>% 
    mutate(clust_flag=1) %>% 
    pivot_wider(id_cols="person_id", names_from="cluster", values_from="clust_flag", values_fill=0, names_prefix="post_") %>% 
    compute_new(index="person_id")
  
  myList <- setNames(lapply(vector("list", ncol(cohort_clust_occurrence)), function(x) x <- 0), colnames(cohort_clust_occurrence))
  
  cohort %>% left_join(cohort_clust_occurrence, by="person_id") %>%
    replace_na(myList) %>%compute_new(index="person_id")
  
}




#' Add a site identifier, splitting Nemours by region
#'
#' Given a tbl containing `person_id`s, add a `site` column indicating the home
#' site for that person, based on the location of that person's home site as
#' specified in `site_tbl`. For patients whose home site is Nemours, further
#' indicate whether their care is based in Delaware or Florida, based on their
#' home `care_site` as specified by `site_tbl`.
#'
#' @param cohort The tbl containing the persons of interest.
#' @param site_tbl A tbl containing at least `person_id`, `care_site_id`, and
#'   `site` columns to place the patient.
#' @return The cohort tbl with a `site` column as described.  Any existing
#'   `site` column is removed.
#' @md
add_site_split <- function(cohort, site_tbl = add_site(cdm_tbl('person'))) {
  sites <- site_tbl %>% semi_join(cohort, by = 'person_id') %>%
    select(person_id, care_site_id, site) %>%
    left_join(select(cdm_tbl('care_site', db = site_tbl$src), care_site_id,
                     location_id), by = 'care_site_id') %>%
    left_join(select(cdm_tbl('location', db = site_tbl$src),
                     location_id, state), by = 'location_id') %>%
    mutate(site = case_when(site != 'nemours' ~ site,
                            state == 'FL' ~ 'nemours_fl',
                            TRUE ~ 'nemours_de')) %>%
    select(person_id, site)
  
  if (any(tbl_vars(cohort) == 'site')) cohort <- select(cohort, -site)
  left_join(cohort, sites, by = 'person_id')
}


get_util<-function(cohort,
                   visit_tbl=cdm_tbl("visit_occurrence") %>% select(visit_start_date, person_id)
                   ){
  cohort_util<-cohort %>% 
    inner_join(visit_tbl, by="person_id") %>%
    filter(visit_start_date<cohort_entry_date, visit_start_date>cohort_entry_date-months(18)) %>%
    group_by(person_id) %>%
    summarize(n_visits=n_distinct(visit_start_date)) %>%
    mutate(visits_per_month=n_visits/18) %>%
    compute_new(index="person_id")
  
  cohort %>%
    left_join(cohort_util, by="person_id")
    
    
  
}


#' Function to find any ICU event in relation to an cohort_entry_date
#' @param adt_tbl table with adt_occurrences, defaulting to the cdm table adt_occurrence
#' @param cohort_tbl table with all person_ids for cohort, including their cohort_entry_date
#' @return table with NICU, PICU, CICU occurrences of any kind along with adt_type_concept_id and adt_date
find_icu <- function(adt_tbl = cdm_tbl('adt_occurrence'),
                     cohort_tbl) {
  cohort_tbl %>% select(person_id, cohort_entry_date) %>%
    inner_join(adt_tbl, by = 'person_id') %>%
    filter(service_concept_id %in% c(2000000079L,2000000080L,2000000078L)) %>%
    select(person_id, adt_date, cohort_entry_date, adt_type_concept_id) 
}

#' Function to find hospitalizations in relation to cohort_entry_date
#' @param visit_tbl table with visit_occurrences, defaulting to the cdm table visit_occurrence
#' @param cohort_tbl table with person_ids for cohort, including their cohort_entry_date
#' @param hosp_visit_types a list of visit_concept_ids to classify as hospitalizations
#' @return table with hospitalizations for the cohort at any time along with visit_start_date and visit_concept_id
find_hosp <- function(visit_tbl = cdm_tbl('visit_occurrence'),
                      cohort_tbl,
                      hosp_visit_types = c(9201L,
                                           2000000048L,
                                           2000000088L)) {
  cohort_tbl %>% select(person_id, cohort_entry_date) %>%
    inner_join(visit_tbl, by = 'person_id') %>%
    filter(visit_concept_id %in% !!hosp_visit_types) %>%
    select(person_id, visit_start_date, cohort_entry_date, visit_concept_id)
}

make_icu_flag<-function(cohort_tbl, days_min=-7, days_max=13){
  cohort_icu<-cohort_tbl %>%
    find_icu(cohort_tbl=.) %>%
    filter(adt_date>=cohort_entry_date+days(!!days_min), adt_date<cohort_entry_date+days(!!days_max)) %>%
    mutate(icu_flag=1) %>%
    distinct(person_id, icu_flag) %>%
    compute_new(index="person_id")
  
  cohort_tbl<- cohort_tbl %>%
    left_join(cohort_icu, by="person_id") %>%
    mutate(icu_flag=case_when(is.na(icu_flag)~0,
                              TRUE~icu_flag)) %>%
    compute_new(index="person_id")
    return(cohort_tbl)
}


