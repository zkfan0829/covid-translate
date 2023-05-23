
get_covid_positives<-function(observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                              require_covid_inpatient_ed=FALSE, max_date="2022-12-01"){
  
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
  }else{
    covid_dx=covid_dx_temp
  }
  
  
  pasc_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001520L)
  
  misc_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==703578L)
  
  
  positive_cohort<-(pcr_positive %>%
                      dplyr::union(antigen_positive) %>%
                      dplyr::union(serology_positive) %>% 
                      dplyr::union(covid_dx)) %>%
    get_test_type_and_loc() %>%
    compute_new(indices=list(c("person_id", "visit_occurrence_id", "observation_id")))
  
  
  positive_first<-positive_cohort %>% 
    group_by(person_id) %>% 
    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup() %>% 
    rename(cohort_entry_date=observation_date) %>% 
    distinct(person_id, cohort_entry_date, site, event_type, event_loc) %>%
    compute_new(indices=c("person_id", "visit_occurrence_id"))
  
  b94_dx=(get_b94_dx() %>% anti_join(positive_first, by="person_id")) %>% 
    anti_join(misc_dx, by="person_id") %>% 
    compute_new(index="person_id")
  
  # include misc, make PASC observation date - 28 days as cohort entry date (covid infection date)
  pasc_misc_positive_first <- (pasc_dx %>% left_join(misc_dx)) %>%
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
    dplyr::union(pasc_misc_positive_first) %>%
    compute_new(index="person_id")
  
  
  
  return(positive_cohort)
}


get_covid_negatives<-function(observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                              max_date="2022-12-01"){
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
  
  
  
  # identify test records
  pcr_negative<-obs_der_tbl %>%
    filter(observation_concept_id==2000001530L, value_as_concept_id %in% c(9189L))
  antigen_negative<-obs_der_tbl %>%
    filter(observation_concept_id==2000001529L, value_as_concept_id %in% c(9189L))
  serology_negative<-obs_der_tbl %>%
    filter(observation_concept_id==2000001528L, value_as_concept_id %in% c(9189L)) %>%
    mutate(observation_date=observation_date-weeks(4))
  
  negatives<-pcr_negative %>% 
    dplyr::union(antigen_negative) %>%
    dplyr::union(serology_negative) %>% 
    filter(observation_date>"2020-03-01")
  
  # remove test records for positive patients
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


### covariates
get_prior_visits<-function(cohort){
  condition_visit_tbl<-cdm_tbl("condition_occurrence") %>% 
    dplyr::select(person_id, condition_source_concept_id, visit_occurrence_id) %>%
    inner_join(cdm_tbl("visit_occurrence") %>% filter(visit_concept_id %in% c(9202L,9201L,9203L,
                                                                              2000000048L,2000000088L,581399L)) %>%
                 dplyr::select(visit_occurrence_id, visit_start_date, visit_end_date, visit_concept_id, visit_concept_name)) %>%
    dplyr::select(-visit_occurrence_id)
  
  prior_visits<-cohort %>% distinct(person_id, cohort_entry_date) %>%
    inner_join(condition_visit_tbl, by="person_id") %>%
    #inner_join(cluster_master, by=c("cluster", "condition_source_concept_id"="concept_id")) %>%
    filter(visit_start_date<cohort_entry_date-days(7) & visit_start_date> cohort_entry_date-months(18)) %>%
    mutate(
      visit_loc =case_when(
        visit_concept_id %in% c(9201L,2000000088L,2000000048L) ~ 'Inpatient',
        visit_concept_id %in% c(9202L,581399L) ~ 'Outpatient',
        visit_concept_id %in% c(9203L) ~ 'ED'
      )
    )%>%
    dplyr::select(-visit_concept_id, visit_concept_name) %>%
    compute_new(index="person_id")
  
  ED_visits<-prior_visits %>% filter(visit_loc=="ED") %>% group_by(person_id) %>% 
    summarize(n_ED = n_distinct(visit_start_date))%>%
    compute_new(index="person_id")
  Inpatient_visits<-prior_visits %>% filter(visit_loc=="Inpatient") %>% group_by(person_id) %>% 
    summarize(n_inpatient = n_distinct(visit_start_date))%>%
    compute_new(index="person_id")
  Outpatient_visits<-prior_visits %>% filter(visit_loc=="Outpatient") %>% group_by(person_id) %>% 
    summarize(n_outpatient = n_distinct(visit_start_date))%>%
    compute_new(index="person_id")
  
  cohort_utility <- cohort %>% left_join(ED_visits, by="person_id") %>%
    left_join(Inpatient_visits, by="person_id") %>%
    left_join(Outpatient_visits, by="person_id") %>%
    compute_new(index="person_id")
  
  return(cohort_utility)
}


cat_obesity <- function(cohort,
                        anthro = cdm_tbl('measurement_anthro'),
                        person = cdm_tbl('person')) {
  obesity_tbl <- cohort %>% distinct(person_id, cohort_entry_date) %>%
    inner_join(anthro, by = 'person_id') %>%
    inner_join(person %>% select(person_id, birth_date), by = 'person_id') %>%
    filter(measurement_date<cohort_entry_date) %>%
    filter( (measurement_date - birth_date < 24 * 30.5 &
               measurement_date - birth_date > 0 &
               measurement_concept_id == 2000000041L &
               value_as_number > 1.64) |
              (measurement_date - birth_date < 240 * 30.5 &
                 measurement_date - birth_date > 0 &
                 measurement_concept_id == 2000000043L &
                 value_as_number > 1.64) |
              (measurement_date - birth_date >= 240 * 30.5 &
                 measurement_date - birth_date > 0 &
                 measurement_concept_id == 3038553L &
                 value_as_number > 30)
    ) %>%
    mutate(obese=1) %>%
    distinct(
      person_id,
      obese
    ) %>% compute_new(index="person_id")
  
  cohort_obesity<-cohort %>% left_join(obesity_tbl, by="person_id") %>%
    mutate(obese=case_when(
      is.na(obese)~0,
      TRUE~1
    )) %>%
    compute_new(index="person_id")
  return(cohort_obesity)
}

get_drug_history<- function(cohort_tbl){
  drug_tbl<-cdm_tbl("drug_exposure") 
  cohort_drug <- cohort_tbl %>% inner_join(drug_tbl, by = "person_id")%>% 
    filter(drug_exposure_start_date>=cohort_entry_date-months(18) & drug_exposure_start_date <=cohort_entry_date-days(7)) %>%
    group_by(person_id) %>%
    summarize(n_drug=n_distinct(drug_concept_id)) %>%
    filter(n_drug>0) %>% 
    ungroup %>% compute_new(index = "person_id")
  
  cohort_tbl %>% left_join(select(cohort_drug, person_id, n_drug), by = "person_id") %>% compute_new(index = "person_id")
}


get_medical_history<-function(cohort_tbl, 
                              cond_codeset=load_codeset("cluster_master"),
                              condition_tbl=cdm_tbl("condition_occurrence"),
                              visit_tbl=cdm_tbl("visit_occurrence")){
  cond_visit_tbl<-cdm_tbl("condition_occurrence") %>% 
    inner_join(cdm_tbl("visit_occurrence") %>% select(visit_occurrence_id, visit_start_date), by="visit_occurrence_id")
  
  cohort_medical<-cohort_tbl %>% 
    inner_join(cond_visit_tbl %>% select(-site), by="person_id") %>%
    inner_join(cond_codeset %>% filter(chronic_flag == "Y"), by=c("condition_source_concept_id"="concept_id")) %>%
    filter(visit_start_date>=cohort_entry_date-months(18), visit_start_date<=cohort_entry_date-days(7)) %>%
    group_by(person_id, cluster) %>%
    summarize(n_visits=n_distinct(visit_start_date)) %>%
    filter(n_visits>0) %>% 
    ungroup %>% 
    mutate(clust_flag=1) %>% 
    pivot_wider(id_cols="person_id", names_from="cluster", values_from="clust_flag", values_fill=0, names_prefix="medical_") %>% 
    compute_new(index="person_id")
  
  myList <- setNames(lapply(vector("list", ncol(cohort_medical)), function(x) x <- 0), colnames(cohort_medical))
  
  cohort_tbl %>% left_join(cohort_medical, by="person_id") %>%
    replace_na(myList) %>%compute_new(index="person_id")
}

############################################################################################################


#' @md find coded and uncoded immunizations
#'
#' @param imm_codeset
#' @param immunization_tbl
#'
#' @return the immunization tbl filtered for immunizations
#' that are either coded to covid19 vaccinations or have a source value
#' that indicates a patient received a covid19 vaccination
#'

find_all_immunizations <- function(imm_codeset=load_codeset('covid_vx_codeset'),
                                   immunization_tbl=cdm_tbl('immunization')) {
  
  imm_coded <- immunization_tbl %>%
    inner_join(imm_codeset,
               by=c('immunization_concept_id'='concept_id')) %>%
    mutate(
      code_flag = 'Coded'
    ) %>%
    compute_new(indexes = list('person_id'))
  
  imm_no_coded <- immunization_tbl  %>%
    filter(immunization_concept_id == 0L &
             str_detect(lower(immunization_source_value), 'covid|sars')) %>%
    mutate(
      code_flag = 'Uncoded'
    ) %>%
    compute_new(temporary=TRUE)
  
  dplyr::union(imm_coded,
               imm_no_coded)
  
}




#' @md age at first immunization with metadata around timing of immunization availability
#'
#' @param imm_tbl_narrowed the cohort of patients with immunizations
#' @param person_tbl the pedsnet person tbl
#'
#' @return a tbl with the following columns:
#'
#' `person_id` | `birth_date` | `imm_age_first` | `imm_date_first` | `imm_age_first_cat` | `imm_date_flag` | `imm_age_5to17` | `visit_concept_id_flag` | `visit_concept_id` | `state_reg` | `cnty_reg`

compute_first_imm <- function(imm_tbl_narrowed,
                              person_tbl=cdm_tbl('person'),
                              visit_occurrence_tbl=cdm_tbl('visit_occurrence'),
                              location_tbl=cdm_tbl('location')) {
  
  person_tbl_narrowed <- select(person_tbl,person_id,provider_id,birth_date,location_id)
  visit_occurrence_tbl_narrowed <- select(visit_occurrence_tbl,visit_occurrence_id,visit_concept_id)
  location_tbl_narrowed <- select(location_tbl,location_id,state,county)
  
  #Try to get the minimum imm date for each person; if multiple minimum immunization dates, pick the lowest immunization id
  first_imm <-
    imm_tbl_narrowed %>%
    group_by(person_id,immunization_date) %>%
    mutate(min_imm_id=min(immunization_id)) %>%
    ungroup() %>%
    group_by(person_id) %>%
    mutate(imm_date_first=min(immunization_date)) %>%
    filter(immunization_date==imm_date_first) %>%
    filter(immunization_id==min_imm_id) %>%
    ungroup() %>%
    select(immunization_id,person_id,imm_date_first,visit_occurrence_id,imm_manufacturer) %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id','visit_occurrence_id'))
  
  first_imm_meta <-
    first_imm %>%
    inner_join(person_tbl_narrowed,by='person_id') %>%
    left_join(visit_occurrence_tbl_narrowed, by='visit_occurrence_id') %>%
    left_join(location_tbl_narrowed,by='location_id') %>%
    mutate(imm_age_first=round((imm_date_first-birth_date)/365.25,2)) %>%
    mutate(
      imm_age_first_cat=
        case_when(imm_age_first < 1 ~ '<1',
                  imm_age_first < 5 ~ '1-4',
                  imm_age_first < 12 ~ '5-11',
                  imm_age_first < 16 ~ '12-15',
                  imm_age_first < 21 ~ '16-20',
                  TRUE ~ 'old')
    ) %>%
    mutate(
      imm_age_5to17=
        case_when(imm_age_first < 5 ~ '<1-4',
                  imm_age_first <18 ~ '5-17',
                  TRUE ~ '18+')
    ) %>%
    mutate(
      imm_date_flag=
        case_when(
          imm_date_first < '2020-01-01' ~ 'Date implausibly early',
          imm_date_first < '2020-12-14' ~ 'Date prior to availability',
          imm_date_first < '2021-03-17' ~ 'Date: ages 18+ eligible',
          # imm_date_first < '2021-04-19' ~ 'Date: ages 18+ eligible',
          imm_date_first < '2021-05-10' ~ 'Date: ages 16+ eligible',
          imm_date_first < '2021-10-29' ~ 'Date: ages 12+ eligible',
          imm_date_first < '2021-06-18' ~ 'Date: ages 5+ eligible',
          imm_date_first < '2022-12-31' ~ 'Date: ages 6 months+ eligible',
          imm_date_first >= '2022-12-31' ~ 'Date too late'
        )
    ) %>%
    mutate(
      visit_concept_id_flag=
        case_when(visit_concept_id == 9201L ~ 'Inpatient Hospital Stay',
                  visit_concept_id == 9202L ~ 'Ambulatory/Outpatient Visit (With a Physician)',
                  visit_concept_id == 9203L ~ 'Emergency Department',
                  visit_concept_id == 581399L ~ 'Interactive Telemedicine Service',
                  visit_concept_id == 42898160L ~ 'Long Term Care Visit',
                  visit_concept_id == 44814710L ~ 'Non-Acute Institutional',
                  visit_concept_id == 44814649L ~ 'Other',
                  visit_concept_id == 44814711L ~ 'Other Ambulatory Visit (OA)',
                  visit_concept_id == 2000000048L ~ 'Emergency Department Admit to Inpatient Hospital Stay',
                  visit_concept_id == 2000000088L ~ 'Observation Visit',
                  visit_concept_id == 2000000104L ~ 'Administrative Visit',
                  visit_concept_id == 2000000469L ~ 'Outpatient Non Physician'
        )
    ) %>%
    mutate(
      visit_type_flag=
        case_when(visit_concept_id %in% c(9201L, 2000000048L, 2000000088L) ~ 'Inpatient',
                  visit_concept_id %in% c(9202L, 581399L) ~ 'Outpatient',
                  visit_concept_id %in% c(9203L) ~ 'ED',
                  TRUE ~ 'Other')
    ) %>%
    mutate(
      state_reg = case_when(nchar(state)==2 ~ state,
                            str_detect(lower(state),'pennsylvania') ~ 'PA',
                            str_detect(lower(state),'new jersey') ~ 'NJ',
                            str_detect(lower(state),'delaware') ~ 'DE')
    ) %>%
    mutate(
      cnty_reg = case_when(nchar(county)>0 ~ str_replace(county," County","") %>%
                             paste0(" County") %>%
                             str_to_title(),
                           nchar(county)==0 ~ "")
    ) %>%
    mutate(
      imm_manufact_cat = case_when(str_detect(lower(imm_manufacturer),'johnson|janssen') ~ 'Johnson and Johnson',
                                   str_detect(lower(imm_manufacturer),'pfizer') ~ 'Pfizer',
                                   str_detect(lower(imm_manufacturer),'moderna') ~ 'Moderna',
                                   str_detect(lower(imm_manufacturer),'astrazeneca') ~ 'AstraZeneca',
                                   str_detect(lower(imm_manufacturer),'unknown') ~ 'Unknown',
                                   is.na(imm_manufacturer) ~ 'NA',
                                   TRUE ~ 'Other'
      )
    ) %>%
    select(person_id,
           birth_date,
           imm_age_first,
           imm_date_first,
           imm_age_first_cat,
           imm_age_5to17,
           imm_date_flag,
           imm_manufacturer,
           imm_manufact_cat,
           visit_concept_id,
           visit_concept_id_flag,
           state_reg,
           cnty_reg)
}

#' @md number of immunizations
#'
#' @param imm_tbl_narrowed cohort of patients with immunizations
#'
#' @return a tbl that contains person id and the
#' number of distinct dates an immunization was given
#'

compute_imm_num <- function(imm_tbl_narrowed) {
  
  imm_num <-
    imm_tbl_narrowed %>%
    group_by(site,person_id) %>%
    summarise(imm_ct_dates=n_distinct(immunization_date)) %>%
    ungroup()
}



#'
#' @md create immunization metadata
#'
#' @param imm_tbl_narrowed tbl with just immunizations of interest
#'
#' @return
#'


compute_imm_metadata <- function(imm_tbl_narrowed) {
  
  imm_num <- compute_imm_num(imm_tbl_narrowed = imm_tbl_narrowed)
  
  imm_first <- compute_first_imm(imm_tbl_narrowed = imm_tbl_narrowed) %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id'))
  
  imm_tbl_narrowed %>%
    distinct(person_id) %>%
    left_join(imm_num) %>%
    left_join(imm_first)
  
}


#' @md date of second and third dose for patients who had a second dose
#' @param immunization_metadata from compute_imm_metadata
#' @param imm_tbl_narrowed immunization lookup table
#' 
#' @return the immunization_metadata table with an additional column including imm_date_second

compute_second_third_imm <- function(imm_tbl_narrowed,
                                     imm_metadata) {
  
  second_imm <- imm_tbl_narrowed %>%
    left_join(select(imm_metadata,
                     person_id,
                     imm_date_first), by='person_id') %>%
    filter(!(immunization_date==imm_date_first)) %>%
    group_by(person_id) %>% 
    slice_min(immunization_date, with_ties=FALSE) %>%
    ungroup() %>%
    rename(imm_date_second = immunization_date) %>%
    mutate(imm_date_diff = imm_date_second - imm_date_first) %>%
    mutate(imm_date_diff_grp = case_when(imm_date_diff < 28 ~ '01_1-27',
                                         imm_date_diff < 61 ~ '02_28-60',
                                         imm_date_diff < 91 ~ '03_61-90',
                                         imm_date_diff < 121 ~ '04_91-120',
                                         imm_date_diff < 151 ~ '05_121-150',
                                         imm_date_diff < 181 ~ '06_151-180',
                                         imm_date_diff < 211 ~ '07_181-210',
                                         imm_date_diff < 241 ~ '08_211-240',
                                         imm_date_diff < 271 ~ '09_241-270',
                                         imm_date_diff < 301 ~ '10_271-300',
                                         imm_date_diff < 331 ~ '11_301-330',
                                         imm_date_diff < 365 ~ '12_331-364',
                                         imm_date_diff >= 365 ~ '13_365plus')) %>%
    mutate(
      imm_manufact_cat_second = case_when(str_detect(lower(imm_manufacturer),'johnson|janssen') ~ 'Johnson and Johnson',
                                          str_detect(lower(imm_manufacturer),'pfizer') ~ 'Pfizer',
                                          str_detect(lower(imm_manufacturer),'moderna') ~ 'Moderna',
                                          str_detect(lower(imm_manufacturer),'astrazeneca') ~ 'AstraZeneca',
                                          str_detect(lower(imm_manufacturer),'unknown') ~ 'Unknown',
                                          is.na(imm_manufacturer) ~ 'NA',
                                          TRUE ~ 'Other'
      ))%>% 
    select(person_id,
           imm_date_second,
           imm_date_diff,
           imm_date_diff_grp, imm_manufact_cat_second) %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id'))
  
  third_imm <- imm_tbl_narrowed %>%
    left_join(select(second_imm,
                     person_id,
                     imm_date_second), by='person_id') %>%
    filter(!(immunization_date<=imm_date_second)) %>%
    group_by(person_id) %>% 
    slice_min(immunization_date, with_ties=FALSE) %>%
    ungroup() %>%
    rename(imm_date_third = immunization_date) %>%
    mutate(imm_date_diff2 = imm_date_third - imm_date_second) %>%
    mutate(imm_date_diff2_grp = case_when(imm_date_diff2 < 28 ~ '01_1-27',
                                          imm_date_diff2 < 61 ~ '02_28-60',
                                          imm_date_diff2 < 91 ~ '03_61-90',
                                          imm_date_diff2 < 121 ~ '04_91-120',
                                          imm_date_diff2 < 151 ~ '05_121-150',
                                          imm_date_diff2 < 181 ~ '06_151-180',
                                          imm_date_diff2 < 211 ~ '07_181-210',
                                          imm_date_diff2 < 241 ~ '08_211-240',
                                          imm_date_diff2 < 271 ~ '09_241-270',
                                          imm_date_diff2 < 301 ~ '10_271-300',
                                          imm_date_diff2 < 331 ~ '11_301-330',
                                          imm_date_diff2 < 365 ~ '12_331-364',
                                          imm_date_diff2 >= 365 ~ '13_365plus')) %>%
    mutate(
      imm_manufact_cat_third = case_when(str_detect(lower(imm_manufacturer),'johnson|janssen') ~ 'Johnson and Johnson',
                                         str_detect(lower(imm_manufacturer),'pfizer') ~ 'Pfizer',
                                         str_detect(lower(imm_manufacturer),'moderna') ~ 'Moderna',
                                         str_detect(lower(imm_manufacturer),'astrazeneca') ~ 'AstraZeneca',
                                         str_detect(lower(imm_manufacturer),'unknown') ~ 'Unknown',
                                         is.na(imm_manufacturer) ~ 'NA',
                                         TRUE ~ 'Other'
      ))%>% 
    select(person_id,
           imm_date_third,
           imm_date_diff2,
           imm_date_diff2_grp,
           imm_manufact_cat_third) %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id'))
  
  second_third_imm <- second_imm %>%
    left_join(third_imm, by='person_id') %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id'))
}




#' @md age at first immunization with metadata around timing of immunization availability
#'
#' @param imm_tbl_narrowed the cohort of patients with immunizations
#' @param person_tbl the pedsnet person tbl
#'
#' @return a tbl with the following columns:
#'
#' `person_id` | `birth_date` | `imm_age_first` | `imm_date_first` | `imm_age_first_cat` | `imm_date_flag` | `imm_age_5to17` | `visit_concept_id_flag` | `visit_concept_id` | `state_reg` | `cnty_reg`

compute_first_imm <- function(imm_tbl_narrowed,
                              person_tbl=cdm_tbl('person'),
                              visit_occurrence_tbl=cdm_tbl('visit_occurrence'),
                              location_tbl=cdm_tbl('location')) {
  
  person_tbl_narrowed <- select(person_tbl,person_id,provider_id,birth_date,location_id)
  visit_occurrence_tbl_narrowed <- select(visit_occurrence_tbl,visit_occurrence_id,visit_concept_id)
  location_tbl_narrowed <- select(location_tbl,location_id,state,county)
  
  #Try to get the minimum imm date for each person; if multiple minimum immunization dates, pick the lowest immunization id
  first_imm <-
    imm_tbl_narrowed %>%
    group_by(person_id,immunization_date) %>%
    mutate(min_imm_id=min(immunization_id)) %>%
    ungroup() %>%
    group_by(person_id) %>%
    mutate(imm_date_first=min(immunization_date)) %>%
    filter(immunization_date==imm_date_first) %>%
    filter(immunization_id==min_imm_id) %>%
    ungroup() %>%
    select(immunization_id,person_id,imm_date_first,visit_occurrence_id,imm_manufacturer) %>%
    compute_new(temporary=TRUE,
                indexes=list('person_id','visit_occurrence_id'))
  
  first_imm_meta <-
    first_imm %>%
    inner_join(person_tbl_narrowed,by='person_id') %>%
    left_join(visit_occurrence_tbl_narrowed, by='visit_occurrence_id') %>%
    left_join(location_tbl_narrowed,by='location_id') %>%
    mutate(imm_age_first=round((imm_date_first-birth_date)/365.25,2)) %>%
    mutate(
      imm_age_first_cat=
        case_when(imm_age_first < 1 ~ '<1',
                  imm_age_first < 5 ~ '1-4',
                  imm_age_first < 12 ~ '5-11',
                  imm_age_first < 16 ~ '12-15',
                  imm_age_first < 21 ~ '16-20',
                  TRUE ~ 'old')
    ) %>%
    mutate(
      imm_age_5to17=
        case_when(imm_age_first < 5 ~ '<1-4',
                  imm_age_first <18 ~ '5-17',
                  TRUE ~ '18+')
    ) %>%
    mutate(
      imm_date_flag=
        case_when(
          imm_date_first < '2020-01-01' ~ 'Date implausibly early',
          imm_date_first < '2020-12-14' ~ 'Date prior to availability',
          imm_date_first < '2021-03-17' ~ 'Date: ages 18+ eligible',
          # imm_date_first < '2021-04-19' ~ 'Date: ages 18+ eligible',
          imm_date_first < '2021-05-10' ~ 'Date: ages 16+ eligible',
          imm_date_first < '2021-10-29' ~ 'Date: ages 12+ eligible',
          imm_date_first < '2021-06-18' ~ 'Date: ages 5+ eligible',
          imm_date_first < '2022-12-31' ~ 'Date: ages 6 months+ eligible',
          imm_date_first >= '2022-12-31' ~ 'Date too late'
        )
    ) %>%
    mutate(
      visit_concept_id_flag=
        case_when(visit_concept_id == 9201L ~ 'Inpatient Hospital Stay',
                  visit_concept_id == 9202L ~ 'Ambulatory/Outpatient Visit (With a Physician)',
                  visit_concept_id == 9203L ~ 'Emergency Department',
                  visit_concept_id == 581399L ~ 'Interactive Telemedicine Service',
                  visit_concept_id == 42898160L ~ 'Long Term Care Visit',
                  visit_concept_id == 44814710L ~ 'Non-Acute Institutional',
                  visit_concept_id == 44814649L ~ 'Other',
                  visit_concept_id == 44814711L ~ 'Other Ambulatory Visit (OA)',
                  visit_concept_id == 2000000048L ~ 'Emergency Department Admit to Inpatient Hospital Stay',
                  visit_concept_id == 2000000088L ~ 'Observation Visit',
                  visit_concept_id == 2000000104L ~ 'Administrative Visit',
                  visit_concept_id == 2000000469L ~ 'Outpatient Non Physician'
        )
    ) %>%
    mutate(
      visit_type_flag=
        case_when(visit_concept_id %in% c(9201L, 2000000048L, 2000000088L) ~ 'Inpatient',
                  visit_concept_id %in% c(9202L, 581399L) ~ 'Outpatient',
                  visit_concept_id %in% c(9203L) ~ 'ED',
                  TRUE ~ 'Other')
    ) %>%
    mutate(
      state_reg = case_when(nchar(state)==2 ~ state,
                            str_detect(lower(state),'pennsylvania') ~ 'PA',
                            str_detect(lower(state),'new jersey') ~ 'NJ',
                            str_detect(lower(state),'delaware') ~ 'DE')
    ) %>%
    mutate(
      cnty_reg = case_when(nchar(county)>0 ~ str_replace(county," County","") %>%
                             paste0(" County") %>%
                             str_to_title(),
                           nchar(county)==0 ~ "")
    ) %>%
    mutate(
      imm_manufact_cat = case_when(str_detect(lower(imm_manufacturer),'johnson|janssen') ~ 'Johnson and Johnson',
                                   str_detect(lower(imm_manufacturer),'pfizer') ~ 'Pfizer',
                                   str_detect(lower(imm_manufacturer),'moderna') ~ 'Moderna',
                                   str_detect(lower(imm_manufacturer),'astrazeneca') ~ 'AstraZeneca',
                                   str_detect(lower(imm_manufacturer),'unknown') ~ 'Unknown',
                                   is.na(imm_manufacturer) ~ 'NA',
                                   TRUE ~ 'Other'
      )
    ) %>%
    select(person_id,
           birth_date,
           imm_age_first,
           imm_date_first,
           imm_age_first_cat,
           imm_age_5to17,
           imm_date_flag,
           imm_manufacturer,
           imm_manufact_cat,
           visit_concept_id,
           visit_concept_id_flag,
           visit_type_flag,
           state_reg,
           cnty_reg)
}


imput_immu_index <- function(cohort, immu_first, immu_second_third, study_start_date, study_end_date){
  
  ## combine the input cohort (first, second)
  cohort_full <- cohort%>%rename(covid_visit_date = cohort_entry_date)%>% 
    left_join(immu_first%>%select(-birth_date), by = "person_id") %>% left_join(immu_second_third, by = "person_id")
  
  ## read the visit_occurrence table
  visit_occurrence_tbl=cdm_tbl('visit_occurrence')
  
  ## add a column for the imm_date_first missing status
  cohort_full_with_missing_ind <- cohort_full %>%
    select(person_id, imm_date_first,covid_visit_date,test_result,visit_concept_id_flag,visit_type_flag,event_type) %>% # select first immu date
    left_join(visit_occurrence_tbl, by='person_id') %>% # combine with the occurrence table
    mutate(imm_date_missing_indicator = case_when(!is.na(imm_date_first) ~ "available",
                                                  is.na(imm_date_first) ~ "missing"))
  
  ### Patients who have imm_date_first
  ## make sure imm_first_date within the timeframe
  cohort_imm_available <- cohort_full_with_missing_ind %>%
    filter(imm_date_missing_indicator == "available") %>%
    filter(as.Date(imm_date_first) <= as.Date(study_end_date))  %>%
    filter(as.Date(imm_date_first) >= as.Date(study_start_date))  %>%
    as.data.frame()
  # length(unique(cohort_imm_available$person_id)) 
  
  ### Patients who do NOT have imm_date_first
  cohort_imm_missing <- cohort_full_with_missing_ind %>%
    filter(imm_date_missing_indicator == "missing") %>%
    as.data.frame()
  # length(unique(cohort_imm_missing$person_id)) 
  
  
  #### cmobined two datasets above
  cohort_clean_full <- bind_rows(cohort_imm_available, cohort_imm_missing) 
  
  # change the ngative patients' covid_visit_date to NA
  cohort_clean_full[which(cohort_clean_full$test_result == "negative"),]$covid_visit_date = NA
  
  #### --------------------------------------------------------------------------------------
  ### Step 1: save the first immu date to be imputed for each month based on vaccinated people with imm_date_first
  imm_positive_group <- cohort_clean_full %>%
    filter((test_result == "positive" & !is.na(imm_date_first))) %>%
    filter(as.Date(covid_visit_date) > as.Date(imm_date_first))
  # length(unique(imm_positive_group$person_id))
  
  imm_negative_group <- cohort_clean_full %>%
    filter(!is.na(imm_date_first) & test_result == "negative")
  # length(unique(imm_negative_group$person_id))
  
  immu_group <- bind_rows(imm_positive_group, imm_negative_group) 
  
  
  #### --------------------------------------------------------------------------------------
  ### Step 2: count table (which summarize the weights of months from the imm_group's first imm date)
  count_table <- immu_group %>%
    select(person_id, imm_date_first)  %>%
    distinct() %>%
    mutate(month = floor_date(imm_date_first,"month")) %>%
    select(month) %>%
    as.data.frame()
  output_tab = as.data.frame(table(count_table))
  output_tab$weights = output_tab$Freq/sum(output_tab$Freq)
  # output_tab$month = c(1:(dim(output_tab)[1]))
  output_tab$month = c(month(study_start_date):month(study_end_date))
  output_tab%>%output_tbl("weights_table") # data.frame cannot be combined with the tbl, so we have to save the data.frame then read it back again
  weights <- results_tbl("weights_table")
  

  #### --------------------------------------------------------------------------------------
  ### Step 3: count month index for unvaccinated patients 
  imm_no_negative_group <- cohort_clean_full %>%  
    filter(as.Date(visit_start_date) <= as.Date(study_end_date)) %>% # this constraint will be used to count the total number of visits during imputation
    filter(as.Date(visit_start_date) >= as.Date(study_start_date)) %>%
    filter((is.na(imm_date_first) & test_result == "negative"))
  # length(unique(imm_no_negative_group$person_id)) 
  
  imm_no_positive_group <- cohort_clean_full %>% 
    filter(as.Date(visit_start_date) <= as.Date(study_end_date)) %>% # this constraint will be used to count the total number of visits during imputation
    filter(as.Date(visit_start_date) >= as.Date(study_start_date)) %>%
    filter((test_result == "positive" & is.na(imm_date_first))|
              (test_result == "positive" & (as.Date(covid_visit_date) + days(28) < as.Date(imm_date_first))))
  # length(unique(imm_no_positive_group$person_id)) 
  
  imm_no_group <- bind_rows(imm_no_positive_group, imm_no_negative_group) 
    
  
  # select and imput dates for patients without vaccination dates
  month_index_data <- imm_no_group %>% 
    mutate(month_index = (floor_date(visit_start_date,"month"))) %>% # extract the month
    group_by(person_id, month_index) %>% # prepare to count the month to recalculate the weights
    mutate(count = n()) %>% # count number of visits within month
    select(person_id, visit_start_date,month_index, count,imm_date_first,covid_visit_date, test_result, event_type) %>%
    as.data.frame() 
  month_index_data$month <- month(month_index_data$month_index)
  month_index_data_new <- left_join(month_index_data, output_tab, by = "month")
  month_index_data_new$new_weights <- month_index_data_new$weights/month_index_data_new$count
  set.seed(1234)
  month_index_data_new_sampled <- month_index_data_new  %>% group_by(person_id) %>% sample_n(1, weight = new_weights)  %>%
    rename(empirical_sample_visit_date=visit_start_date) 
  month_index_data_new_final<- month_index_data_new_sampled %>% 
    mutate(imputation_status = "yes") 
  
  # add criteria for positive patients (empirical dates should be smaller than covid visit date)
  month_index_data_new_final_negative <- month_index_data_new_final%>% filter(test_result == "negative") 
  month_index_data_new_final_positive <- month_index_data_new_final %>% filter(test_result == "positive") %>% 
    filter(as.Date(empirical_sample_visit_date) < as.Date(covid_visit_date)) 
  month_index_data_new_final_comb <- bind_rows(month_index_data_new_final_negative, month_index_data_new_final_positive)
  imm_no_group_updated <- left_join(month_index_data_new_final_comb, imm_no_group)
  
  
  cohort_full_new <- bind_rows(imm_no_group_updated, immu_group)
  cohort_full_new %>% output_tbl("cohort_full_updated")
  cohort_full_new_tbl <- results_tbl("cohort_full_updated")
  
  
  cohort_full_final <- cohort_full_new_tbl %>%
    select(person_id,empirical_sample_visit_date,imputation_status,imm_date_first,imm_date_missing_indicator,
           test_result,visit_concept_id_flag,visit_type_flag,covid_visit_date,event_type) %>%
    left_join(visit_occurrence_tbl, by='person_id') %>%
    group_by(person_id) %>%
    slice_sample(visit_start_date, n=1) %>%
    ungroup() %>%
    mutate(empirical_sample_visit_concept_id_flag=
             case_when(visit_concept_id == 9201L ~ 'Inpatient Hospital Stay',
                       visit_concept_id == 9202L ~ 'Ambulatory/Outpatient Visit (With a Physician)',
                       visit_concept_id == 9203L ~ 'Emergency Department',
                       visit_concept_id == 581399L ~ 'Interactive Telemedicine Service',
                       visit_concept_id == 42898160L ~ 'Long Term Care Visit',
                       visit_concept_id == 44814710L ~ 'Non-Acute Institutional',
                       visit_concept_id == 44814649L ~ 'Other',
                       visit_concept_id == 44814711L ~ 'Other Ambulatory Visit (OA)',
                       visit_concept_id == 2000000048L ~ 'Emergency Department Admit to Inpatient Hospital Stay',
                       visit_concept_id == 2000000088L ~ 'Observation Visit',
                       visit_concept_id == 2000000104L ~ 'Administrative Visit',
                       visit_concept_id == 2000000469L ~ 'Outpatient Non Physician'
             ))%>%
    mutate(
      empirical_sample_visit_type_flag=
        case_when(visit_concept_id %in% c(9201L, 2000000048L, 2000000088L) ~ 'Inpatient',
                  visit_concept_id %in% c(9202L, 581399L) ~ 'Outpatient',
                  visit_concept_id %in% c(9203L) ~ 'ED',
                  TRUE ~ 'Other')
    )%>%
    compute_new(index='person_id')

  
  ## obtain final table
  colnames_to_select <- c(read.csv("./specs/imm_table_column_names.csv")[,1])
  cohort_full_final_full_list <- cohort_full_final %>% select(person_id, imputation_status, 
                                                              empirical_sample_visit_date,
                                                              empirical_sample_visit_concept_id_flag,
                                                              empirical_sample_visit_type_flag)  %>% left_join(cohort_full)
  cohort_full_output <- cohort_full_final_full_list %>% 
    mutate(imm_index_date = case_when(!is.na(imputation_status) ~ empirical_sample_visit_date,
                                      is.na(imputation_status) ~ imm_date_first))%>%
    mutate(imm_visit_concept_id_flag = case_when(!is.na(imputation_status) ~ empirical_sample_visit_concept_id_flag,
                                                 is.na(imputation_status) ~ visit_concept_id_flag))%>%
    mutate(imm_visit_type_flag = case_when(!is.na(imputation_status) ~ empirical_sample_visit_type_flag,
                                           is.na(imputation_status) ~ visit_type_flag)) 

}



###########################################################################################3
## outcomes

#' Function to find any ICU event in relation to an cohort_entry_date
#' @param adt_tbl table with adt_occurrences, defaulting to the cdm table adt_occurrence
#' @param cohort_tbl table with all person_ids for cohort, including their cohort_entry_date
#' @return table with NICU, PICU, CICU occurrences of any kind along with adt_type_concept_id and adt_date
find_icu <- function(adt_tbl = cdm_tbl('adt_occurrence'),
                     cohort_tbl) {
  cohort_tbl %>% select(person_id, covid_visit_date) %>%
    inner_join(adt_tbl, by = 'person_id') %>%
    filter(service_concept_id %in% c(2000000079L,2000000080L,2000000078L)) %>%
    select(person_id, adt_date, covid_visit_date, adt_type_concept_id) 
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
  cohort_tbl %>% select(person_id, covid_visit_date) %>%
    inner_join(visit_tbl, by = 'person_id') %>%
    filter(visit_concept_id %in% !!hosp_visit_types) %>%
    select(person_id, visit_start_date, covid_visit_date, visit_concept_id)
}

make_icu_flag<-function(cohort_tbl, days_min=-7, days_max=13){
  cohort_icu<-cohort_tbl %>%
    find_icu(cohort_tbl=.) %>%
    filter(adt_date>=covid_visit_date+days(!!days_min), adt_date<covid_visit_date+days(!!days_max)) %>%
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


make_hosp_flag<-function(cohort_tbl, days_min=-7, days_max=13){
  cohort_hosp<-cohort_tbl %>%
    find_hosp(cohort_tbl=.) %>%
    filter(visit_start_date>=covid_visit_date+days(!!days_min), visit_start_date<covid_visit_date+days(!!days_max)) %>%
    mutate(hosp_flag=1) %>%
    distinct(person_id, hosp_flag) %>%
    compute_new(index="person_id")
  
  cohort_tbl<-cohort_tbl %>%
    left_join(cohort_hosp, by="person_id") %>%
    mutate(hosp_flag=case_when(is.na(hosp_flag)~0,
                               TRUE~hosp_flag)) %>%
    compute_new(index="person_id") %>%
    return(cohort_tbl)
}


get_pasc_dx<-function(cohort, observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                      max_date="2022-10-01"){
  obs_der_tbl<-observation_derivation_recover%>% filter(observation_date<max_date)
  
  u09_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001520L)
  
  first_u09 <- u09_dx%>% 
    get_test_type_and_loc() %>% 
    distinct(person_id, observation_date, site, event_type, event_loc) %>%
    group_by(person_id) %>% 
    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup() %>% 
    compute_new(indices="person_id")  # n=1759
  
  
  b94_dx<-(get_b94_dx() ) %>% 
    rename(observation_date = cohort_entry_date)%>%
    compute_new(index="person_id")
  b94_dx <- b94_dx %>% mutate(observation_date = as.Date(observation_date))
  
  
  first_b94 <- b94_dx%>% 
    distinct(person_id, observation_date) %>%
    group_by(person_id) %>% 
    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup() %>% 
    compute_new(indices="person_id")  # n=1425
  
  
  pasc_dx<-first_u09 %>%
    dplyr::union(first_b94) %>%select(person_id,observation_date)%>%
    compute_new(index="person_id")
  
  cohort_dx <- cohort %>% inner_join(pasc_dx, by = "person_id") %>%
    filter(observation_date>=covid_visit_date+days(28), observation_date<=covid_visit_date+days(179))%>%
    select(person_id, observation_date) %>%
    group_by(person_id) %>% summarize_all(min)
  
  cohort%>%left_join(cohort_dx)%>%mutate(post_pasc_dx=case_when(
    is.na(observation_date)~0,
    TRUE~1
  )) %>% select(-observation_date)%>%
    compute_new(index="person_id")
  
}


get_misc_dx<-function(cohort, observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                      max_date="2022-10-01"){
  
  misc_dx<-observation_derivation_recover%>% filter(observation_date<max_date) %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==703578L)
  
  first_misc <- misc_dx%>% 
    get_test_type_and_loc() %>% 
    distinct(person_id, observation_date, site, event_type, event_loc) %>%
    group_by(person_id) %>% 
    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup() %>% select(person_id,observation_date)%>%
    compute_new(indices="person_id")   # n=1717
  
  # updated by Jessie. The old version will give multiple same rows for a single person
  cohort_misc_dx <- cohort %>% inner_join(misc_dx, by = "person_id") %>%
    filter(observation_date>=covid_visit_date+days(28), observation_date<=covid_visit_date+days(179))%>%
    select(person_id, observation_date) %>%
    group_by(person_id) %>% summarize_all(min)
  
  
  cohort%>%left_join(cohort_misc_dx)%>%mutate(post_misc_dx=case_when(
    is.na(observation_date)~0,
    TRUE~1
  )) %>% select(-observation_date)%>%
    compute_new(index="person_id")
  
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
get_serology_positives<-function(observation_derivation_recover=cdm_tbl("observation_derivation_recover"), max_date="2022-10-01"){
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

join_cohort_demo<-function(cohort, max_date="2022-10-01"){
  cohort_demo<- cohort %>% 
    select(-birth_date)%>%
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
    b94_src_val_exclude=results_tbl('b94_src_val_exclude')){
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
get_pasc_dx_flag<-function(cohort, max_date="2022-10-01"){
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


# Function to get first infection for all patients
# (1) get a cohort with all positive covid records
# (1.1) get a cohort with the earlies covid records
# (2) get a cohort with all long covid records (pasc/b94/misc)
# (2.1) get the earliest long covid
# (3) separate the cohort to (a) covid then pasc, (b) covid date=pasc date, (c) pasc then/no covid 
# (3.1) kernel estimator from (a)
# (3.2) impute covid date for (c)
# (4) combine cohorts from (1.1) and (3.2)


get_covid_positives_impute<-function(observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                                     require_covid_inpatient_ed=FALSE, max_date="2022-11-30",
                                     min_date = "2022-01-01"){
  
  #(1)
  obs_der_tbl<-observation_derivation_recover%>% 
    filter(observation_date<=max_date) %>%
    filter(observation_date>=min_date)
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
  }else{
    covid_dx=covid_dx_temp
  }
  
  
  
  # cohort with any positive covid test or diagnosis
  positive_cohort<-(pcr_positive %>%
                      dplyr::union(antigen_positive) %>%
                      dplyr::union(serology_positive) %>% 
                      dplyr::union(covid_dx)) %>%
    get_test_type_and_loc() %>%
    compute_new(indices=list(c("person_id", "visit_occurrence_id", "observation_id")))
  
  # (1.1) get earliest occurence of covid 
  positive_first<-positive_cohort %>% 
    group_by(person_id) %>% 
    slice_min(observation_date, with_ties=FALSE) %>%
    ungroup() %>% 
    rename(cohort_entry_date=observation_date) %>% 
    distinct(person_id, cohort_entry_date, site, event_type, event_loc) %>%
    compute_new(indices=c("person_id", "visit_occurrence_id"))
  
  # (2) cohort with pasc, misc diagnosis
  pasc_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==2000001520L)
  
  misc_dx<-obs_der_tbl %>%
    filter(observation_concept_id==2000001527L, value_as_concept_id==703578L)
  
  b94_dx= get_b94_dx() %>%filter(!is.na(cohort_entry_date))%>% # removing missing date
    compute_new(index="person_id")
  
  
  pasc_cohort<- pasc_dx %>% 
    dplyr::union(misc_dx) %>% 
    get_test_type_and_loc() %>%
    rename(cohort_entry_date=observation_date) %>%
    distinct(person_id, cohort_entry_date, site, event_type, event_loc) %>%
    compute_new(indices=list(c("person_id", "visit_occurrence_id")))
  
  pasc_cohort <- pasc_cohort%>%dplyr::union(b94_dx)
  
  # (2.1) get earliest occurence of pasc, misc, b94 (4219 subjects have long covid dx)
  pasc_first_date<-pasc_cohort %>% 
    group_by(person_id) %>% 
    slice_min(cohort_entry_date, with_ties=FALSE) %>%
    ungroup() %>% 
    rename(observation_date = cohort_entry_date)%>%
    distinct(person_id, observation_date, site, event_type, event_loc) %>%
    compute_new(indices=c("person_id", "visit_occurrence_id"))
  
  # check if missing date
  #pasc_first_date%>%filter(is.na(observation_date))
  
  ############################################################################
  # (3)
  # cohort if covid followed with pasc
  cohort_cv_pasc <- pasc_first_date%>%
    left_join(positive_first %>%select(person_id, cohort_entry_date))%>%
    filter(cohort_entry_date<observation_date)
  
  
  # cohort if two dates are equal
  cohort_equal <- pasc_first_date%>%
    left_join(positive_first %>%select(person_id, cohort_entry_date))%>%
    filter(cohort_entry_date==observation_date)
  
  
  # cohort needs to impute the entry date
  cohort_impute <- pasc_first_date%>%
    left_join(positive_first %>%select(person_id, cohort_entry_date))%>%
    anti_join(cohort_cv_pasc)%>%anti_join(cohort_equal)
  n_impute<- count(cohort_impute)%>%collect_new
  n_impute<- as.numeric(n_impute)
  
  # (3.1) prepare the distribution to sample (kernel smoothing in a bounded range)
  date_diff <- cohort_cv_pasc%>%collect_new()
  date_diff0 <- as.numeric(difftime(date_diff$observation_date,
                                    date_diff$cohort_entry_date), units="days")
  date_diff <- date_diff0[(date_diff0>27&date_diff0<180)]
  #dd_density <- kde.boundary(x=date_diff, xmin=c(28), xmax=c(179), boundary.kernel="beta")
  dd_density <- kde.boundary(x=date_diff,binned = TRUE)
  
  # (3.2) sample dates (remove dates outside 28~179)
  dd_impute<- round(rkde(2*n_impute,dd_density))
  dd_impute<- dd_impute[(dd_impute>27&dd_impute<180)]
  dd_impute <-dd_impute[1:n_impute]
  
  cohort_impute <- cohort_impute%>%collect_new()
  cohort_impute$dd_impute <- dd_impute
  cohort_impute$observation_date_impute <- as.Date(cohort_impute$observation_date)-cohort_impute$dd_impute
  
  cohort_impute%>%output_tbl("vaccine_pasc_impute",index = "person_id")
  
  cohort_impute<- results_tbl("vaccine_pasc_impute")%>%
    select(-c(observation_date,cohort_entry_date,dd_impute))%>%
    rename(cohort_entry_date=observation_date_impute)
  
  
  # add cohort_impute to positive_first
  cohort_covid <- (positive_first%>%anti_join(cohort_impute, by = "person_id"))
  cohort_full <- cohort_covid%>%dplyr::union(cohort_impute)
  
  return(cohort_full)
}

# Function to get the negative cohort defined as patients with a visit in study period 
#  and not in the positive cohort

get_covid_negatives_any_visit<-function(observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                                        positive_cohort, study_start, study_end, max_date="2022-11-30"){
  
  obs_der_tbl<-observation_derivation_recover%>% 
    filter(observation_date<=max_date)%>% 
    filter(observation_date>"2020-03-01")
  
  # visit in study period
  negatives<-obs_der_tbl %>% 
    filter(observation_date>=study_start)%>% 
    filter(observation_date<=study_end)
  
  # remove test records for positive patients (positive by the end of start)
  positive_filtered <- positive_cohort %>%
    filter(cohort_entry_date<=study_end)
  
  negatives_filtered<-negatives %>%
    anti_join(positive_filtered, by="person_id") %>% 
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

# Function to get the negative cohort defined as patients with negative test in study period 
#  and not in the positive cohort

get_covid_negatives_test_negative<-function(observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                                            positive_cohort, study_start, study_end, max_date="2022-11-30"){
  
  obs_der_tbl<-observation_derivation_recover%>% 
    filter(observation_date<max_date)%>% 
    filter(observation_date>"2020-03-01")
  
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
  
  # negative test in study period
  negatives<-negatives %>% 
    filter(observation_date>=study_start)%>% 
    filter(observation_date<=study_end)
  
  # remove test records for positive patients (positive by the end of start)
  positive_filtered <- positive_cohort %>%
    filter(cohort_entry_date<=study_end)
  
  negatives_filtered<-negatives %>%
    anti_join(positive_filtered, by="person_id") %>% 
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

# Function to make cohort for each study period
# if any visit == TRUE, the negative cohort is defined as patients
#  with any visit in study period, 
#  if FALSE, patients with any covid negative test in study period


make_cohort <- function(positive_cohort, study_start, study_end, max_date = "2022-11-30",
                        any_visit = TRUE){
  positive_study <- positive_cohort%>% 
    filter(cohort_entry_date>=study_start)%>% 
    filter(cohort_entry_date<=study_end)
  if(any_visit == TRUE){
    negative_study <- get_covid_negatives_any_visit(positive_cohort = positive_cohort, 
                                                    study_start = study_start, 
                                                    study_end = study_end,
                                                    max_date = max_date)
  }else{
    negative_study <- get_covid_negatives_test_negative(positive_cohort = positive_cohort, 
                                                        study_start = study_start, 
                                                        study_end = study_end,
                                                        max_date = max_date)
  }
  positive_study <- positive_study%>%mutate(test_result = "positive")
  negative_study <-  negative_study%>%mutate(test_result="negative")
  
  study_cohort <- positive_study %>% dplyr::union(negative_study)
  
  return(study_cohort)
}



#' Find in-person visits near test date
#'
#' Given a cohort of patients with test results (see [get_tested()]), retrieve
#' all in-person visits..
#'
#' An outpatient visit is considerd to occur on the day of `visit_start_date`.
#' For inpatient visits, if `visit_end_date` is not present, it is set to the
#' current date (i.e. we presume the visit is ongoing).  The visit is included
#' if it ended no earlier than 7 days prior to the test date.
#'
#' @param cohort The cohort of patients for whom to find visits.  In addition to
#'   the `person_id`, this should have the test date in `observation_date`.
#' @param visit_tbl The visit data
#' @param adt_tbl The adt occurrence table
#'
#' @return A tbl of visit date, with the end date for inpatient visits
#'   potentially set as described above, a `visit_offset` column noting the
#'   days between the test date and the visit_start_date, an `icu_yn` flag if the 
#'   patient was admitted to the icu, the `icu_admit_date` and an `icu_lag` column noting the
#'   number of days the after the visit start date the patient was admitted to the icu.
#' @md
get_visits <- function(cohort,
                       visit_tbl = cdm_tbl('visit_occurrence'),
                       adt_tbl = cdm_tbl('adt_occurrence')
) {
  
  adt_vis <- cohort%>% ## question: Do we want to filter and restrict to inpatient visits? ##EB: Thinking we might be okay with a left join below? This is 
    inner_join(adt_tbl,by='person_id')%>%
    filter(service_concept_id %in% c(2000000079L,2000000080L,2000000078L))%>%
    group_by(person_id,visit_occurrence_id)%>%
    summarise(icu_admit_date=min(adt_date))%>%
    distinct()%>% ungroup() %>%
    mutate(icu_yn='Y')
  
  cohort_vis <- visit_tbl %>% inner_join(cohort, by = 'person_id')%>%
    left_join(adt_vis,by=c('visit_occurrence_id','person_id'))
  
  op_vis <- cohort_vis %>%
    filter(visit_concept_id %in% c(9202L, 9203L,581399L,44814711L) &
             visit_start_date >= observation_date - days(7) & ## question: any date before the covid positive test?
             visit_start_date <= observation_date + days(13)) %>%
    mutate(visit_offset = visit_start_date - observation_date) %>%
    mutate(visit_type = 
             case_when(visit_concept_id %in% c(9202L) ~ 'op',
                       visit_concept_id %in% c(9203L) ~ 'ed',
                       visit_concept_id %in% c(581399L) ~ 'telehealth',
                       visit_concept_id %in% c(44814711L) ~ 'oa'))
  
  ip_vis <- cohort_vis %>%
    filter(visit_concept_id %in% c(9201L, 2000000048L, 2000000088L)) %>%
    mutate(visit_offset = visit_start_date - observation_date,
           visit_end_date = case_when(!is.na(visit_end_date) ~ visit_end_date,
                                      TRUE ~ sql('current_date'))) %>%
    filter(visit_start_date >= observation_date - days(7) & visit_start_date <= observation_date + days(13)) %>%
    mutate(visit_type =
             case_when(visit_concept_id %in% c(9201L) ~ 'ip',
                       visit_concept_id %in% c(2000000048L) ~ 'ed/ip',
                       visit_concept_id %in% c(2000000088L) ~ 'obs'),
           los=visit_end_date-visit_start_date+1)
  
  combined <- 
    dplyr::union(ip_vis, op_vis) %>%
    dplyr::select(person_id, test_result, event_type, observation_date,visit_occurrence_id, visit_concept_id,
                  visit_start_date,visit_start_datetime, 
                  visit_end_date, visit_offset, 
                  visit_type,icu_yn,
) %>%
    mutate(icu_admit_lag=icu_admit_date-visit_start_date,
           icu_test_lag=icu_admit_date-observation_date)%>%
    distinct()
  
  neither <- cohort %>%
    anti_join(combined,
              by=c('person_id',
                   'observation_date')) %>%
    mutate(visit_type = 'none')
  
  dplyr::union(
    combined,
    neither
  )
}



