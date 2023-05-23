# Vector of additional packages to load before executing the request
config_append('extra_packages', c('tidyverse'))

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run  <- function() {

  message('Starting execution with framework version ',
          config('framework_version'))

  # Set up the step log with as many attrition columns as you need.
  # For example, this call sets up the log with a `persons` count that will be
  # required at each step.
  init_sum(cohort = 'Start', persons = 0)

  # By convention, accumulate execution results in a list rather than as
  # independent variables, in order to make returning the entire set easier
  rslt <- list()
  
  
  ## Get COVID positives (PCR, antigen, qualifying serology, and PASC, excluding MIS-C)
  rslt$positives<-get_covid_positives()
  
  rslt$negatives<-get_covid_negatives()
  
  ## Require visit in the 7 days to 18 months prior
  rslt$positives_history<-require_prior_encounter(rslt$positives)
  
  rslt$negatives_history<-require_prior_encounter(rslt$negatives)
  
  ## Append flag indicating patients who had a U09.9 or B94.8 diagnosis
  rslt$positives_pasc_dx_flag<-get_pasc_dx_flag(rslt$positives_history)
  
  ## For patients who don't have a U09.9 or B94.8 diagnosis,
  ## require a visit from a qualifying cluster in the 28 days to
  ## 179 days following cohort entry.
  rslt$positive_cohort<-require_follow_up_code(rslt$positives_pasc_dx_flag)
  
  rslt$positive_cohort %>%
    output_tbl("positive_cohort_temp", index="person_id")
  
  rslt$negatives_history %>%
    output_tbl("negative_cohort_temp", index="person_id")
  
  ## Get PMCA indices by body system
  ### cases
  cohort=results_tbl("positive_cohort_temp")
  pmca_lookup <- 
    produce_pmca_lookup(cohort=cohort) %>%
    output_tbl('pmca_lookup_pos',
               indexes=list('person_id',
                            'body_system',
                            'condition_concept_id',
                            'visit_occurrence_id'),
               db=TRUE,file=FALSE)
  
  pmca_summary <- 
    compute_pmca_summary(pmca_lookup_tbl=results_tbl('pmca_lookup_pos')) %>%
    output_tbl('pmca_summary_pos',
               indexes=list('person_id',
                            'body_system'))
  
  pmca_cats_lib <- 
    compute_pmca_cats_lib(pmca_summary_tbl = results_tbl('pmca_summary_pos')) %>%
    output_tbl('pmca_cats_lib_pos',
               indexes=list('person_id',
                            'cohort_entry_date')) 
  
  pmca_tbl<-results_tbl("pmca_lookup_pos") %>%
    mutate(temp_index=case_when(
      progressive %in% c("yes", "n/a")~2,
      TRUE~1
    )) %>% 
    group_by(person_id, body_system) %>%
    summarize(pmca_index=max(temp_index)) %>% 
    ungroup %>% 
    compute_new(index="person_id")
  
  
  cohort_pmca_temp<- cohort %>%
    left_join(
      pmca_tbl%>% 
        distinct(person_id, body_system, pmca_index),
      by="person_id"
    ) %>%
    mutate(pmca_index=case_when(is.na(pmca_index)~0, TRUE~pmca_index)) %>%
    pivot_wider(id_cols=
                  c(person_id, site, event_type, event_loc, cohort_entry_date, pasc_flag, code_occurrence_flag),
                names_from=body_system, values_from=pmca_index, values_fill=0, names_prefix="pmca_") %>% 
    select(-'pmca_NA') 
  
  cohort_pmca<-cohort_pmca_temp %>%
    replace_na(setNames(lapply(vector("list", ncol(cohort_pmca)-5), function(x) x <- 0), setdiff(colnames(cohort_pmca), 
                                                                                                 c("person_id", "site", "event_type", "event_loc", "cohort_entry_date")))) %>%
    compute_new(index="person_id")
  
  cohort_util<-cohort_pmca %>%
    get_util()
  
  output_tbl(cohort_util %>% rename(n_visits_past=n_visits), "positive_cohort_pmca", index="person_id")
  
  
  ### controls
  
  cohort_neg=results_tbl("negative_cohort_temp")
  pmca_lookup <- 
    produce_pmca_lookup(cohort=cohort_neg) %>%
    output_tbl('pmca_lookup_neg',
               indexes=list('person_id',
                            'body_system',
                            'condition_concept_id',
                            'visit_occurrence_id'),
               db=TRUE,file=FALSE)
  
  pmca_summary <- 
    compute_pmca_summary(pmca_lookup_tbl=results_tbl('pmca_lookup_neg')) %>%
    output_tbl('pmca_summary_neg',
               indexes=list('person_id',
                            'body_system'))
  
  pmca_cats_lib <- 
    compute_pmca_cats_lib(pmca_summary_tbl = results_tbl('pmca_summary_neg')) %>%
    output_tbl('pmca_cats_lib_neg',
               indexes=list('person_id',
                            'cohort_entry_date')) 
  
  pmca_tbl<-results_tbl("pmca_lookup_neg") %>%
    mutate(temp_index=case_when(
      progressive %in% c("yes", "n/a")~2,
      TRUE~1
    )) %>% 
    group_by(person_id, body_system) %>%
    summarize(pmca_index=max(temp_index)) %>% 
    ungroup %>% 
    compute_new(index="person_id")
  
  
  cohort_pmca_temp<- cohort_neg %>%
    left_join(
      pmca_tbl%>% 
        distinct(person_id, body_system, pmca_index),
      by="person_id"
    ) %>%
    mutate(pmca_index=case_when(is.na(pmca_index)~0, TRUE~pmca_index)) %>%
    pivot_wider(id_cols=
                  c(person_id, site, event_type, event_loc, cohort_entry_date),
                names_from=body_system, values_from=pmca_index, values_fill=0, names_prefix="pmca_") %>% 
    select(-'pmca_NA') 
  
  cohort_pmca<-cohort_pmca_temp %>%
    replace_na(setNames(lapply(vector("list", ncol(cohort_pmca_temp)-5), function(x) x <- 0), setdiff(colnames(cohort_pmca_temp), 
                                                                                                 c("person_id", "site", "event_type", "event_loc", "cohort_entry_date")))) %>%
    compute_new(index="person_id")
  
  
  cohort_util<-cohort_pmca %>%
    get_util()
  
  output_tbl(cohort_util %>% rename(n_visits_past=n_visits), "negative_cohort_pmca", index="person_id")
  
  
  

  
  
  
  ## Get cohort demographic info, filter for age<21, get icu flag
  rslt$positive_cohort_demo<-results_tbl("positive_cohort_pmca") %>%
    join_cohort_demo() %>%
    make_icu_flag()
  
  rslt$negative_cohort_demo<-results_tbl("negative_cohort_pmca") %>%
    join_cohort_demo() %>%
    make_icu_flag()
  
    
  ## Get binary indicator variables for presence of diagnosis
  ## in each cluster
  rslt$positive_cohort_visits<-get_cluster_visits(rslt$positive_cohort_demo)
  
  rslt$positive_cohort_visits %>%
    output_tbl("case_cohort", index="person_id")
  
  rslt$positive_cohort_addl_visits<-get_cluster_visits(rslt$positive_cohort_demo, cond_codeset=load_codeset('pasc_ccsr_codes'))
  
  rslt$positive_cohort_addl_visits %>% 
    output_tbl("case_cohort_addl_visits", index="person_id")
  
  rslt$negative_cohort_visits<-get_cluster_visits(rslt$negative_cohort_demo)
  
  rslt$negative_cohort_visits %>%
    output_tbl("control_cohort", index="person_id")
  
  rslt$negative_cohort_addl_visits<-get_cluster_visits(rslt$negative_cohort_demo, cond_codeset=load_codeset('pasc_ccsr_codes'))
  
  rslt$negative_cohort_addl_visits %>% 
    output_tbl("control_cohort_addl_visits", index="person_id")
  
#  pasc_ccsr_cats<-read.csv('./specs/pasc_ccsr_cats.csv') %>% 
#    select(ccsr_cat=PASC..CCSR.Category.1.)
#  
#  ccsr_icd_crosswalk<-read.csv("./specs/icd_ccsr_crosswalk2.csv") %>%
#    select(ccsr_cat_code=CCSR.CATEGORY.1, ccsr_cat_name=CCSR.CATEGORY.1..PASC..DESCRIPTION, icd_code=ICD.10.CM.Code) %>%
#    mutate(concept_code=case_when(
#      nchar(icd_code)==3~icd_code,
#      TRUE~paste(substr(icd_code, 1, 3), ".", substr(icd_code, 4, nchar(icd_code)), sep="")))
#  
#  pasc_ccsr_codes<-ccsr_icd_crosswalk %>% select(ccsr_cat_code, ccsr_cat_name, concept_code) %>%
#    output_tbl("pasc_ccsr_crosswalk", index="concept_code", temp=TRUE) %>% 
#    inner_join(vocabulary_tbl("concept") %>% filter(vocabulary_id=="ICD10CM") %>% select(concept_id, concept_code, concept_name), by="concept_code")
#    
#  pasc_ccsr_codes %>% 
#    select(concept_id, concept_code, concept_name, ccsr_cat_code, cluster=ccsr_cat_name) %>%
#    write.csv("./specs/pasc_ccsr_codes.csv", row.names=FALSE)
  

  
  
#  ccsr_icd_crosswalk<-read.csv("./specs/ccsr_icd_crosswalk.csv") %>% 
#    select(icd_code=ICD.10.CM.Code, ccsr_cat=CCSR.Category.Description)
    

  
  
  
  

  message('Done.')

  invisible(rslt)

}
