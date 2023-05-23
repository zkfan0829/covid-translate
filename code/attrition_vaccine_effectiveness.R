## This is the code to calculate the attrition of the extract cohort (e.g., number of unvaccinated vs vaccinated kids)


## ------------ Step 1: read in data ------------
## Note: we listed all the three cohorts here. Please feel free to comment out 
read_tbl <- results_tbl('omicron_age12_imputed')
read_tbl <- results_tbl('delta_age12_imputed')
read_tbl <- results_tbl('omicron_age5_imputed')


## ------------ Step 2: add the vaccination status column to the data and calculate the attrition (two versions)------------
## Version 1: a rough categorization of vaccination status
read_tbl <- read_tbl %>% mutate(vaccination_status = case_when(
  
  test_result == "negative" & !is.na(imm_date_first) ~ "vaccinated control",
  test_result == "positive" & as.Date(covid_visit_date) > as.Date(imm_date_first)  ~ "vaccinated cases",
  
  (test_result == "positive" & as.Date(covid_visit_date) > as.Date(empirical_sample_visit_date))|
    (test_result == "positive" & is.na(imm_date_first)) ~ "unvaccinated cases",
  test_result == "negative" & is.na(imm_date_first) ~ "unvaccinated control"
))
read_tbl %>% group_by(vaccination_status) %>% summarise(n()) 

## Version 2: a more detailed categorization of vaccination status
read_tbl <- read_tbl%>% mutate(vaccination_status = case_when(
  test_result=="negative" & is.na(imm_date_first) ~ 'unvaccinated controls',
  test_result=="negative" & !is.na(imm_date_first) ~ 'vaccinated controls',
  test_result=="positive" & as.Date(covid_visit_date) - imm_date_first >= 28 ~ 'vaccinated cases',
  test_result=="positive" & as.Date(covid_visit_date) - imm_date_first < 28 & 
    as.Date(covid_visit_date) - imm_date_first >= 0 ~ 'partial cases',
  test_result=="positive" & as.Date(covid_visit_date) - cohort_entry_date >= 28 ~ 'unvaccinated cases',
  test_result=="positive" & as.Date(covid_visit_date) - cohort_entry_date < 28 ~ 'remove unvaccinated cases'
))
read_tbl %>% group_by(vaccination_status) %>% summarise(n()) 


## ------------ Step 3: some other summaries-----------
read_tbl %>% group_by(test_result) %>% summarise(n()) # positive vs negative
read_tbl %>% group_by(imputation_status) %>% summarise(n()) # yes or NA (the total number of yes = total number of unvaccinated)
read_tbl %>% group_by(person_severity_index) %>% summarise(n()) # PCR, serology, etc
read_tbl %>% group_by(event_type) %>% summarise(n())



## ------------ Step 4: summarize on booster-----------
read_tbl %>% group_by(imm_date_second) %>% summarise(n()) # number of patients who have second vaccination
read_tbl %>% group_by(imm_date_third) %>% summarise(n()) # number of patients who have third vaccination
read_tbl %>% group_by(imm_manufacturer) %>% summarise(n()) # table that summarizes the vaccine brands (e.g., J&J, Pfizer, Moderna)


