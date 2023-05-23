# This code is used to calculate the attrition related to second dose and booster
# Specifically, this is use to calculate the distribution of different brands
# Fact: in Penn data, most of the kids' 1st, 2nd, and 3rd doses are Pfizer (>90%), which makes the comparison between brands using Penn's data infeasible
# Goal: using Florida's data to check the brand distribution and feasibility of comparing the vaccine effectiveness of different brands

# Input: extracted cohorts (e.g., Omicron for age 5-12, Omicron for age 12-21, Delta for age 12 -21
# Output: tables that summarize the brands distribution

## ====================================================
## Step 1: read in the extracted cohort data
data = results_tbl("omicron_age5_final_table_paper1") # example, using omicron age5 group



## ====================================================
## Step 2: summarize the brand distribution for dose 1
# imm_age_first_cat column is the first dose vaccination brands. 
# In this column, the raw messy manufacturer labels, such as "PFIZER" or "pfizer" or "Pfizer covid-19" have been all categorised into "Pfizer"
# Please check if the messy labels have been categorized before calculate the attrition

data %>% 
  filter(vaccination_status == "vaccinated cases" |
           vaccination_status == "vaccinated controls") %>% 
  group_by(imm_manufact_cat) %>% 
  summarise(n())

# NOTE: if there is "NA", please check if you have any "source value" to map the messy labels to the existing brand caterories


## ====================================================
## Step 3: summarize the brand distribution for dose 2, conditioning on already got 1st dose

data %>% 
  filter(vaccination_status == "vaccinated cases" |
           vaccination_status == "vaccinated controls") %>% 
  filter(!is.na(imm_date_first) & !is.na(imm_date_second)) %>%
  group_by(imm_manufact_cat_second) %>% 
  summarise(n())


## ====================================================
## Step 4: summarize the brand distribution for dose 3, conditioning on already got 2nd dose

data %>% 
  filter(vaccination_status == "vaccinated cases" |
           vaccination_status == "vaccinated controls") %>% 
  filter(!is.na(imm_date_first) & !is.na(imm_date_second) & !is.na(imm_date_third)) %>%
  group_by(imm_manufact_cat_third) %>% 
  summarise(n())

# the outcome will look like:
# imm_manufact_cat    `n()`
# <chr>               <dbl>
# 1 Johnson and Johnson     2
# 2 Moderna                65
# 3 NA                  15116
# 4 Other                  54
# 5 Pfizer              40278
# 6 Unknown                10

