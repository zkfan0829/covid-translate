# This code is used to generate the summary table for booster infomation


## -----------------------------------------------------
## Step 1: read in the table
read_tbl_o5 <- results_tbl('omicron_age5_final_table')
read_tbl_o12 <- results_tbl('omicron_age12_final_table')
read_tbl_d12 <- results_tbl('delta_age12_final_table')

## -----------------------------------------------------
## Step 2: imm_manufacturer & categories
## -- Omicron age 5
read_tbl_o5 %>% mutate(count_na = !is.na(imm_manufacturer))%>%group_by(count_na)%>%summarise(n())
read_tbl_o5 %>%group_by(imm_manufacturer)%>%summarise(n())
read_tbl_o5 %>% mutate(count_na = !is.na(imm_manufact_cat))%>%group_by(count_na)%>%summarise(n())
read_tbl_o5 %>%group_by(imm_manufact_cat)%>%summarise(n())

## -- Omicron age 12 
read_tbl_o12 %>% mutate(count_na = !is.na(imm_manufacturer))%>%group_by(count_na)%>%summarise(n())
read_tbl_o12 %>% mutate(count_na = !is.na(imm_manufact_cat))%>%group_by(count_na)%>%summarise(n())

## -- Delta age 12
read_tbl_d12 %>% mutate(count_na = !is.na(imm_manufacturer))%>%group_by(count_na)%>%summarise(n())
read_tbl_d12 %>% mutate(count_na = !is.na(imm_manufact_cat))%>%group_by(count_na)%>%summarise(n())


## -----------------------------------------------------
## Step 3: generate second vaccine number
## -- Omicron age 5
read_tbl_o5 %>% mutate(count_na = !is.na(imm_date_second))%>%group_by(count_na)%>%summarise(n())

## -- Omicron age 12
read_tbl_o12 %>% mutate(count_na = !is.na(imm_date_second))%>%group_by(count_na)%>%summarise(n())

## -- Delta age 12
read_tbl_d12 %>% mutate(count_na = !is.na(imm_date_second))%>%group_by(count_na)%>%summarise(n())


## -----------------------------------------------------
## Step 4: generate vaccine manufacturer number for second vaccine
## -- Omicron age 5
read_tbl_o5 %>% mutate(count_na = !is.na(imm_manufact_cat_second))%>%group_by(count_na)%>%summarise(n())

## -- Omicron age 12
read_tbl_o12 %>% mutate(count_na = !is.na(imm_manufact_cat_second))%>%group_by(count_na)%>%summarise(n())

## -- Delta age 12
read_tbl_d12 %>% mutate(count_na = !is.na(imm_manufact_cat_second))%>%group_by(count_na)%>%summarise(n())


## -----------------------------------------------------
## Step 5: generate third vaccine number
## -- Omicron age 5
read_tbl_o5 %>% mutate(count_na = !is.na(imm_date_third))%>%group_by(count_na)%>%summarise(n())

## -- Omicron age 12
read_tbl_o12 %>% mutate(count_na = !is.na(imm_date_third))%>%group_by(count_na)%>%summarise(n())

## -- Delta age 12
read_tbl_d12 %>% mutate(count_na = !is.na(imm_date_third))%>%group_by(count_na)%>%summarise(n())


## -----------------------------------------------------
## Step 6: generate vaccine manufacturer number for third vaccine
## -- Omicron age 5
read_tbl_o5 %>% mutate(count_na = !is.na(imm_manufact_cat_third))%>%group_by(count_na)%>%summarise(n())

## -- Omicron age 12
read_tbl_o12 %>% mutate(count_na = !is.na(imm_manufact_cat_third))%>%group_by(count_na)%>%summarise(n())

## -- Delta age 12
read_tbl_d12 %>% mutate(count_na = !is.na(imm_manufact_cat_third))%>%group_by(count_na)%>%summarise(n())