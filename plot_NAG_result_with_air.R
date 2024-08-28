# Load the OPGEE remote run results and clean it into a nice data frame
# originally for Jan 22 2024 run

# Author          Spencer Zhang
# Date            Jan 22 2024
# Last Updated    Jan 22 2024

# Updated         Mar 4 2024
#                 New run with pure aerial loss rates


library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(sf)
library(mapview)
library(tigris)
library(RColorBrewer)
library(leaflet)
library(leafpop) # for popup tables
library(openxlsx)

# run this only once
# Specify the range of columns to be removed
start_column <- "AREA"
end_column <- "AVG_SALE87" 

# Find the indexes of the start and end columns
start_index <- which(colnames(US_OIL) == start_column)
end_index <- which(colnames(US_OIL) == end_column)

# Create a sequence of these indexes
columns_to_remove <- seq(start_index, end_index)

# Remove the columns by excluding them
US_OIL <- US_OIL[ , -columns_to_remove]

# 0 Global variables and load functions ----
source("load_functions.R")
PATH = "~/GitHub/PhD/North-America-Gas-2021/NAG run 20240122/"

# 1 Gas runs ----
## 1.1 group gas run results ----
FILES = list.files(PATH, pattern = "^result_gas*") 

# test
df = read_excel(paste0(PATH, FILES[1]))
cleaned_df = clean_opgee_remote_run_result(df)
View(cleaned_df)

# official
cleaned_df = data.frame()
for (file in FILES){
  df = read_excel(paste0(PATH, file))
  cleaned = clean_opgee_remote_run_result(df)
  
  cleaned_df = rbind(cleaned_df, cleaned)
}
df_gas_result_with_air = cleaned_df %>%
  filter(`194 Lifecycle GHG emissions Total CO2 sequestered` == "OK")

length(unique(df_gas_result_with_air$`20 Field properties Field name NA`)) # 620

## 1.2 join result with input and coords ----

View(US_GAS_WITH_COMP) # check pure gas input sheet

df_gas_run_with_air_with_input <- US_GAS_WITH_COMP %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(unique(df_gas_result_with_air) %>%
              mutate(`20 Field properties Field name NA` = as.numeric(`20 Field properties Field name NA`)),
            by = c("GEOID" = "20 Field properties Field name NA")) %>%
  rename("CI_gCO2_MJ" = "192 Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ",
         "Check" = "194 Lifecycle GHG emissions Total CO2 sequestered")

View(df_gas_run_with_air_with_input)

## 1.3 Viz1: map CI ----
mapview(df_gas_run_with_air_with_input %>%
          mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ)),
        zcol = "CI_gCO2_MJ",
        popup = popupTable(df_gas_run_with_air_with_input,
                           zcol = c("CI_gCO2_MJ")))

length(unique(cleaned_df$`20 Field properties Field name NA`))
length(unique(US_GAS_WITH_COMP$GEOID))

# 2 Oil runs  ----
FILES = list.files(PATH, pattern = "^result_oil*") 
df = read_excel(paste0(PATH, FILES[1]))
cleaned_df = clean_opgee_remote_run_result(df)

View(cleaned_df)

df_oil_result_with_air = cleaned_df %>%
  filter(`194 Lifecycle GHG emissions Total CO2 sequestered` == "OK")

length(unique(df_oil_result_with_air$`20 Field properties Field name NA`)) # 674

## 1.2 join result with input and coords ----

View(US_OIL) # check pure gas input sheet

df_oil_result_with_air_with_input <- US_OIL %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(unique(df_oil_result_with_air) %>%
              mutate(`20 Field properties Field name NA` = as.numeric(`20 Field properties Field name NA`)),
            by = c("GEOID" = "20 Field properties Field name NA")) %>%
  rename("CI_gCO2_MJ" = "192 Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ",
         "Check" = "194 Lifecycle GHG emissions Total CO2 sequestered")

View(df_oil_result_with_air_with_input)

## 1.3 Viz1: map CI ----
mapview(df_oil_result_with_air_with_input %>%
          mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ)),
        zcol = "CI_gCO2_MJ",
        popup = popupTable(df_oil_result_with_air_with_input,
                           zcol = c("CI_gCO2_MJ")))

## 1.4 check error oil fields ----
# summary: 64 error fields; 0.117% gas production volume of the oil fields, we can ignore it!
df_oil_error = df_oil_result_with_air_with_input %>%
  filter(is.na(CI_gCO2_MJ))

nrow(df_oil_error) # 64

sum(df_oil_error$Annual_Gas) / sum(df_oil_result_with_air_with_input$Annual_Gas) # 0.117% we can ignore it!

## 1.5 oil result with input w/o error fields ----
df_oil_result_with_air_with_input_pure = df_oil_result_with_air_with_input %>%
  filter(!is.na(CI_gCO2_MJ))

nrow(df_oil_result_with_air_with_input_pure) # 674

# 3 Merge oil fields and gas fields ----

# check column names
ncol(df_gas_run_with_air_with_input) #318
ncol(df_oil_result_with_air_with_input_pure) #349

colnames(df_gas_run_with_air_with_input)
colnames(df_oil_result_with_air_with_input_pure)

sum(colnames(df_gas_run_with_air_with_input) %in% colnames(df_oil_result_with_air_with_input_pure)) # 302

# check which columns of gas result doesnt exist in oil result
# all normal; expected
colnames(df_gas_run_with_air_with_input)[!colnames(df_gas_run_with_air_with_input) %in% colnames(df_oil_result_with_air_with_input_pure)]
# [1] "census_name"    "full_state"     "full_county"    "c1_full"        "C2"             "C3"             "C4+"            "N2"            
# [9] "CO2"            "c1_ghgrp"       "c1_usgs"        "reporting_year" "C1"             "C1_cate"        "normalized_gas" "ci_category"   
#TODO----

# 4 The old runs without airplane loss rates ----

## 4.1 old gas -----

path_old_gas = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG run 20231128/NAG_CI_20231128_with_input_no_sf.xlsx"
df_gas_old = read_excel(path_old_gas) %>% # field 625
  filter(Check == "OK") # field 492 due to the ERROR fields 
nrow(df_gas_old) # 625? -> 492 
ncol(df_gas_old) # 317; new gas 318 pretty similar

path_old_gas_error_raw_result = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG run 20231128/us_gas_error_fields_rerun_20240107.xlsx"
df_gas_old_error = clean_opgee_remote_run_result(read_excel(path_old_gas_error_raw_result))

View(df_gas_old_error)

df_gas_old_error_with_input <- US_GAS_WITH_COMP %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(unique(df_gas_old_error) %>%
              mutate(`20 Field properties Field name NA` = as.numeric(`20 Field properties Field name NA`)),
            by = c("GEOID" = "20 Field properties Field name NA")) %>%
  rename("CI_gCO2_MJ" = "192 Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ",
         "Check" = "194 Lifecycle GHG emissions Total CO2 sequestered") %>%
  filter(!is.na(CI_gCO2_MJ))

View(df_gas_old_error_with_input)

nrow(df_gas_old_error_with_input) #129
ncol(df_gas_old_error_with_input) #318, same as df_gas_old

colnames(df_gas_old)[colnames(df_gas_old) %in% colnames(df_gas_old_error_with_input)]

path_offshore = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG run 20231128/offshore_GOM_1field_manual_run.xlsx"

df_old_offshore = read_excel(path_offshore)
df_old_offshore_cleaned = clean_opgee_remote_run_result(df_old_offshore)
View(df_old_offshore_cleaned)

### old gas result ----
df_gas_old # 492
df_gas_old_error_with_input # 129
# total 621 fields
df_old_offshore_cleaned # 1

## 4.2 old oil runs -----

path_old_oil = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG oil run 20230115/result_oil_20240115.xlsx"
df_oil_old = read_excel(path_old_oil)
df_oil_old_cleaned = clean_opgee_remote_run_result(df_oil_old) %>%
  filter(`194 Lifecycle GHG emissions Total CO2 sequestered` == "OK")

length(unique(df_oil_old_cleaned$`20 Field properties Field name NA`)) # 467


View(US_OIL) # check pure gas input sheet

df_oil_old_cleaned_with_input <- US_OIL %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(unique(df_oil_old_cleaned) %>%
              mutate(`20 Field properties Field name NA` = as.numeric(`20 Field properties Field name NA`)),
            by = c("GEOID" = "20 Field properties Field name NA")) %>%
  rename("CI_gCO2_MJ" = "192 Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ",
         "Check" = "194 Lifecycle GHG emissions Total CO2 sequestered")

View(df_oil_old_cleaned_with_input)

df_oil_old_cleaned_with_input_pure <- df_oil_old_cleaned_with_input %>%
  filter(!is.na(CI_gCO2_MJ))
nrow(df_oil_old_cleaned_with_input_pure) 

### old oil result -----
df_oil_old_cleaned_with_input_pure

# 5 VISUALIZATION -----

## 5.0 save all files to local ----
path_visual = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG Jan 22 visualization/"
write_csv(df_gas_run_with_air_with_input %>% st_drop_geometry(), paste0(path_visual, "new_gas_result_with_input.csv"))
saveRDS(df_gas_run_with_air_with_input, paste0(path_visual, "new_gas_result_with_input.rds"))

## 5.1 Basin weighted average CI by wellsite/midstream -----

df_gas_run_with_air_with_input

df_new_gas <- df_gas_run_with_air_with_input %>%
  select(FIPS, Basin = AAPG.Basin.x, Annual_Gas, Annual_Oil, 
         exploration_gCO2_MJ = `130 Exploration (e) Total GHG emissions gCO2eq/MJ`,
         drilling_dev_gCO2_MJ = `136 Drilling & Development (d) Total GHG emissions gCO2eq/MJ`,
         crude_prod_extra_gCO2_MJ = `142 Crude production & extraction (p) Total GHG emissions gCO2eq/MJ`,
         surface_gCO2_MJ = `148 Surface processing (s) Total GHG emissions gCO2eq/MJ`,
         lng_gCO2_MJ = `154 Liquefied natural gas Total GHG emissions gCO2eq/MJ`,
         maintenance_gCO2_MJ = `160 Maintenance (m) Total GHG emissions gCO2eq/MJ`,
         waste_gCO2_MJ = `166 Waste disposal (w) Total GHG emissions gCO2eq/MJ`,
         crude_transport_gCO2_MJ =`172 Crude transport (t) Total GHG emissions gCO2eq/MJ`,
         gas_dis_gCO2_MJ = `179 Gas Distribution Total GHG emissions gCO2eq/MJ`,
         other_gCO2_MJ = `183 Other small sources Total GHG emissions gCO2eq/MJ`,
         offsite_gCO2_MJ = `185 Offsite emissions credit/debit Total GHG emissions gCO2eq/MJ`,
         CO2_seq_gCO2_MJ = `190 Carbon dioxide sequestration Total CO2 sequestered gCO2/MJ`,
         CI_gCO2_MJ)

View(df_new_gas)



# 7 Feb1 similarity match -----

# read gas raw results
gas_similarity_match_20240130_result <- read_excel("NAG Jan 22 visualization/gas_similarity_match_20240130_result.xlsx")

df_gas_similarity_with_input = clean_and_join_input(gas_similarity_match_20240130_result, US_GAS_WITH_COMP_scaled)

View(df_gas_similarity_with_input)

weighted_average(as.numeric(df_gas_similarity_with_input$CI_gCO2_MJ), as.numeric(df_gas_similarity_with_input$Annual_Gas)) # 10.15895

sum(as.numeric(df_gas_similarity_with_input$Annual_Gas)) # 26767442820

# read oil raw results
oil_similarity_match_20240130_result <- read_excel("NAG Jan 22 visualization/20240130_oil_similarity_result.xlsx")

df_oil_sim_with_input = clean_and_join_input(oil_similarity_match_20240130_result,US_OIL) 

# check the format of oil and gas results
colnames(df_gas_similarity_with_input)[!colnames(df_gas_similarity_with_input) %in% colnames(df_oil_sim_with_input)]
# [1] "census_name"    "full_state"     "full_county"    "c1_full"        "C2"             "C3"            
# [7] "C4+"            "N2"             "CO2"            "c1_ghgrp"       "c1_usgs"        "reporting_year"
# [13] "C1"             "C1_cate" 

weighted_average(as.numeric(df_oil_sim_with_input$CI_gCO2_MJ), as.numeric(df_oil_sim_with_input$Annual_Gas)) # 13.71979

sum(df_oil_sim_with_input$Annual_Gas)

## 7.1 Feb1 plot ----
df_gas_old_error_with_input


df_gas_sim <- CI_by_process(df_gas_similarity_with_input)
View(df_gas_sim)
df_oil_sim <- CI_by_process(df_oil_sim_with_input)
View(df_oil_sim)

tmp_field = df_gas_sim %>%
  st_drop_geometry() %>%
  select(FIPS, County, Basin, Annual_Gas, Annual_Oil, BCM2022, new_CI = CI_gCO2_MJ) %>%
  left_join(df_old_gas_all %>% select(FIPS, base_CI = CI_gCO2_MJ)) %>%
  mutate(new_CI = as.numeric(new_CI),
         base_CI = as.numeric(base_CI))

View(tmp_field)
tmp = df_gas_sim %>%
  st_drop_geometry() %>%
  select(FIPS, County, Basin, Annual_Gas, Annual_Oil, BCM2022, new_CI = CI_gCO2_MJ) %>%
  left_join(df_old_gas_all %>% select(FIPS, base_CI = CI_gCO2_MJ)) %>%
  #mutate(CI_diff = new_CI - base_CI) %>%
  group_by(Basin) %>%
  summarize(Annual_gas = sum(Annual_Gas, na.rm=T),
            Annual_oil = sum(Annual_Oil, na.rm=T),
    base_CI = weighted.mean(as.numeric(base_CI), as.numeric(Annual_Gas), na.rm = T),
            new_CI = weighted.mean(as.numeric(new_CI), as.numeric(Annual_Gas), na.rm = T),
            CI_diff= new_CI - base_CI)

View(tmp)
write_csv(tmp, "CI_diff_by_basin.csv")

# 7 Mar4 similarity match -----

# read gas raw results
gas_mar4_result <- read_excel("/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG aerial loss run 20240304/gas_runs_pure_aerial_results.xlsx")

df_gas_with_input = clean_and_join_input(gas_mar4_result, US_GAS_WITH_COMP_scaled)

View(df_gas_with_input)

weighted_average(as.numeric(df_gas_with_input$CI_gCO2_MJ), as.numeric(df_gas_with_input$Annual_Gas)) # 9.33

sum(as.numeric(df_gas_with_input$Annual_Gas)) # 26767442820

# read oil raw results
oil_mar4_result <- read_excel("/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG aerial loss run 20240304/oil_runs_pure_aerial_results.xlsx")

df_oil_with_input = clean_and_join_input(oil_mar4_result,US_OIL) 

# check the format of oil and gas results
colnames(df_gas_with_input)[!colnames(df_gas_with_input) %in% colnames(df_oil_with_input)]
# [1] "census_name"    "full_state"     "full_county"    "c1_full"        "C2"             "C3"            
# [7] "C4+"            "N2"             "CO2"            "c1_ghgrp"       "c1_usgs"        "reporting_year"
# [13] "C1"             "C1_cate" 

weighted_average(as.numeric(df_oil_with_input$CI_gCO2_MJ), as.numeric(df_oil_with_input$Annual_Gas)) # 13.71979

sum(df_oil_with_input$Annual_Gas)

## 7.1 Feb1 plot ----
df_gas_old_error_with_input


df_gas_sim <- CI_by_process(df_gas_similarity_with_input)
View(df_gas_sim)
df_oil_sim <- CI_by_process(df_oil_sim_with_input)
View(df_oil_sim)

tmp_field = df_gas_sim %>%
  st_drop_geometry() %>%
  select(FIPS, County, Basin, Annual_Gas, Annual_Oil, BCM2022, new_CI = CI_gCO2_MJ) %>%
  left_join(df_old_gas_all %>% select(FIPS, base_CI = CI_gCO2_MJ)) %>%
  mutate(new_CI = as.numeric(new_CI),
         base_CI = as.numeric(base_CI))

View(tmp_field)
tmp = df_gas_sim %>%
  st_drop_geometry() %>%
  select(FIPS, County, Basin, Annual_Gas, Annual_Oil, BCM2022, new_CI = CI_gCO2_MJ) %>%
  left_join(df_old_gas_all %>% select(FIPS, base_CI = CI_gCO2_MJ)) %>%
  #mutate(CI_diff = new_CI - base_CI) %>%
  group_by(Basin) %>%
  summarize(Annual_gas = sum(Annual_Gas, na.rm=T),
            Annual_oil = sum(Annual_Oil, na.rm=T),
            base_CI = weighted.mean(as.numeric(base_CI), as.numeric(Annual_Gas), na.rm = T),
            new_CI = weighted.mean(as.numeric(new_CI), as.numeric(Annual_Gas), na.rm = T),
            CI_diff= new_CI - base_CI)

View(tmp)
write_csv(tmp, "CI_diff_by_basin.csv")


# 8 Mar7 uncertainty ----

PATH = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG runs/20240306 uncertainty/"

gas_up <- read_excel(paste0(PATH,"gas upper (2).xlsx"))
gas_lo <- read_excel(paste0(PATH,"gas lower (2).xlsx"))
oil_up <- read_excel(paste0(PATH,"oil upper (2).xlsx"))
oil_lo <- read_excel(paste0(PATH,"oil lower (2).xlsx"))

df_gas_up = clean_and_join_input(gas_up, US_GAS_WITH_COMP_scaled)
View(df_gas_up)
weighted_average(as.numeric(df_gas_up$CI_gCO2_MJ), as.numeric(df_gas_up$Annual_Gas)) 
# 9.89

df_gas_lo = clean_and_join_input(gas_lo, US_GAS_WITH_COMP_scaled)
#View(df_gas_with_input)
weighted_average(as.numeric(df_gas_lo$CI_gCO2_MJ), as.numeric(df_gas_lo$Annual_Gas)) 
# 8.77

df_oil_lo = clean_and_join_input(oil_lo, US_OIL)
#View(df_gas_with_input)
weighted_average(as.numeric(df_oil_lo$CI_gCO2_MJ), as.numeric(df_oil_lo$Annual_Gas)) 
# 14.59

df_oil_up = clean_and_join_input(oil_up, US_OIL)
#View(df_gas_with_input)
weighted_average(as.numeric(df_oil_up$CI_gCO2_MJ), as.numeric(df_oil_up$Annual_Gas)) 
# 16.27

select_CI_and_gas <- function(df){
  return(df%>% select(CI_gCO2_MJ, Annual_Gas))
}

vol_wtd_CI <- function(df){
  return(weighted_average(as.numeric(df$CI_gCO2_MJ), as.numeric(df$Annual_Gas)) )
}

df_offshore <- data.frame(CI_gCO2_MJ = 9.46, Annual_Gas = 758000000)
View(df_offshore)

vol_wtd_CI(rbind(select_CI_and_gas(df_gas_up) %>% st_drop_geometry(),select_CI_and_gas(df_oil_up)%>% st_drop_geometry(),df_offshore)) # 11.965
           
vol_wtd_CI(rbind(select_CI_and_gas(df_gas_lo)%>% st_drop_geometry(),select_CI_and_gas(df_oil_lo)%>% st_drop_geometry(),df_offshore)) # 10.68
           