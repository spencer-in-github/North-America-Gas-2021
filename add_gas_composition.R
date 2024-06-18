# CODE SCRIPT No.2/3

# Add gas composition from Philippine Burdeau's dataset
# to OPGEE input list
# for running OPGEE3.0c for 
# North America Gas CI 2022

# Author          Spencer Zhang
# Date            Nov 9 2023
# Last Updated    Jan 30 2024

# Updated Jan 7 2024
#         scaled the gas composition to ignore H2 and everything else add to one

# Updated Jan 30 2024
#         merge the scaled error fields with other fields
#         so we can rerun with only one input sheet

----------------------------------------
#---         1 Package              ----       
----------------------------------------

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(mapview)
library(sf)
library(tigris)
library(openxlsx)

----------------------------------------
#---      2 Global Variables        ----     
----------------------------------------
  
PATH_GAS_COMP_FOLDER = "~/GitHub/PhD/North-America-Gas-2021/Data/Gas composition"
PATH_FULL_GAS_COMP = paste0(PATH_GAS_COMP_FOLDER, "/full_composition.csv")

PATH_INPUT_FOLDER = "~/GitHub/PhD/North-America-Gas-2021/Compiled OPGEE Input"
PATH_INPUT_v1 = paste0(PATH_INPUT_FOLDER,"/US_INPUT_2022_GAS_v1_20230925.csv")

----------------------------------------
#---        3 Function              ----     
----------------------------------------
  
  
----------------------------------------
#---      4 Data Preparation        ----     
----------------------------------------

# raw data composition file
RAW_FULL_GAS_COMP = read.csv(PATH_FULL_GAS_COMP)
RAW_FULL_GAS_COLUMNS = colnames(RAW_FULL_GAS_COMP)
RAW_FULL_GAS_COLUMNS
# [1] "X"                       "state"                   "county"                 
# [4] "ch4_weighted_avg"        "co2_emissions_subpart_c" "C1_fraction"            
# [7] "remaining_comps_ghgrp"   "HE_fraction"             "CO2_fraction"           
# [10] "H2_fraction"             "N2_fraction"             "O2_fraction"            
# [13] "C2_fraction"             "C3_fraction"             "N.C4_fraction"          
# [16] "I.C4_fraction"           "N.C5_fraction"           "I.C5_fraction"          
# [19] "C6._fraction"            "BASIN_NAME_x"            "BASIN_NAME_y" 

RAW_CH4_ghgrp_prod = read.csv(paste0(PATH_GAS_COMP_FOLDER, "/county_level_ghgrp_production.csv"))
RAW_CH4_usgs = read.csv(paste0(PATH_GAS_COMP_FOLDER, "/county_level_usgs.csv"))

# read in previous OPGEE input file (no offshore, no gas composition)
OPGEE_INPUT_v1 = read.csv(PATH_INPUT_v1) # 517 counties, why 622 length??
## column names
# [1] "classified.type" "AAPG.Basin.x"    "STATEFP"         "COUNTYFP"       
# [5] "GEOID"           "County_Name.x"   "API_UWI"         "Annual_Gas"     
# [9] "Annual_Oil"      "Annual_Wat"      "API14"           "True_Verti"     
# [13] "Completion"      "First_Prod"      "Age_count"       "Depth_coun"     
# [17] "Well_count"      "geometry"        "County_Name.y"   "AAPG.Basin.y"   
# [21] "BCM2022"         "Avg.Temp"        "FOR_mscf_bbl"    "age_since_"     
# [25] "Oil_Graviy"      "API_Double"      "API_count"       "FOR_mscf_b"     
# [29] "FID"             "Join_Count"      "TARGET_FID"      "Join_Cou_1"     
# [33] "TARGET_F_1"      "NAME"            "STATE_NAME"      "STATE_FIPS"     
# [37] "CNTY_FIPS"       "AREA"            "POP1990"         "POP1999"        
# [41] "POP90_SQMI"      "HOUSEHOLDS"      "MALES"           "FEMALES"        
# [45] "WHITE"           "BLACK"           "AMERI_ES"        "ASIAN_PI"       
# [49] "OTHER"           "HISPANIC"        "AGE_UNDER5"      "AGE_5_17"       
# [53] "AGE_18_29"       "AGE_30_49"       "AGE_50_64"       "AGE_65_UP"
# [57] "NEVERMARRY"      "MARRIED"         "SEPARATED"       "WIDOWED"        
# [61] "DIVORCED"        "HSEHLD_1_M"      "HSEHLD_1_F"      "MARHH_CHD"      
# [65] "MARHH_NO_C"      "MHH_CHILD"       "FHH_CHILD"       "HSE_UNITS"      
# [69] "VACANT"          "OWNER_OCC"       "RENTER_OCC"      "MEDIAN_VAL"     
# [73] "MEDIANRENT"      "UNITS_1DET"      "UNITS_1ATT"      "UNITS2"         
# [77] "UNITS3_9"        "UNITS10_49"      "UNITS50_UP"      "MOBILEHOME"     
# [81] "NO_FARMS87"      "AVG_SIZE87"      "CROP_ACR87"      "AVG_SALE87"     
# [85] "Shape_Leng"      "Shape_Area"      "Field1"          "Prov_Cod_1"     
# [89] "GeoProvinc"      "GeoProvi_1"      "GeoProvi_2"      "GeoProvi_3"     
# [93] "GeoProvi_4"      "FIPS"  

PATH_PARTIAL_RESULT = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Visual/Gas comp/"
----------------------------------------
#---      5 Merge gas comp        ----     
----------------------------------------

# Join by State and county name
# Note some counties have the same name
  
census_states <- states() %>% select(STATEFP, STUSPS)
census_counties <- counties() %>%
  select(STATEFP, COUNTYFP,GEOID,NAME) %>%
  left_join(census_states %>% st_drop_geometry())

## add GEOID to Phi's dataset ----
full_gas_comp = RAW_FULL_GAS_COMP %>%
  mutate(county = toupper(county)) %>%
  left_join(census_counties %>%
              mutate(NAME = toupper(NAME)), by = c("state" = "STUSPS", "county" ="NAME")) %>%
  st_as_sf()

ch4_usgs = RAW_CH4_usgs %>%
  mutate(county = toupper(county)) %>%
  left_join(census_counties %>%
              mutate(NAME = toupper(NAME)), by = c("state" = "STUSPS", "county" ="NAME")) %>%
  st_as_sf()

m=mapview(ch4_usgs)
mapshot(m,url = paste0(PATH_PARTIAL_RESULT,"CH4_USGS_COVERAGE.html"))

ch4_ghgrp_prod = RAW_CH4_ghgrp_prod %>%
  mutate(county = toupper(county)) %>%
  left_join(census_counties %>%
              mutate(NAME = toupper(NAME)), by = c("state" = "STUSPS", "county" ="NAME")) %>%
  st_as_sf()

m=mapview(ch4_ghgrp_prod)
mapshot(m,url = paste0(PATH_PARTIAL_RESULT,"CH4_GHGRP_coverage.html"))

mapview(full_gas_comp, col.regions = "green") +
  mapview(US_FIELD_WITH_FLARE %>% filter(classified.type == "GAS"), col.regions = "grey")  +
  mapview(ch4_ghgrp_prod %>% filter(reporting_year == "2021" & !is.na(ch4_fraction_ghgrp_prod)), col.region = "blue") +
  mapview(ch4_usgs %>% filter(reporting_year == 2014 & !is.na(fraction)), col.region = "purple")

## modify gas comp to OPGEE input requirement ----
gas_comp_revised = full_gas_comp %>%
  select(GEOID, 
         "STATE_NAME" = "state",
         "COUNTY_NAME" = "county",
         ch4_weighted_avg,
         ends_with("fraction"),
         "BASIN_NAME" = "BASIN_NAME_x") %>%
  mutate(GEOID, 
         STATE_NAME,
         COUNTY_NAME,
         "C1" = ch4_weighted_avg,
         "C2" = C2_fraction,
         "C3" = C3_fraction,
         "C4+" = `N.C4_fraction` + `I.C4_fraction` + `N.C5_fraction`+ `I.C5_fraction`+ `C6._fraction`,
         "N2" = N2_fraction,
         #"H2S" = H2_fraction, #TODO: H2 is hydrogen, no H2S data yet
         "CO2" = CO2_fraction) %>% 
  select(GEOID, 
         STATE_NAME,
         COUNTY_NAME,
         C1,
         C2,
         C3,
         `C4+`,
         N2,
         CO2,
         #H2S,
         BASIN_NAME)

write_csv(gas_comp_revised, paste0(PATH_GAS_COMP_FOLDER, "/full_gas_comp_revised_v1.csv"))

gas_comp_revised <- read.csv(paste0(PATH_GAS_COMP_FOLDER, "/full_gas_comp_revised_v1.csv"))


## 20240107 scale C1-N2 for H2S ------

gas_comp_revised <- read.csv(paste0(PATH_GAS_COMP_FOLDER, "/full_gas_comp_revised_v1.csv"))

gas_comp_scaled <- gas_comp_revised %>%
  mutate(non_c1_all = C2 + C3 + C4. + N2+ CO2,
         c1_original = C1,
         C1 = ifelse(is.na(non_c1_all), C1, 1 - non_c1_all))
----------------------------------------
#---    6 Merge into input   ----     
----------------------------------------
# Difficulty in joining the production field
# with phi's composition dataset
  # 1 production field shapefile is spotty
  # 2 phi's county shapefile overlaps on boundaries of multiple counties
  
# Solution
  # 1 save production field spotty shapefile separately first
  # 2 add whole county shapefile to production field
  # 3 take centroids of composition data
  # 4 join
  # 5 rejoin with step1 saved production field spotty shapefile

# 1 save production field actual spotty shapefile separately
US_GAS_FIELD_SHAPEFILE = US_GAS %>%
  select(GEOID)

# 2 add whole county shapefile to production field dataset
us_gas_county_shape = US_GAS %>%
  select(-c(38:84)) %>%
  st_drop_geometry() %>%
  left_join(census_counties %>%
              select(GEOID, census_name = NAME)) %>%
  st_as_sf()

# check if the census county match the production field county
View(us_gas_county_shape %>% select(County_Name.x, census_name))
mapview(us_gas_county_shape)
  
# 3&4 take centroids of gas composition data and merge into production data
us_gas_with_comp = us_gas_county_shape %>%
    #st_join(gas_comp_revised %>%
  st_join(gas_comp_scaled %>%
              select(full_state = STATE_NAME,
                     full_county = COUNTY_NAME,
                     c1_full = C1, C2, C3, `C4.`, N2, CO2) %>%
              st_centroid()) %>%
          st_join(ch4_ghgrp_prod %>% 
            filter(reporting_year == "2021" & !is.na(ch4_fraction_ghgrp_prod)) %>%
            select(c1_ghgrp = ch4_fraction_ghgrp_prod) %>%
            st_centroid()) %>%
          st_join(ch4_usgs %>% 
                    filter(!is.na(fraction) & reporting_year == 2014) %>% 
                    select(c1_usgs = fraction, reporting_year) %>%
                    st_centroid())
          
# Check if production county and composition county match after taking centroid
View(us_gas_with_comp %>% select(STATE_NAME, County_Name.x, full_state, full_county))

mapview(us_gas_with_comp, zcol = "c1_usgs")

us_gas_with_comp = us_gas_with_comp %>%
  mutate(C1 = case_when(
    !is.na(c1_full) ~ c1_full,
    !is.na(c1_ghgrp) ~ c1_ghgrp,
    !is.na(c1_usgs) ~ c1_usgs,
    T ~ NA
  ),
  C1_cate = case_when(
    !is.na(c1_full) ~ "c1_full",
    !is.na(c1_ghgrp) ~ "c1_ghgrp",
    !is.na(c1_usgs) ~ "c1_usgs",
    T ~ NA))

mapview(us_gas_with_comp, zcol = "C1")

# 5 convert back to the actual spotty production field shapefile 
us_gas_with_comp = us_gas_with_comp %>%
  st_drop_geometry() %>%
  left_join(US_GAS_FIELD_SHAPEFILE) %>%
  st_as_sf()

mapview(us_gas_with_comp, zcol ="C1")

## stats ----
us_gas_with_comp %>%
  select(starts_with("C")) %>%
  mean(., na.rm =T)

## availability ----
sum(!is.na(us_gas_with_comp$C1))/nrow(us_gas_with_comp)
sum(!is.na(us_gas_with_comp$C2))/nrow(us_gas_with_comp)
sum(!is.na(us_gas_with_comp$C3))/nrow(us_gas_with_comp)
sum(!is.na(us_gas_with_comp$`C4+`))/nrow(us_gas_with_comp)
sum(!is.na(us_gas_with_comp$N2))/nrow(us_gas_with_comp)
sum(!is.na(us_gas_with_comp$CO2))/nrow(us_gas_with_comp)
# sum(!is.na(us_gas_with_comp$H2S))/nrow(us_gas_with_comp) # No H2S data yet (Nov20)

colnames = c("C1", "C2", "C3", "C4+", "N2", "CO2")
for (i in colnames){
  print(paste0(i, " mean ", mean(us_gas_with_comp[[i]], na.rm =T)))
  print(paste0(i, " std ", sd(us_gas_with_comp[[i]], na.rm =T)))
}

----------------------------------------
#---  7 Save latest OPGEE Input    ----     
----------------------------------------

write_csv(us_gas_with_comp %>% st_drop_geometry(), paste0(PATH_INPUT_FOLDER, "/US_GAS_with_comp_scaled_v3_20240107.csv"))
write.xlsx(us_gas_with_comp%>% st_drop_geometry(), paste0(PATH_INPUT_FOLDER, "/US_GAS_with_comp_scaled_v3_20240107.xlsx"))

saveRDS(us_gas_with_comp, paste0(PATH_INPUT_FOLDER, "/US_GAS_with_comp_scaled_v3_20240107_sf.rds"))


----------------------------------------
# Jan 7 2024 run only 134 error fields from 20231128 ---- 
----------------------------------------

library(readxl)
ERROR_fields_input_v1_dec12023 <- read_excel("NAG run 20231128/ERROR_fields_input_v1_dec12023.xlsx")
View(ERROR_fields_input_v1_dec12023)

selected_geoid = ERROR_fields_input_v1_dec12023[20,]
View(selected_geoid)

selected_geoids = as.list(selected_geoid)

View(us_gas_with_comp)
us_gas_error_fields <- us_gas_with_comp %>%
  filter(as.numeric(GEOID) %in% selected_geoids)

# check if gas comp adds to one
us_gas_error_fields <-  us_gas_error_fields %>%
  mutate(C1 =  1 -  (C2 + C3 + `C4+` + N2 + CO2)) %>%
  mutate(check = C1 +C2 +C3 +`C4+` + N2+CO2)

write_csv(us_gas_error_fields %>% st_drop_geometry(), paste0(PATH_INPUT_FOLDER, "/US_GAS_error_scaled_v3_20240107.csv"))
write.xlsx(us_gas_error_fields%>% st_drop_geometry(), paste0(PATH_INPUT_FOLDER, "/US_GAS_error_scaled_v3_20240107.xlsx"))

saveRDS(us_gas_error_fields, paste0(PATH_INPUT_FOLDER, "/US_GAS_error_scaled_v3_20240107_sf.rds"))


----------------------------------------
# Jan 30 2024 clean gas input sheet ----    
----------------------------------------

us_gas_error_fields <- readRDS(paste0(PATH_INPUT_FOLDER, "/US_GAS_error_scaled_v3_20240107_sf.rds")) %>%
  unique()

View(us_gas_error_fields) # 129 fields, 62 columns; 128 unique fields
  # 22101 St Mary has repeated entries (2)

View(US_GAS_WITH_COMP)

US_GAS_WITH_COMP_scaled <- US_GAS_WITH_COMP %>%
  filter(!GEOID %in% unique(us_gas_error_fields$GEOID)) %>%
  rbind(us_gas_error_fields %>% select(-check)) %>%
  group_by(FIPS) %>%
  summarise_all(first) # taking only one entry of St Mary

write_csv(US_GAS_WITH_COMP_scaled %>% st_drop_geometry(), paste0(PATH_INPUT_FOLDER, "/US_GAS_INPUT_v4_complete_comp_scaled_20240130.csv"))
write.xlsx(US_GAS_WITH_COMP_scaled%>% st_drop_geometry(), paste0(PATH_INPUT_FOLDER, "/US_GAS_INPUT_v4_complete_comp_scaled_20240130.xlsx"))

saveRDS(US_GAS_WITH_COMP_scaled, paste0(PATH_INPUT_FOLDER, "/US_GAS_INPUT_v4_complete_comp_scaled_20240130_sf.rds"))
