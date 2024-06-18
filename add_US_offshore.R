# CODE SCRIPT No.3/3

# Add offshore fields
# to OPGEE input list
# for running OPGEE3.0c for 
# North America Gas CI 2022

# Data source: BOEM, Enverus

# Author          Spencer Zhang
# Date            October 22 2023
# Last Updated    November 20 2023

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
library(rgdal)
library(openxlsx)

----------------------------------------
#---      2 Global Variables        ----     
----------------------------------------
  
PATH_INPUT_FOLDER = "~/GitHub/PhD/North-America-Gas-2021/Compiled OPGEE Input"
PATH_INPUT_v2 = paste0(PATH_INPUT_FOLDER,"/US_GAS_with_comp_v2_20231120_sf.rds")
US_GAS_WITH_COMP = readRDS(PATH_INPUT_v2)

PATH_RESULT =         "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Result Data"

----------------------------------------
#---        3 Function              ----     
----------------------------------------
  
  
----------------------------------------
#---         Misc              ----     
----------------------------------------
  
## Oct 22 Save U.S. shapefile for James ----

map_us_gas <- read_rds(paste0(PATH_RESULT,"/us_gas_sf.rds"))
mapview(map_us_gas)

shp_us_gas <- map_us_gas %>%
  select(Type = classified.type,
         Basin = AAPG.Basin.x,
         STATEFP,
         COUNTYFP,
         GEOID,
         COUNTYNAME = County_Name.x,
         API_UWI,
         Annual_Gas,
         Annual_Oil,
         Annual_Wat,
         True_Verti,
         Completion,
         First_Prod,
         Age_count,
         Depth_coun,
         Well_count,
         BCM2022,
         Avg.Temp,
         FOR_mscf_b,
         age_since_,
         Oil_Graviy,
         FID,
         Join_Count
         )

st_write(shp_us_gas, paste0(PATH_RESULT,"/US_FIELD_SHAPEFILE_GAS_2022.shp"))
write_csv(shp_us_gas,paste0(PATH_RESULT,"/US_FIELD_SHAPEFILE_GAS_2022.csv"))

----------------------------------------
#---       Offshore Raw Data       ----     
----------------------------------------
# 5 GOM region ----
## 5.1 BOEM active lease map (GOM) ----

setwd("~/GitHub/PhD/North-America-Gas-2021")

gom_lease_spdf <- readOGR( 
  dsn= paste0(getwd(),"/Data/Offshore BOEM/Mapping/active_lease") , 
  layer="al_20231002",
  verbose=FALSE
)

mapview(gom_lease_spdf)
View(gom_lease_spdf)

gom_lease_sf <- st_as_sf(gom_lease_spdf)
View(gom_lease_sf)
mapview(gom_lease_sf)

# csb_spdf <- readOGR( 
#   dsn= paste0(getwd(),"/Data/Offshore BOEM/BSEE_Atlantic_Wells") , 
#   layer="atlantic_wells",
#   verbose=FALSE
# )
# 
# mapview(csb_spdf)

## 5.2 BOEM well production ----

#PATH_OGOR_A = "~/GitHub/PhD/North-America-Gas-2021/Data/Offshore BOEM/Production/ogora2022delimit.xlsx"
#ogor_a <- read.xlsx(PATH_OGOR_A, colNames = F)
#View(ogor_a)

PATH_OGOR_A = "~/GitHub/PhD/North-America-Gas-2021/Data/Offshore BOEM/Production/ogora2022delimit.txt"
ogor_a <- read.table(PATH_OGOR_A, sep = ",", header = F)

column_names <- c("LEASE_NUMBER",
                  "COMPLETION_NAME",
                  "PRODUCTION_DATE",
                  "DAYS_ON_PROD",
                  "PRODUCT_CODE",
                  "MON_O_PROD_VOL",
                  "MON_G_PROD_VOL",
                  "MON_WTR_PROD",
                  "API_WELL_NUM",
                  "WELL_STAT_CD",
                  "AREA_CODE",
                  "OPERATOR_NUM",
                  "SORT_NAME",
                  "FIELD_NAME_CODE",
                  "INJECTION_VOLUME",
                  "PROD_INTERVAL_CD",
                  "FIRST_PROD_DATE",
                  "UNIT_AGT_NUMBER",
                  "UNIT_ALOC_SUFFIX")

colnames(ogor_a) <- column_names

## 5.3 merge active lease map with well production ----
ogor_a_lease_grouped <- ogor_a %>%
  group_by(LEASE_NUMBER) %>%
  summarize(
    Annual_Oil = sum(MON_O_PROD_VOL),
    Annual_Gas = sum(MON_G_PROD_VOL),
    Annual_Wat = sum(MON_WTR_PROD),
    Injection_Vol = sum(INJECTION_VOLUME),
    First_Prod_Date = first(ymd(FIRST_PROD_DATE)),
    Well_Count = n())

View(ogor_a_lease_grouped)

gom_joined_sf = gom_lease_sf %>%
  left_join(ogor_a_lease_grouped, by = c("LEASE_NUMB" = "LEASE_NUMBER")) %>%
  mutate(GOR_rep = if_else(Annual_Oil > 0, Annual_Gas*1000/Annual_Oil, Annual_Gas*1000/Well_Count),
         gas_10000_cutoff_flag = if_else(GOR_rep < 10000, "O", "G"))
mapview(gom_joined_sf,
        zcol = "gas_10000_cutoff_flag") +
  mapview(RAW_FLARE_2022)

sum(gom_joined_sf$Annual_Gas, na.rm =T) 
# 758400998 MCF 
# => 2.07 BCF/day
# => 758 BCF
# ref EIA 119 Bcf/d
sum(gom_joined_sf$Annual_Oil, na.rm =T) 
# 623834171 BBL
sum(gom_joined_sf$Well_Count, na.rm =T)
#56184
sum(US_GAS$Annual_Gas)
# [1] 26767442820


offshore_flare_per_lease = RAW_FLARE_2022 %>%
  st_join(gom_joined_sf %>% st_transform(st_crs(RAW_FLARE_2022))) %>%
  filter(!is.na(LEASE_NUMB)) %>%
  group_by(LEASE_NUMB) %>%
  summarise(
    BCM2022 = sum(`BCM 2022`),
    Ave_temp_K = mean(`Avg. temp., K`)
  ) %>%
  st_drop_geometry()
View(offshore_flare_per_lease)

geom_with_flare_sf = gom_joined_sf %>%
  left_join(offshore_flare_per_lease) %>%
  filter(!is.na(gas_10000_cutoff_flag))

geom_with_flare_sf %>%
  group_by(gas_10000_cutoff_flag) %>%
  summarise(annual_gas_bcf = sum(Annual_Gas, na.rm = T)/1000000,
            annual_oil_mmbbl = sum(Annual_Oil, na.rm = T)/1000000,
            Injection_Vol = sum(Injection_Vol, na.rm = T),
            average_gor = mean(GOR_rep, na.rm = T),
            BCM2022 = sum(BCM2022,na.rm =T),
            well_count = sum(Well_Count, na.rm = T))

geom_with_flare_sf %>%
  #group_by(gas_10000_cutoff_flag) %>%
  summarise(annual_gas_bcf = sum(Annual_Gas, na.rm = T)/1000000,
            annual_oil_mmbbl = sum(Annual_Oil, na.rm = T)/1000000,
            Injection_Vol = sum(Injection_Vol, na.rm = T),
            average_gor = mean(GOR_rep, na.rm = T),
            BCM2022 = sum(BCM2022,na.rm =T),
            well_count = sum(Well_Count, na.rm = T))

## 5.4 OPGEE input example ----
offshore_input = geom_with_flare_sf %>%
  group_by(gas_10000_cutoff_flag) %>%
  summarize(#Field_location = "GOM",
            #Field_name = "GOM total gas",
            Field_age = as.numeric(today()-(mean(geom_with_flare_sf$First_Prod_Date, na.rm=T)))/365,
            Oil_production_volume_bbl_d = sum(Annual_Oil, na.rm = T)/365,
            Number_of_producing_well = sum(Well_Count, na.rm =T),
            annual_oil_bbl = sum(Annual_Oil, na.rm = T),
            annual_gas_scf = sum(Annual_Gas, na.rm = T)*1000,
            annual_water_bbl = sum(Annual_Wat, na.rm =T),
            GOR_scf_bbl = annual_gas_scf/annual_oil_bbl,
            WOR = annual_water_bbl/annual_oil_bbl,
            flaring_to_oil = sum(BCM2022,na.rm=T)*35314666572.222/annual_oil_bbl,
            Injection_Vol = sum(Injection_Vol, na.rm = T),
            average_gor = mean(GOR_rep, na.rm = T),
            BCM2022 = sum(BCM2022,na.rm =T),
            well_count = sum(Well_Count, na.rm = T))

write.xlsx(offshore_input %>% st_drop_geometry(), "offshore_input.xlsx")

offshore_input_total = geom_with_flare_sf %>%
  #group_by(gas_10000_cutoff_flag) %>%
  summarize(#Field_location = "GOM",
    #Field_name = "GOM total gas",
    Field_age = as.numeric(today()-(mean(geom_with_flare_sf$First_Prod_Date, na.rm=T)))/365,
    Oil_production_volume_bbl_d = sum(Annual_Oil, na.rm = T)/365,
    Number_of_producing_well = sum(Well_Count, na.rm =T),
    annual_oil_bbl = sum(Annual_Oil, na.rm = T),
    annual_gas_scf = sum(Annual_Gas, na.rm = T)*1000,
    annual_water_bbl = sum(Annual_Wat, na.rm =T),
    GOR_scf_bbl = annual_gas_scf/annual_oil_bbl,
    WOR = annual_water_bbl/annual_oil_bbl,
    flaring_to_oil = sum(BCM2022,na.rm=T)*35314666572.222/annual_oil_bbl,
    Injection_Vol = sum(Injection_Vol, na.rm = T),
    average_gor = mean(GOR_rep, na.rm = T),
    BCM2022 = sum(BCM2022,na.rm =T),
    well_count = sum(Well_Count, na.rm = T))

write.xlsx(offshore_input_total %>% st_drop_geometry(), "offshore_input_total.xlsx")

write.xlsx(geom_with_flare_sf %>% st_drop_geometry(), "geom_with_flare_sf.xlsx")

# 6 Pacific well geo dataset ----
pacific_wells <- readOGR( 
  dsn= paste0(getwd(),"/Data/Offshore BOEM/Pacific-Wells") , 
  layer="Pacific_Wells_04_07_15",
  verbose=FALSE
)

mapview(pacific_wells)

pacific_wells = st_as_sf(pacific_wells)
View(pacific_wells)


----------------------------------------
#---    7 Enverus offshore data     ----     
----------------------------------------
enverus_data <- read.csv(paste0(PATH_DATA, "/Active_O&G_Well_Headers_n_Annual_Production_", YEAR, ".CSV"))

enverus_data_sf <- enverus_data %>%
  st_as_sf(coords = c("Surface.Hole.Longitude..WGS84.", "Surface.Hole.Latitude..WGS84."), crs = 4326)

dissolve_sf <- st_union(census_states)

intersection_matrix <- st_contains(dissolve_sf %>% st_transform(4326), enverus_data_sf)


enverus_onshore_df <- enverus_data_sf %>% .[dissolve_sf %>% st_transform(4326),]
enverus_onshore_df = unique(enverus_onshore_df$API14)
enverus_offshore_sf <- enverus_data_sf %>% filter(!API14 %in% enverus_onshore_df)
# Only GOM again??
  
mapview(enverus_offshore_sf)

# 7.2 clean OPGEE input ----
colnames(enverus_offshore_sf)

enverus_offshore_sf_production <- enverus_offshore_sf %>%
  filter(Annual.Gas > 0 & !is.na(Annual.Gas))
sum(enverus_offshore_sf_production$Annual.Gas) # 641554073 MCF
mapview(enverus_offshore_sf_production, zcol = "DI.Basin")

enverus_offshore_sf_raw_gas <- enverus_offshore_sf %>% 
  filter(Production.Type %in%c("GAS", "O&G"))
sum(enverus_offshore_sf_raw_gas$Annual.Gas, na.rm = T) # 177671361 MCF
mapview(enverus_offshore_sf_raw_gas)

----------------------------------------
#---    8 Merge BOEM with Enverus   ----     
----------------------------------------

sum(US_GAS_WITH_COMP$Annual_Gas, na.rm = T) #2.027957e+12
sum(enverus_offshore_sf_raw_gas$Annual.Gas, na.rm = T)/sum(US_GAS_WITH_COMP$Annual_Gas, na.rm = T) # 0.008%
sum(enverus_offshore_sf_production$Annual.Gas, na.rm = T)/sum(US_GAS_WITH_COMP$Annual_Gas, na.rm = T) # 0.03%

sum(enverus_offshore_sf$Annual.Oil, na.rm = T)/sum(US_OIL_WITH_COMP$Annual_Gas, na.rm = T) # 0.03%
                     