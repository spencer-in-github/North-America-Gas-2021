# CODE SCRIPT No.1/3

# Author          Zhihao(Spencer) Zhang
# Start Date      June 2023
# Last Updated    June 18 2024

# 1 Data
# Loaded raw Well Headers and Well Monthly Production csv files downloaded from Enverus
# Cleaned the well information with annual production of the specified year also from Enverus
# Added public datasets for specific states that are missing from Enverus
#
# 2 Classification
# Classified active producing wells into gas wells and oil wells based on production volumes
#
# 3 Oil- and Gas-zone creation with Voronoi diagrams
# Created oil- and gas-zones using the Voronoi diagram method
#
# 4 Other OPGEE related inputs
# Merged with flaring data from Elvidge et al

----------------------------------------
#---         1 Package              ----       
----------------------------------------

## Install Packages ----
# run only once per machine
install.packages("tidyr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("readr")
install.packages("mapview")
install.packages("sf")
install.packages("tigris")
install.packages("roxygen2") # For code documentation https://github.com/r-lib/roxygen2
install.packages("tidycensus")
insatll.packages("scales")
install.packages("fuzzyjoin")

## Load Packages ----
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(mapview)
library(sf)
library(tigris)
library(roxygen2)
library(tidycensus)
library(scales)
library(readxl)
library(fuzzyjoin) # for joining on character columns ignoring case


----------------------------------------
#---      2 Global Variables        ----     
----------------------------------------

census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab")

PATH_DATA =         "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data"
PATH_VIZ =          "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Visual"
PATH_WELL =         "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Raw Well Headers"
PATH_PRODUCTION =   "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Raw Well Monthly Production"
PATH_ANNUAL =       "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Annual Well Production"
PATH_FLARE =        "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Raw Flaring 2022/VIIRS_Global_flaring_d.7_slope_0.029353_2022_v20230526_web.xlsx"
PATH_RESULT =       "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Result Data/"
FILES_WELL =        list.files(PATH_WELL, "*.CSV")
FILES_PRODUCTION =  list.files(PATH_PRODUCTION, "*.CSV")
COUNTY_SHP_NAME =   "cb_2018_us_county_500k"
US_COUNTIES =       read_sf(paste0(PATH_DATA, "/", COUNTY_SHP_NAME), layer = COUNTY_SHP_NAME)

PATH_PUBLICATION = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Code Availability/XLSX"
YEAR = 2022

# TODO insert annual production reference
ANNUAL_OIL_mbbl_per_day = 11.9  # ref https://www.eia.gov/todayinenergy/detail.php?id=56540#:~:text=U.S.%20crude%20oil%20production%20grew,and%20Natural%20Gas%20Production%20report.
ANNUAL_GAS_bcf_per_day = 119    # ref https://www.eia.gov/todayinenergy/detail.php?id=56000

# Unit conversiion

BBL_to_MBBL =   1000000
MCF_to_BCF =    1000000

----------------------------------------
#---        3 Function              ----     
----------------------------------------

#' Title 
#' Clean the raw Enverus-downloaded well monthly production dataset
#'
#' @param path The local path to the raw Enverus well production dataset.
#' @param file_name The file name to the specific well production dataset (ends with '.CSV').
#' @param year_of_interest The production year of interest. Default set to 2022.
#' @param save_path The local path of which to save the cleaned annual dataset.
#'
#' @return writes the well annual production dataset to a local path.
#' @export
#'
#' @examples
clean_raw_well_monthly_production_to_annual = 
  function(path, file_name, year_of_interest = 2022, save_path){
    df = read.csv(paste0(path, "/",file_name)) %>%
      filter(`Monthly.Production.Date` > paste0(year_of_interest-1, '-12-01') 
             & `Monthly.Production.Date` < paste0(year_of_interest+1, '-01-01')) %>%
      group_by(API.UWI) %>%
      summarize_at(c("Monthly.Oil", "Monthly.Gas", "Monthly.Water", "Monthly.BOE"), sum, na.rm = T) %>% # This step removed the non-producing wells from the annual dataset, may cause spatial incompleteness due to lack of production data
      rename(Annual.Oil = Monthly.Oil, 
             Annual.Gas = Monthly.Gas, 
             Annual.Water = Monthly.Water, 
             Annual.BOE = Monthly.BOE) %>%
      mutate(Production.Year = year_of_interest)
    
    write_csv(df, paste0(save_path, "/Annual ", file_name))
  } 


#' Title
#' Merge large same-format CSV files into one dataframe.
#'
#' @param path The local path to which all of the same-format CSV files are stored.
#'
#' @return A merged dataframe.
#' @export
#'
#' @examples
merge_CSV = 
  function(path){
    file_list = list.files(path, "*.CSV")
    setwd(path)
    
    df <- data.frame()
    
    for (file in file_list) {
      tmp <- read_csv(file, col_types = cols())  # col_types = cols() reads all columns as character
      tmp <- type.convert(tmp, as.is = TRUE)      # Convert character columns to numeric where possible
      df <- rbind(df, tmp)
    }
    
    return(df)
  }


#' Title Load raw well headers
#' 
#' Loads the raw well header dataset (.CSV), filter to only active wells, and return a dataframe.
#'
#' @param path The local path to the folder storing the raw Enverus well header datasets.
#' @param file_name The file name of a well header dataset (ends with '.CSV').
#'
#' @return a dataframe with only active wells.
#' @export
#'
#' @examples
load_raw_well_headers = 
  function(path, file_name){
    df = read.csv(paste0(path, "/", file_name)) %>%
      filter(Well.Status == "ACTIVE") %>%
      
    
    return(df)
  }


#' Title Classify wells into OIL or GAS based on its annual production
#'
#' @param df A dataframe containing well information and annual production data ('Annual.Oil' and 'Annual.Gas').
#' @param gor_sep The gas-oil-ratio used to separate onshore oil wells from gas wells (unit: mcf/bbl). Default set to 100 according to EPA.
#' @param gas_gor_indicator The indicative GOR for gas wells who can't derive a GOR due to a zero oil production rate. Default set to 10000.
#'
#' @return df_classified with 'classified.type'.
#' @export
#'
#' @examples
classify_production = function(df, gor_sep = 100, gas_gor_indicator = 106000){
  
  df_classified <- df %>%
    mutate(gor = case_when(Annual.Oil >0 ~ Annual.Gas/Annual.Oil,
                           Annual.Oil == 0 & Annual.Gas > 0 ~ 10000),
           classified.type = case_when(
             `Production Type` %in% c("OIL", "GAS") ~ `Production Type`,
             gor < gor_sep  ~ "OIL",
             gor > gor_sep ~ "GAS",
             is.na(gor) ~ "OIL"
           ))
  
  return(df_classified)
}


----------------------------------------
#---      4 Data Preparation        ----     
----------------------------------------

# 1 - Clean the raw monthly production data into clean annual data
# TAKE LONG TIME! please run this only once
for (i in c(2:length(FILES_PRODUCTION))){
  clean_raw_well_monthly_production_to_annual(PATH_PRODUCTION,FILES_PRODUCTION[i], 2022, PATH_ANNUAL)
}

# 2 - Summarize well sheet with annual production 
df_annual = merge_CSV(PATH_ANNUAL)
write_csv(df_annual, paste0(PATH_DATA, "/Well_Annual_Production_", YEAR, ".CSV"))

df_annual = read_csv(paste0(PATH_DATA, "/Well_Annual_Production_", YEAR, ".CSV"))

# 3 - Merge with well information
df_headers = merge_CSV(PATH_WELL)
write_csv(df_headers, paste0(PATH_DATA, "/Well_Headers_", YEAR, ".CSV"))

df_headers = read_csv(paste0(PATH_DATA, "/Well_Headers_", YEAR, ".CSV"))

# 4 - filter for active wells and merge with annual production
df = df_headers %>% 
  filter(`Well Status` == "ACTIVE") %>%
  left_join(df_annual,
            by = c("API14" = "API.UWI"))  %>% # left join annual production, maintaining wells without production
  filter(`Production Type` %in% c("GAS", "OIL", "OIL & GAS"))
write_csv(df, paste0(PATH_DATA, "/Active_O&G_Well_Headers_n_Annual_Production_", YEAR, ".CSV"))

df_wells_with_annual_prod = read_csv(paste0(PATH_DATA, "/Active_O&G_Well_Headers_n_Annual_Production_", YEAR, ".CSV"))

colnames(df_wells_with_annual_prod)

----------------------------------------
#---    5 Well O&G Classification   ----     
----------------------------------------

sample_df = sample_n(df,1000) %>%
  st_as_sf(coords = c("Surface Hole Longitude (WGS84)", "Surface Hole Latitude (WGS84)"), crs = 4326)

mapview(sample_df, zcol = "Production Type")

## 1 - get State map ----
states <- states()

## 1.1 - full states ----
df_states <- states %>%
  filter(STUSPS %in% unique(df$State))
mapview(df_states)

## 1.2 - states with production ----
df_states_prod <- states %>%
  filter(STUSPS %in% unique(df %>% filter(!is.na(Annual.Oil) | !is.na(Annual.Gas)) %>%.$State))
m = mapview(df_states_prod) +
  mapview(df_states)
mapshot(m,url = "state_diff_missing_annual_production.html")

## 1.3 - classification ----

sum(df$`Production Type` == "OIL & GAS")/nrow(df) # 9.6%

df_classified <- classify_production(df) # contain full state coverage
write_csv(df_classified, paste0(PATH_DATA, "/", "WELL_2022_classified.csv"))
# test gor and classification

df_classified %>%
  mutate(gor_log = log(gor)) %>%
  ggplot(.) +
    geom_boxplot(aes(x = classified.type, y = gor_log))

sample_df = sample_n(df_classified,10000) %>%
  st_as_sf(coords = c("Surface Hole Longitude (WGS84)", "Surface Hole Latitude (WGS84)"), crs = 4326)

mapview(sample_df, zcol = "classified.type")
               
----------------------------------------
#---    6 Voronoi Diagram           ----     
----------------------------------------

df_classified_sf <- df_classified %>% st_as_sf(coords = c("Surface Hole Longitude (WGS84)","Surface Hole Latitude (WGS84)"), crs = st_crs(US_COUNTIES))
COUNTIES <- US_COUNTIES[df_classified_sf,]
#st_write(COUNTIES, paste0(PATH_DATA, "/","well_covered_counties/well_covered_counties.shp"))

## 6.1 County level voronoi loop ----

# empty dataframe to store field info
US_SHAPEFILE_COUNTY_LEVEL <- data_frame()

# loop
for(i in c(1:nrow(COUNTIES))){
  print(paste0("---- Creating Voronoi for County No.", i, " ----"))
  
  county = COUNTIES[i,]
  temp = df_classified_sf[county,]
  
  county_voronoi <-
    temp %>%
    st_transform(2236) %>%
    st_union() %>% 
    st_voronoi() %>% 
    st_cast() %>% 
    st_as_sf() %>% 
    st_join(temp %>% st_transform(2236)) %>%
    st_intersection(.,county %>% st_transform(2236)) %>%
    st_transform(st_crs(df_classified_sf))
  
  fixed_geometries <- county_voronoi$geometry %>% st_make_valid()
  
  county_voronoi$geometry <- fixed_geometries
  
  aapg = county_voronoi %>% 
    group_by(AAPG.Geologic.Province) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    pull(AAPG.Geologic.Province) %>%
    .[1]
  
  merged <- county_voronoi %>%
    st_as_sf() %>%
    group_by(classified.type) %>%
    summarize(AAPG.Basin = aapg,
              STATEFP = county$STATEFP,
              COUNTYFP = county$COUNTYFP,
              GEOID = county$GEOID,
              County_Name = county$NAME,
              API_UWI = mean(API14, na.rm = T),
              Annual_Gas = sum(Annual.Gas, na.rm = T),
              Annual_Oil = sum(Annual.Oil, na.rm = T),
              Annual_Wat = sum(Annual.Water, na.rm = T),
              API14 = mean(API14, na.rm =T),
              True_Verti = mean(True.Vertical.Depth, na.rm = T),
              Completion = mean(as.Date(Completion.Date), na.rm=T),
              First.Prod = mean(as.Date(First.Prod.Date), na.rm =T),
              #Oil_Gravity = weighted.mean(Oil.Gravity, Annual_Oil, na.rm = T),
              #Oil_Gravity = Annual_Gas/Annual_Oil*1000,
              #API_count = sum(!is.na(Oil_Gravity)),
              Age_count = sum(!is.na(First.Prod.Date)),
              Depth_count = sum(!is.na(True.Vertical.Depth)),
              #Monthly.BOE = sum(Monthly.BOE, na.rm = T),
              Well_count = n(),
              #FOR_BCM2021 = sum(temp_flare$`BCM 2021`),
              geometry = st_union(geometry)) %>%
    ungroup()
  
  US_SHAPEFILE_COUNTY_LEVEL <- rbind(US_SHAPEFILE_COUNTY_LEVEL, merged)
}

## 6.2 Visualize fieldshapes ----
mapview(US_SHAPEFILE_COUNTY_LEVEL) #TODO still have overlaps?

## 6.3 Inspect overlaps ----
m = mapview(US_SHAPEFILE_COUNTY_LEVEL)
mapshot(m, url = paste0(PATH_VIZ, "/US_field_2022_all.html"))

mapview(US_SHAPEFILE_COUNTY_LEVEL %>% filter(Well_count >10), zcol = "classified.type")
mapshot(m0, url = paste0(PATH_VIZ, "/US_field_2022_filter_wellcount10.html"))

m1 = mapview(US_SHAPEFILE_COUNTY_LEVEL %>% filter(classified.type == "GAS"))
mapshot(m1, url = paste0(PATH_VIZ, "/US_field_2022_gas.html"))

m2 = mapview(US_SHAPEFILE_COUNTY_LEVEL %>% filter(classified.type == "OIL"))
mapshot(m2, url = paste0(PATH_VIZ, "/US_field_2022_oil.html"))


----------------------------------------
#---  7 Inspect total production    ----     
----------------------------------------

perc_annual_oil = (sum(US_SHAPEFILE_COUNTY_LEVEL$Annual_Oil)+ sum(enverus_offshore_sf_production$Annual.Oil))/365/BBL_to_MBBL/ANNUAL_OIL_mbbl_per_day
perc_annual_gas = (sum(US_SHAPEFILE_COUNTY_LEVEL$Annual_Gas) + sum(enverus_offshore_sf_production$Annual.Gas))/365/MCF_to_BCF/ANNUAL_GAS_bcf_per_day

print(paste0("------ Annual Oil Percent of US Production ", percent(perc_annual_oil), " -------"))
# change from 80% - 92% after adding GOM
print(paste0("------ Annual Gas Percent of US Production ", percent(perc_annual_gas), " -------"))
# change from 92% -> 93% after adding GOM

(sum(df_classified_sf$Annual.Oil, na.rm =T))/365/BBL_to_MBBL/ANNUAL_OIL_mbbl_per_day
#[1] 0.9250564
(sum(df_classified_sf$Annual.Gas, na.rm =T))/365/MCF_to_BCF/ANNUAL_GAS_bcf_per_day
#[1] 0.9406385

## 7.1 Missing production ----

m = mapview(US_SHAPEFILE_COUNTY_LEVEL %>%
          mutate(no_production_flag = case_when(
            (Annual_Gas == 0 | is.na(Annual_Gas)) & (Annual_Oil == 0 | is.na(Annual_Oil)) ~ "NA",
            TRUE ~ "YES")), zcol = "no_production_flag")

m
mapshot(m, url = paste0(PATH_VIZ, "/US_field_2022_production_flag.html"))

us_field_production = US_SHAPEFILE_COUNTY_LEVEL %>%
  mutate(no_production_flag = case_when(
    (Annual_Gas == 0 | is.na(Annual_Gas)) & (Annual_Oil == 0 | is.na(Annual_Oil)) ~ "NA",
    TRUE ~ "YES")) %>%
  filter(no_production_flag == "NA")

## 7.2 Add Kansas Production ----

PATH_KANSAS = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Kansas Production/2022.xlsx"

RAW_KANSAS <- read_excel(PATH_KANSAS) %>%
  group_by(COUNTY) %>%
  summarise(OIL_PRODUCTION = sum(OIL_PRODUCTION),
         GAS_PRODUCTION = sum(GAS_PRODUCTION),
         OIL_WELLS = mean(OIL_WELLS),
         GAS_WELLS = mean(GAS_WELLS))


US_FIELD <- US_SHAPEFILE_COUNTY_LEVEL %>%
  left_join(RAW_KANSAS, by = c("County_Name" = "COUNTY")) %>%
  mutate(
    Annual_Gas = ifelse(!is.na(GAS_PRODUCTION) & classified.type == "GAS", 
                        Annual_Gas + GAS_PRODUCTION, 
                        Annual_Gas),
    Annual_Oil = ifelse(!is.na(OIL_PRODUCTION) & classified.type == "OIL", 
                        Annual_Oil + OIL_PRODUCTION,
                        Annual_Oil)
    #Annual_Oil = Annual_Oil + OIL_PRODUCTION
  ) %>%
  select(-c( "OIL_PRODUCTION",
             "GAS_PRODUCTION",
             "OIL_WELLS",
             "GAS_WELLS"))

## 7.3 Map after KS production ----
mapview(US_FIELD %>%
          mutate(no_production_flag = case_when(
            (Annual_Gas == 0 | is.na(Annual_Gas)) & (Annual_Oil == 0 | is.na(Annual_Oil)) ~ "NA",
            TRUE ~ "YES")), zcol = "no_production_flag")

## 7.4 Michigan Production ----
PATH_MI_WELL = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Michigan Production/PRUWells.csv"
PATH_MI_PROD = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Michigan Production/ProductionData.csv"

MI_WELL = read.csv(PATH_MI_WELL)
MI_PROD = read.csv(PATH_MI_PROD) 

MI_PROD_2022 = MI_PROD %>% 
  filter(grepl('2022', rptdate)) %>%
  group_by(PRUNumber) %>%
  summarize(oilprod = sum(oilprod),
            condprod = sum(condprod),
            nglprod = sum(nglprod),
            wtrprod = sum(wtrprod),
            gassold = sum(gassold),
            FieldType = first(FieldType),
            FieldTyleNo = length(unique(FieldType)))

mi_county_prod <- MI_PROD_2022 %>%
  left_join(MI_WELL, by = c("PRUNumber" = "PRUNumber")) %>%
  mutate(FieldType_edited = case_when(
    FieldType %in% c("O", "OG", "") ~ "OIL",
    FieldType %in% c("G", "GS", "GC") & nglprod > 0 ~ "GAS",
    TRUE ~ "OIL"
  )) %>%
  group_by(County, FieldType_edited) %>%
  summarize(oilprod = sum(oilprod) + sum(condprod),
            nglprod = sum(nglprod),
            wtrprod = sum(wtrprod),
            gassold = sum(gassold))

US_FIELD_with_mi <- US_FIELD %>%
  regex_left_join(mi_county_prod, 
                  by = c("County_Name" = "County", 
                         "classified.type" = "FieldType_edited"), 
                  ignore_case = T) %>%
  mutate(
    Annual_Gas = ifelse(!is.na(nglprod) & classified.type == "GAS", 
                        Annual_Gas + nglprod, 
                        Annual_Gas),
    Annual_Oil = ifelse(!is.na(oilprod) & classified.type == "OIL", 
                        Annual_Oil + oilprod,
                        Annual_Oil),
    Annual_Wat = ifelse(!is.na(wtrprod), Annual_Wat + wtrprod, Annual_Wat)
    #Annual_Oil = Annual_Oil + OIL_PRODUCTION
  ) %>%
  select(-c("County","FieldType_edited","oilprod","nglprod","wtrprod","gassold"))
  
## 7.5 Map after MI production ----
m = mapview(US_FIELD_with_mi %>%
          mutate(no_production_flag = case_when(
            (Annual_Gas == 0 | is.na(Annual_Gas)) & (Annual_Oil == 0 | is.na(Annual_Oil)) ~ "NA",
            TRUE ~ "YES")), zcol = "no_production_flag")
mapshot(m, url = paste0(PATH_VIZ, "/US_field_2022_production_flag_with_KS_MI.html"))

US_FIELD_final = US_FIELD_with_mi
----------------------------------------
#---    8 Add 2022 Flaring         ----     
--------------------------------------
  
## 8.1 get flaring 2022 ----
RAW_FLARE_2022 <- read_excel(PATH_FLARE) %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4269) %>%
  filter(Country == "United States")

## 8.2 Map flaring with field ----

mapview(RAW_FLARE_2022) + 
  mapview(US_FIELD_with_mi %>%
            mutate(no_production_flag = case_when(
              (Annual_Gas == 0 | is.na(Annual_Gas)) & (Annual_Oil == 0 | is.na(Annual_Oil)) ~ "NA",
              TRUE ~ "YES")), zcol = "no_production_flag")


## 8.3 compare with 2021 flaring ----
PATH_FLARE_2021 = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Raw Flaring 2022"
RAW_FLARE_2021 <- read_csv(paste0(PATH_FLARE_2021, "/US_flaring_2021.csv")) %>%
                  st_as_sf( 
                       coords = c("Longitude",
                                  "Latitude"), 
                       crs = 4269)

m = mapview(RAW_FLARE_2021, col.region ="blue") +
  mapview(RAW_FLARE_2022, col.region ="red")
mapshot(m, url = paste0(PATH_VIZ, "/flaring_2021_against_2022.html"))

## 8.3 summarizing by field ----

RAW_FLARE_2022 %>%
  st_join(US_FIELD_final) %>%
  group_by(classified.type, GEOID, County_Name, AAPG.Basin) %>%
  summarize(BCM2022 = sum(`BCM 2022`),
            Avg.Temp = mean(`Avg. temp., K`, na.rm=T)) %>%
  st_drop_geometry() %>%
  write_csv(paste0(PATH_RESULT,"US_FLARE_2022_BY_FIELD.csv"))

flaring_per_field = RAW_FLARE_2022 %>%
          st_join(US_FIELD_final) %>%
          group_by(classified.type, GEOID, County_Name, AAPG.Basin) %>%
          summarize(BCM2022 = sum(`BCM 2022`),
                    Avg.Temp = mean(`Avg. temp., K`, na.rm=T)) %>%
          st_drop_geometry()

US_FIELD_WITH_FLARE <- US_FIELD_final %>%
  left_join(flaring_per_field, by = c("classified.type", "GEOID")) %>%
  mutate(BCM2022 = if_else(is.na(BCM2022), 0, BCM2022),
         FOR_mscf_bbl = if_else(is.na(Annual_Oil) | Annual_Oil == 0, BCM2022*1000/1, BCM2022*1000/Annual_Oil))

## 8.4 FOR input sheet for python run ----

flaring_per_field %>%
  filter(classified.type == "OIL") %>%
  left_join()

US_OIL %>%
  select(FIPS) %>%
  st_drop_geometry() %>%
  left_join(flaring_per_field %>%
              filter(classified.type == "OIL") %>%
              select(FIPS = GEOID,BCM2022)) %>%
  select(-classified.type, -County_Name) %>%
  mutate(BCM2022 = ifelse(is.na(BCM2022), 0, BCM2022)) %>%
  write.csv(.,paste0(PATH_RESULT,"/UScounty_withflaring_2022_oil.csv"))

----------------------------------------
#---    9 Field OPGEE Input         ----     
----------------------------------------

# Creating OPGEE inputs for each field.
# Each county has maximum two fields, OIL and/or GAS.

## 9.0 complie v1 input ----
df_ref <- read_csv(paste0(PATH_DATA, "/COUNTIES_SpatialJoin_22720_flarev3.csv"))

US_FIELD_INPUT_2022 <- US_FIELD_WITH_FLARE %>%
  mutate(age_since_ = difftime(today(), First.Prod),
         Oil_Graviy = NA,
         API_Double = NA,
         API_count = NA,
         FOR_mscf_b = FOR_mscf_bbl) %>%
  left_join(df_ref %>%
              select(-c("API_UWI",
                        "Annual_Oil",
                        "Annual_Gas",
                        "Annual_Wat",
                        "API14",
                        "True_Verti",
                        "Completion",
                        "First_Prod",
                        "Oil_Graviy",
                        "Surface_Ho",
                        "Surface__1",
                        "age_since_",
                        "API_Double",
                        "API_count",
                        "Age_count",
                        "Depth_coun",
                        #"Prov_Cod_1",
                        "FOR_mscf_b")) %>%
              mutate(FIPS = as.character(FIPS)),
            by = c("GEOID" = "FIPS")) %>%
  mutate(FIPS = GEOID) %>%
  rename(Depth_coun = Depth_count,
         First_Prod = First.Prod
  )

write_csv(US_FIELD_INPUT_2022, paste0(PATH_RESULT, "US_INPUT_2022.csv"))

US_FIELD_INPUT_2022 %>%
  filter(classified.type == "GAS") %>%
  write_csv(paste0(PATH_RESULT, "US_INPUT_2022_GAS.csv"))

## 9.1 compile input with non-zero oil per day (avoid error on Productivity Index) ----

US_GAS = US_FIELD_INPUT_2022 %>%
  mutate(
    Annual_Oil = if_else(Annual_Oil == 0, Well_count, Annual_Oil)
  ) %>%
  filter(classified.type == "GAS" & Well_count > 10) 

US_GAS %>%
  write_csv(paste0(PATH_RESULT, "US_INPUT_2022_GAS.csv", na = ""))
write_rds(US_GAS, paste0(PATH_RESULT, "us_gas_sf.rds"))

US_OIL = US_FIELD_INPUT_2022 %>%
  mutate(
    Annual_Oil = if_else(Annual_Oil == 0, Well_count, Annual_Oil)
  ) %>%
  filter(classified.type == "OIL" & Well_count > 10) 
write_rds(US_OIL, paste0(PATH_RESULT, "us_oil_sf.rds"))

US_OIL %>%
  mutate(
    C1= NA, C2 = NA, C3 = NA, `C4+`=NA, N2=NA, CO2=NA
  ) %>%
  st_drop_geometry() %>%
  write_csv(paste0(PATH_RESULT, "US_INPUT_2022_OIL.csv", na = ""))

## 9.2 Flaring 2022 input csv ----
PATH_FLARE_REF = "/Users/spencerzhang/Library/CloudStorage/GoogleDrive-zhang99@stanford.edu/Other computers/My ZBB Desktop/Spencer/Research/North America Gas/Global_Gas_OPGEE-master/Input Data/FOR/UScounty_withflaring2021_gas.csv"

ref_flare = read_csv(PATH_FLARE_REF)

flare_2022 = US_GAS %>%
  select(FIPS, BCM2022) %>%
  st_drop_geometry()

write.csv(flare_2022, paste0(PATH_RESULT, "UScounty_withflaring2022_gas.csv"))
