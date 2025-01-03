# Plot for manuscript

# Author          Spencer Zhang
# Date            July 2024
# Last Updated    Aug 22 2024

# Updated         July 2024
#                 initialization
# Updated         Aug 22 2024
#                 clean the results with detailed process breakdown 
#                 with input into the final Excel



# 0 Environment ---------------------------
# load packages
if (!(require("tidyr"))) install.packages("tidyr"); library(tidyr)
if (!(require("dplyr"))) install.packages("dplyr"); library(dplyr)
if (!(require("readr"))) install.packages("readr"); library(readr)
if (!(require("readxl"))) install.packages("readxl"); library(readxl)
if (!(require("sf"))) install.packages("sf"); library(sf)
if (!(require("mapview"))) install.packages("mapview"); library(mapview)
if (!(require("tigris"))) install.packages("tigris"); library(tigris)
if (!(require("RColorBrewer"))) install.packages("RColorBrewer"); library(RColorBrewer)
if (!(require("leaflet"))) install.packages("leaflet"); library(leaflet)
if (!(require("leafpop"))) install.packages("leafpop"); library(leafpop)
if (!(require("openxlsx"))) install.packages("openxlsx"); library(openxlsx)
if (!(require("ggplot2"))) install.packages("ggplot2"); library(ggplot2)
if (!(require("maps"))) install.packages("maps"); library(maps)
if (!(require("forcats"))) install.packages("forcats"); library(forcats)
if (!(require("purrr"))) install.packages("purrr"); library(purrr)
if (!(require("gridExtra"))) install.packages("gridExtra"); library(gridExtra)
if (!(require("mekko"))) install.packages("mekko"); library(mekko)
if (!(require("stringr"))) install.packages("stringr"); library(stringr)
if (!(require("ggtext"))) install.packages("ggtext"); library(ggtext)
if (!(require("paletteer"))) install.packages("paletteer"); library(paletteer)


# 1 Global Variables ---------------------------

# set working directory
setwd("~/GitHub/PhD/North-America-Gas-2021")

# load helper functions from another file
source("load_functions.R")
source("ggplot_NAG_theme/theme_NAG_publication.R")

# folder for raw offshore shapefile from BOEM
PATH_OFFSHORE_RAW_SHP = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Offshore BOEM/GOM block shapefile"
# folder for storing latest OPGEE input sheet/dataframe
PATH_INPUT = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG Latest Input Sheet/"
# folder for latest OPGEE run results
PATH_OPGEE_RESULT_SHEET = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG runs/NAG aerial loss run 20240304/"
# folder for latest OPGEE uncertainty results
PATH_OPGEE_UNCERTAINTY = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG runs/20240306 uncertainty/"
# folder for storing plotted figures
PATH_SAVE_VIZ = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG publishable plot/"


# 2 Data ---------------------------

## Offshore shapefile ----
gom_sf <- read_sf(dsn = PATH_OFFSHORE_RAW_SHP, layer = "gom_block")

# merge RAW offshore block-wise shapefile into offshore_oil and offshore_gas
gom_sf_final <- enverus_offshore_sf %>%
  st_join(gom_sf %>% st_transform(st_crs(enverus_offshore_sf))) %>% 
  group_by(OBJECTID) %>%
  summarize(well_count = n(),
            Annual_Gas = sum(Annual.Gas, na.rm = T),
            Annual_Oil = sum(Annual.Oil, na.rm = T),
            Production_Type = names(which.max(table(Production.Type)))) %>%
  st_drop_geometry() %>%
  left_join(gom_sf %>% select(OBJECTID)) %>%
  st_as_sf() %>%
  group_by(Production_Type) %>%
  summarize(well_count = sum(well_count),
            Annual_Gas = sum(Annual_Gas, na.rm = T),
            Annual_Oil = sum(Annual_Oil, na.rm = T))

mapview(gom_sf_final, zcol = "Production_Type")


## Offshore Input ----
offshore_input <- enverus_offshore_sf %>%
  filter(AAPG.Geologic.Province %in% c("GOM - SHELF",      # filter points in GOM                  
                                       "GOM - DEEPWATER",
                                       "TEXAS & LOUISIANA GULF COAST BASIN")) %>% 
  mutate(First.Prod.Date = as.Date(First.Prod.Date),       # for field age
         age_in_year = as.numeric(Sys.Date() - First.Prod.Date)/365.23) %>%
  group_by(Production.Type) %>%       # group by production type, from well to field
  summarize(well_count = n(),
            Field_age = mean(age_in_year, na.rm = T),
            Field_depth = mean(True.Vertical.Depth, na.rm =T),
            Number_of_producing_well = sum(Well.Status == "ACTIVE"),
            Annual_Oil = sum(Annual.Oil, na.rm = T),
            Oil_bbl_day = Annual_Oil/365,
            Annual_Gas = sum(Annual.Gas, na.rm = T),
            Annual_Wat = sum(Annual.Water, na.rm =T),
            GOR_scf_bbl = Annual_Gas * 1000/Annual_Oil,
            WOR = Annual_Wat/Annual_Oil) %>%
  st_drop_geometry() %>% # drop enverus well geometry, match with BOEM block geometry later
  left_join(.,gom_sf_final %>% st_transform(st_crs(enverus_offshore_sf)) %>% select(Production_Type), by = c("Production.Type" = "Production_Type")) %>%
  filter(Production.Type != "OIL & GAS") %>% 
  left_join(RAW_FLARE_2022 %>% # merge with flaring
              st_join(gom_sf_final) %>%
              group_by(Production_Type) %>%
              summarize(BCM2022 = sum(`BCM 2022`, na.rm =T)) %>%
              st_drop_geometry(),  by = c("Production.Type" = "Production_Type")) %>%
  mutate(flare_to_oil_ratio = BCM2022 * 35314666572.222/Annual_Oil) %>%
  st_as_sf()

# save offshore data to local files
# csv w/o coords
write_csv(offshore_input %>% st_drop_geometry(), paste0(PATH_INPUT,"offshore_input.csv"))
# RDS with coords
saveRDS(offshore_input, paste0(PATH_INPUT, "offshore_input.RDS"))

offshore_input <- readRDS(paste0(PATH_INPUT, "offshore_input.RDS"))
offshore_input <- read.csv(paste0(PATH_INPUT,"offshore_input.csv"))

# 3 Policy run ----

clean_result = function(PATH, 
                        gas_file, oil_file, offshore_file, 
                        save_name, PATH_save){
  
  # # # for debug
  # PATH = PATH_POLICY
  # gas_file = "gas_run_updated_opgee_0719.xlsx"
  # oil_file = "oil_run_updated_opgee_0719.xlsx"
  # offshore_file = "offshore_base.xlsx"
  
  ### read OPGEE run result sheets
  gas_results = read_excel(paste0(PATH,gas_file))
  oil_results = read_excel(paste0(PATH,oil_file))
  offshore_results = read_excel(paste0(PATH, offshore_file))
  
  # # for debug
  # v(gas_results)
  # v(offshore_results)
  
  # merge with input, clean dataframe for visualization (input, process breakdown, coords, etc)
  df_viz_gas <- CI_by_process_gas(clean_and_join_input(gas_results,US_GAS_WITH_COMP_scaled))
  df_viz_oil <- CI_by_process_oil(clean_and_join_input(oil_results,US_OIL))
  df_viz_offshore <- CI_by_process_offshore(offshore_results, offshore_input, gom_sf_final) 
  
  # # for debug
  # v(df_viz_gas)
  # v(df_viz_oil)
  
  loss_rate = read.csv("/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Airplane loss rate extraction/AIRPLANE_LOSS_RATE_LOOKUP_TABLE_v3_similarity_all_airplane.csv")
  
  df_base = rbind(df_viz_gas %>% mutate(Production_Type = "GAS"), df_viz_oil%>% mutate(Production_Type = "OIL")) %>%
    rbind(df_viz_offshore) %>%
    filter(Annual_Gas > 0 & !is.na(CI_gCO2_MJ)) %>%
    mutate(GOR_scf_bbl = Annual_Gas*1000/Annual_Oil) %>% 
    select(-STATE_NAME) %>%
    left_join(states %>% st_drop_geometry() %>%
                select(STATEFP, STATE_NAME = NAME), by = c("STATEFP" = "STATEFP")) %>%
    left_join(loss_rate, by = c("GEOID" = "FIPS")) %>%
    mutate(Field_Age = as.numeric(difftime(ymd("2022-12-31"), Completion, units = "weeks")) / 52.25) %>%
    mutate(across(contains("VFF"), as.numeric)) %>%
    mutate(across(contains("Land"), as.numeric)) %>%
    rowwise() %>%
    mutate(sum_VFF = sum(c_across(contains("VFF")), na.rm = TRUE)) %>%
    mutate(sum_LUC = sum(c_across(contains("Land")), na.rm = TRUE))
  
  total_gas = sum(df_base %>%
                    filter(!is.na(CI_gCO2_MJ)) %>%
                    mutate(Annual_Gas = as.numeric(Annual_Gas)) %>%
                    .$Annual_Gas)
  
  df_base$gas_percent = df_base$Annual_Gas/total_gas
  
  df_base = df_base %>%
    mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ)) 
  df_base = df_base[order(df_base$CI_gCO2_MJ),]
  df_base$gas_cum = cumsum(df_base$gas_percent)
  
  write_csv(df_base %>% st_drop_geometry(), paste0(PATH_save, "/", save_name, ".csv"))
  
  return(df_base)
}

## base results ----

PATH_POLICY = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG runs/Policy run/"
PATH_POLICY_Save = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG runs/Policy run/policy_visualization"

files = list.files(PATH_POLICY)
files

loss_rate = read.csv("/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Airplane loss rate extraction/AIRPLANE_LOSS_RATE_LOOKUP_TABLE_v3_similarity_all_airplane.csv")

df_base %>%
  mutate(across(contains("VFF"), as.numeric)) %>%
  mutate(across(contains("Land"), as.numeric)) %>%
  rowwise() %>%
  mutate(sum_VFF = sum(c_across(contains("VFF")), na.rm = TRUE)) %>%
  mutate(sum_LUC = sum(c_across(contains("Land")), na.rm = TRUE)) %>%
  View()

df_base_prev = read.csv(paste0(PATH_POLICY_Save, "/df_base.csv"))
df_base = clean_result(PATH_POLICY,
                       #"gas_run_updated_opgee_0719.xlsx",
                       "gas_base_with_gas_comp.xlsx",
                       "oil_run_updated_opgee_0719.xlsx", 
                       "offshore_base.xlsx",
                       "df_base_20240826",
                       PATH_POLICY_Save)

View(df_base)

weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas) # 13.28 with gas composition
weighted.mean(df_base_prev$CI_gCO2_MJ, df_base_prev$Annual_Gas) # 13.39 without gas composition

## policy run results ----
df_flare_25 = clean_result(PATH_POLICY,
                           "gas_flare_25percentile (1).xlsx",
                           "no_routine_flare_25_oil.xlsx", 
                           "offshore_flaring_moderate.xlsx",
                           "df_flare_25",
                           PATH_POLICY_Save)
df_flare_5 = clean_result(PATH_POLICY,
                          "gas_flare_5percentile (1).xlsx",
                          "no_routine_flare_5_oil.xlsx",
                          "offshore_flaring_extreme.xlsx", 
                          "df_flare_5",
                          PATH_POLICY_Save)
df_extreme = clean_result(PATH_POLICY,
                          "gas_both_extreme (1).xlsx",
                          "flare_5_vf_25_oil.xlsx", 
                          "offshore_extreme_flare5tile_vf75reduction.xlsx",
                          "df_extreme",
                          PATH_POLICY_Save)
df_vf_75 = clean_result(PATH_POLICY,
                        "gas_fugitive_75 (1).xlsx",
                        "fugitives_75_oil.xlsx", 
                        "offshore_vf_75_reduction.xlsx",
                        "df_vf_75",
                        PATH_POLICY_Save)
df_vf_50 = clean_result(PATH_POLICY,
                        "gas_fugitive_50 (1).xlsx",
                        "fugitives_50_oil.xlsx",
                        "offshore_vf_50_reduction.xlsx",
                        "df_vf_50",
                        PATH_POLICY_Save)

weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas) # 13.29

sum(df_base$Annual_Gas) # 40.7TCF 
sum(df_base$Annual_Gas)/1e6/365 # 111.5BCF/day

1-weighted.mean(df_vf_50$CI_gCO2_MJ, df_vf_50$Annual_Gas)/weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas) # 34.94%
1-weighted.mean(df_vf_75$CI_gCO2_MJ, df_vf_75$Annual_Gas)/weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas) # 52.28%

1-weighted.mean(df_flare_25$CI_gCO2_MJ, df_flare_25$Annual_Gas)/weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas) # 0.02%
1-weighted.mean(df_flare_5$CI_gCO2_MJ, df_flare_5$Annual_Gas)/weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas) # 2.08%

1- weighted.mean(df_extreme$CI_gCO2_MJ, df_extreme$Annual_Gas)/weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas) # 54.37%

energy_conversion_unit = 1093 # MJ/MSCF

df_policy = df_base %>% st_drop_geometry() %>%
  left_join(df_flare_25 %>%
              select(GEOID, Production_Type, flare_25 = CI_gCO2_MJ) %>% st_drop_geometry()) %>%
  left_join(df_flare_5 %>%
              select(GEOID, Production_Type, flare_5 = CI_gCO2_MJ)%>% st_drop_geometry()) %>%
  left_join(df_extreme %>%
              select(GEOID, Production_Type, extreme = CI_gCO2_MJ)%>% st_drop_geometry()) %>%
  left_join(df_vf_50 %>%
              select(GEOID, Production_Type, vf_50 = CI_gCO2_MJ)%>% st_drop_geometry()) %>%
  left_join(df_vf_75 %>%
              select(GEOID, Production_Type, vf_75 = CI_gCO2_MJ)%>% st_drop_geometry()) %>%
  mutate(
    GHG_emission_base = Annual_Gas*CI_gCO2_MJ*energy_conversion_unit/1e12,
    GHG_emission_flare25 = Annual_Gas*flare_25*energy_conversion_unit/1e12,
    GHG_emission_flare5 = Annual_Gas*flare_5*energy_conversion_unit/1e12,
    GHG_emission_vf50 = Annual_Gas*vf_50*energy_conversion_unit/1e12,
    GHG_emission_vf75 = Annual_Gas*vf_75*energy_conversion_unit/1e12,
    GHG_emission_extreme = Annual_Gas*extreme*energy_conversion_unit/1e12
  )

colnames(df_policy) = c("Geographic ID", 
                 "State FIPS Code", 
                 "County Name", 
                 "Basin Name", 
                 "Well Count", 
                 "Annual Gas Production (MSCF)", 
                 "Annual Oil Production (BBL)", 
                 "Annual Water Production (BBL)", 
                 "Flaring Volume (BCM)", 
                 "Well Depth (FT)", 
                 "Well Completion", 
                 "Methane (C1) Percentage", 
                 "Ethane (C2) Percentage", 
                 "Propane (C3) Percentage",
                 "Butane Plus (C4+) Percentage", 
                 "Nitrogen (N2) Percentage", 
                 "Carbon Dioxide (CO2) Percentage",
                 "Exploration Energy Consumption (MJ/MJ)", 
                 "Exploration GHG Emissions (gCO2eq/MJ)", 
                 "Exploration Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
                 "Exploration VFF GHG Emissions (gCO2eq/MJ)", 
                 "Drilling & Development Energy Consumption (MJ/MJ)",
                 "Drilling & Development GHG Emissions (gCO2eq/MJ)", 
                 "Drilling & Development Combustion/Land Use GHG Emissions (gCO2eq/MJ)",
                 "Drilling & Development VFF GHG Emissions (gCO2eq/MJ)", 
                 "Crude Production Energy Consumption (MJ/MJ)", 
                 "Crude Production GHG Emissions (gCO2eq/MJ)", 
                 "Crude Production Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
                 "Crude Production VFF GHG Emissions (gCO2eq/MJ)", 
                 "Surface Processing Energy Consumption (MJ/MJ)", 
                 "Surface Processing GHG Emissions (gCO2eq/MJ)", 
                 "Surface Processing Combustion/Land Use GHG Emissions (gCO2eq/MJ)",
                 "Surface Processing VFF GHG Emissions (gCO2eq/MJ)", 
                 "LNG Energy Consumption (MJ/MJ)", 
                 "LNG GHG Emissions (gCO2eq/MJ)", 
                 "LNG Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
                 "LNG VFF GHG Emissions (gCO2eq/MJ)",
                 "Maintenance Energy Consumption (MJ/MJ)", 
                 "Maintenance GHG Emissions (gCO2eq/MJ)", 
                 "Maintenance Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
                 "Maintenance VFF GHG Emissions (gCO2eq/MJ)", 
                 "Waste Disposal Energy Consumption (MJ/MJ)", 
                 "Waste Disposal GHG Emissions (gCO2eq/MJ)", 
                 "Waste Disposal Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
                 "Waste Disposal VFF GHG Emissions (gCO2eq/MJ)", 
                 "Crude Transport Energy Consumption (MJ/MJ)", 
                 "Crude Transport GHG Emissions (gCO2eq/MJ)", 
                 "Crude Transport Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
                 "Crude Transport VFF GHG Emissions (gCO2eq/MJ)", 
                 "Crude Transport Loss Factor", 
                 "Gas Distribution Energy Consumption (MJ/MJ)", 
                 "Gas Distribution GHG Emissions (gCO2eq/MJ)", 
                 "Gas Distribution Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
                 "Gas Distribution VFF GHG Emissions (gCO2eq/MJ)", 
                 "Other Small Sources GHG Emissions (gCO2eq/MJ)", 
                 "Offsite Emissions Credit/Debit (gCO2eq/MJ)", 
                 "Process Level CH4 Emission (tCH4/day)", 
                 "CO2 Sequestration (gCO2eq/MJ)", 
                 "Upstream Carbon Intensity (gCO2eq/MJ)", 
                 "Check", 
                 #"geometry", 
                 "Production Type", 
                 "Gas-Oil Ratio (scf/bbl)", 
                 "State Name",
                 "X",
                 "Basin", 
                 "Aerial Loss Rate Source Basin", 
                 "Aerial Wellsite Methane Loss Rate", 
                 "Aerial Midstream Methane Loss Rate", 
                 "Field Age", 
                 "Total VFF GHG Emissions (gCO2eq/MJ)", 
                 "Total Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
                 "Gas Production Percentage", 
                 "Cumulative Gas Production Percentage", 
                 "CI minimal flare - moderate",
                 "CI minimal flare - extreme",
                 "CI minimal flare & minimal VFF",
                 "CI minimal VFF - moderate",
                 "CI minimal VFF - extreme",
                 "GHG emission base",
                 "GHG emission flare25",
                 "GHG emission flare5",
                 "GHG emission vf50",
                 "GHG emission vf75",
                 "GHG emission extreme")

save_local(df_policy,"df_policy_run_merged")


## uncertainty run results ----
df_up = clean_result(PATH_POLICY,
                     "gas_upper_with_gas_comp.xlsx",
                     "oil_up.xlsx", 
                     "offshore_up.xlsx",
                     "base_up",
                     PATH_POLICY_Save)
df_lo = clean_result(PATH_POLICY,
                     "gas_lower_with_gas_comp.xlsx",
                     "oil_lo.xlsx", 
                     "offshore_lo.xlsx",
                     "base_lo",
                     PATH_POLICY_Save)

colnames(df_up)
library(lubridate)

df_base_full = df_base %>%
  #st_drop_geometry() %>%
  left_join(df_lo %>%
              st_drop_geometry() %>%
              select(GEOID, Production_Type, CI_lo = CI_gCO2_MJ)) %>%
  left_join(df_up %>%
              st_drop_geometry() %>%
              select(GEOID, Production_Type, CI_up = CI_gCO2_MJ))

View(df_base_full)

save_local(df_base_full, "df_base_full")

## read midstream ----
A_field_midstream = read.csv(paste0(PATH_SAVE_VIZ, "/A_field_midstream_no_coords.csv"))

df_viz = df_base_full %>%
  select(-STATE_NAME) %>%
  left_join(states %>% st_drop_geometry() %>%
              select(STATEFP, STATE_NAME = NAME)) %>%
  mutate(STATE_NAME = case_when(is.na(STATE_NAME) ~ "Offshore Gulf of Mexico",
                                TRUE ~ STATE_NAME)) %>%
  left_join(A_field_midstream, by = c("STATE_NAME" = "prod_state")) %>%
  filter(Basin != "ARCTIC OCEAN, FED.") %>% # removed Arctic Ocean
  mutate(midstream_lo = field_midstream_factor_gCO2_MJ * 3.772/4,
         midstream_up = field_midstream_factor_gCO2_MJ * 4.228/4) %>%
  filter(Basin != "(N/A)") %>%
  mutate(Basin = case_when(
    Basin == "Offshore GOM" ~ "Offshore Gulf of Mexico",
    TRUE ~ str_to_title(Basin)
  ))

df_viz$gas_percent = df_viz$Annual_Gas/sum(df_viz$Annual_Gas)
df_viz$gas_cum = cumsum(df_viz$gas_percent)

View(df_viz)

## save cleaned df_viz ----
save_local(df_viz, "df_viz")
df_viz = readRDS(paste0(PATH_SAVE_VIZ, "df_viz.rds"))

colnames(df_viz)

df_viz_to_save = df_viz

colnames(df_viz_to_save) <- c(
  "Geographic ID", 
  "State FIPS Code", 
  "County Name", 
  "Basin Name", 
  "Well Count", 
  "Annual Gas Production (MSCF)", 
  "Annual Oil Production (BBL)", 
  "Annual Water Production (BBL)", 
  "Flaring Volume (BCM)", 
  "Well Depth (FT)", 
  "Well Completion", 
  "Methane (C1) Percentage", 
  "Ethane (C2) Percentage", 
  "Propane (C3) Percentage",
  "Butane Plus (C4+) Percentage", 
  "Nitrogen (N2) Percentage", 
  "Carbon Dioxide (CO2) Percentage",
  "Exploration Energy Consumption (MJ/MJ)", 
  "Exploration GHG Emissions (gCO2eq/MJ)", 
  "Exploration Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
  "Exploration VFF GHG Emissions (gCO2eq/MJ)", 
  "Drilling & Development Energy Consumption (MJ/MJ)",
  "Drilling & Development GHG Emissions (gCO2eq/MJ)", 
  "Drilling & Development Combustion/Land Use GHG Emissions (gCO2eq/MJ)",
  "Drilling & Development VFF GHG Emissions (gCO2eq/MJ)", 
  "Crude Production Energy Consumption (MJ/MJ)", 
  "Crude Production GHG Emissions (gCO2eq/MJ)", 
  "Crude Production Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
  "Crude Production VFF GHG Emissions (gCO2eq/MJ)", 
  "Surface Processing Energy Consumption (MJ/MJ)", 
  "Surface Processing GHG Emissions (gCO2eq/MJ)", 
  "Surface Processing Combustion/Land Use GHG Emissions (gCO2eq/MJ)",
  "Surface Processing VFF GHG Emissions (gCO2eq/MJ)", 
  "LNG Energy Consumption (MJ/MJ)", 
  "LNG GHG Emissions (gCO2eq/MJ)", 
  "LNG Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
  "LNG VFF GHG Emissions (gCO2eq/MJ)",
  "Maintenance Energy Consumption (MJ/MJ)", 
  "Maintenance GHG Emissions (gCO2eq/MJ)", 
  "Maintenance Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
  "Maintenance VFF GHG Emissions (gCO2eq/MJ)", 
  "Waste Disposal Energy Consumption (MJ/MJ)", 
  "Waste Disposal GHG Emissions (gCO2eq/MJ)", 
  "Waste Disposal Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
  "Waste Disposal VFF GHG Emissions (gCO2eq/MJ)", 
  "Crude Transport Energy Consumption (MJ/MJ)", 
  "Crude Transport GHG Emissions (gCO2eq/MJ)", 
  "Crude Transport Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
  "Crude Transport VFF GHG Emissions (gCO2eq/MJ)", 
  "Crude Transport Loss Factor", 
  "Gas Distribution Energy Consumption (MJ/MJ)", 
  "Gas Distribution GHG Emissions (gCO2eq/MJ)", 
  "Gas Distribution Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
  "Gas Distribution VFF GHG Emissions (gCO2eq/MJ)", 
  "Other Small Sources GHG Emissions (gCO2eq/MJ)", 
  "Offsite Emissions Credit/Debit (gCO2eq/MJ)", 
  "Process Level CH4 Emission (tCH4/day)", 
  "CO2 Sequestration (gCO2eq/MJ)", 
  "Upstream Carbon Intensity (gCO2eq/MJ)", 
  "Check", 
  "geometry", 
  "Production Type", 
  "Gas-Oil Ratio (scf/bbl)", 
  "Longitude", 
  "Basin", 
  "Aerial Loss Rate Source Basin", 
  "Aerial Wellsite Methane Loss Rate", 
  "Aerial Midstream Methane Loss Rate", 
  "Field Age", 
  "Total VFF GHG Emissions (gCO2eq/MJ)", 
  "Total Combustion/Land Use GHG Emissions (gCO2eq/MJ)", 
  "Gas Production Percentage", 
  "Cumulative Gas Production Percentage", 
  "Upstream Carbon Intensity (gCO2eq/MJ) Lower Bound", 
  "Upstream Carbon Intensity (gCO2eq/MJ) Upper Bound", 
  "State Name", 
  "Midstream Carbon Intensity (gCO2eq/MJ)", 
  "Midstream Carbon Intensity (gCO2eq/MJ) Lower Bound",
  "Midstream Carbon Intensity (gCO2eq/MJ) Upper Bound"
)

View(df_viz_to_save)

## calc GHG emissions in MMT ----
energy_conversion_unit = 1093 # MJ/MSCF

df_viz_to_save = df_viz_to_save %>%
  mutate(`Midstream Carbon Intensity (gCO2eq/MJ)` = ifelse(is.na(`Midstream Carbon Intensity (gCO2eq/MJ)`), 0, `Midstream Carbon Intensity (gCO2eq/MJ)`)) %>%
  mutate(`Upstream GHG Emissions (MMT CO2eq)` = `Upstream Carbon Intensity (gCO2eq/MJ)`*`Annual Gas Production (MSCF)`*energy_conversion_unit/1e12,
         `Midstream GHG Emissions (MMT CO2eq)` = `Midstream Carbon Intensity (gCO2eq/MJ)`*`Annual Gas Production (MSCF)`*energy_conversion_unit/1e12,
         `GHG Emissions (MMT CO2eq)` = `Upstream GHG Emissions (MMT CO2eq)`+`Midstream GHG Emissions (MMT CO2eq)`)

View(t)

sum(is.na(t$`GHG Emissions (MMT CO2eq)`))

s(df_viz_to_save %>% select(-geometry,-Longitude, -Basin), "df_viz_renamed")

columns_to_summarize <- df_viz_to_save %>% 
  select(`Exploration Energy Consumption (MJ/MJ)`:`Upstream Carbon Intensity (gCO2eq/MJ)`,
         `Total VFF GHG Emissions (gCO2eq/MJ)`:`Total Combustion/Land Use GHG Emissions (gCO2eq/MJ)`,
         `Upstream Carbon Intensity (gCO2eq/MJ) Lower Bound`:`Upstream Carbon Intensity (gCO2eq/MJ) Upper Bound`,
  `Midstream Carbon Intensity (gCO2eq/MJ)`:`Midstream Carbon Intensity (gCO2eq/MJ) Upper Bound`) %>% 
  st_drop_geometry() %>%
  colnames()

## basin summary ----
df_viz_bibasin <- df_viz_to_save %>%
  filter(!is.na(`Upstream Carbon Intensity (gCO2/MJ)`)) %>%
  group_by(`Basin Name`, `Production Type`) %>%
  summarize(
    `Field Count` = n(),
    across(.cols = all_of(columns_to_summarize), 
           .fns = ~weighted.mean(as.numeric(.x), `Annual Gas Production (MSCF)`, na.rm =T), 
           .names = "{.col}"),
    `Well Count` = sum(as.numeric(`Well Count`), na.rm = T),
    `Annual Gas Production (MSCF)` = sum(`Annual Gas Production (MSCF)`, na.rm = T),
    `Annual Oil Production (BBL)` = sum(`Annual Oil Production (BBL)`, na.rm = T),
    `Flaring Volume (BCM)` = sum(`Flaring Volume (BCM)`, na.rm =T)
  )%>%
  arrange(desc(`Annual Gas Production (MSCF)`)) %>%
  mutate(`Gas Production Percentage` = `Annual Gas Production (MSCF)`/sum(df_viz$Annual_Gas),
         `Cumulative Gas Production Percentage` = cumsum(`Gas Production Percentage`))

View(df_viz_bibasin)
s(df_viz_bibasin, "df_viz_bibasin")
df_viz_basin <- df_viz_to_save %>%
  filter(!is.na(`Upstream Carbon Intensity (gCO2eq/MJ)`)) %>%
  group_by(`Basin Name`) %>%
  summarize(
    `Field Count` = n(),
    across(.cols = all_of(columns_to_summarize), 
           .fns = ~weighted.mean(as.numeric(.x), `Annual Gas Production (MSCF)`, na.rm =T), 
           .names = "{.col}"),
    `Well Count` = sum(as.numeric(`Well Count`), na.rm = T),
    `Annual Gas Production (MSCF)` = sum(`Annual Gas Production (MSCF)`, na.rm = T),
    `Annual Oil Production (BBL)` = sum(`Annual Oil Production (BBL)`, na.rm = T),
    `Flaring Volume (BCM)` = sum(`Flaring Volume (BCM)`, na.rm =T),
    across(.cols = all_of(c("Upstream GHG Emissions (MMT CO2eq)",                                  
                            "Midstream GHG Emissions (MMT CO2eq)",                                 
                            "GHG Emissions (MMT CO2eq)")), 
           .fns = ~sum(as.numeric(.x), na.rm =T), 
           .names = "{.col}"),
  )%>%
  arrange(desc(`Annual Gas Production (MSCF)`)) %>%
  mutate(`Gas Production Percentage` = `Annual Gas Production (MSCF)`/sum(df_viz$Annual_Gas),
         `Cumulative Gas Production Percentage` = cumsum(`Gas Production Percentage`))

save_local(df_viz_basin, "df_viz_basin")

save_local(df_viz_bibasin, "df_viz_bibasin")

# Assuming your sf object is named `sf_object`
st_write(df_viz_bibasin, paste0(PATH_SAVE_VIZ, "df_viz_bibasin.geojson"), driver = "GeoJSON")

df_viz_basin = readRDS(paste0(PATH_SAVE_VIZ, "df_viz_basin.rds"))
df_viz_bibasin = readRDS(paste0(PATH_SAVE_VIZ, "df_viz_bibasin.rds"))

## top 15 basins ----
top_15_basin <- df_viz_basin %>%
  filter(`Basin Name` != "(N/A)") %>%
  arrange(desc(`Annual Gas Production (MSCF)`)) %>%
  head(n=15) %>%
  .$`Basin Name`

df_15_basins =df_viz_basin %>%
  filter(`Basin Name` %in% top_15_basin)

top_20_basin <- df_viz_basin %>%
  filter(`Basin Name` != "(N/A)") %>%
  arrange(desc(`Annual Gas Production (MSCF)`)) %>%
  head(n=20) %>%
  .$Basin

df_viz_bistate <- df_viz_to_save %>%
  filter(!is.na(`Upstream Carbon Intensity (gCO2eq/MJ)`)) %>%
  group_by(`State FIPS Code`, `State Name`, `Production Type`) %>%
  summarize(
    `Field Count` = n(),
    across(.cols = all_of(columns_to_summarize), 
           .fns = ~weighted.mean(as.numeric(.x), `Annual Gas Production (MSCF)`, na.rm =T), 
           .names = "{.col}"),
    `Well Count` = sum(as.numeric(`Well Count`), na.rm = T),
    `Annual Gas Production (MSCF)` = sum(`Annual Gas Production (MSCF)`, na.rm = T),
    `Annual Oil Production (BBL)` = sum(`Annual Oil Production (BBL)`, na.rm = T),
    `Flaring Volume (BCM)` = sum(`Flaring Volume (BCM)`, na.rm =T),
    across(.cols = all_of(c("Upstream GHG Emissions (MMT CO2eq)",
                            "Midstream GHG Emissions (MMT CO2eq)",
                            "GHG Emissions (MMT CO2eq)")), 
           .fns = ~sum(as.numeric(.x), na.rm =T), .names = "{.col}")
  )%>%
  arrange(desc(`Annual Gas Production (MSCF)`)) %>%
  mutate(`Gas Production Percentage` = `Annual Gas Production (MSCF)`/sum(df_viz$Annual_Gas),
         `Cumulative Gas Production Percentage` = cumsum(`Gas Production Percentage`))

df_viz_state <- df_viz_to_save %>%
  filter(!is.na(`Upstream Carbon Intensity (gCO2eq/MJ)`)) %>%
  group_by(`State FIPS Code`, `State Name`) %>%
  summarize(
    `Field Count` = n(),
    across(.cols = all_of(columns_to_summarize), 
           .fns = ~weighted.mean(as.numeric(.x), `Annual Gas Production (MSCF)`, na.rm =T), 
           .names = "{.col}"),
    `Well Count` = sum(as.numeric(`Well Count`), na.rm = T),
    `Annual Gas Production (MSCF)` = sum(`Annual Gas Production (MSCF)`, na.rm = T),
    `Annual Oil Production (BBL)` = sum(`Annual Oil Production (BBL)`, na.rm = T),
    `Flaring Volume (BCM)` = sum(`Flaring Volume (BCM)`, na.rm =T),
    across(.cols = all_of(c("Upstream GHG Emissions (MMT CO2eq)",
                            "Midstream GHG Emissions (MMT CO2eq)",
                            "GHG Emissions (MMT CO2eq)")), 
           .fns = ~sum(as.numeric(.x), na.rm =T), .names = "{.col}")
  )%>%
  arrange(desc(`Annual Gas Production (MSCF)`)) %>%
  mutate(`Gas Production Percentage` = `Annual Gas Production (MSCF)`/sum(df_viz$Annual_Gas),
         `Cumulative Gas Production Percentage` = cumsum(`Gas Production Percentage`))
save_local(df_viz_bistate, "df_viz_bistate")
save_local(df_viz_state, "df_viz_state")

df_viz_bistate = readRDS(paste0(PATH_SAVE_VIZ, "df_viz_bistate.rds"))
df_viz_state = readRDS(paste0(PATH_SAVE_VIZ, "df_viz_state.rds"))

st_write(df_viz, paste0(PATH_SAVE_VIZ, "df_viz_shp/df_viz_field.shp"), append=FALSE) #overwrite if already exists

# 5 Data for cum curve ----
df_viz %>%
  arrange(CI_gCO2_MJ) %>%
  mutate(#gas_percent = Annual_Gas/sum(df_viz$Annual_Gas),
         #gas_cum = cumsum(gas_percent),
         left = gas_cum-gas_percent,
         width = gas_percent,
         right = gas_cum) %>%
  #View(.)
  save_local(., "df_ci_cumulative")


# 6 Data for barplot ----

View(df_viz %>% filter(Basin == "Green River") %>%
       group_by(Basin) %>%
       mutate(weighted_upstream_CI_lo = sum(CI_lo * Annual_Gas) / sum(Annual_Gas)))

df_v <- df_viz %>%
  filter(Annual_Gas > 0 & !is.na(CI_gCO2_MJ) & Basin != "(N/A)") %>%
  mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ),
         Annual_Gas = as.numeric(Annual_Gas),
         field_mid_em_factor_gCO2_MJ = as.numeric(field_midstream_factor_gCO2_MJ),
         CI_lo = as.numeric(CI_lo),
         CI_up = as.numeric(CI_up)) %>%
  st_drop_geometry() %>%
  group_by(Basin) %>%
    summarise(
      `Field Count` = n(),
      `Well Count` = sum(as.numeric(Well_count), na.rm = TRUE),
      `Annual Gas Production (MSCF/year)` = sum(Annual_Gas, na.rm = TRUE),
      `Annual Oil Production (BBL/year)` = sum(Annual_Oil, na.rm = TRUE),
      `Annual Flare Volume (BCM/year)` = sum(BCM2022, na.rm = TRUE),
      weighted_upstream_CI = sum(CI_gCO2_MJ * Annual_Gas, na.rm = TRUE) / sum(ifelse(!is.na(CI_gCO2_MJ), Annual_Gas, 0), na.rm = TRUE),
      weighted_upstream_CI_lo = sum(CI_lo * Annual_Gas, na.rm = TRUE) / sum(ifelse(!is.na(CI_lo), Annual_Gas, 0), na.rm = TRUE),
      weighted_upstream_CI_up = sum(CI_up * Annual_Gas, na.rm = TRUE) / sum(ifelse(!is.na(CI_up), Annual_Gas, 0), na.rm = TRUE),
      weighted_midstream_CI = sum(field_midstream_factor_gCO2_MJ * Annual_Gas, na.rm = TRUE) / sum(ifelse(!is.na(field_midstream_factor_gCO2_MJ), Annual_Gas, 0), na.rm = TRUE),
      weighted_midstream_CI_lo = sum(midstream_lo * Annual_Gas, na.rm = TRUE) / sum(ifelse(!is.na(midstream_lo), Annual_Gas, 0), na.rm = TRUE),
      weighted_midstream_CI_up = sum(midstream_up * Annual_Gas, na.rm = TRUE) / sum(ifelse(!is.na(midstream_up), Annual_Gas, 0), na.rm = TRUE),
      Gas_Production_MCF = sum(Annual_Gas, na.rm = TRUE)
    ) %>%
  mutate(total_CI = weighted_upstream_CI + weighted_midstream_CI,
         lo = weighted_upstream_CI_lo + weighted_midstream_CI_lo,
         up = weighted_upstream_CI_up + weighted_midstream_CI_up)

View(df_v)
save_local(df_v, "df_basin")
# Filter top 15 basins by production volume
top_basins <- df_v %>%
  arrange(desc(Gas_Production_MCF)) %>%
  slice(1:15)

top_basins %>%
  rename(Upstream = weighted_upstream_CI, Midstream = weighted_midstream_CI, `Gas production` = Gas_Production_MCF) %>%
  arrange(`Gas production`) %>%
  mutate(lo_diff = total_CI - lo,
         up_diff = up - total_CI) %>%
  write_csv(., paste0(PATH_SAVE_VIZ, "df_barplot.csv"))

weighted.mean(top_basins$weighted_upstream_CI, top_basins$Gas_Production_MCF) +
weighted.mean(top_basins$weighted_midstream_CI, top_basins$Gas_Production_MCF)

# Figure 4 CI on map ---------

# Modify df_viz to include centroids

df_map <- df_viz %>%
  filter(!is.na(CI_gCO2_MJ)) %>%
  group_by(Basin) %>%
  summarize(
    # `Field Count` = n(),
    across(.cols = all_of(c("CI_gCO2_MJ", "field_midstream_factor_gCO2_MJ")),
           .fns = ~weighted.mean(as.numeric(.x), Annual_Gas, na.rm =T),
           .names = "{.col}"),
    # `Well Count` = sum(as.numeric(`Well Count`), na.rm = T),
    `Annual Gas Production (MSCF)` = sum(Annual_Gas, na.rm = T),
    # `Annual Oil Production (BBL)` = sum(`Annual Oil Production (BBL)`, na.rm = T),
    # `Flaring Volume (BCM)` = sum(`Flaring Volume (BCM)`, na.rm =T),
    # across(.cols = all_of(c("Upstream GHG Emissions (MMT CO2eq)",                                  
    #                         "Midstream GHG Emissions (MMT CO2eq)",                                 
    #                         "GHG Emissions (MMT CO2eq)")), 
    #        .fns = ~sum(as.numeric(.x), na.rm =T), 
    #        .names = "{.col}"),
  )

df_map$geometry <- st_make_valid(df_map$geometry)
df_map <- df_map %>%
  #st_as_sf(coords = c("lon", "lat"), crs = st_crs(states)) %>%  # convert df_viz to an sf object
  mutate(field_midstream_factor_gCO2_MJ = ifelse(is.na(field_midstream_factor_gCO2_MJ), 0, field_midstream_factor_gCO2_MJ),
    CI_gCO2_MJ = as.numeric(CI_gCO2_MJ),
         CI_total = CI_gCO2_MJ + field_midstream_factor_gCO2_MJ,
         CI_cate = case_when(
           #CI_gCO2_MJ < 5 ~ "0-5",
           CI_gCO2_MJ < 10 ~ "0-10",
           CI_gCO2_MJ < 20 ~ "10-20",
           CI_gCO2_MJ < 50 ~ "20-50",
           TRUE ~ ">50"
         ),
         CI_cate = fct_reorder(CI_cate, CI_total),
         CI_cate2 = case_when(
           #CI_total < 5 ~ "0-5",
           CI_total < 10 ~ "0-10",
           CI_total < 20 ~ "10-20",
           CI_total < 50 ~ "20-50",
           TRUE ~ ">50"
         ),
         CI_cate2 = fct_reorder(CI_cate2, CI_total))

# Plotting
ci_on_map <-
  ggplot() +
  geom_sf(data = states, fill = "white", color = "black", lwd = 0.2) +  # Plot US boundaries
  geom_sf(data = df_map, aes(fill = CI_cate2, alpha = `Annual Gas Production (MSCF)`), color = NA) + # plot basin as base map
  scale_fill_manual(values = c("0-10" = "#006F54", 
                               #"5-10" = "#1AECBA",
                               "10-20" = "#FEDD5C",
                               "20-50" = "#E98300",
                               ">50" = "#B1040E"),
                    name = "Carbon Intensity Range \n gCO2e MJ^-1") +
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + 
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude")

ggsave(filename = paste0(PATH_SAVE_VIZ, "ci_on_map_total.svg"), 
       plot= ci_on_map,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")

alaska_ci <- 
  ggplot() +
  geom_sf(data = alaska_sf, fill = "white", color = "black", lwd = 0.7) + 
  geom_sf(data = df_map, aes(fill = CI_cate2, alpha = `Annual Gas Production (MSCF)`), color = NA) + # plot basin as base map
  scale_fill_manual(values = c("0-10" = "#006F54", 
                               #"5-10" = "#1AECBA",
                               "10-20" = "#FEDD5C",
                               "20-50" = "#E98300",
                               ">50" = "#B1040E"),
                    name = "Carbon Intensity Range \n gCO2e MJ^-1") +
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + 
  coord_sf(expand = FALSE,xlim = c(-180, -128), ylim = c(49, 73))  

ggsave(filename = paste0(PATH_SAVE_VIZ, "alaska_ci_total.svg"), 
       plot= alaska_ci,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")


# VFF contribution ----

vff_df <- function(raw){
  
  #raw = gas_results
  df= raw[c(126,357:533),5:ncol(raw)]
  #View(df)
  
  df[1,1] = "GEOID"
  df[2:43,1] = "Venting_ton_CO2"
  df[45:86,1] = "Flaring_ton_CO2"
  df[88:129,1] = "Fugitives_ton_CO2"
  df[131:172,1] = "Total_ton_CO2"
  df[174,1] = "CI_denominator_MJ"
  
  sheet = df %>%
    unite(., Var, 1,2, sep = "_", remove = T, na.rm = T) %>%
    filter(!str_detect(Var, "-") & str_length(Var) > 0)
  
  #View(sheet)
  
  data1 = as.data.frame(t(sheet))
  colnames(data1) = data1[1,]
  data1 = data1[2:nrow(data1),]
  row.names(data1) = NULL
  
  View(data1)
  
  df <- data1 %>%
    mutate_if(is.character,as.numeric) %>%
    filter(!is.na(GEOID)) %>%
    rowwise() %>%
    mutate(
      venting = sum(c_across(starts_with("Venting_"))),
      flaring = sum(c_across(starts_with("Flaring_"))),
      fugitives = sum(c_across(starts_with("Fugitives_"))),
      total = sum(c_across(starts_with("Total_")))
    ) %>%
    ungroup() %>%
    select(GEOID, CI_denominator_MJ:total)
  
  #View(df)
  return(df)
}

vff_process_df <- function(raw){
  
  raw = gas_results
  df= raw[c(126,357:533),5:ncol(raw)]
  #View(df)
  
  df[1,1] = "GEOID"
  df[2:43,1] = "Venting_ton_CO2"
  df[45:86,1] = "Flaring_ton_CO2"
  df[88:129,1] = "Fugitives_ton_CO2"
  df[131:172,1] = "Total_ton_CO2"
  df[174,1] = "CI_denominator_MJ"
  
  sheet = df %>%
    unite(., Var, 1,2, sep = "_", remove = T, na.rm = T) %>%
    #filter(!str_detect(Var, "-") & str_length(Var) > 0)
    filter(str_length(Var) > 0)
  
  #View(sheet)
  
  data1 = as.data.frame(t(sheet))
  colnames(data1) = data1[1,]
  data1 = data1[2:nrow(data1),]
  row.names(data1) = NULL
  
  #View(data1)
  
  df <- data1 %>%
    mutate_if(is.character,as.numeric) %>%
    filter(!is.na(GEOID)) %>%
    select_if(~ !all(. == 0))
    # rowwise() %>%
    # mutate(
    #   venting = sum(c_across(starts_with("Venting_"))),
    #   flaring = sum(c_across(starts_with("Flaring_"))),
    #   fugitives = sum(c_across(starts_with("Fugitives_"))),
    #   total = sum(c_across(starts_with("Total_")))
    # ) %>%
    # ungroup() %>%
    #select(GEOID, CI_denominator_MJ:total)
  
  #View(df)
  return(df)
}


# Manuscript data ----

## upstream CI ----

View(df_viz_to_save)
weighted.mean(df_viz_to_save$`Upstream Carbon Intensity (gCO2eq/MJ)`, df_viz_to_save$`Annual Gas Production (MSCF)`) # 12.36
weighted.mean(df_viz_to_save$`Upstream Carbon Intensity (gCO2eq/MJ) Lower Bound`, df_viz_to_save$`Annual Gas Production (MSCF)`, na.rm = T) # 11.92
weighted.mean(df_viz_to_save$`Upstream Carbon Intensity (gCO2eq/MJ) Upper Bound`, df_viz_to_save$`Annual Gas Production (MSCF)`) # 13.03

## midstream CI ----
weighted.mean(df_viz_to_save$`Midstream Carbon Intensity (gCO2eq/MJ)`, df_viz_to_save$`Annual Gas Production (MSCF)`) # 3.10
weighted.mean(df_viz_to_save$`Midstream Carbon Intensity (gCO2eq/MJ) Lower Bound`, df_viz_to_save$`Annual Gas Production (MSCF)`, na.rm=T) # 2.93
weighted.mean(df_viz_to_save$`Midstream Carbon Intensity (gCO2eq/MJ) Upper Bound`, df_viz_to_save$`Annual Gas Production (MSCF)`, na.rm=T) # 3.28

## total CI ----
weighted.mean(df_viz_to_save$`Midstream Carbon Intensity (gCO2eq/MJ)`, df_viz_to_save$`Annual Gas Production (MSCF)`) + weighted.mean(df_viz_to_save$`Upstream Carbon Intensity (gCO2eq/MJ)`, df_viz_to_save$`Annual Gas Production (MSCF)`) # 15.46

## total GHG emissions ----

sum(df_viz_to_save$`GHG Emissions (MMT CO2eq)`) # 631.22 MMT CO2eq

## vff contribution ----

# vff contribution to upstream CI
weighted.mean(df_viz_to_save$`Total VFF GHG Emissions (gCO2eq/MJ)`/(df_viz_to_save$`Upstream Carbon Intensity (gCO2eq/MJ)`), df_viz_to_save$`Annual Gas Production (MSCF)`)  # 85.56%

# vff contribution to total CI
weighted.mean(df_viz_to_save$`Total VFF GHG Emissions (gCO2eq/MJ)`/(df_viz_to_save$`Upstream Carbon Intensity (gCO2eq/MJ)`+df_viz_to_save$`Midstream Carbon Intensity (gCO2eq/MJ)`), df_viz_to_save$`Annual Gas Production (MSCF)`) # 65.17%

gas_results = read_excel(paste0(PATH_POLICY, "gas_base_with_gas_comp.xlsx"))
oil_results = read_excel(paste0(PATH_POLICY, "oil_run_updated_opgee_0719.xlsx"))

View(vff_df(gas_results))

df_vff = vff_df(gas_results) %>%
  mutate(Production_Type = "GAS") %>%
  rbind(
    vff_df(oil_results) %>%
      mutate(Production_Type = "OIL") 
  ) %>%
  left_join(df_viz) %>%
  mutate(vf_portion = (fugitives+venting)/total,
         venting_portion = (venting)/total,
         fugitive_portion = (fugitives)/total,
         flaring_portion = (flaring)/total,
         fugitive_CI = fugitives*1000000/CI_denominator_MJ,
         venting_CI = venting*1000000/CI_denominator_MJ,
         flaring_CI = flaring*1000000/CI_denominator_MJ) %>%
  filter(!is.na(CI_gCO2_MJ))

df_vff_process = vff_process_df(gas_results) %>%
  mutate(Production_Type = "GAS") %>%
  rbind(
    vff_process_df(oil_results) %>%
      mutate(Production_Type = "OIL") 
  ) %>%
  left_join(df_viz) %>%
  mutate(across(`Venting_ton_CO2_Production & Extraction`:`Total_ton_CO2_- Demethanizer`, ~ . * 1000000 / CI_denominator_MJ, 
                       .names = "{col}_CI")) %>%
  filter(!is.na(CI_gCO2_MJ)) %>%
  arrange(CI_gCO2_MJ) %>%
  mutate(gas_percent = Annual_Gas/sum(df_vff_process$Annual_Gas),
    gas_cum = cumsum(gas_percent),
    left = gas_cum-gas_percent,
    width = gas_percent,
    right = gas_cum)

s(df_vff_process, "df_vff_process")

colnames(df_vff_process)

View(df_vff_process)

weighted.mean(df_vff$vf_portion, df_vff$Annual_Gas) # 89.05%
weighted.mean(df_vff$venting_portion, df_vff$Annual_Gas) # 15.75%
weighted.mean(df_vff$fugitive_portion, df_vff$Annual_Gas) # 73.30%
weighted.mean(df_vff$flaring_portion, df_vff$Annual_Gas)# 1.62%

## associated gas production ----

sum(df_viz_to_save %>%
  filter(`Production Type` == "OIL") %>%
  .$`Annual Gas Production (MSCF)`)/sum(df_viz_to_save$`Annual Gas Production (MSCF)`) # 27.96% from the fields with CI results

nrow(df_viz_to_save) # 977 fields with CIs

## gas fields with flaring ----

df_viz_to_save %>%
  filter(`Production Type` == "GAS" & !is.na(`Flaring Volume (BCM)`) & `Flaring Volume (BCM)` > 0) %>% nrow(.) # 73

df_viz_to_save %>%
  filter(`Production Type` == "GAS") %>% nrow(.) # 541

## flaring volume ----

sum(df_viz_to_save %>%
      filter(`Production Type` == "GAS") %>% .$`Flaring Volume (BCM)`)/sum(df_viz_to_save$`Flaring Volume (BCM)`) # 11.52%

## reduce flaring at the gas fields

colnames(df_vff)

sum(df_vff %>% 
  filter(Production_Type == "GAS") %>%
  .$flaring) * 365/1e6

t = df_vff %>%
  mutate(new_ci = ifelse(Production_Type == "GAS", (total-flaring)*1e6/CI_denominator_MJ,(total)*1e6/CI_denominator_MJ),
         prev_ci = total*1e6/CI_denominator_MJ) %>%
  filter(Production_Type == "GAS")

1-weighted.mean(t$new_ci, t$Annual_Gas, na.rm = T)/weighted.mean(t$prev_ci, t$Annual_Gas, na.rm = T) # 0.4% reduction in upstream CI for national average and 0.6% reduction in upstream for gas fields

## basin-level CIs ----

View(df_viz_basin)
summary(df_15_basins)

weighted.mean(df_15_basins$`Upstream Carbon Intensity (gCO2eq/MJ)`, df_15_basins$`Annual Gas Production (MSCF)`) # 12.01

weighted.mean(df_15_basins$`Upstream Carbon Intensity (gCO2eq/MJ) Upper Bound`, df_15_basins$`Annual Gas Production (MSCF)`) # 12.66
weighted.mean(df_15_basins$`Upstream Carbon Intensity (gCO2eq/MJ) Lower Bound`, df_15_basins$`Annual Gas Production (MSCF)`) # 11.35

min(df_15_basins$`Upstream Carbon Intensity (gCO2eq/MJ)`) # 6.37
max(df_15_basins$`Upstream Carbon Intensity (gCO2eq/MJ)`) # 26.16

weighted.mean(df_15_basins$`Midstream Carbon Intensity (gCO2eq/MJ)`, df_15_basins$`Annual Gas Production (MSCF)`) # 3.07

weighted.mean(df_15_basins$`Midstream Carbon Intensity (gCO2eq/MJ) Upper Bound`, df_15_basins$`Annual Gas Production (MSCF)`) # 3.25
weighted.mean(df_15_basins$`Midstream Carbon Intensity (gCO2eq/MJ) Lower Bound`, df_15_basins$`Annual Gas Production (MSCF)`) # 2.90

min(df_15_basins$`Midstream Carbon Intensity (gCO2eq/MJ)`) # 2.32
max(df_15_basins$`Midstream Carbon Intensity (gCO2eq/MJ)`) # 5.95

View(df_15_basins)
s(df_15_basins, "df_15_basins")

## compare basin-level vf contribution ----

View(df_vff)
colnames(df_vff)

columns_to_summarize_with_vff <- c("vf_portion",
                                   "venting_portion", 
                                   "flaring_portion", 
                                   "fugitive_portion")
basin_vff <- df_vff %>%
  filter(!is.na(CI_gCO2_MJ)) %>%
  group_by(Basin) %>%
  summarize(
    
    across(.cols = all_of(columns_to_summarize_with_vff), 
           .fns = ~weighted.mean(as.numeric(.x), Annual_Gas, na.rm =T), 
           .names = "{.col}"),
    Annual_Gas = sum(Annual_Gas)
    # `Well Count` = sum(as.numeric(`Well Count`), na.rm = T),
    # `Annual Gas Production (MSCF)` = sum(`Annual Gas Production (MSCF)`, na.rm = T),
    # `Annual Oil Production (BBL)` = sum(`Annual Oil Production (BBL)`, na.rm = T),
    # `Flaring Volume (BCM)` = sum(`Flaring Volume (BCM)`, na.rm =T),
    # across(.cols = all_of(c("Upstream GHG Emissions (MMT CO2eq)",                                  
    #                         "Midstream GHG Emissions (MMT CO2eq)",                                 
    #                         "GHG Emissions (MMT CO2eq)")), 
    #        .fns = ~sum(as.numeric(.x), na.rm =T), 
    #        .names = "{.col}"),
  )%>%
  arrange(desc(Annual_Gas)) %>%
  mutate(`Gas Production Percentage` = Annual_Gas/sum(df_viz$Annual_Gas),
         `Cumulative Gas Production Percentage` = cumsum(`Gas Production Percentage`))
View(basin_vff)

s(basin_vff, "basin_vff")

## basin scenario ----

View(df_policy )
columns_to_ave = c("Upstream Carbon Intensity (gCO2eq/MJ)",                  
                   #"Midstream Carbon Intensity (gCO2eq/MJ)",
                   "CI minimal flare - moderate",                                         
                   "CI minimal flare - extreme"  ,                                        
                   "CI minimal flare & minimal VFF",                                      
                   "CI minimal VFF - moderate"      ,                                     
                   "CI minimal VFF - extreme")
columns_to_sum = c("Well Count",                                                          
                   "Annual Gas Production (MSCF)",                                        
                   "Annual Oil Production (BBL)",                                         
                   "Annual Water Production (BBL)",                                       
                   "Flaring Volume (BCM)",                               
                   "GHG emission base"                ,                                   
                   "GHG emission flare25"              ,                                  
                   "GHG emission flare5"                ,                                 
                   "GHG emission vf50"                   ,                                
                   "GHG emission vf75"                    ,                               
                   "GHG emission extreme")
df_policy_basin <-  df_policy %>%
  group_by(`Basin Name`) %>%
  summarize(
    across(.cols = all_of(columns_to_ave), 
           .fns = ~weighted.mean(as.numeric(.x), `Annual Gas Production (MSCF)`, na.rm =T), 
           .names = "{.col}"),
    across(.cols = all_of(columns_to_sum),
           .fns = ~sum(as.numeric(.x), na.rm =T),
           .names = "{.col}"),
  )%>%
  arrange(desc(`Annual Gas Production (MSCF)`)) %>%
  mutate(`Gas Production Percentage` = `Annual Gas Production (MSCF)`/sum(df_viz$Annual_Gas),
         `Cumulative Gas Production Percentage` = cumsum(`Gas Production Percentage`))

View(df_policy_basin)

## highest 5percentile fields ----

View(df_viz)

df_highest_5 = df_viz %>%
  filter(gas_cum > 0.95) %>%
  mutate(vf_portion = sum_VFF/CI_gCO2_MJ)
weighted.mean(df_highest_5$CI_gCO2_MJ, df_highest_5$Annual_Gas) # 27.52
weighted.mean(df_highest_5$vf_portion, df_highest_5$Annual_Gas) # 0.94.18

df_median = df_viz %>%
  filter(gas_cum > 0.45 & gas_cum < 0.55) %>%
  mutate(vf_portion = sum_VFF/CI_gCO2_MJ)
weighted.mean(df_median$CI_gCO2_MJ, df_median$Annual_Gas) # 7.85
weighted.mean(df_median$vf_portion, df_median$Annual_Gas) # 0.8109

weighted.mean(df_viz$CI_gCO2_MJ, df_viz$Annual_Gas) # 12.35
weighted.mean(df_viz$vf_portion, df_viz$Annual_Gas) # 0.8109

weighted.mean(df_highest_5$CI_gCO2_MJ, df_highest_5$Annual_Gas)/weighted.mean(df_median$CI_gCO2_MJ, df_median$Annual_Gas) # 3.5 higher

weighted.mean(df_highest_5$CI_gCO2_MJ, df_highest_5$Annual_Gas)/weighted.mean(df_viz$CI_gCO2_MJ, df_viz$Annual_Gas) # 2.23 higher

## emissions from vf/f ----

View(df_vff)

vff_total = df_vff %>%
  mutate(venting_ghg_mmt = venting*365/1e6,
         flaring_ghg_mmt = flaring*365/1e6,
         fugitive_ghg_mmt = fugitives*365/1e6,
         total_ghg_mmt = total*365/1e6)
sum(vff_total$venting_ghg_mmt)
sum(vff_total$flaring_ghg_mmt)
sum(vff_total$fugitive_ghg_mmt) +sum(vff_total$venting_ghg_mmt)

sum(vff_total$total_ghg_mmt)

weighted.mean(df_vff$fugitive_portion, df_vff$Annual_Gas)
weighted.mean(df_vff$vf_portion, df_vff$Annual_Gas) # 89.05%

weighted.mean(df_vff$venting_portion, df_vff$Annual_Gas) # 89.05%
weighted.mean(df_vff$flaring_portion, df_vff$Annual_Gas) # 89.05%
weighted.mean(df_vff$fugitive_portion, df_vff$Annual_Gas) # 89.05%

sum(df_viz_to_save$`Upstream GHG Emissions (MMT CO2eq)`) * weighted.mean(df_vff$vf_portion, df_vff$Annual_Gas) # 449.39

sum(df_viz_to_save$`Upstream GHG Emissions (MMT CO2eq)`) * weighted.mean(df_vff$flaring_portion, df_vff$Annual_Gas) # 8.15


df_viz_full = df_viz_to_save %>%
  left_join(df_vff %>%
              select(
                GEOID, Production_Type, venting_portion, flaring_portion, fugitive_portion, vf_portion
              ), by = c("Geographic ID" = "GEOID", "Production Type" = "Production_Type")) %>%
  mutate(venting_ghg_mmt = `Upstream GHG Emissions (MMT CO2eq)` * venting_portion,
         flaring_ghg_mmt = `Upstream GHG Emissions (MMT CO2eq)` * flaring_portion,
         fugitive_ghg_mmt = `Upstream GHG Emissions (MMT CO2eq)` * fugitive_portion
         )

sum(df_viz_full$flaring_ghg_mmt, na.rm =T)
sum(df_viz_full$fugitive_ghg_mmt, na.rm =T)+sum(df_viz_full$venting_ghg_mmt, na.rm =T)


sum(df_viz_full %>%
  filter(`Production Type` == "GAS") %>%
  .$flaring_ghg_mmt, na.rm=T)

sum(df_viz_full %>%
      #filter(`Production Type` == "GAS") %>%
      .$flaring_ghg_mmt, na.rm=T)

View(basin_vff)

View(df_viz_basin)
1.704413e+02 * 0.87
7.719237e+01 * 0.9085715

## scenario run CI ----

weighted.mean(df_policy$`CI minimal flare & minimal VFF`,df_policy$`Annual Gas Production (MSCF)`) # 6.04
weighted.mean(df_policy$`CI minimal flare - extreme`,df_policy$`Annual Gas Production (MSCF)`) # 13.05
weighted.mean(df_policy$`CI minimal flare - moderate`,df_policy$`Annual Gas Production (MSCF)`,na.rm=T) # 13.25
weighted.mean(df_policy$`CI minimal VFF - extreme`,df_policy$`Annual Gas Production (MSCF)`) # 6.28
weighted.mean(df_policy$`CI minimal VFF - moderate`,df_policy$`Annual Gas Production (MSCF)`) # 8.60

1 - weighted.mean(df_policy$`CI minimal flare & minimal VFF`,df_policy$`Annual Gas Production (MSCF)`)/weighted.mean(df_policy$`Upstream Carbon Intensity (gCO2eq/MJ)`, df_policy$`Annual Gas Production (MSCF)`) # 54.56%
1 - weighted.mean(df_policy$`CI minimal flare - extreme`,df_policy$`Annual Gas Production (MSCF)`)/weighted.mean(df_policy$`Upstream Carbon Intensity (gCO2eq/MJ)`, df_policy$`Annual Gas Production (MSCF)`) # 1.80%
1 - weighted.mean(df_policy$`CI minimal flare - moderate`,df_policy$`Annual Gas Production (MSCF)`,na.rm=T)/weighted.mean(df_policy$`Upstream Carbon Intensity (gCO2eq/MJ)`, df_policy$`Annual Gas Production (MSCF)`) # 0.23%
1 - weighted.mean(df_policy$`CI minimal VFF - extreme`,df_policy$`Annual Gas Production (MSCF)`)/weighted.mean(df_policy$`Upstream Carbon Intensity (gCO2eq/MJ)`, df_policy$`Annual Gas Production (MSCF)`) # 52.77%
1 - weighted.mean(df_policy$`CI minimal VFF - moderate`,df_policy$`Annual Gas Production (MSCF)`)/weighted.mean(df_policy$`Upstream Carbon Intensity (gCO2eq/MJ)`, df_policy$`Annual Gas Production (MSCF)`) # 35.27%

## process breakdown ---

colnames(df_viz_full)

weighted.mean(as.numeric(df_viz_full$`Surface Processing GHG Emissions (gCO2eq/MJ)`), df_viz_full$`Annual Gas Production (MSCF)`, na.rm=T)/weighted.mean(as.numeric(df_viz_full$`Upstream Carbon Intensity (gCO2eq/MJ)`), df_viz_full$`Annual Gas Production (MSCF)`, na.rm=T)
weighted.mean(as.numeric(df_viz_full$`Crude Production GHG Emissions (gCO2eq/MJ)`), df_viz_full$`Annual Gas Production (MSCF)`, na.rm=T)/weighted.mean(as.numeric(df_viz_full$`Upstream Carbon Intensity (gCO2eq/MJ)`), df_viz_full$`Annual Gas Production (MSCF)`, na.rm=T)

## asso / non-asso split ----

# Q what is the associated gas / non-asso gas production split for each basin?
View(df_viz)
colnames(df_viz)
View(df_bibasin)

# Oral presentation ----
## flare maps ----
main_map <- 
  ggplot() +
  geom_sf(data = states, fill = "white", color = "black", lwd = 0.2) +  # Plot US boundaries
  geom_sf(data = RAW_FLARE_2022 %>% mutate(`BCM 2022` = as.numeric(`BCM 2022`)), 
          aes(color = `BCM 2022`, size = `BCM 2022`)) +  # Map 'BCM 2022' to both color and size
  scale_color_viridis_c(option = "plasma", direction = -1) +  # Use a continuous color scale (viridis)
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + 
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude")  # Customize labels

# Save the plot
ggsave(filename = paste0(PATH_SAVE_VIZ, "flare.svg"), 
       plot= main_map,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")


## gas composition maps ----
main_map <- 
  ggplot() +
  geom_sf(data = states, fill = "white", color = "black", lwd = 0.2) +  # Plot US boundaries
  geom_sf(data = full_gas_comp %>% mutate(ch4_weighted_avg = as.numeric(ch4_weighted_avg)), 
          aes(fill = ch4_weighted_avg)) +  # Map 'BCM 2022' to both color and size
  scale_fill_gradient(low = "red", high = "green") +  # Use a red-to-green gradient
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + 
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude")  # Customize labels

# Save the plot
ggsave(filename = paste0(PATH_SAVE_VIZ, "gas_comp.svg"), 
       plot= main_map,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")


## fugitive loss rate maps ----

aerial_basins = c("Permian Basin", "San Joaquin Basin", "Denver Julesburg", "Appalachian", "Uinta", "Ft Worth Basin")
aerial_basins = toupper(aerial_basins)


main_map <- 
  ggplot() +
  geom_sf(data = states, fill = "white", color = "black", lwd = 0.2) +  # Plot US boundaries
  geom_sf(data = US_SHAPEFILE_COUNTY_LEVEL %>% filter(AAPG.Basin %in% aerial_basins), 
          aes(fill = AAPG.Basin)) +  # Map 'BCM 2022' to both color and size
  scale_fill_gradient(low = "red", high = "green") +  # Use a red-to-green gradient
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + 
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude")  # Customize labels

# Save the plot
ggsave(filename = paste0(PATH_SAVE_VIZ, "fugitive_loss_rates.svg"), 
       plot= main_map,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")

