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

weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas) # 13.36 with gas composition
weighted.mean(df_base_prev$CI_gCO2_MJ, df_base_prev$Annual_Gas) # 13.39 without gas composition

# Note 20250826
# adding in the gas composition only affect the CI number by little
# GWP changed it drastically?

df_flare_25 = clean_result(PATH_POLICY,
                           "no_routine_flare_25_gas.xlsx",
                           "no_routine_flare_25_oil.xlsx", 
                           "offshore_flaring_moderate.xlsx",
                           "df_flare_25",
                           PATH_POLICY_Save)
df_flare_5 = clean_result(PATH_POLICY,
                          "no_routine_flare_5_gas.xlsx",
                          "no_routine_flare_5_oil.xlsx",
                          "offshore_flaring_extreme.xlsx", 
                          "df_flare_5",
                          PATH_POLICY_Save)
df_extreme = clean_result(PATH_POLICY,
                          "flare_5_vf_25_gas.xlsx",
                          "flare_5_vf_25_oil.xlsx", 
                          "offshore_extreme_flare5tile_vf75reduction.xlsx",
                          "df_extreme",
                          PATH_POLICY_Save)
df_vf_75 = clean_result(PATH_POLICY,
                        "fugitives_75_gas.xlsx",
                        "fugitives_75_oil.xlsx", 
                        "offshore_vf_75_reduction.xlsx",
                        "df_vf_75",
                        PATH_POLICY_Save)
df_vf_50 = clean_result(PATH_POLICY,
                        "fugitives_50_gas.xlsx",
                        "fugitives_50_oil.xlsx",
                        "offshore_vf_50_reduction.xlsx",
                        "df_vf_50",
                        PATH_POLICY_Save)

weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas)

1-weighted.mean(df_vf_50$CI_gCO2_MJ, df_vf_50$Annual_Gas)/weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas)
1-weighted.mean(df_vf_75$CI_gCO2_MJ, df_vf_75$Annual_Gas)/weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas)

1-weighted.mean(df_flare_25$CI_gCO2_MJ, df_flare_25$Annual_Gas)/weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas)
1-weighted.mean(df_flare_5$CI_gCO2_MJ, df_flare_5$Annual_Gas)/weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas)

1- weighted.mean(df_extreme$CI_gCO2_MJ, df_extreme$Annual_Gas)/weighted.mean(df_base$CI_gCO2_MJ, df_base$Annual_Gas)


df = df_base %>% st_drop_geometry() %>%
  left_join(df_flare_25 %>%
              select(GEOID, Production_Type, flare_25 = CI_gCO2_MJ) %>% st_drop_geometry()) %>%
  left_join(df_flare_5 %>%
              select(GEOID, Production_Type, flare_5 = CI_gCO2_MJ)%>% st_drop_geometry()) %>%
  left_join(df_extreme %>%
              select(GEOID, Production_Type, extreme = CI_gCO2_MJ)%>% st_drop_geometry()) %>%
  left_join(df_vf_50 %>%
              select(GEOID, Production_Type, vf_50 = CI_gCO2_MJ)%>% st_drop_geometry()) %>%
  left_join(df_vf_75 %>%
              select(GEOID, Production_Type, vf_75 = CI_gCO2_MJ)%>% st_drop_geometry()) 

save_local(df,"df_policy_run_merged")


# 4 OPGEE raw data ----
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
  st_drop_geometry() %>%
  left_join(df_lo %>%
              st_drop_geometry() %>%
              select(GEOID, Production_Type, CI_lo = CI_gCO2_MJ)) %>%
  left_join(df_up %>%
              st_drop_geometry() %>%
              select(GEOID, Production_Type, CI_up = CI_gCO2_MJ))

View(df_base_full)

save_local(df_base_full, "df_base_full")

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

# save final visualization dataframe to local files
save_local(df_viz, "df_viz")
df_viz = readRDS(paste0(PATH_SAVE_VIZ, "df_viz.rds"))

colnames(df_viz)

columns_to_summarize <- df_viz %>% 
  select(`129 Exploration (e) Total energy consumption MJ/MJ`:CI_gCO2_MJ,CI_lo, CI_up,
         field_midstream_factor_gCO2_MJ:midstream_up) %>% 
  colnames()

df_viz_bibasin <- df_viz %>%
  filter(!is.na(CI_gCO2_MJ)) %>%
  group_by(Basin, Production_Type) %>%
  summarize(
    field_count = n(),
    across(.cols = all_of(columns_to_summarize), 
           .fns = ~weighted.mean(as.numeric(.x), Annual_Gas, na.rm =T), 
           .names = "{.col}"),
    Well_count = sum(as.numeric(Well_count), na.rm = T),
    Annual_Gas = sum(Annual_Gas, na.rm = T),
    Annual_Oil = sum(Annual_Oil, na.rm = T),
    BCM2022 = sum(BCM2022, na.rm =T),
  )%>%
  arrange(desc(Annual_Gas)) %>%
  mutate(gas_percentage = Annual_Gas/sum(df_viz$Annual_Gas),
         gas_cum = cumsum(gas_percentage))

df_viz_basin <- df_viz %>%
  filter(!is.na(CI_gCO2_MJ)) %>%
  group_by(Basin) %>%
  summarize(
    field_count = n(),
    across(.cols = all_of(columns_to_summarize), 
           .fns = ~weighted.mean(as.numeric(.x), Annual_Gas, na.rm =T), 
           .names = "{.col}"),
    Well_count = sum(as.numeric(Well_count), na.rm = T),
    Annual_Gas = sum(Annual_Gas, na.rm = T),
    Annual_Oil = sum(Annual_Oil, na.rm = T),
    BCM2022 = sum(BCM2022, na.rm =T),
  ) %>%
  arrange(desc(Annual_Gas)) %>%
  mutate(gas_percentage = Annual_Gas/sum(df_viz$Annual_Gas),
         gas_cum = cumsum(gas_percentage))

save_local(df_viz_basin, "df_viz_basin")

save_local(df_viz_bibasin, "df_viz_bibasin")

# Assuming your sf object is named `sf_object`
st_write(df_viz_bibasin, paste0(PATH_SAVE_VIZ, "df_viz_bibasin.geojson"), driver = "GeoJSON")

df_viz_basin = readRDS(paste0(PATH_SAVE_VIZ, "df_viz_basin.rds"))
df_viz_bibasin = readRDS(paste0(PATH_SAVE_VIZ, "df_viz_bibasin.rds"))

top_15_basin <- df_viz_basin %>%
  filter(Basin != "(N/A)") %>%
  arrange(desc(Annual_Gas)) %>%
  head(n=15) %>%
  .$Basin

top_20_basin <- df_viz_basin %>%
  filter(Basin != "(N/A)") %>%
  arrange(desc(Annual_Gas)) %>%
  head(n=20) %>%
  .$Basin

df_viz_bistate <- df_viz %>%
  group_by(STATEFP, STATE_NAME, Production_Type) %>%
  summarize(
    field_count = n(),
    across(.cols = all_of(columns_to_summarize), 
           .fns = ~weighted.mean(as.numeric(.x), Annual_Gas, na.rm =T), 
           .names = "{.col}"),
    Well_count = sum(as.numeric(Well_count), na.rm = T),
    Annual_Gas = sum(Annual_Gas, na.rm = T),
    Annual_Oil = sum(Annual_Oil, na.rm = T),
    BCM2022 = sum(BCM2022, na.rm =T),
  )%>%
  arrange(desc(Annual_Gas)) %>%
  mutate(gas_percentage = Annual_Gas/sum(df_viz$Annual_Gas),
         gas_cum = cumsum(gas_percentage))

df_viz_state <- df_viz %>%
  st_drop_geometry() %>%
  group_by(STATEFP, STATE_NAME) %>%
  summarize(
    field_count = n(),
    across(.cols = all_of(columns_to_summarize), 
           .fns = ~weighted.mean(as.numeric(.x), Annual_Gas, na.rm =T), 
           .names = "{.col}"),
    Well_count = sum(as.numeric(Well_count), na.rm = T),
    Annual_Gas = sum(Annual_Gas, na.rm = T),
    Annual_Oil = sum(Annual_Oil, na.rm = T),
    BCM2022 = sum(BCM2022, na.rm =T),
  )%>%
  arrange(desc(Annual_Gas)) %>%
  mutate(gas_percentage = Annual_Gas/sum(df_viz$Annual_Gas),
         gas_cum = cumsum(gas_percentage))

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

# VFF contribution ----

vff_df <- function(raw){
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
  
  #View(data1)
  
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

df_vff = vff_df(gas_results) %>%
  mutate(Production_Type = "GAS") %>%
  rbind(
    vff_df(oil_results) %>%
      mutate(Production_Type = "OIL") 
  ) %>%
  left_join(df_viz) %>%
  mutate(fugitive_CI = fugitives*1000000/CI_denominator_MJ) %>%
  filter(!is.na(CI_gCO2_MJ))

save_local(df_vff,"df_vff")

weighted.mean(df_vff$fugitive_CI, df_vff$Annual_Gas, na.rm =T)/weighted.mean(df_vff$CI_gCO2_MJ, df_vff$Annual_Gas, na.rm =T)

(sum(df_vff$venting) + sum(df_vff$flaring))*365/1e6
sum(df_vff$fugitives)*365/1e6

View(df_vff)

sum(df_vff$fugitives)/sum(df_vff$total)
sum(df_vff$flaring)/sum(df_vff$total)
sum(df_vff$fugitives)/1e6*365
sum(df_vff$flaring)/1e6*365

df_appa = df_vff%>%
  filter(Basin == "Appalachian")

sum(df_appa$fugitives)/1e6*365

df_appa = df_vff%>%
  filter(Basin == "Permian Basin")

sum(df_appa$fugitives)/1e6*365
sum(df_appa$flaring)/1e6*365

df_vff_basin = df_vff %>%
  group_by(Basin) %>%
  summarize(Annual_Gas = sum(Annual_Gas),
            venting = sum(venting),
            flaring = sum(flaring),
            fugitives = sum(fugitives),
            total = sum(total),
            CI_de = sum(CI_denominator_MJ)) %>%
  mutate(v_mmt = venting*365/1e6,
         flare_mmt = flaring*365/1e6,
         fugitives_mmt = fugitives*365/1e6)

View(df_vff_basin)

sum(df_vff_basin$fugitives_mmt)/sum(df_vff_basin$flare_mmt)

sum(df_vff %>%
  filter(Production_Type == "GAS") %>%
  .$flaring)*365/1e6

sum(df_vff %>%
      filter(Production_Type == "GAS") %>%
      .$flaring)/sum(df_vff %>%
                       filter(Production_Type == "GAS") %>%
                       .$total)
