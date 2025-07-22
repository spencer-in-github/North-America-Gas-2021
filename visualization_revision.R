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
library(lubridate)


# set working directory
setwd("~/GitHub/PhD/North-America-Gas-2021")

# load helper functions from another file
source("load_functions.R")

clean_result = function(PATH, 
                        gas_file, oil_file, 
                        #offshore_file,
                        loss_rate_path,
                        save_name, PATH_save){

  
  ### read OPGEE run result sheets
  gas_results = read_excel(paste0(PATH,gas_file))
  oil_results = read_excel(paste0(PATH,oil_file))
  #offshore_results = read_excel(paste0(PATH, offshore_file))
  
  # merge with input, clean dataframe for visualization (input, process breakdown, coords, etc)
  df_viz_gas <- CI_by_process_gas(clean_and_join_input(gas_results,US_GAS_WITH_COMP_scaled))
  df_viz_oil <- CI_by_process_oil(clean_and_join_input(oil_results,US_OIL))
  #df_viz_offshore <- CI_by_process_offshore(offshore_results, offshore_input, gom_sf_final) 
  
  loss_rate = read.csv(loss_rate_path)
  
  df_base = rbind(df_viz_gas %>% mutate(Production_Type = "GAS"), df_viz_oil%>% mutate(Production_Type = "OIL")) %>%
    #rbind(df_viz_offshore) %>%
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


df_cleaned = clean_result(PATH = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/result/20250704 UPDATED GHGRP MATCHED AERIAL/', 
            gas_file = "result_gas.xlsx", 
            oil_file = "result_oil.xlsx", #offshore_file, 
            loss_rate_path = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/GHGRP equipment data/equipment_match_aerial_rate_lookup_table.csv',
            save_name = "result_ghgrp_matched_loss_rates", 
            PATH_save = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/result")

weighted.mean(df_cleaned$CI_gCO2_MJ, df_cleaned$Annual_Gas) # now 8.97 / previous 13.28 with gas composition

sum(df_cleaned$Annual_Gas) # 40.06 TCF 
sum(df_cleaned$Annual_Gas)/1e6/365 # 109.77 BCF/day
