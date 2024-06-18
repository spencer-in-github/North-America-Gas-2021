

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))
contain_more_than_one <- function(x) sum(!is.na(x)) > 2

clean_opgee_remote_run_result <- function(df){
  
  colnames(df) <- seq(ncol(df))
  df$index <- seq(nrow(df))
  
  df = df %>%
    select(index, c(1:ncol(df)-1))
  
  df = df %>% 
    mutate(`1` = if_else(grepl("Notes", `1`), NA, `1`)) %>%
    fill(`1`, `2`)
  
  sheet <- df %>%
    unite(., Var, 1,2,3,4,5,6,7,sep = " ", remove = T, na.rm = T) %>%
    select(where(not_all_na))
  
  data1 = as.data.frame(t(sheet[8:nrow(sheet),]))
  colnames(data1) = data1[1,]
  data1 = data1[2:nrow(data1),]
  
  data1 <- data1 %>%
    select(where(not_all_na)) %>%                   # remove columns with all NAs
    subset(!rowSums(is.na(data1)) == ncol(data1))   # remove rows with all NAs
  
  return(data1)
}


keep_ERROR_input_columns <- function(df){
  colnames(df) <- seq(ncol(df))
  df <- df[,c(8:ncol(df))] %>%
    select(where(not_all_na) & where(contain_more_than_one))
  df_filtered = df[,as.vector(df[194,] == "ERROR")]
  
  return(df_filtered)
}

clean_and_join_input = function(df,input_df){
  
  cleaned_df = clean_opgee_remote_run_result(df)
  result_df = cleaned_df %>%
    filter(`194 Lifecycle GHG emissions Total CO2 sequestered` == "OK")
  
  #length(unique(df_gas_result_with_air$`20 Field properties Field name NA`)) # 620
  
  #View(US_GAS_WITH_COMP) # check pure gas input sheet
  
  result_with_input_df <- input_df %>%
    mutate(GEOID = as.numeric(GEOID)) %>%
    left_join(unique(result_df) %>%
                mutate(`20 Field properties Field name NA` = as.numeric(`20 Field properties Field name NA`)),
              by = c("GEOID" = "20 Field properties Field name NA")) %>%
    rename("CI_gCO2_MJ" = "192 Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ",
           "Check" = "194 Lifecycle GHG emissions Total CO2 sequestered")
  
  return(result_with_input_df)
}

CI_by_process = function(df){
  df_new <- df %>%
    select(FIPS, 
           STATEFP = STATEFP,
           STATE_NAME,
           County = County_Name.x, 
           Basin = AAPG.Basin.x, 
           Well_count, 
           Annual_Gas, 
           Annual_Oil, 
           BCM2022,
           Depth = True_Verti,
           Completion,
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
  
  return(df_new)
}

CI_by_process_offshore <- function(df, input_df, df_sf){
  input_df <- input_df %>%
    mutate(field = case_when(
      Production.Type == "GAS" ~ "offshore GOM gas",
      Production.Type == "OIL" ~ "offshore GOM oil",
      # gas_10000_cutoff_flag == "G" ~"offshore GOM gas",
      # gas_10000_cutoff_flag == "O" ~"offshore GOM oil"
    ))
  
  df_new <- clean_opgee_remote_run_result(df) %>%
    left_join(input_df, by = c("20 Field properties Field name NA"  = "field")) %>%
    mutate(
      FIPS = 00000, 
      STATEFP = 00,
      STATE_NAME = "GOM",
      Basin = "Offshore GOM", 
      Completion = NA
    ) %>%
    select(
           FIPS, 
           STATEFP,
           STATE_NAME,
           County = `20 Field properties Field name NA`, 
           Basin, 
           Well_count = `24 Field properties Number of producing wells -`, 
           Annual_Gas, 
           Annual_Oil, 
           BCM2022,
           Depth = `22 Field properties Field depth ft`,
           Completion,
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
           CI_gCO2_MJ = `192 Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ`) %>%
    filter(!is.na(CI_gCO2_MJ)) %>%
    mutate(
      Production_Type = case_when(
        County == "offshore GOM gas" ~ "GAS",
        County == "offshore GOM oil" ~ "OIL"
      )
    ) %>%
    left_join(df_sf %>% select(Production_Type)) %>%
    st_as_sf()
  
  
  
  return(df_new)
}


# a helper function to merge oil and gas results together
# adding a column for production type
join_oil_and_gas_runs <- function(df_g, df_o, df_offshore){
  
  df = df_g %>%
    mutate(Production_Type = "GAS") %>%
    rbind(df_o %>% mutate(Production_Type = "OIL")) %>%
    rbind(df_offshore)
  
  return(df)
}

save_to_local_no_coords <- function(df,fname, path = PATH_SAVE_VIZ){
  write_csv(df %>% st_drop_geometry(), paste0(path, fname, ".csv"))
}


save_to_local_with_coords <- function(df,fname, path = PATH_SAVE_VIZ){
  saveRDS(df, paste0(path, fname, ".rds"))
}

save_local <- function(df, fname, path = PATH_SAVE_VIZ){
  write_csv(df %>% st_drop_geometry(), paste0(path, fname, "_no_coords.csv"))
  write_csv(df, paste0(path, fname, "_with_coords.csv"))
  saveRDS(df, paste0(path, fname, ".rds"))
}

cn <- function(df){
  colnames(df)
}

v <- function(df){
  View(df)
}

u <- function(coln){
  unique(coln)
}

s <- function(df, fname){
  save_local(df, fname)
}
