# Load the OPGEE remote run results and clean it into a nice data frame
# originally for Nov 28 2023 run

# Author          Spencer Zhang
# Date            November 29 2023
# Last Updated    Jan 22 2024

# Note1: No offshore run yet
# Note2: 134 error fields from Excel checks


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

# 0 Global variables and load functions ----
source("load_functions.R")

PATH = "~/GitHub/PhD/North-America-Gas-2021/NAG run 20231128/"

FILES = list.files(PATH, pattern = "^RESULT_Data*") 

# 1 Data preparation ----

## 1.1 read raw excel files ----
df = read_excel(paste0(PATH, FILES[1]))
cleaned_df = clean_opgee_remote_run_result(df)

View(cleaned_df)

cleaned_df = data.frame()
for (file in FILES){
  df = read_excel(paste0(PATH, file))
  cleaned = clean_opgee_remote_run_result(df)
  
  cleaned_df = rbind(cleaned_df, cleaned)
}

## 1.2 join OPGEE input with shapefiles for visualization ----
us_gas_result_sf <- US_GAS_WITH_COMP %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(cleaned_df %>%
              mutate(`20 Field properties Field name NA` = as.numeric(`20 Field properties Field name NA`)), by = c("GEOID" = "20 Field properties Field name NA")) %>%
  rename("CI_gCO2_MJ" = "192 Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ",
         "Check" = "194 Lifecycle GHG emissions Total CO2 sequestered")

# Viz1: map CI ----
mapview(us_gas_result_sf %>%
          mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ)),
        zcol = "CI_gCO2_MJ",
        popup = popupTable(us_gas_result_sf,
                           zcol = c("CI_gCO2_MJ")))

length(unique(cleaned_df$`20 Field properties Field name NA`))
length(unique(US_GAS_WITH_COMP$GEOID))

## 5.1 Plot county CI  ----
ci_with_input$normalized_gas = scale(ci_with_input$Annual_Gas)

# Define breaks for your categories
breaks <- c(0, 10, 50, 100, 1000)
# Create categories based on the breaks
us_gas_result_sf$ci_category <- cut(as.numeric(us_gas_result_sf$CI_gCO2_MJ), 
                                 breaks = breaks, 
                                 labels = c("0-10",
                                            "10-50",
                                            "50-100",
                                            "100-1000"))
# Define a color palette with distinct colors for each category
color_palette <- colorRampPalette(c("darkgreen", "greenyellow", "yellow", "red"))(length(breaks) - 1)

m = mapview(us_gas_result_sf, zcol = "ci_category", 
            layer.name= "Field CI Range (gCO2e per MJ)",
            col.regions = color_palette,
            alpha.regions = "Annual_Gas",
            popup = popupTable(us_gas_result_sf,
                               zcol = c("County_Name.x",
                                        "CI_gCO2_MJ",
                                        "Annual_Gas",
                                        "Annual_Oil",
                                        "BCM2022")))
#alpha.regions = "normalized_gas")
mapshot(m, paste0(PATH, "/NAG_CI_2022_with_production_alpha_channel.html"))

# Flag1: check ERROR vs OK? ----

## filter fields with ERROR and save its original input/result sheet to a separate file ----
## filter GEOID


df = read_excel(paste0(PATH,FILES[1]))
df_error1 = keep_ERROR_input_columns(df)

df = read_excel(paste0(PATH,FILES[2]))
df_error2 = keep_ERROR_input_columns(df)

df = read_excel(paste0(PATH,FILES[3]))
df_error3 = keep_ERROR_input_columns(df)

df_errors <- cbind(df_error1,df_error2)
df_errors <- cbind(df_errors,df_error3)

write.xlsx(df_errors, "ERROR_fields_input_v1_dec12023.xlsx")


# Flag2: scatter plot Volume vs CI ----
# in python
library(ggplot2)
ggplot(us_gas_result_sf %>% 
         mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ),
                Annual_Gas = as.numeric(Annual_Gas)), 
       aes(x = "Annual_Gas", y = "CI_gCO2_MJ")) +
  geom_point()


# Misc1: save excel to local for python plotting ----
write.xlsx(us_gas_result_sf%>% st_drop_geometry(), paste0(PATH, "NAG_CI_20231128_with_input_no_sf.xlsx"))

# Stat1: vol-weighted CI
weighted.mean(as.numeric(us_gas_result_sf$CI_gCO2_MJ), as.numeric(us_gas_result_sf$Annual_Gas))


# Misc2: inspect overlap in us map 2022 ----
mapview(US_FIELD_INPUT_2022 %>%
  filter(GEOID == "48021"), zcol = "classified.type")
mapview(US_FIELD_INPUT_2022 %>%
          filter(GEOID == "48021" & classified.type == "GAS"), zcol = "classified.type")

mapview(US_FIELD_INPUT_2022 %>%
          filter(GEOID == "48021" & classified.type == "OIL"), zcol = "classified.type")

## re-run voronoi for this county and see why ----

  county = COUNTIES %>% filter(GEOID == "48021")
  temp = df_classified_sf[county,]
  
  temp_dup = temp %>%
    group_by(geometry) %>%
    mutate(cnt = n()) %>%
    filter(cnt > 1) #%>%
    #mutate(geometry = geometry + 0.0001*runif(1))
  
  length(unique(temp_dup$geometry)) 
  
  mapview(temp_dup)
  
  mapview(temp, zcol = "classified.type") +
    mapview(county)
  
  county_voronoi <-
    temp %>%
    st_transform(2236) %>%
    st_union() %>% 
    st_voronoi() %>% 
    st_cast() %>% 
    st_as_sf() %>% 
    st_join(temp %>% st_transform(2236)) %>%
    st_intersection(.,county %>% st_transform(2236)) %>%
    st_transform(st_crs(df_classified_sf)) %>%
    group_by(geometry) %>%  # there are wells that have duplicated geometry
      mutate(classified.type = first(classified.type)) %>% # using this grouping method for simple handling 
      group_by(classified.type) %>%
      summarize()
    
  
  mapview(county_voronoi) + 
    mapview(temp, zcol = "classified.type")
  
  fixed_geometries <- county_voronoi$geometry %>% st_make_valid()
  
  county_voronoi$geometry <- fixed_geometries
  
  aapg = county_voronoi %>% 
    group_by(AAPG.Geologic.Province) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    pull(AAPG.Geologic.Province) %>%
    .[1]
  
  merged <- temp %>%
    #st_as_sf() %>%
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
              True_Verti = mean(`True Vertical Depth`, na.rm = T),
              Completion = mean(as.Date(`Completion Date`), na.rm=T),
              First.Prod = mean(as.Date(`First Prod Date`), na.rm =T),
              #Oil_Gravity = weighted.mean(Oil.Gravity, Annual_Oil, na.rm = T),
              #Oil_Gravity = Annual_Gas/Annual_Oil*1000,
              #API_count = sum(!is.na(Oil_Gravity)),
              Age_count = sum(!is.na(`First Prod Date`)),
              Depth_count = sum(!is.na(`True Vertical Depth`)),
              #Monthly.BOE = sum(Monthly.BOE, na.rm = T),
              Well_count = n()
              #FOR_BCM2021 = sum(temp_flare$`BCM 2021`)
              ) %>%
    st_drop_geometry() %>%
    left_join(county_voronoi) %>%
    st_as_sf()
  mapview(merged, zcol = "classified.type")
  
  
  
# find out duplicated geometery
  
temp_dup = temp %>%
  group_by(geometry) %>%
  mutate(cnt = n()) %>%
  filter(cnt > 1)

View(temp_dup)

# Jan 22 with Airplane loss rate -----

PATH = "~/GitHub/PhD/North-America-Gas-2021/NAG run 20240122/"

# 1 Gas runs ----
FILES = list.files(PATH, pattern = "^result_gas*") 

# Data preparation 
# read raw excel files 
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

## 1.2 join OPGEE input with shapefiles for visualization ----

View(US_GAS_WITH_COMP) # check pure gas input sheet

df_gas_run_with_air_with_input <- US_GAS_WITH_COMP %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(unique(df_gas_result_with_air) %>%
              mutate(`20 Field properties Field name NA` = as.numeric(`20 Field properties Field name NA`)),
            by = c("GEOID" = "20 Field properties Field name NA")) %>%
  rename("CI_gCO2_MJ" = "192 Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ",
         "Check" = "194 Lifecycle GHG emissions Total CO2 sequestered")

View(df_gas_run_with_air_with_input)

## Viz1: map CI ----
mapview(df_gas_run_with_air_with_input %>%
          mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ)),
        zcol = "CI_gCO2_MJ",
        popup = popupTable(df_gas_run_with_air_with_input,
                           zcol = c("CI_gCO2_MJ")))

length(unique(cleaned_df$`20 Field properties Field name NA`))
length(unique(US_GAS_WITH_COMP$GEOID))

# 2 Oil runs  ----
FILES = list.files(PATH, pattern = "^result_oil*") 

# Data preparation 
# read raw excel files 
df = read_excel(paste0(PATH, FILES[1]))
cleaned_df = clean_opgee_remote_run_result(df)

View(cleaned_df)

df_oil_result_with_air = cleaned_df %>%
  filter(`194 Lifecycle GHG emissions Total CO2 sequestered` == "OK")

length(unique(df_oil_result_with_air$`20 Field properties Field name NA`)) # 674

## 1.2 join OPGEE input with shapefiles for visualization ----

View(US_OIL) # check pure gas input sheet

df_oil_result_with_air_with_input <- US_OIL %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(unique(df_oil_result_with_air) %>%
              mutate(`20 Field properties Field name NA` = as.numeric(`20 Field properties Field name NA`)),
            by = c("GEOID" = "20 Field properties Field name NA")) %>%
  rename("CI_gCO2_MJ" = "192 Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ",
         "Check" = "194 Lifecycle GHG emissions Total CO2 sequestered")

View(df_oil_result_with_air_with_input)
# Viz1: map CI ----
mapview(df_oil_result_with_air_with_input %>%
          mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ)),
        zcol = "CI_gCO2_MJ",
        popup = popupTable(df_oil_result_with_air_with_input,
                           zcol = c("CI_gCO2_MJ")))



## 5.1 Plot county CI  ----
df_gas_run_with_air_with_input$normalized_gas = scale(df_gas_run_with_air_with_input$Annual_Gas)

# Define breaks for your categories
breaks <- c(0, 10, 50, 100, 1000)
# Create categories based on the breaks
df_gas_run_with_air_with_input$ci_category <- cut(as.numeric(df_gas_run_with_air_with_input$CI_gCO2_MJ), 
                                    breaks = breaks, 
                                    labels = c("0-10",
                                               "10-50",
                                               "50-100",
                                               "100-1000"))

# Define a color palette with distinct colors for each category
color_palette <- colorRampPalette(c("darkgreen", "greenyellow", "yellow", "red"))(length(breaks) - 1)

m = mapview(df_gas_run_with_air_with_input, zcol = "ci_category", 
            layer.name= "Field CI Range (gCO2e per MJ)",
            col.regions = color_palette,
            alpha.regions = "Annual_Gas",
            popup = popupTable(df_gas_run_with_air_with_input,
                               zcol = c("County_Name.x",
                                        "CI_gCO2_MJ",
                                        "Annual_Gas",
                                        "Annual_Oil",
                                        "BCM2022")))

mapshot(m, paste0(PATH, "/NAG_CI_2022_with_production_alpha_channel.html"))

# Misc1: save excel to local for python plotting ----
write.xlsx(df_gas_run_with_air_with_input %>% st_drop_geometry(), paste0(PATH, "/NAG_CI_with_air_20240122_with_input_no_sf.xlsx"))

saveRDS(df_gas_run_with_air_with_input, paste0(PATH, "/NAG_CI_with_air_20240122_with_input_with_sf.rds"))

# Stat1: vol-weighted CI
weighted.mean(as.numeric(us_gas_result_sf$CI_gCO2_MJ), as.numeric(us_gas_result_sf$Annual_Gas))


# Misc2: inspect overlap in us map 2022 ----
mapview(US_FIELD_INPUT_2022 %>%
          filter(GEOID == "48021"), zcol = "classified.type")
mapview(US_FIELD_INPUT_2022 %>%
          filter(GEOID == "48021" & classified.type == "GAS"), zcol = "classified.type")

mapview(US_FIELD_INPUT_2022 %>%
          filter(GEOID == "48021" & classified.type == "OIL"), zcol = "classified.type")

