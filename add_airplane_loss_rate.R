# CODE SCRIPT No.4/4

# Create airplane loss rate
# lookup table (Field - basin - lossrate)
# for NAG 2022 project

# Data source: Evan Sherwin 2023 Nat Com paper

# Author          Spencer Zhang
# Date            Jan 18 2024
# Updated         Jan 30 2024
#                 added basin similarity match
# Updated         Mar 2 2024
#                 create a table for total aerial loss rates 
#                 (include both sim and aerial loss rates reported in Evan's paper)
# Updated         Mar 6 2024
#                 aerial loss rate upper and lower limits

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
  
#PATH_RAW_AIRPLANE = "~/GitHub/PhD/North-America-Gas-2021/Data/Airplane loss rate extraction/airplane_loss_rate_lookup_table_20240118.xlsx"

#PATH_RAW_AIRPLANE = "~/GitHub/PhD/North-America-Gas-2021/Data/Airplane loss rate extraction/aerial_and_simulated_loss_rate_lookup_table_20240302.xlsx"

PATH_RAW_AIRPLANE = "~/GitHub/PhD/North-America-Gas-2021/Data/Airplane loss rate extraction/uncertainty_lower_lookup_table_20240305.xlsx"

PATH_AIRPLANE = "~/GitHub/PhD/North-America-Gas-2021/Data/Airplane loss rate extraction/"

PATH_RESULT =         "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Result Data"

----------------------------------------
#---      3 Simple Match        ----     
----------------------------------------

unique(US_GAS_WITH_COMP$`AAPG.Basin.x`)

mapview(US_GAS_WITH_COMP, zcol = "AAPG.Basin.x")


airplane_table = read.xlsx(PATH_RAW_AIRPLANE)

View(first(US_GAS_WITH_COMP))

air_gas_lookup = US_GAS_WITH_COMP %>%
  st_drop_geometry() %>%
  select(FIPS, AAPG.Basin.x) %>%
  mutate(MATCH_NAME = if_else(AAPG.Basin.x %in% unique(airplane_table$MATCH_NAME), AAPG.Basin.x, "U.S. MEAN")) %>%
  select(FIPS, MATCH_NAME) %>%
  left_join(airplane_table %>% select(-PAPER_NAME))

air_oil_lookup = US_OIL %>%
  st_drop_geometry() %>%
  select(FIPS, AAPG.Basin.x) %>%
  mutate(MATCH_NAME = if_else(AAPG.Basin.x %in% unique(airplane_table$MATCH_NAME), AAPG.Basin.x, "U.S. MEAN")) %>%
  select(FIPS, MATCH_NAME) %>%
  left_join(airplane_table %>% select(-PAPER_NAME))


field_air_lookup = rbind(air_gas_lookup,air_oil_lookup) %>%
  unique()

View(field_air_lookup)

write.csv(field_air_lookup, paste0(PATH_AIRPLANE, "FIELD_AIRPLANE_LOSS_RATE_LOOKUP_TABLE.csv"))

----------------------------------------
#---      4 Similarity Match        ----     
----------------------------------------

# updated Jan 30 2024

# starting point
US_FIELD_INPUT_2022 # 1702 fields 

# summarize the basin characteristics
BASIN_CHARACTERISTICS <- US_FIELD_INPUT_2022 %>%
  st_drop_geometry() %>%
  group_by(AAPG.Basin.x) %>%
  summarize(
    well_count = sum(Well_count, na.rm =T),
    annual_gas = sum(Annual_Gas, na.rm = T),
    annual_oil = sum(Annual_Oil, na.rm =T),
    estimated_gor = ifelse(annual_oil > 0, annual_gas/annual_oil*1000,annual_gas/well_count*1000), # Cumulative gas to oil ratio (GOR) = sum (gas) / sum(oil) * 1000 (ref:Enverus)
    well_age = weighted.mean(difftime(today(), First_Prod, unit = "days"), Age_count, na.rm = T),
    age_count = sum(Age_count)
  )

View(BASIN_CHARACTERISTICS)

# match basin to the basins with aerial loss rates

# DONE IN PYTHON, "similarity_match.ipynb"

similarity_match = read_csv(paste0(PATH_AIRPLANE, "/SIMILARITY_MATCH.csv"))
View(similarity_match)

similarity_match_with_loss_rate = similarity_match %>%
  rename(Target_basin = AAPG.Basin.x,
         Aerial_basin = `0`) %>%
  left_join(airplane_table, by=c("Aerial_basin" = "MATCH_NAME"))

View(similarity_match_with_loss_rate)

# repeated, generalize from basin to field
air_gas_lookup_new = US_GAS_WITH_COMP %>%
  st_drop_geometry() %>%
  select(FIPS, AAPG.Basin.x) %>%
  left_join(similarity_match_with_loss_rate, by = c("AAPG.Basin.x" = "Target_basin"))

View(air_gas_lookup_new)

air_oil_lookup_new = US_OIL %>%
  st_drop_geometry() %>%
  select(FIPS, AAPG.Basin.x) %>%
  left_join(similarity_match_with_loss_rate, by = c("AAPG.Basin.x" = "Target_basin"))


field_air_lookup_new = rbind(air_gas_lookup_new,air_oil_lookup_new) %>%
  unique()

View(field_air_lookup_new)

field_air_lookup_new %>% 
  mutate(AIR_WELL_LOSS_RATE = 0.01*AIR_WELL_LOSS_RATE,
         AIR_MID_LOSS_RATE = 0.01*AIR_MID_LOSS_RATE)

# write.csv(field_air_lookup_new, paste0(PATH_AIRPLANE, "AIRPLANE_LOSS_RATE_LOOKUP_TABLE_v2_similarity.csv"))

# write.csv(field_air_lookup_new %>% 
#             mutate(AIR_WELL_LOSS_RATE = 0.01*AIR_WELL_LOSS_RATE,
#                    AIR_MID_LOSS_RATE = 0.01*AIR_MID_LOSS_RATE), paste0(PATH_AIRPLANE, "AIRPLANE_LOSS_RATE_LOOKUP_TABLE_v3_similarity_all_airplane.csv"))

write.csv(field_air_lookup_new %>% 
            mutate(AIR_WELL_LOSS_RATE = 0.01*AIR_WELL_LOSS_RATE,
                   AIR_MID_LOSS_RATE = 0.01*AIR_MID_LOSS_RATE), paste0(PATH_AIRPLANE, "AIRPLANE_LOSS_RATE_LOOKUP_TABLE_v4_uncertainty_lower.csv"))

plot_df = BASIN_CHARACTERISTICS %>%
  #st_drop_geometry() %>%
  #select(FIPS, AAPG.Basin.x) %>%
  left_join(similarity_match_with_loss_rate, by = c("AAPG.Basin.x" = "Target_basin"))

write_csv(plot_df, paste0(PATH_AIRPLANE, "basin_similarity.csv"))

View(plot_df)
ggplot(plot_df) + 
  geom_boxplot(mapping = aes(x = "Aerial_basin",y ="annual_gas"))
