# ============================================================================
# Midstream Gas Flow Distance Pairs
# 
# Author: Zhihao (Spencer) Zhang
# PI: Adam Brandt
# Energy Science & Engineering, Stanford University
# Last Updated: July 30, 2025
# First Created: July 30 2025
#
# This script calculates the distance pairs for US natural gas
# interstate flows, from production field (county specific) to processing plant (plant specific)
# to delivery (state level).
# 
# The final output is a set of field-plant-state distance pairs.
# ============================================================================

# ────────────────────────────────────────────────────────────────
# Setup: Working Directory and Packages ----
# ────────────────────────────────────────────────────────────────
setwd("~/GitHub/PhD/North-America-Gas-2021")

packages <- c("tidycensus", "tigris", "dplyr", "tidyr", "sf", "mapview", 
              "readxl", "readr", "stringr", "ggplot2", "plotly")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# ────────────────────────────────────────────────────────────────
# Path and data ----
# ────────────────────────────────────────────────────────────────
path_midstream = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/midstream/"
path_new_midstream = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/midstream/"
path_delivery = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state delivery centroids/state_level_weighted_delivery_centroids.rds"
path_OPTIM = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Midstream/Optimized_Flow_updated.xlsx"
path_us_fields = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/field/us_field_2023.rds"

# US O&G FIELDS 2023
us_fields = readRDS(path_us_fields)

# PROD-PROC match from optimization model
A_field_plant = readRDS(paste0(path_midstream, "A_field_plant.rds"))
View(A_field_plant)

# PROCESSING PLANT LOCATION
plant_location = read_excel(path_OPTIM, sheet = "Original Processing plants data")
View(plant_location)

# PROCESSING PLANT LOCATION sf object
plant_location_sf = st_as_sf(plant_location, coords = c("X", "Y"), crs = 3857) %>%
  st_transform(crs = st_crs(us_fields))

mapview(plant_location_sf)

# ────────────────────────────────────────────────────────────────
# Production field to plant contribution ----
# ────────────────────────────────────────────────────────────────
# join the plant coordinates to the A_field_plant
A_field_plant_df <- A_field_plant %>%
  left_join(plant_location_sf %>% select(OBJECTID, NGPPID, NAME, CITY, PLANT_STATE = STATE, PLANT_ZIP = ZIP, PLANT_LONG = LONGITUDE, PLANT_LAT = LATITUDE), by = c("proc" = "NAME"))
View(A_field_plant_df)

A_field_plant_df <- A_field_plant_df %>%
  filter(throughput > 0) %>%
  group_by(prod_field, prod_state) %>%
  mutate(throughput_percentage = throughput/sum(throughput))

write_csv(A_field_plant_df, paste0(path_new_midstream, "field_to_proc_plant.csv"))

# sanity check 
A_field_plant_df %>%
  group_by(prod_field, prod_state) %>%
  summarise(throughput_percentage = sum(throughput_percentage))

# ────────────────────────────────────────────────────────────────
# Field to production county match ----
# ────────────────────────────────────────────────────────────────

View(us_fields)
sum(unique(us_fields$County_Name) %in% A_field_plant_df$prod_field) # 542 fields matched directly to the prod-plant mapping
sum(!unique(us_fields$County_Name) %in% A_field_plant_df$prod_field) # 113 fields weren't matched to the prod-plant mapping


# field/county names in PROD-PROC mapping
optim_field_list <- A_field_plant_df %>%
  select(prod_field, prod_state, prod_fips, prod_state_fips)
optim_field_list = unique(optim_field_list)

View(optim_field_list)

# field/county names in OPG input 2023
us_field_county_list <- us_fields %>%
  select(offshore,classified.type, FIPS,County_Name) %>%
  st_centroid()
View(us_field_county_list)

# directly match OPG fields to PROD-PROC mapping
match = us_field_county_list %>%
  st_drop_geometry() %>%
  mutate(FIPS = case_when(
    !is.na(FIPS) ~ FIPS,
    offshore == "offshore" ~ "0",
    offshore == "onshore" ~ "1"
  ),
    FIPS = as.numeric(FIPS)) %>%
  left_join(optim_field_list, by = c("FIPS" = "prod_fips"))

match = unique(match)
View(match)

# matched OPG fields
matched_field_to_field = match %>%
  filter(!is.na(prod_field))
nrow(matched_field_to_field) # 1138

# unmatched OPG fields
unmatched_field_to_field = match %>%
  filter(is.na(prod_field))
nrow(unmatched_field_to_field) # 150

# add sf coordinates to unmatched OPG fields
unmatched_field_to_field <- unmatched_field_to_field %>%
  left_join(us_field_county_list %>% 
              mutate(FIPS = case_when(
    !is.na(FIPS) ~ FIPS,
    offshore == "offshore" ~ "0",
    offshore == "onshore" ~ "1"
  ),
  FIPS = as.numeric(FIPS)) %>% select(offshore, classified.type, FIPS)) %>%
  select(offshore, classified.type, FIPS, County_Name, geometry) %>%
  st_as_sf()
View(unmatched_field_to_field)

# add sf coordinates to PROD-PROC fields
optim_field_list_sf <- optim_field_list %>%
  left_join(US_COUNTIES %>% select(GEOID) %>% mutate(GEOID = as.numeric(GEOID)),
            by = c("prod_fips" = "GEOID")) %>%
  st_as_sf() 
View(optim_field_list_sf)

# check if both sf objects have the same crs
st_crs(unmatched_field_to_field)
st_crs(optim_field_list_sf)
optim_field_list_sf_centroids <- optim_field_list_sf %>% st_centroid()
st_crs(optim_field_list_sf_centroids)

# find nearest PROD-PROC county to unmatched OPG county
nearest_idx <- st_nearest_feature(unmatched_field_to_field, optim_field_list_sf_centroids)
optim_field_nearest <- optim_field_list_sf_centroids[nearest_idx,]

unmatched_mapping <- unmatched_field_to_field %>%
  st_drop_geometry() %>%
  cbind(optim_field_nearest %>% st_drop_geometry())

final_matched_prod_to_prod <- matched_field_to_field %>%
  mutate(prod_fips = FIPS) %>%
  rbind(unmatched_mapping)

View(final_matched_prod_to_prod)
write_csv(final_matched_prod_to_prod, paste0(path_new_midstream, "matched_OPG_field_to_OPTIM_field.csv"))
saveRDS(final_matched_prod_to_prod, paste0(path_new_midstream, "matched_OPG_field_to_OPTIM_field.rds"))

final_matched_prod_to_prod = readRDS(paste0(path_new_midstream, "matched_OPG_field_to_OPTIM_field.rds"))


# ────────────────────────────────────────────────────────────────
# Plant to Delivery distance ----
# ────────────────────────────────────────────────────────────────
 
View(final_matched_prod_to_prod)
View(A_field_plant_df) # production field to processing plant (plant, state)

prod_contribution = read.csv(paste0(path_new_midstream,"proc_to_rnd_production_contribution.csv"))
recei_contribution = read.csv(paste0(path_new_midstream,"proc_to_rnd_receiving_contribution.csv"))
View(prod_contribution)

delivery_centroid = readRDS('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state delivery centroids/state_level_weighted_delivery_centroids.rds')

# plant list 
# include plant, plant_state, plant location
plant_list <- A_field_plant_df %>%
  ungroup() %>%
  select(proc, proc_state, geometry) %>%
  filter(!is.na(proc))
plant_list = unique(plant_list)
View(plant_list)

us_states <- states()
View(us_states)

plant_list <- plant_list %>%
  left_join(us_states %>% st_drop_geometry() %>% select(proc_state = STUSPS, proc_state_name = NAME))
write_csv(plant_list, paste0(path_new_midstream, "plant_list.csv"))
saveRDS(plant_list, paste0(path_new_midstream, "plant_list.rds"))
plant_list = readRDS(paste0(path_new_midstream, "plant_list.rds")) %>% st_as_sf()

# calculate plant to delivery distance 
A = delivery_centroid
B = plant_list %>% st_transform(st_crs(A))
  
dist_matrix <- st_distance(A, B)
head(dist_matrix)

rownames(dist_matrix) <- A$NAME
colnames(dist_matrix) <- B$proc

plant_to_delivery_distance <- as.data.frame(as.table(dist_matrix)) %>%
  rename(delivery_state = Var1,
         plant = Var2,
         distance_m = Freq) %>%
  drop_na(distance_m)

write_csv(plant_to_delivery_distance, paste0(path_new_midstream, "plant_to_delivery_distance.csv"))
saveRDS(dist_matrix, paste0(path_new_midstream, "plant_to_delivery_dist_matrix.rds"))

# ────────────────────────────────────────────────────────────────
# Plant-delivery pair emission factor ----
# ────────────────────────────────────────────────────────────────
plant_to_delivery_distance = readRDS(paste0(path_new_midstream, "plant_to_delivery_dist_matrix.rds"))

head(plant_to_delivery_distance)

midstream_emission_factor = 4 # kgCO2e/MMCF-km
midstream_emission_factor_low = 3.77
midstream_emission_factor_high = 4.28
tortuosity_factor_low = 1.03
tortuosity_factor_high = 1.16
tortuosity_factor = (tortuosity_factor_low+tortuosity_factor_high)/2
MJ_per_MMCF = 1094000 # MJ/MMCF
kgCO2_mmcf_to_g_MJ = 1000/MJ_per_MMCF

plant_to_delivery_EF <- plant_to_delivery_distance %>%
  mutate(distance_m=as.numeric(distance_m),
         midstream_EF_g_MJ = as.numeric(distance_m * 0.001 * midstream_emission_factor * kgCO2_mmcf_to_g_MJ*tortuosity_factor),
         midstream_EF_g_MJ_low = as.numeric(distance_m * 0.001 * midstream_emission_factor_low * kgCO2_mmcf_to_g_MJ*tortuosity_factor_low),
         midstream_EF_g_MJ_high = as.numeric(distance_m * 0.001 * midstream_emission_factor_high * kgCO2_mmcf_to_g_MJ*tortuosity_factor_high))

head(plant_to_delivery_EF)
write_csv(plant_to_delivery_EF, paste0(path_new_midstream, "plant_to_delivery_EF.csv"))
saveRDS(plant_to_delivery_EF, paste0(path_new_midstream, "plant_to_delivery_EF.rds"))

plant_to_delivery_EF = readRDS(paste0(path_new_midstream, "plant_to_delivery_EF.rds"))
head(plant_to_delivery_EF)

# ────────────────────────────────────────────────────────────────
# Production perspective emission factor ----
# ────────────────────────────────────────────────────────────────

plant_production_perspective_EF = plant_to_delivery_EF %>%
  left_join(plant_list %>% st_drop_geometry() %>% select(plant = proc, plant_state = proc_state_name)) %>%
  left_join(prod_contribution, by = c("plant_state" = "source", "delivery_state" = "destination")) %>%
  group_by(plant, plant_state) %>%
  summarize(
    midstream_EF_g_MJ = weighted.mean(midstream_EF_g_MJ,contribution_percentage, na.rm=T),
    midstream_EF_g_MJ_low = weighted.mean(midstream_EF_g_MJ_low,contribution_percentage, na.rm=T),
    midstream_EF_g_MJ_high = weighted.mean(midstream_EF_g_MJ_high,contribution_percentage, na.rm=T),
    distance = weighted.mean(distance_m,contribution_percentage, na.rm=T)
  )
View(plant_production_perspective_EF)
write_csv(plant_production_perspective_EF, paste0(path_new_midstream, "plant_perspective_EF.csv"))
plant_production_perspective_EF = read.csv(paste0(path_new_midstream, "plant_perspective_EF.csv"))

View(plant_production_perspective_EF)

# field to plant contribution
A_field_plant_df = unique(A_field_plant_df)
View(A_field_plant_df)

field_to_plant = A_field_plant_df %>%
  select(prod_fips, prod_field, prod_state, proc, proc_state, throughput) %>%
  arrange(desc(prod_fips))
head(field_to_plant)
View(field_to_plant)

field_production_perspective_EF = field_to_plant %>%
  left_join(plant_production_perspective_EF, by = c("proc" = "plant")) %>%
  filter(throughput > 0) %>%
  group_by(prod_fips, prod_field, prod_state) %>%
  summarize(
    midstream_EF_g_MJ = weighted.mean(midstream_EF_g_MJ,throughput, na.rm=T),
    midstream_EF_g_MJ_low = weighted.mean(midstream_EF_g_MJ_low,throughput, na.rm=T),
    midstream_EF_g_MJ_high = weighted.mean(midstream_EF_g_MJ_high,throughput, na.rm=T),
    distance = weighted.mean(distance,throughput, na.rm=T)
  )

View(field_production_perspective_EF)

View(final_matched_prod_to_prod)
opg_field_production_perspective_midstream_EF = final_matched_prod_to_prod %>%
  left_join(field_production_perspective_EF %>% select(prod_fips, midstream_EF_g_MJ, midstream_EF_g_MJ_low, midstream_EF_g_MJ_high, distance),
            by = c("prod_fips" = "prod_fips"))


View(opg_field_production_perspective_midstream_EF)

write_csv(opg_field_production_perspective_midstream_EF, paste0(path_new_midstream, "opg_field_production_perspective_midstream_EF.csv"))
saveRDS(opg_field_production_perspective_midstream_EF, paste0(path_new_midstream, "opg_field_production_perspective_midstream_EF.rds"))


# ────────────────────────────────────────────────────────────────
# Delivery perspective emission factor ----
# ────────────────────────────────────────────────────────────────

# filter to prod_fips that are matched to opg fields
field_to_plant_matched = field_to_plant %>%
  filter(throughput > 0 && prod_fips %in% final_matched_prod_to_prod$prod_fips)
nrow(field_to_plant_matched)

delivery_perspective_EF = plant_to_delivery_EF %>%
  left_join(plant_list %>% st_drop_geometry() %>% select(plant = proc, plant_state = proc_state_name)) %>%
  left_join(recei_contribution, by = c("plant_state" = "source", "delivery_state" = "destination")) %>%
  group_by(delivery_state) %>%
  summarize(
    midstream_EF_g_MJ = sum(midstream_EF_g_MJ * contribution_percentage * 0.01)/sum(contribution_percentage * 0.01),
    midstream_EF_g_MJ_low = sum(midstream_EF_g_MJ_low * contribution_percentage * 0.01)/sum(contribution_percentage * 0.01),
    midstream_EF_g_MJ_high = sum(midstream_EF_g_MJ_high * contribution_percentage * 0.01)/sum(contribution_percentage * 0.01),
    distance = weighted.mean(distance_m,contribution_percentage)
  )
  
View(delivery_perspective_EF)

write_csv(delivery_perspective_EF, paste0(path_new_midstream, "delivery_perspective_EF.csv"))



