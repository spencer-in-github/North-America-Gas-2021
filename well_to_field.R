# ────────────────────────────────────────────────────────────────
# Setup: Working Directory and Packages ----
# ────────────────────────────────────────────────────────────────
setwd("~/GitHub/PhD/North-America-Gas-2021")

packages <- c("sf", "mapview",
              "readr", "dplyr",
              "readxl", "rgdal")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# ────────────────────────────────────────────────────────────────
# US O&G field shapefile ----
# ────────────────────────────────────────────────────────────────
# contain gas composition
us_gas_sf <- readRDS('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Compiled OPGEE Input/US_GAS_INPUT_v4_complete_comp_scaled_20240130_sf.rds')
View(us_gas_sf)

us_oil_sf <- readRDS('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Result Data/us_oil_sf.rds')
View(us_oil_sf)

mapview(us_oil_sf, col.regions = 'green') + mapview(us_gas_sf, col.regions = 'red')


# ────────────────────────────────────────────────────────────────
# Group 2023 production data ----
# ────────────────────────────────────────────────────────────────
# Step 1: Read the CSV
well_sf <- read_csv('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/enverus 2023/annual_well_production_2023.csv')

# Step 2: Inspect column names to identify coordinates
names(well_sf)  # Check for column names like "longitude", "latitude", "lon", "lat", etc.

# Let's assume columns are named "Longitude" and "Latitude"
# Step 3: Convert to sf object
well_sf <- well_sf %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(us_gas_sf))  # Use WGS 84 (EPSG:4326)

# Step 4: Plot (optional)
mapview::mapview(well_sf %>%
                   slice_sample(n = 1000))

# Step 3: Spatial join wells to fields (first gas, then oil for unmatched)
well_gas <- st_join(well_sf, us_gas_sf, join = st_within, left = FALSE)
remaining_wells <- well_sf[!well_sf$API_UWI %in% well_gas$API_UWI.x, ]
well_oil <- st_join(remaining_wells, us_oil_sf, join = st_within, left = FALSE)
remaining_wells <- remaining_wells[!remaining_wells$API_UWI %in% well_oil$API_UWI.x, ]
mapview(remaining_wells)

sum(remaining_wells$GasProd_MCF)/sum(well_sf$GasProd_MCF) # 2%
sum(remaining_wells$LiquidsProd_BBL)/sum(well_sf$LiquidsProd_BBL) # 16%

table(well_gas$ENVWellType)

# Combine both
well_all <- bind_rows(well_gas, well_oil)

# Step 4: Aggregation by Field_ID (replace with actual field ID column name)
field_summary <- well_all %>%
  st_drop_geometry() %>%
  group_by(FIPS, classified.type) %>%  # Replace with actual column from shapefile
  summarise(
    total_oil_bbl = sum(LiquidsProd_BBL, na.rm = TRUE),
    total_gas_mcf = sum(GasProd_MCF, na.rm = TRUE),
    total_water_bbl = sum(WaterProd_BBL, na.rm = TRUE),
    
    # Volume-weighted averages (handle NA-safe)
    avg_True_depth_ft = weighted.mean(TVD_FT, Prod_MCFE, na.rm = TRUE),
    avg_temp_degF = weighted.mean(Bottom_Hole_Temp_DEGF, Prod_MCFE, na.rm = TRUE),
    avg_oil_api = weighted.mean(OilGravity_API, LiquidsProd_BBL, na.rm = TRUE),
    avg_gas_gravity = weighted.mean(GasGravity_SG, GasProd_MCF, na.rm = TRUE),
    avg_field_age = 2023- min(as.numeric(format(as.Date(CompletionDate), "%Y"))),
    
    # Count well types
    oil_well_count = sum(ENVWellType == "OIL", na.rm = TRUE),
    gas_well_count = sum(ENVWellType == "GAS", na.rm = TRUE),
    ong_well_count = sum(ENVWellType == "OIL & GAS", na.rm = TRUE),
    
    total_well_count = n(),
    offshore = "onshore"
  )

View(field_summary)

# Optional: Join summary back to polygon field layer

us_gas_sf_2023 <- us_gas_sf %>% 
  left_join(field_summary, by = c("FIPS", "classified.type"))

us_oil_sf_2023 <- us_oil_sf %>% 
  left_join(field_summary, by = c("FIPS", "classified.type"))

mapview(us_gas_sf_2023) + mapview(us_oil_sf_2023)

saveRDS(us_gas_sf_2023, "revision_data/field/us_gas_sf_2023.rds")
saveRDS(us_oil_sf_2023, "revision_data/field/us_oil_sf_2023.rds")
# ────────────────────────────────────────────────────────────────
# offshore ----
# ────────────────────────────────────────────────────────────────

setwd("~/GitHub/PhD/North-America-Gas-2021")

gom_lease_spdf <- readOGR( 
  dsn= paste0(getwd(),"/Data/Offshore BOEM/Mapping/active_lease") , 
  layer="al_20231002",
  verbose=FALSE
)

mapview(gom_lease_spdf)
View(gom_lease_spdf)

gom_lease_sf <- st_as_sf(gom_lease_spdf) %>%
  select(LEASE_NUMB,geometry)

offshore <- remaining_wells %>%
  filter(LiquidsProd_BBL+GasProd_MCF > 0 ) %>%
  st_join(gom_lease_sf %>% st_transform(st_crs(remaining_wells))) %>%
  st_drop_geometry() %>%
  group_by(LEASE_NUMB) %>%  # Replace with actual column from shapefile
  summarise(
    total_oil_bbl = sum(LiquidsProd_BBL, na.rm = TRUE),
    total_gas_mcf = sum(GasProd_MCF, na.rm = TRUE),
    total_water_bbl = sum(WaterProd_BBL, na.rm = TRUE),
    
    # Volume-weighted averages (handle NA-safe)
    avg_True_depth_ft = weighted.mean(TVD_FT, Prod_MCFE, na.rm = TRUE),
    avg_temp_degF = weighted.mean(Bottom_Hole_Temp_DEGF, Prod_MCFE, na.rm = TRUE),
    avg_oil_api = weighted.mean(OilGravity_API, LiquidsProd_BBL, na.rm = TRUE),
    avg_gas_gravity = weighted.mean(GasGravity_SG, GasProd_MCF, na.rm = TRUE),
    avg_field_age = 2023- min(as.numeric(format(as.Date(CompletionDate), "%Y"))),
    
    # Count well types
    oil_well_count = sum(ENVWellType == "OIL", na.rm = TRUE),
    gas_well_count = sum(ENVWellType == "GAS", na.rm = TRUE),
    ong_well_count = sum(ENVWellType == "OIL & GAS", na.rm = TRUE),
    
    total_well_count = n()
  ) %>%
  mutate(offshore = ifelse(is.na(LEASE_NUMB), "onshore", "offshore")) %>%
  mutate(classified.type = ifelse(total_gas_mcf*1000/total_oil_bbl > 10000, "GAS", "OIL")) %>%
  left_join(gom_lease_sf %>% st_transform(st_crs(remaining_wells))) %>%
  st_as_sf() %>%
  group_by(offshore, classified.type) %>%
  summarise(
    
    
    # Volume-weighted averages (handle NA-safe)
    avg_True_depth_ft = weighted.mean(avg_True_depth_ft, total_gas_mcf, na.rm = TRUE),
    avg_temp_degF = weighted.mean(avg_temp_degF, total_gas_mcf, na.rm = TRUE),
    avg_oil_api = weighted.mean(avg_oil_api, total_oil_bbl, na.rm = TRUE),
    avg_gas_gravity = weighted.mean(avg_gas_gravity, total_gas_mcf, na.rm = TRUE),
    avg_field_age = max(avg_field_age),
    
    total_oil_bbl = sum(total_oil_bbl, na.rm = TRUE),
    total_gas_mcf = sum(total_gas_mcf, na.rm = TRUE),
    total_water_bbl = sum(total_water_bbl, na.rm = TRUE),
    
    # Count well types
    oil_well_count = sum(oil_well_count, na.rm = TRUE),
    gas_well_count = sum(gas_well_count, na.rm = TRUE),
    ong_well_count = sum(ong_well_count, na.rm = TRUE),
    
    total_well_count = sum(total_well_count, na.rm = TRUE)
  )
  

mapview(offshore, zcol = "classified.type")


offshore %>%
  group_by(offshore) %>%
  summarise( total_oil_bbl = sum(total_oil_bbl, na.rm = TRUE),
             total_gas_mcf = sum(total_gas_mcf, na.rm = TRUE),
             total_water_bbl = sum(total_water_bbl, na.rm = TRUE))

saveRDS(offshore, "revision_data/field/offshore.rds")

# ────────────────────────────────────────────────────────────────
# merge all fields ----
# ────────────────────────────────────────────────────────────────


# common columns in both oil and gas df
common_cols <- intersect(names(us_gas_sf_2023), names(us_oil_sf_2023))
common_cols

# oil unique columns, from vintage census data, not important
oil_unique_cols <- setdiff(names(us_oil_sf_2023), names(us_gas_sf_2023))
oil_unique_cols

# final columns to keep in all df
all_cols <- Reduce(union, list(names(offshore), names(us_gas_sf_2023)))
all_cols

# Step 3: Define a helper function to align columns
align_columns <- function(sf_obj, all_cols) {
  missing_cols <- setdiff(all_cols, names(sf_obj))
  for (col in missing_cols) {
    sf_obj[[col]] <- NA
  }
  sf_obj <- sf_obj[, all_cols]  # reorder columns
  return(sf_obj)
}

# Step 4: Apply the function to each sf object
sf_offshore <- align_columns(offshore, all_cols)
sf_gas <- align_columns(us_gas_sf_2023, all_cols)
sf_oil <- align_columns(us_oil_sf_2023, all_cols)

# Step 5: Row-bind the aligned sf dataframes
sf_combined <- bind_rows(sf_offshore, sf_gas, sf_oil) %>%
  mutate(across(
    .cols = !all_of(attr(., "sf_column")),  # exclude geometry column
    .fns = ~ ifelse(is.nan(.), NA, .)
  ))

# Step 6: View row count to confirm
nrow(sf_combined) 
View(sf_combined)

write.csv(sf_combined %>% st_drop_geometry(), "revision_data/field/sf_combined.csv")

saveRDS(sf_combined, "revision_data/field/sf_combined.rds")
mapview(sf_combined, zcol = "classified.type")

# ────────────────────────────────────────────────────────────────
# flaring ----
# ────────────────────────────────────────────────────────────────

# Step 1: Read Excel file (adjust sheet name if needed)
total_flare_df <- read_excel('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/flaring 2023/VIIRS_Global_flaring_d.7_slope_0.029353_2023_v20230614_web_IDmatch.xlsx', sheet = "flare upstream") %>%
  #filter(Country == "United States") %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(us_gas_sf))

total_flare_df %>%
  group_by(Country) %>%
  summarize(flare = sum(`BCM 2023`))%>%
  arrange(desc(flare)) # russia, iran, iraq

sum(total_flare_df %>%
  filter(Country %in% c("Russian Federation", "Iran", "Iraq", "United States")) %>%
  .$`BCM 2023`) / sum(total_flare_df$`BCM 2023`) # 0.5254

flaring_df <- read_excel('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/flaring 2023/VIIRS_Global_flaring_d.7_slope_0.029353_2023_v20230614_web_IDmatch.xlsx', sheet = "flare upstream") %>%
  filter(Country == "United States") %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(us_gas_sf))

View(flaring_df)
sum(flaring_df$`BCM 2023`) # US flared volume 9.55

sum(flaring_df$`BCM 2023`)/sum(total_flare_df$`BCM 2023`) # US contribution 0.06611899

 # Step 4: Spatial join flaring points to fields
flaring_joined <- st_join(flaring_df, sf_combined %>% filter(!is.na(total_well_count)), join = st_within, left = TRUE)
View(flaring_joined)

# Step 5: Flaring matched to fields: sum by field ID
# Replace 'Field_ID' with actual field identifier column if needed
flaring_by_field <- flaring_joined %>%
  filter(!is.na(offshore)) %>%
  st_drop_geometry() %>%
  group_by(classified.type, FIPS, offshore) %>%
  summarise(total_bcm = sum(`BCM 2023`, na.rm = TRUE))
View(flaring_by_field)
# Step 6: Total flaring volume
total_bcm_all <- sum(flaring_df$`BCM 2023`, na.rm = TRUE)
total_bcm_all

# Step 7: Total matched flaring volume
total_bcm_matched <- sum(flaring_by_field$total_bcm, na.rm = TRUE)
total_bcm_matched

# Step 8: Unmatched flaring volume
total_bcm_unmatched <- total_bcm_all - total_bcm_matched
total_bcm_unmatched

# Results
print(flaring_by_field)
cat("Total flaring (BCM):", total_bcm_all, "\n")
cat("Matched to fields (BCM):", total_bcm_matched, "\n")
cat("Unmatched flaring (BCM):", total_bcm_unmatched, "\n")



# ────────────────────────────────────────────────────────────────
# cleaned 2023 input sheet ----
# ────────────────────────────────────────────────────────────────

# remove non-gas producing fields in 2023
us_field_2023 <- sf_combined %>% 
  filter(!is.na(total_gas_mcf) | total_gas_mcf != 0) %>%
  mutate(AAPG.Basin = ifelse(!is.na(AAPG.Basin.x), AAPG.Basin.x, AAPG.Basin.y),
         County_Name = ifelse(!is.na(County_Name.x), County_Name.x, County_Name.y)) %>%
  select(-AAPG.Basin.x, -AAPG.Basin.y, -County_Name.x, -County_Name.y,
         -c("Shape_Leng","Shape_Area","Field1","Prov_Cod_1","GeoProvinc","GeoProvi_1","GeoProvi_2","GeoProvi_3","GeoProvi_4")) %>%
  left_join(flaring_by_field, by = c("FIPS", "classified.type", "offshore")) %>% # join flaring 
  rename(
    Annual_Gas_2022 = Annual_Gas,
    Annual_Oil_2022 = Annual_Oil,
    Annual_Wat_2022 = Annual_Wat,
    Annual_Gas = total_gas_mcf,
    Annual_Oil = total_oil_bbl,
    Annual_Wat = total_water_bbl,
    BCM2023 = total_bcm
  ) %>%
  mutate(FOR_scf_bbl = ifelse(is.na(Annual_Oil) | Annual_Oil == 0, BCM2023 * 35.147 * 1e9 / (0.1*365), BCM2023 * 35.147 * 1e9/Annual_Oil),
         GOR_scf_bbl = ifelse(is.na(Annual_Oil) | Annual_Oil == 0, Annual_Gas *1e3 / (0.1*365), Annual_Gas *1e3 / Annual_Oil))

View(us_field_2023)

saveRDS(us_field_2023, "revision_data/field/us_field_2023.rds")
write_csv(us_field_2023 %>% st_drop_geometry(), "revision_data/field/us_field_2023.csv")

