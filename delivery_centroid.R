# ============================================================================
# State-Level Natural Gas Delivery Centroids (Weighted & Category-Specific)
# 
# Author: Zhihao (Spencer) Zhang
# PI: Adam Brandt
# Energy Science & Engineering, Stanford University
# Last Updated: July 7, 2025
# First Created: June 2025
#
# This script merges population-weighted, industrial, and power plant gas 
# demand centroids, joins them with sectoral natural gas consumption weights 
# (from EIA), and computes weighted state-level delivery points. It generates 
# and saves a Plotly map visualizing all centroids by category and exports the 
# combined and weighted centroids for downstream use.
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
# Global Variables ----
# ────────────────────────────────────────────────────────────────
census_api_key("4c037031845fa098391217f4a02a1f8aef1dedd5", install = TRUE, overwrite = TRUE)
save_path <- '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state delivery centroids'

# ────────────────────────────────────────────────────────────────
# Population Centroids by State ----
# ────────────────────────────────────────────────────────────────
# update with 2020 population data
county_pop_data <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  year = 2020,
  geometry = TRUE
)

# state references (state ID, state abbrev., state name)
state_ref <- states(cb = TRUE) %>%
  st_drop_geometry() %>%
  select(STATEFP, STUSPS, NAME) %>%
  rename(state_fips = STATEFP, state_abbr = STUSPS, state_name = NAME)

county_pop_data <- county_pop_data %>%
  mutate(state_fips = substr(GEOID, 1, 2),
         county_fips = substr(GEOID, 3, 5)) %>%
  separate(NAME, into = c("county_name", "state_name_lookup"), sep = ", ", remove = FALSE) %>%
  left_join(state_ref, by = "state_fips") %>%
  select(GEOID, state_fips, state_abbr, state_name, county_name, value, geometry)

county_centroids_geom <- st_centroid(county_pop_data$geometry)
coords <- st_coordinates(county_centroids_geom)

county_coords <- county_pop_data %>%
  mutate(centroid_x = coords[, 1], centroid_y = coords[, 2])

state_pw_centroids <- county_coords %>%
  st_drop_geometry() %>%
  group_by(state_name) %>%
  summarize(
    total_pop = sum(value, na.rm = TRUE),
    pw_x = sum(centroid_x * value, na.rm = TRUE) / total_pop,
    pw_y = sum(centroid_y * value, na.rm = TRUE) / total_pop
  ) %>%
  st_as_sf(coords = c("pw_x", "pw_y"), crs = st_crs(county_coords))

# ────────────────────────────────────────────────────────────────
# Industrial User Centroids ----
# ────────────────────────────────────────────────────────────────
path_industrial <- "revision_data/IndustrialNaturalGasDemand.csv"
industrial_user <- read.csv(path_industrial)

sf_industrial <- industrial_user %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(county_coords))

coords <- st_coordinates(st_centroid(sf_industrial$geometry))
sf_industrial_centroids <- sf_industrial %>%
  mutate(centroid_x = coords[, 1], centroid_y = coords[, 2])

state_industrial_centroids <- sf_industrial_centroids %>%
  st_drop_geometry() %>%
  group_by(State) %>%
  summarize(
    total_demand_MWh = sum(Annual.Fuel.Demand, na.rm = TRUE),
    pw_x = sum(centroid_x * Annual.Fuel.Demand, na.rm = TRUE) / total_demand_MWh,
    pw_y = sum(centroid_y * Annual.Fuel.Demand, na.rm = TRUE) / total_demand_MWh
  ) %>%
  st_as_sf(coords = c("pw_x", "pw_y"), crs = st_crs(county_coords))

# ────────────────────────────────────────────────────────────────
# Electric Power Sector Gas Use Centroids ----
# ────────────────────────────────────────────────────────────────
fuel_data <- read_excel("revision_data/Power_plant_fuel_data_2022_EIA.xlsx", 
                        col_names = TRUE, skip = 5)[, 1:31] %>%
  rename_with(~ str_replace_all(.x, "[\\s\\r\\n]+", "_")) %>%
  rename_with(~ str_replace_all(.x, "_+$", ""))

fuel_data_ng <- fuel_data %>%
  filter(Reported_Fuel_Type_Code == "NG") %>%
  mutate(Annual_NG_Consumption_MCF = rowSums(across(starts_with("Quantity_"), ~ as.numeric(gsub(",", "", .x))), na.rm = TRUE)) %>%
  select(-starts_with("Quantity_"))

plant_loc <- read.csv("revision_data/Power_Plants.csv")

merged_data <- fuel_data_ng %>%
  mutate(Plant_Id = as.numeric(Plant_Id)) %>%
  left_join(plant_loc %>% mutate(Plant_Code = as.numeric(Plant_Code)), by = c("Plant_Id" = "Plant_Code"))

sf_plant_ng_consumption <- merged_data %>%
  select(Plant_Id, State, Longitude, Latitude, Annual_NG_Consumption_MCF) %>%
  drop_na() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

coords <- st_coordinates(st_centroid(sf_plant_ng_consumption$geometry))
sf_plant_ng_consumption_centroids <- sf_plant_ng_consumption %>%
  mutate(centroid_x = coords[, 1], centroid_y = coords[, 2])

state_power_centroids <- sf_plant_ng_consumption_centroids %>%
  st_drop_geometry() %>%
  group_by(State) %>%
  summarize(
    total_NG_mcf = sum(Annual_NG_Consumption_MCF, na.rm = TRUE),
    pw_x = sum(centroid_x * Annual_NG_Consumption_MCF, na.rm = TRUE) / total_NG_mcf,
    pw_y = sum(centroid_y * Annual_NG_Consumption_MCF, na.rm = TRUE) / total_NG_mcf
  ) %>%
  st_as_sf(coords = c("pw_x", "pw_y"), crs = st_crs(county_coords))

# ────────────────────────────────────────────────────────────────
# Combine and Export Centroids ----
# ────────────────────────────────────────────────────────────────
us_states <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AS", "GU", "MP", "PR", "VI")) %>%
  st_transform(crs = 4326)

pw_centroids <- state_pw_centroids %>%
  mutate(category = "Population-weighted") %>%
  select(NAME = state_name, geometry, category) %>%
  left_join(us_states %>% st_drop_geometry() %>% select(STATEFP, STUSPS, NAME), by = "NAME")

ind_centroids <- state_industrial_centroids %>%
  mutate(category = "Industrial") %>%
  select(STUSPS = State, geometry, category) %>%
  left_join(us_states %>% st_drop_geometry() %>% select(STATEFP, STUSPS, NAME), by = "STUSPS")

power_centroids <- state_power_centroids %>%
  mutate(category = "Power Plants") %>%
  select(NAME = State, geometry, category) %>%
  left_join(us_states %>% st_drop_geometry() %>% select(STATEFP, STUSPS, NAME), by = "NAME")

# Export centroids
write.csv(pw_centroids, "revision_data/state delivery centroids/pw_centroids.csv")
write.csv(ind_centroids, "revision_data/state delivery centroids/ind_centroids.csv")
write.csv(power_centroids, "revision_data/state delivery centroids/power_centroids.csv")

saveRDS(pw_centroids, "revision_data/state delivery centroids/pw_centroids.rds")
saveRDS(ind_centroids, "revision_data/state delivery centroids/ind_centroids.rds")
saveRDS(power_centroids, "revision_data/state delivery centroids/power_centroids.rds")

# ────────────────────────────────────────────────────────────────
# Plot All Centroids on US Map ----
# ────────────────────────────────────────────────────────────────
centroids_all <- bind_rows(pw_centroids, ind_centroids, power_centroids)
centroid_colors <- c("Population-weighted" = "yellow", "Industrial" = "green", "Power Plants" = "blue")

g = ggplot() +
  geom_sf(data = us_states, fill = "gray95", color = "gray70", linewidth = 0.3) +
  geom_sf(data = centroids_all, aes(fill = category), color = "black", size = 3, shape = 21) +
  scale_fill_manual(values = centroid_colors) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(title = "State-Level Reference Centroids (CONUS)")

# Define output path and file name
output_path <- "revision_data/state delivery centroids/state_reference_centroids_map.png"

# Save the plot
ggsave(
  filename = output_path,
  plot = g,
  width = 12, height = 8, dpi = 300
)

# ────────────────────────────────────────────────────────────────
# Load and Standardize Centroids ----
# ────────────────────────────────────────────────────────────────

# Combine all centroid types into one dataframe
centroids_all <- power_centroids %>%
  rbind(ind_centroids) %>%
  rbind(pw_centroids) %>%
  drop_na()

# Export combined centroids to CSV
write.csv(
  centroids_all,
  'revision_data/state delivery centroids/centroids_all.csv',
  row.names = FALSE
)

# ────────────────────────────────────────────────────────────────
# Compute Weighted Coordinates ----
# ────────────────────────────────────────────────────────────────

# Read in sector-level natural gas consumption weights
weights <- read_csv(
  'revision_data/EIA state-level gas consumption by sector/state_consumption_weights.csv'
)

# Join weights with centroids and compute weighted location
centroids_weighted <- centroids_all %>%
  mutate(category = trimws(category)) %>%
  left_join(weights %>% select(-`...1`), by = c("NAME" = "state")) %>%
  mutate(
    weight = case_when(
      category == "Population-weighted" ~ others,
      category == "Industrial" ~ ind,
      category == "Power Plants" ~ pow,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(weight)) %>%
  mutate(coords = st_coordinates(geometry)) %>%
  mutate(
    centroid_x = coords[, "X"],
    centroid_y = coords[, "Y"]
  )

# ────────────────────────────────────────────────────────────────
# 4. Aggregate Weighted Centroids by State
# ────────────────────────────────────────────────────────────────

state_weighted_centroids <- centroids_weighted %>%
  st_drop_geometry() %>%
  group_by(STATEFP, STUSPS, NAME) %>%
  summarize(
    total_weight = sum(weight, na.rm = TRUE),
    pw_x = sum(centroid_x * weight, na.rm = TRUE) / total_weight,
    pw_y = sum(centroid_y * weight, na.rm = TRUE) / total_weight,
    .groups = "drop"
  ) %>%
  st_as_sf(coords = c("pw_x", "pw_y"), crs = 4326)

# Save the weighted centroids
write.csv(
  state_weighted_centroids,
  'revision_data/state delivery centroids/state_level_weighted_delivery_centroids.csv',
  row.names = FALSE
)
saveRDS(
  state_weighted_centroids,
  'revision_data/state delivery centroids/state_level_weighted_delivery_centroids.rds'
)

# ────────────────────────────────────────────────────────────────
# Visualize Centroids with Plotly ----
# ────────────────────────────────────────────────────────────────

# Extract coordinates for individual and weighted centroids
centroids_all_plot <- centroids_all %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )

centroids_weighted <- state_weighted_centroids %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2],
    category = "Weighted"
  )

# Combine all centroids for legend display
centroids_all_combined <- bind_rows(
  centroids_all_plot,
  centroids_weighted
)

# Define color palette for categories
colors <- c(
  "Population-weighted" = "orange",
  "Industrial" = "green",
  "Power Plants" = "red",
  "Weighted" = "blue"
)

# Create an interactive Plotly map
fig <- plot_ly()

for (cat in unique(centroids_all_combined$category)) {
  df_cat <- filter(centroids_all_combined, category == cat)
  fig <- fig %>% add_trace(
    data = df_cat,
    type = "scattergeo",
    mode = "markers",
    lat = ~lat,
    lon = ~lon,
    text = ~paste(NAME, "-", category),
    name = cat,
    marker = list(
      size = ifelse(cat == "Weighted", 10, 7),
      color = colors[cat],
      line = list(width = 1, color = 'white')
    )
  )
}

fig <- fig %>% layout(
  title = "State-Level Centroids: Weighted and Category-Specific",
  geo = list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = 'rgb(240, 240, 240)',
    subunitwidth = 1,
    countrywidth = 1,
    subunitcolor = 'rgb(255,255,255)',
    countrycolor = 'rgb(255,255,255)'
  )
)

# Save Plotly map to HTML
htmlwidgets::saveWidget(
  fig,
  file = "revision_data/state delivery centroids/state_centroids_map.html",
  selfcontained = TRUE
)