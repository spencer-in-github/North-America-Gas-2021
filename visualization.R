# Plot for manuscript

# Author          Spencer Zhang
# Date            Apr 2 2024
# Last Updated    May 5 2024

# Updated         Apr 2 2024
#                 initialization
# Updated         May 5 2024
#                 clean the datafile, stablize code so that it reads all future OPGEE runs



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



## OPGEE files ----

### read OPGEE run result sheets ----
gas_results = read_excel(paste0(PATH_OPGEE_RESULT_SHEET,"gas_runs_pure_aerial_results.xlsx"))
oil_results = read_excel(paste0(PATH_OPGEE_RESULT_SHEET,"oil_runs_pure_aerial_results.xlsx"))
offshore_results = read_excel(paste0(PATH_OPGEE_RESULT_SHEET, "offshore_runs_pure_results.xlsx"))

# merge with input, clean dataframe for visualization (input, process breakdown, coords, etc)
df_viz_gas <- CI_by_process(clean_and_join_input(gas_results,US_GAS_WITH_COMP_scaled))
df_viz_oil <- CI_by_process(clean_and_join_input(oil_results,US_OIL))
df_viz_offshore <- CI_by_process_offshore(offshore_results, offshore_input, gom_sf_final) 

View(df_viz_gas)
View(df_viz_offshore)
View(gom_sf_final)

# df_viz_vff = rbind(df_viz_gas, df_viz_oil)
# write_csv(df_viz_vff %>% st_drop_geometry(), "df_viz_vff.csv")

# View the updated dataframe
print(df_viz_gas)

### read OPGEE uncertainty run result sheets ----
gas_up <- read_excel(paste0(PATH_OPGEE_UNCERTAINTY,"gas upper (2).xlsx"))
gas_lo <- read_excel(paste0(PATH_OPGEE_UNCERTAINTY,"gas lower (2).xlsx"))
oil_up <- read_excel(paste0(PATH_OPGEE_UNCERTAINTY,"oil upper (2).xlsx"))
oil_lo <- read_excel(paste0(PATH_OPGEE_UNCERTAINTY,"oil lower (2).xlsx"))
offshore_up <- read_excel(paste0(PATH_OPGEE_UNCERTAINTY,"offshore_up.xlsx"))
offshore_lo <- read_excel(paste0(PATH_OPGEE_UNCERTAINTY,"offshore_lo.xlsx"))

# merge with input, clean dataframe for visualization (input, process breakdown, coords, etc)
df_viz_gas_up <- CI_by_process(clean_and_join_input(gas_up,US_GAS_WITH_COMP_scaled))
df_viz_gas_lo <- CI_by_process(clean_and_join_input(gas_lo,US_GAS_WITH_COMP_scaled))
df_viz_oil_up <- CI_by_process(clean_and_join_input(oil_up,US_OIL))
df_viz_oil_lo <- CI_by_process(clean_and_join_input(oil_lo,US_OIL))

df_viz_offshore_up <- CI_by_process_offshore(offshore_up, offshore_input, gom_sf_final)
df_viz_offshore_lo <- CI_by_process_offshore(offshore_lo, offshore_input, gom_sf_final)

View(df_viz_offshore_lo)

# merge oil run, gas run, and offshore run for uncertainty ranges
df_viz_up = join_oil_and_gas_runs(df_viz_gas_up, df_viz_oil_up, df_viz_offshore_up)
df_viz_lo = join_oil_and_gas_runs(df_viz_gas_lo, df_viz_oil_lo, df_viz_offshore_lo)

# merge the main OPGEE result with uncertainty ranges
### FINAL field-level dataframe for visualization ----
df_viz = join_oil_and_gas_runs(df_viz_gas, df_viz_oil, df_viz_offshore) %>%
  select(-STATE_NAME) %>%
  left_join(states %>% st_drop_geometry() %>%
              select(STATEFP, STATE_NAME = NAME)) %>%
  mutate(STATE_NAME = case_when(is.na(STATE_NAME) ~ "Offshore Gulf of Mexico",
                                TRUE ~ STATE_NAME)) %>%
  left_join(A_field_midstream, by = c("STATE_NAME" = "prod_state")) %>%
  left_join(df_viz_lo %>% 
              st_drop_geometry() %>% 
              select(FIPS, Production_Type, CI_lo = CI_gCO2_MJ),
            by= c("FIPS" = "FIPS", "Production_Type" = "Production_Type")) %>%
  left_join(df_viz_up %>% 
              st_drop_geometry() %>% 
              select(FIPS, Production_Type, CI_up = CI_gCO2_MJ),
            by= c("FIPS" = "FIPS", "Production_Type" = "Production_Type")) %>%
  filter(Basin != "ARCTIC OCEAN, FED.") %>% # removed Arctic Ocean
  mutate(midstream_lo = field_midstream_factor_gCO2_MJ * 3.772/4,
         midstream_up = field_midstream_factor_gCO2_MJ * 4.228/4) %>%
  filter(Basin != "(N/A)") %>%
  mutate(Basin = case_when(
    Basin == "Offshore GOM" ~ "Offshore Gulf of Mexico",
    TRUE ~ str_to_title(Basin)
  ))

View(df_viz)


# save final visualization dataframe to local files
save_local(df_viz, "df_viz")
df_viz = readRDS(paste0(PATH_SAVE_VIZ, "df_viz.rds"))

columns_to_summarize <- c("exploration_gCO2_MJ", 
                          "drilling_dev_gCO2_MJ", 
                          "crude_prod_extra_gCO2_MJ",
                          "surface_gCO2_MJ", 
                          "lng_gCO2_MJ", 
                          "maintenance_gCO2_MJ", 
                          "waste_gCO2_MJ", 
                          "crude_transport_gCO2_MJ", 
                          "gas_dis_gCO2_MJ", 
                          "other_gCO2_MJ", 
                          "offsite_gCO2_MJ", 
                          "CO2_seq_gCO2_MJ", 
                          "CI_gCO2_MJ", 
                          "field_midstream_factor_gCO2_MJ", 
                          "CI_lo", "CI_up",
                          "midstream_lo", "midstream_up")

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
  )

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
  )

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
  )

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
  )

save_local(df_viz_bistate, "df_viz_bistate")
save_local(df_viz_state, "df_viz_state")

df_viz_bistate = readRDS(paste0(PATH_SAVE_VIZ, "df_viz_bistate.rds"))
df_viz_state = readRDS(paste0(PATH_SAVE_VIZ, "df_viz_state.rds"))

st_write(df_viz, paste0(PATH_SAVE_VIZ, "df_viz_shp/df_viz_field.shp"), append=FALSE) #overwrite if already exists

# 3 Results numbers ---------------------------

# final input dataframe
View(US_GAS_WITH_COMP_scaled)
View(US_OIL)
View(offshore_input)

## 2.1 map characteristics ----

# Number of wells
sum(as.numeric(df_viz$Well_count), na.rm =T) # 920661
# Number of basins
length(unique(df_viz$Basin)) # 52+1 (offshore GOM)
# Number of states
length(unique(df_viz$STATEFP)) # 27+1 (offshore GOM)

# gas production
## gas production total 
sum(df_viz$Annual_Gas) # 37404993659 MCF
sum(df_viz$Annual_Gas)/1000000/365 # 102.4797 BCF/day

## gas from gas fields
sum(df_viz[df_viz$Production_Type == "GAS",]$Annual_Gas)/sum(df_viz$Annual_Gas) # 72.01%
## gas from oil fields
sum(df_viz[df_viz$Production_Type == "OIL",]$Annual_Gas)/sum(df_viz$Annual_Gas) # 27.99%


# how much flaring is attributed to gas fields
sum(df_viz[df_viz$Production_Type == "GAS",]$BCM2022)/sum(df_viz$BCM2022)
View(df_viz)

## 2.1 Michigan basin production ----
MCF_to_BCM = 1/1000000 * 0.0283
MCF_to_BCF = 1/1000000

t = df_viz %>%
  filter(Basin == "Michigan Basin") %>%
  mutate(Annual_Oil = as.numeric(Annual_Oil))
sum(t$Annual_Oil)/365.25 # 183016.6
sum(t$Annual_Gas)*MCF_to_BCF # 8.402464e-05
sum(t$Annual_Gas)/sum(df_viz$Annual_Gas)


## 2.2 CI ----

# how much gas is covered by the 15 basins

sum(df_viz[df_viz$Basin %in% top_15_basin,]$Annual_Gas)/sum(df_viz$Annual_Gas)

# Basin level upstream CI range
View(df_viz_basin) 
# smallest 5.5 Arkla gCO2/MJ
# largest 241.97 Michigan
sml = 5.5
sml * MMCF_to_MJ


## CI numbers ----

weighted.mean(as.numeric(df_viz$CI_gCO2_MJ), df_viz$Annual_Gas, na.rm = T) + weighted.mean(df_viz$field_midstream_factor_gCO2_MJ, df_viz$Annual_Gas, na.rm = T)  
# 13.748

weighted.mean(as.numeric(df_viz$CI_lo), df_viz$Annual_Gas, na.rm = T) + weighted.mean(df_viz$midstream_lo, df_viz$Annual_Gas, na.rm = T)  

weighted.mean(as.numeric(df_viz$CI_up), df_viz$Annual_Gas, na.rm = T) + weighted.mean(df_viz$midstream_up, df_viz$Annual_Gas, na.rm = T)  

weighted.mean(as.numeric(df_viz$CI_gCO2_MJ), df_viz$Annual_Gas, na.rm = T) # 10.64
weighted.mean(as.numeric(df_viz$CI_lo), df_viz$Annual_Gas, na.rm = T) # 10.07
weighted.mean(as.numeric(df_viz$CI_up), df_viz$Annual_Gas, na.rm = T) # 11.21

weighted.mean(df_viz$field_midstream_factor_gCO2_MJ, df_viz$Annual_Gas, na.rm = T) # 3.105
weighted.mean(df_viz$midstream_lo, df_viz$Annual_Gas, na.rm = T) # 2.93
weighted.mean(df_viz$midstream_up, df_viz$Annual_Gas, na.rm = T) # 3.283.

sum(df_viz$Annual_Gas) # MCF
sum(df_viz$Annual_Gas)/1000000/365 # 102 BCF/day

sum(df_viz$Annual_Gas)/1000000/365/119 # 0.8752442
# EIA gross natural gas withdrawal in 2022: 119BCF/day

## 2.3 midstream ----
weighted.mean(df_viz$midstream_lo, df_viz$Annual_Gas, na.rm =T)
weighted.mean(df_viz$midstream_up, df_viz$Annual_Gas, na.rm =T)
weighted.mean(df_viz$field_midstream_factor_gCO2_MJ, df_viz$Annual_Gas, na.rm =T)


## 3.2 data ----
# Permian, San Joaquin, Denver-Julesburg, Appalachian, Uinta, and Fort Worth
u(df_viz$Basin)
aerial_basins = c("San Joaquin Basin","Denver Julesburg", "Permian Basin", "Appalachian", "Ft Worth Basin", "Uinta")
sum(df_viz[df_viz$Basin %in% aerial_basins,]$Annual_Gas,na.rm=T)/sum(df_viz$Annual_Gas, na.rm =T)


sum(df_viz[df_viz$Basin %in% top_15_basin,]$Annual_Gas,na.rm=T)/sum(df_viz$Annual_Gas, na.rm =T)

# total flare in US in 2022
sum(flare_2022$BCM2022) #0.8586

# gas flare in gas fields
sum(df_viz[df_viz$Production_Type == "GAS",]$BCM2022)
sum(df_viz$BCM2022)

sum(df_viz[df_viz$Production_Type == "GAS",]$BCM2022)/sum(df_viz$BCM2022)

total_MJ = sum(df_viz$Annual_Gas)/1000 * MMCF_to_MJ # MJ
(upstream_mean * total_MJ - 0.85 * 1e9 * 0.657 * 1000 * 28) / (total_MJ + 0.85 * 1e9 * 55.5)# 10.155

1-(upstream_mean * total_MJ - 0.85 * 1e9 * 0.657 * 1000 * 28) / (total_MJ + 0.85 * 1e9 * 55.5)/upstream_mean

saved_CO2_kg_from_gas_flare = 0.85 * 1e9 * 0.657 * 28 # 0.85BCM * 1e9 m3/bcm * 0.657 kg/m3 * 28CO2e/CH4
saved_CO2_kg_from_gas_flare/1000/1000000000 # 0.0156 Gt / 15.6 Million metric tons

# total Gt CO2
#gCO2e/MJ * MCF * MJ/MMCF *1000 /g->kg / kg->ton
weighted.mean(as.numeric(df_viz$CI_gCO2_MJ), df_viz$Annual_Gas, na.rm = T) * sum(df_viz$Annual_Gas) * MMCF_to_MJ * 1000/1e15


sum(df_viz[df_viz$Basin == "Michigan Basin",]$Annual_Gas,na.rm=T)/sum(df_viz$Annual_Gas, na.rm =T)


# Figure 1 Map ---------------------------
# updated May 6 2024

## Data ------------
# enverus well-level dataset
# onshore&offshore
View(df_classified_sf) # 927513 wells
# offshore
View(enverus_offshore_sf) # ~3061 wells

## Well-voronoi-field schematic ------------

# filter wells in Wind River basin
wind_river_wells <- df_classified_sf %>%
  filter(`AAPG Geologic Province` == "WIND RIVER") # 2863 wells
# filter counties for Wind River basin
wind_river_counties <- US_COUNTIES[wind_river_wells,] # 2 counties

### 1.a Well schematic ----

p1a <-
  ggplot() + 
  geom_sf(data = wind_river_counties, color = "black", fill = "white", lwd = 0.5) + 
  geom_sf(data = wind_river_wells, aes(color = classified.type)) +
  theme_NAG_publication() +
  scale_color_discrete(guide = "none") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  #scale_color_oil_gas() + 
  scale_color_manual(values = c("GAS" = "#B1040E", "OIL" = "#008566")) + 
  labs(color = "Production Type") +
  guides(color ="none")
 
ggsave(filename = paste0(PATH_SAVE_VIZ, "f1a_well.svg"), 
       plot= p1a,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")

### 1.b Voronoi schematic ----

wind_river_voronoi <-
  wind_river_wells %>%
  st_transform(2236) %>%
  st_union() %>% 
  st_voronoi() %>% 
  st_cast() %>% 
  st_as_sf() %>% 
  st_join(wind_river_wells %>% st_transform(2236)) %>%
  st_intersection(.,wind_river_counties %>% st_transform(2236)) %>%
  st_transform(st_crs(wind_river_wells))

fixed_geometries <- wind_river_voronoi$geometry %>% st_make_valid()

wind_river_voronoi$geometry <- fixed_geometries

p1b <-
  ggplot() + 
  #geom_sf(data = wind_river_counties, color = "black", fill = "white", lwd = 0.5) + 
  geom_sf(data = wind_river_voronoi, 
          color = "black", 
          lwd = 0.5, 
          aes(fill = classified.type)) +
    geom_sf(data = wind_river_wells,
            lwd = 0.01,
            aes(color = classified.type))+
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  #scale_color_oil_gas() + 
    scale_fill_manual(values = c("GAS" = "#E50808", "OIL" = "#1AECBA")) +
    scale_color_manual(values = c("GAS" = "#B1040E", "OIL" = "#008566")) +
    #scale_fill_oil_gas() + 
  labs(color = "Production Type") +
  guides(color ="none", fill = "none")

ggsave(filename = paste0(PATH_SAVE_VIZ, "f1a_voronoi.svg"), 
       plot= p1b,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")


### 1.c Field schematic ----

wind_river_fields <- wind_river_voronoi %>%
  st_as_sf() %>%
  group_by(COUNTYFP,classified.type) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()

mapview(wind_river_fields, zcol = "classified.type")

p1c <-
  ggplot() + 
  #geom_sf(data = wind_river_counties, color = "black", fill = "white", lwd = 0.5) + 
  geom_sf(data = wind_river_fields, 
          color = "black", 
          lwd = 0.5, 
          aes(fill = classified.type)) +
  # geom_sf(data = wind_river_wells,
  #         lwd = 0.01,
  #         aes(color = classified.type))+
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  #scale_color_oil_gas() + 
  scale_color_manual(values = c("GAS" = "#B1040E", "OIL" = "#008566")) +
  scale_fill_manual(values = c("GAS" = "#E50808", "OIL" = "#1AECBA")) +
  #scale_fill_oil_gas() + 
  labs(color = "Production Type") +
  guides(color ="none", fill = "none")

ggsave(filename = paste0(PATH_SAVE_VIZ, "f1a_field.svg"), 
       plot= p1c,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")


## Map ---------------------------
mapview(US_FIELD_final,zcol = "classified.type")

# Load necessary libraries
us_map <- map_data("state")
mapview(states)

### us main map ----
main_map <- 
  ggplot() +
  geom_sf(data = states, fill = "white", color = "black", lwd = 0.2) +  # Plot US boundaries
  geom_sf(data = df_viz_bibasin %>% rename("Production Type" = Production_Type), aes(fill = `Production Type`), color = NA) +  # Use 'fill' for polygon color
  scale_fill_oil_gas() + 
  scale_fill_manual(values = c("GAS" = "#E50808", "OIL" = "#1AECBA")) +
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

ggsave(filename = paste0(PATH_SAVE_VIZ, "f1d_main.svg"), 
       plot= main_map,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")


### AL map ----

alaska_sf <- states %>% filter(NAME == "Alaska")
mapview(alaska_sf)

alaska_plot <- 
  ggplot() +
  geom_sf(data = alaska_sf, fill = "white", color = "black", lwd = 0.7) + 
  geom_sf(data = US_FIELD_final %>% filter(STATEFP == "02", AAPG.Basin == "COOK INLET BASIN") %>% rename("Production Type" = "classified.type"), aes(fill = `Production Type`), color = NA) +  
  scale_fill_oil_gas() + 
  theme_NAG_publication() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none") + 
  coord_sf(expand = FALSE,xlim = c(-180, -128), ylim = c(49, 73))  

ggsave(filename = paste0(PATH_SAVE_VIZ, "f1d_alaska.png"), 
       plot= alaska_plot,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")


# Figure 4 CI on map ---------

# Modify df_viz to include centroids
df_viz_basin$geometry <- st_make_valid(df_viz_basin$geometry)
df_viz_centroid <- df_viz_basin %>%
  #st_as_sf(coords = c("lon", "lat"), crs = st_crs(states)) %>%  # convert df_viz to an sf object
  mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ),
         CI_cate = case_when(
           CI_gCO2_MJ < 10 ~ "0-10",
           CI_gCO2_MJ < 20 ~ "10-20",
           CI_gCO2_MJ < 50 ~ "20-50",
           CI_gCO2_MJ < 100 ~ "50-100",
           CI_gCO2_MJ < 500 ~ "100-500",
           TRUE ~ ">500"
         ),
         CI_cate = fct_reorder(CI_cate, CI_gCO2_MJ)) %>%
  st_centroid()  # calculate centroids

View(df_viz_centroid)
# Assuming your sf object is named `sf_object`
st_write(df_viz_centroid, paste0(PATH_SAVE_VIZ, "df_viz_centroid.geojson"), driver = "GeoJSON")

# Plotting
ci_on_map<-
  ggplot() +
  geom_sf(data = states, fill = "white", color = "black", lwd = 0.2) +  # Plot US boundaries
  #geom_sf(data = df_viz_basin, fill = "royalblue") + # plot basin as base map
  geom_sf(data = df_viz_centroid, aes(fill = CI_cate, size = Annual_Gas), shape =21, stroke = 0.3) +
  scale_size(range = c(1, 20)) +  # Adjust size range based on your data
  scale_fill_manual(values = c("0-10" = "green", 
                                "10-20" = "yellow",
                                "20-50" = "gold",
                                "50-100" = "orange",
                                "100-500" = "coral",
                                ">500" = "red"),
                     name = "Carbon Intensity Range \n gCO2e MJ^-1") +
  #scale_fill_oil_gas() + 
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

ggsave(filename = paste0(PATH_SAVE_VIZ, "ci_on_map.png"), 
       plot= ci_on_map,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")

alaska_ci <- 
  ggplot() +
  geom_sf(data = alaska_sf, fill = "white", color = "black", lwd = 0.7) + 
  geom_sf(data = df_viz_centroid, aes(fill = CI_cate, size = Annual_Gas), shape =21, stroke = 0.3) +
  scale_size(range = c(1, 20)) +  # Adjust size range based on your data
  scale_fill_manual(values = c("0-10" = "green", 
                               "10-20" = "yellow",
                               "20-50" = "gold",
                               "50-100" = "orange",
                               "100-500" = "coral",
                               ">500" = "red"),
                    name = "Carbon Intensity Range \n gCO2e MJ^-1") +
  #scale_fill_oil_gas() + 
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + 
  coord_sf(expand = FALSE,xlim = c(-180, -128), ylim = c(49, 73))  

ggsave(filename = paste0(PATH_SAVE_VIZ, "alaska_ci.png"), 
       plot= alaska_ci,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")


 # Figure 2 data prep  ---------------------------

# Calculate the weighted values and error bars
df_v <- df_viz %>%
  filter(Annual_Gas > 0 & !is.na(CI_gCO2_MJ) & Basin != "(N/A)") %>%
  mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ),
         Annual_Gas = as.numeric(Annual_Gas),
         field_mid_em_factor_gCO2_MJ = as.numeric(field_mid_em_factor_gCO2_MJ),
         CI_lo = as.numeric(CI_lo),
         CI_up = as.numeric(CI_up)) %>%
  st_drop_geometry() %>%
  group_by(Basin) %>%
  summarise(
    weighted_upstream_CI = sum(CI_gCO2_MJ * Annual_Gas, na.rm = T) / sum(Annual_Gas, na.rm = T),
    weighted_upstream_CI_lo = sum(CI_lo * Annual_Gas, na.rm = T) / sum(Annual_Gas, na.rm = T),
    weighted_upstream_CI_up = sum(CI_up * Annual_Gas, na.rm = T) / sum(Annual_Gas, na.rm = T),
    weighted_midstream_CI = sum(field_mid_em_factor_gCO2_MJ * Annual_Gas, na.rm = T) / sum(Annual_Gas, na.rm = T),
    weighted_midstream_CI_lo = sum(midstream_lo * Annual_Gas, na.rm = T) / sum(Annual_Gas, na.rm = T),
    weighted_midstream_CI_up = sum(midstream_up * Annual_Gas, na.rm = T) / sum(Annual_Gas, na.rm = T),
    Gas_Production_MCF = sum(Annual_Gas, na.rm = T)
  ) %>%
  mutate(total_CI = weighted_upstream_CI + weighted_midstream_CI,
         lo = weighted_upstream_CI_lo + weighted_midstream_CI_lo,
         up = weighted_upstream_CI_up + weighted_midstream_CI_up)

View(df_v)

save_local(df_v, "df_v")


# Load data
df_v <- read_csv(paste0(PATH_SAVE_VIZ, "df_v.csv"))

# Filter top 15 basins by production volume
top_basins <- df_v %>%
  arrange(desc(Gas_Production_MCF)) %>%
  slice(1:15)

top_basins %>%
  rename(Upstream = weighted_upstream_CI, Midstream = weighted_midstream_CI, `Gas production` = Gas_Production_MCF) %>%
  arrange(`Gas production`) %>%
  mutate(lo_diff = total_CI - lo,
         up_diff =up - total_CI) %>%
  write_csv(., paste0(PATH_SAVE_VIZ, "df_barplot.csv"))


## CI bar ----
p2a <-
  top_basins %>%
  rename(Upstream = weighted_upstream_CI, Midstream = weighted_midstream_CI, `Gas production` = Gas_Production_MCF) %>%
  pivot_longer(
    cols = c("Upstream", "Midstream"),
    names_to = "CI",
    values_to = "CI_value"
  ) %>% 
  mutate(Basin = factor(Basin, levels = rev(top_15_basin))) %>%
  ggplot(aes(x = Basin, y = CI_value, fill = CI)) +
           geom_bar(stat = "identity", position = "stack") + 
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.2) +
  coord_flip() +
  theme_NAG_publication() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,30)) +
  geom_hline(yintercept = total_mean, linetype = "dotted", color = "black", size = 0.5) +
  labs(x= NA,
       y = "Volume-weighted average CI (gCO<sub>2</sub>e MJ<sup>-1</sup>) ") +
  scale_fill_manual(values = c("Upstream" = "#2B61FF", "Midstream" = "orange")) +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "upperright",
        legend.direction = "vertical")

ggsave(filename = paste0(PATH_SAVE_VIZ, "f2a_CI_15_basins.png"), 
       plot= p2a,
       width = 17, 
       height = 15,
       scale =1,
       dpi = 300,
       units = "cm",
       bg = "transparent")

## Production volume bar ----
p2b <-
  top_basins %>%
  rename(Upstream = weighted_upstream_CI, Midstream = weighted_midstream_CI, `Gas production` = Gas_Production_MCF) %>%
  # pivot_longer(
  #   cols = c("Upstream", "Midstream"),
  #   names_to = "CI",
  #   values_to = "CI_value"
  # ) %>% 
  mutate(Basin = factor(Basin, levels = rev(top_15_basin))) %>%
  ggplot(aes(x = Basin, y = `Gas production`)) +
  geom_bar(stat = "identity", fill = "orange") + 
  #geom_errorbar(aes(ymin = lo, ymax = up), width = 0.2) +
  coord_flip() +
  theme_NAG_publication() + 
  scale_y_continuous(expand = c(0, 0)) +
  #geom_hline(yintercept = total_mean, linetype = "dotted", color = "black", size = 0.5) +
  labs(x= NA,
       y = "Gas production volume (MMCF year<sup>-1</sup>)") +
  #scale_fill_manual(values = c("Upstream" = "#2B61FF", "Midstream" = "orange")) +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        #axis.text.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))


p2 <- grid.arrange(p2a, p2b, ncol=2)

ggsave(filename = paste0(PATH_SAVE_VIZ, "f2.png"), 
       plot= p2,
       width = 30, 
       height = 15,
       scale =1,
       dpi = 300,
       units = "cm",
       bg = "transparent")


# Figure 3 Cumulative CI supply curve ---------------------------

## F3 data prep ----
# let's think what kind of dataframe we need
View(df_viz)

t <- df_viz %>%
  mutate(GOR = Annual_Gas/Annual_Oil)
View(t)

df_viz %>%
  st_drop_geometry() %>%
  filter(Annual_Gas > 0 & !is.na(CI_gCO2_MJ) & as.numeric(CI_gCO2_MJ) < 500) %>%
  mutate(GOR = Annual_Gas/Annual_Oil,
         Annual_Gas = as.numeric(Annual_Gas),
         CI_gCO2_MJ = as.numeric(CI_gCO2_MJ)) %>%
  arrange(CI_gCO2_MJ) %>%
  mutate(Cumulative_Gas_production = cumsum(Annual_Gas),
         cum_perc = Cumulative_Gas_production/sum(Annual_Gas),
         width = Annual_Gas/sum(Annual_Gas)) %>%
  select(FIPS, CI_gCO2_MJ, GOR, Annual_Gas,Cumulative_Gas_production,cum_perc, width) %>%
  write_csv(., paste0(PATH_SAVE_VIZ,"df_ci_cumulative.csv"))


## previous F3 code ----
data <- df_viz %>%
  arrange(CI_gCO2_MJ) %>%
  mutate(Cumulative_Gas_production = as.numeric(cumsum(Annual_Gas)),
         CI_gCO2_MJ= as.numeric(CI_gCO2_MJ))
data$left <- c(0, data$Cumulative_Gas_production[-length(data$Cumulative_Gas_production)])

barmekko(data %>% arrange(weighted_CI) %>% filter(Basin != "(N/A)"), 
         x = Basin, 
         y = weighted_CI, 
         width = Gas_Production,
         values = F) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),  # Hide the x-axis text
        axis.ticks.x = element_blank()) + # Hide the x-axis ticks
  #scale_y_continuous(expand = c(0, 30)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  ylim(0,30) +
  xlab("Cumulative Gas Production Volume (MMSCF)") +
  ylab("Volume-weighted carbon intensity (g CO2e MJ^-1)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept = upstream_mean, linetype = "dotted", color = "black", size = 0.5) 

ggplot(data, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = Cumulative_Gas_production, ymax = CI_gCO2_MJ))
  

## f5 cumulative by basin ----
barmekko(df_v %>% arrange(weighted_CI) %>% filter(Basin != "(N/A)"), 
         x = Basin, 
         y = weighted_CI, 
         width = Gas_Production,
         values = F) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),  # Hide the x-axis text
        axis.ticks.x = element_blank()) + # Hide the x-axis ticks
  #scale_y_continuous(expand = c(0, 30)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  ylim(0,30) +
  xlab("Cumulative Gas Production Volume (MMSCF)") +
  ylab("Volume-weighted carbon intensity (g CO2e MJ^-1)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept = upstream_mean, linetype = "dotted", color = "black", size = 0.5) 
  # +  geom_hline(yintercept = total_mean, linetype = "dotted", color = "black", size = 0.5) +
  # annotate("text", x = 1e10, y = 20, label = "U.S. volume weighted average CI \n 15.38 gCO2e MJ^-1") +
  #   annotate("text", x = 1e10, y = 20, label = "U.S. volume weighted average CI \n 15.38 gCO2e MJ^-1") +
  # annotate("segment", x = 1e10, y = 18, xend = 1e10, yend =16,
  #          arrow = arrow(type = "closed", length = unit(0.02, "npc")))
  


## f5 cumulative by process ---
df_viz %>%
  pivot_longer(
    cols = c("exploration_gCO2_MJ",
             "drilling_dev_gCO2_MJ",
             "crude_prod_extra_gCO2_MJ",
             "surface_gCO2_MJ",
             "lng_gCO2_MJ",
             "maintenance_gCO2_MJ",
             "waste_gCO2_MJ",
             "crude_transport_gCO2_MJ",
             "gas_dis_gCO2_MJ",
             "other_gCO2_MJ",
             "offsite_gCO2_MJ",
             "CO2_seq_gCO2_MJ",
             "field_mid_em_factor_gCO2_MJ"),
    names_to = "Process",
    values_to = "CI"
  ) %>%
  arrange(CI_gCO2_MJ) %>% filter(Basin != "(N/A)") %>%
  barmekko(., 
           x = Basin, 
           y = CI_gCO2_MJ, 
           width = Annual_Gas,
           values = F) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),  # Hide the x-axis text
        axis.ticks.x = element_blank()) + # Hide the x-axis ticks
  #scale_y_continuous(expand = c(0, 30)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  ylim(0,30) +
  xlab("Cumulative Gas Production Volume (MMSCF)") +
  ylab("Volume-weighted carbon intensity (g CO2e MJ^-1)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept = total_mean, linetype = "dotted", color = "black", size = 0.5) +
  annotate("text", x = 1e11, y = 20, label = "U.S. volume weighted average CI \n 15.7 gCO2e MJ^-1") +
  annotate("segment", x = 1e11, y = 18, xend = 1e11, yend =16,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  scale_fill_manual(values = wes_palette("Moonrise3", 100, type = c("continuous")))





# Figure 5 CI on map ---------------------------

# May 21
# create basin maps 

mapview(df_viz_basin[df_viz_basin$Annual_Gas > 0,], zcol = "CI_gCO2_MJ")


# previous

## f5 map production ----
df_viz$CI_gCO2_MJ = as.numeric(df_viz$CI_gCO2_MJ)

st_write(df_viz %>% st_crs() , paste0(PATH_SAVE_VIZ, "df_viz_field.shp"))

ggplot() +
  geom_sf(data = states, fill = "lightblue", color = "white") +  # Plot US boundaries
  geom_sf(data = df_viz_basin %>%
            mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ),
                   CI_cate = case_when(CI_gCO2_MJ<10 ~ "0-10",
                                       CI_gCO2_MJ < 20 ~ "10-20",
                                       CI_gCO2_MJ < 50 ~ "20-50",
                                       CI_gCO2_MJ< 100 ~ "50-100",
                                       CI_gCO2_MJ <500~ "100-500",
                                       T ~ ">500")) %>%
            mutate(CI_cate = fct_reorder(CI_cate, CI_gCO2_MJ)), 
                   aes(fill = `CI_cate`), color = NA) +  # Use 'fill' for polygon color
  scale_fill_manual(values = c("0-10" = "#fef0d9", 
                               "10-20" = "#fdd49e",
                               "20-50" = "#fdbb84",
                               "50-100" = "#fc8d59",
                               "100-500" = "#e34a33",
                               ">500" = "#b30000"),
                    name = "Carbon Intensity Range \n gCO2e MJ^-1") + 
  theme_minimal() +  # Use a minimal theme to avoid drawing a global background
  theme(panel.grid = element_blank(),
        legend.position = c(0.97, 0.04), # You can adjust these values as needed
        legend.justification = c("right", "bottom")) +
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude") 


# Modify df_viz to include centroids
df_viz_basin$geometry <- st_make_valid(df_viz_basin$geometry)
df_viz_centroid <- df_viz_basin %>%
  #st_as_sf(coords = c("lon", "lat"), crs = st_crs(states)) %>%  # convert df_viz to an sf object
  mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ),
         CI_cate = case_when(
           CI_gCO2_MJ < 10 ~ "0-10",
           CI_gCO2_MJ < 20 ~ "10-20",
           CI_gCO2_MJ < 50 ~ "20-50",
           CI_gCO2_MJ < 100 ~ "50-100",
           CI_gCO2_MJ < 500 ~ "100-500",
           TRUE ~ ">500"
         ),
         CI_cate = fct_reorder(CI_cate, CI_gCO2_MJ)) %>%
  st_centroid()  # calculate centroids

# Plotting
ggplot() +
  geom_sf(data = states, fill = "white", color = "gray") +  # Plot US boundaries
  geom_sf(data = df_viz_centroid, aes(color = CI_cate, size = Annual_Gas)) +
  scale_size(range = c(1, 20)) +  # Adjust size range based on your data
  scale_color_manual(values = c("0-10" = "#fef0d9", 
                               "10-20" = "#fdd49e",
                               "20-50" = "#fdbb84",
                               "50-100" = "#fc8d59",
                               "100-500" = "#e34a33",
                               ">500" = "#b30000"),
                    name = "Carbon Intensity Range \n gCO2e MJ^-1") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.97, 0.04), # Adjust these values as needed
        legend.justification = c("right", "bottom")) +
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude", size = "Gas Production")


## f5 prod-ci map basin ----

ggplot() +
  geom_sf(data = states, fill = "white", color = "grey") +  # Plot US boundaries
  geom_sf(data = df_viz_state %>%
            mutate(CI_gCO2_MJ = as.numeric(CI_gCO2_MJ),
                   CI_cate = case_when(CI_gCO2_MJ < 10 ~ "0-10",
                                       CI_gCO2_MJ < 20 ~ "10-20",
                                       CI_gCO2_MJ < 50 ~ "20-50",
                                       CI_gCO2_MJ < 100 ~ "50-100",
                                       CI_gCO2_MJ < 500~ "100-500",
                                       T ~ ">500")) %>%
            filter(CI_gCO2_MJ < 500) %>%
            mutate(CI_cate = fct_reorder(CI_cate, CI_gCO2_MJ),
                   CI_log = log(CI_gCO2_MJ)), 
          aes(fill = `CI_log`), color = NA) +  # Use 'fill' for polygon color
  scale_fill_gradient2(
    low = "blue", 
    mid = "darkgreen", 
    high = "red", 
    midpoint = 1
  ) +
  # scale_fill_manual(
  #   # values = c("0-10" = "#fef0d9", 
  #   #                            "10-20" = "#fdd49e",
  #   #                            "20-50" = "#fdbb84",
  #   #                            "50-100" = "#fc8d59",
  #   #                            "100-500" = "#e34a33",
  #   #                            ">500" = "#b30000"),
  #                   name = "Carbon Intensity Range \n gCO2e MJ^-1") + 
  theme_Publication() + 
  theme_minimal() +  # Use a minimal theme to avoid drawing a global background
  theme(panel.grid = element_blank(),
        legend.position = c(0.97, 0.04), # You can adjust these values as needed
        legend.justification = c("right", "bottom")) +
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude") 


## f5 prod-ci bubble map
View(df_viz_state)

df_viz_state %>%
  arrange(Annual_Gas) %>%
  filter(Annual_Gas > 0,
         CI_gCO2_MJ < 100) %>%
  ggplot() + 
  geom_sf(data = states, fill = "lightblue", color = "white") + 
  geom_sf(aes(fill = `CI_gCO2_MJ`, alpha = Annual_Gas, color = Production_Type)) +  # Use 'fill' for polygon color
  #scale_fill_binned(type = "viridis") + 
  scale_fill_stepsn(n.breaks = 12, colours = terrain.colors(12)) +
  theme_minimal() +  # Use a minimal theme to avoid drawing a global background
  theme(panel.grid = element_blank()) +
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude")


View(df_viz_basin)
df_viz_basin %>%
  arrange(Annual_Gas) %>%
  filter(Annual_Gas > 0,
         CI_gCO2_MJ < 100) %>%
  ggplot() + 
  geom_sf(data = states, fill = "white", color = "grey") + 
  geom_sf(aes(fill = `CI_gCO2_MJ`, alpha = Annual_Gas),color = NA) +  # Use 'fill' for polygon color
  #scale_fill_binned(type = "viridis") + 
  scale_fill_gradient2(
    low = "darkgreen", 
    mid = "yellow", 
    high = "red", 
    midpoint = 40
  ) +
  theme_minimal() +  # Use a minimal theme to avoid drawing a global background
  theme(panel.grid = element_blank()) +
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude", fill = "Vol-Weighted Production CI \n (gCO2e/MJ)", alpha = "Gas Production \n (MMSCF)")


df_viz_basin %>%
  arrange(Annual_Gas) %>%
  filter(Annual_Gas > 0,
         CI_gCO2_MJ < 100) %>%
  mutate(total_CI = CI_gCO2_MJ + field_mid_em_factor_gCO2_MJ) %>%
  ggplot() + 
  geom_sf(data = states, fill = "white", color = "grey") + 
  geom_sf(aes(fill = `total_CI`, alpha = Annual_Gas),color = NA) +  # Use 'fill' for polygon color
  #scale_fill_binned(type = "viridis") + 
  scale_fill_gradient2(
    low = "blue", 
    mid = "darkgreen", 
    high = "red", 
    midpoint = 40
  ) +
  theme_minimal() +  # Use a minimal theme to avoid drawing a global background
  theme(panel.grid = element_blank()) +
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude", fill = "Vol-Weighted Well-to-consumer CI \n (gCO2e/MJ)", alpha = "Gas Production \n (MMSCF)")

## f5 alaska ----
alaska <- states %>% filter(NAME == "Alaska")
# Now create the plot only for Alaska

df_viz_state %>%
  arrange(Annual_Gas) %>%
  filter(Annual_Gas > 0, CI_gCO2_MJ < 100, STATE_NAME == "Alaska") %>%
  ggplot() + 
  geom_sf(data = alaska, fill = "white", color = "grey") + 
  geom_sf(aes(fill = `CI_gCO2_MJ`, alpha = Annual_Gas), color = NA) +
  scale_fill_gradient2(
    low = "blue", 
    mid = "darkgreen", 
    high = "red", 
    midpoint = 30
  ) +
  scale_alpha(guide = "none") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  coord_sf(crs = st_crs(alaska), xlim = c(-170, -130), ylim = c(50, 72), expand = FALSE) +  # Set the coordinate system to focus on Alaska
  labs(x = "Longitude", y = "Latitude")


grid.arrange(p1, p2, nrow = 1)


p12 <- p1 + inset_element(p2, left = 0.1, bottom = 0.1, right = 0.3, top = 0.3)
p12


p2+p1





# Figure 6 Midstream allocation ---------------------------

states_production <- df_viz %>%
  st_drop_geometry() %>%
  group_by(STATEFP) %>%
  summarize(Annual_Gas = sum(Annual_Gas, na.rm = T),
            Annual_Oil = sum(Annual_Oil, na.rm = T)) %>%
  left_join(states %>%
              select(STATEFP, STUSPS, NAME) %>%
              st_drop_geometry()) %>%
  mutate(
    STUSPS = case_when(STATEFP == 0 ~ "GOM",
                       TRUE ~ STUSPS),
    NAME = case_when(STATEFP == 0 ~ "Offshore Gulf of Mexico",
                     TRUE ~ NAME)
  )
View(states_production)

# kgCO2/MMCF-km

midstream_matrix <- states_production %>%
  mutate(STATEFP = as.numeric(STATEFP)) %>%
  left_join(A_field_plant_w_offshore, by =c("STATEFP" = "prod_state_fips")) %>%
  left_join(A_plant_to_rnd_w_offshore %>% rename(delivery_over_plant_percentage = perc_proc_denominator), by = c("proc_state" = "plant")) %>%
  left_join(A_distance, by = c("proc_state" = "proc_start_w_offshore", "R&D" = "state_end")) %>%
  group_by(STATEFP, STUSPS, NAME,`R&D`) %>%
  summarize(transmitted_gas_MCF = sum(Annual_Gas * perc_prod_denominator * delivery_over_plant_percentage, na.rm = T),
    midstream_factor_kgCO2_MMCF = weighted.mean(4 * distance_km, Annual_Gas * perc_prod_denominator * delivery_over_plant_percentage, na.rm =T),
    distance = weighted.mean(distance_km, Annual_Gas * perc_prod_denominator * delivery_over_plant_percentage)) %>%
  mutate(midstream_factor_gCO2_MJ = midstream_factor_kgCO2_MMCF*1000/MMCF_to_MJ) %>%
  filter(transmitted_gas_MCF > 0) %>%
  left_join(states %>% st_drop_geometry() %>% select(delivery_state = STUSPS, delivery_state_name = NAME), by = c("R&D" = "delivery_state")) %>%
  rename(prod_state_fip = STATEFP,
         prod_state_short = STUSPS,
         prod_state_name = NAME,
         delivery_state_short = `R&D`) %>%
  mutate(emission_tCO2 = transmitted_gas_MCF/1000 * midstream_factor_kgCO2_MMCF/1000)
  
View(midstream_matrix)
s(midstream_matrix, "midstream_matrix")

# wyoming gas to vermont 
midstream_matr

delivery_state_gas <- midstream_matrix %>%
  group_by(delivery_state_short, delivery_state_name) %>%
  summarize(delivered_gas_MCF = sum(transmitted_gas_MCF),
            delivery_state_midstream_factor = weighted.mean(midstream_factor_gCO2_MJ,transmitted_gas_MCF),
            emission_tCO2 = sum(emission_tCO2))

s(delivery_state_gas, "delivery_state_gas")

production_state_gas <- midstream_matrix %>%
  group_by(prod_state_short, prod_state_name) %>%
  summarize(delivered_gas_MCF = sum(transmitted_gas_MCF),
            delivery_state_midstream_factor = weighted.mean(midstream_factor_gCO2_MJ,transmitted_gas_MCF),
            emission_tCO2 = sum(emission_tCO2))

s(production_state_gas, "production_state_gas")
View(production_state_gas)


# Texas gas consumption 
delivery_state_gas[delivery_state_gas$delivery_state_short == "TX",]$delivered_gas_MCF/sum(delivery_state_gas$delivered_gas_MCF) # 11.7%

ggplot(matrix_field_to_delivery %>% filter(transported_gas > 100), 
       aes(x = prod_state, y = `R&D`)) + 
  geom_point(aes(fill = Emission_tonCO2_between_prod_delivery, size = transported_gas), 
             alpha = 0.75, shape = 21) + 
  #scale_size_continuous(limits = c(0.000001, 100), range = c(1,17), breaks = c(1,10,50,75)) + 
  labs(x= "Production State", y = "Receiving State", size = "Transmitted Gas \n (MMSCF)", 
       fill = "Transmission \n Carbon Intensity \n (gCO2e/MJ)")  + 
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 90, vjust = 0.3, hjust = 1), 
        axis.text.y = element_text(colour = "black", face = "bold", size = 11), 
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.title = element_text(size = 12, face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
        legend.position = "right",
        axis.title.x = element_text(face = "bold", size = 12),  # Bold and size for x-axis title
        axis.title.y = element_text(face = "bold", size = 12)) +
  scale_fill_gradient2(
    low = "darkgreen", 
    mid = "yellow", 
    high = "red", 
    midpoint = 4.8
  ) 





# Figure 7 process breakdown ---------------------------
View(df_viz)
View(df_viz_basin)

processes_to_summarize <- c(
                          "exploration_gCO2_MJ", 
                          "drilling_dev_gCO2_MJ", 
                          "crude_prod_extra_gCO2_MJ",
                          "surface_gCO2_MJ", 
                          "lng_gCO2_MJ", 
                          "maintenance_gCO2_MJ", 
                          "waste_gCO2_MJ", 
                          "crude_transport_gCO2_MJ", 
                          "gas_dis_gCO2_MJ", 
                          "other_gCO2_MJ", 
                          "offsite_gCO2_MJ", 
                          "CO2_seq_gCO2_MJ", 
                          "field_mid_em_factor_gCO2_MJ"
                          )

color_palette <- brewer.pal(n = 12, name = "Paired")


# Define a more readable process names and corresponding color palette
process_names <- c("Exploration", 
                   "Drilling & Development", 
                   "Crude Production & Extraction",
                   "Surface Operations", 
                   "LNG Operations", 
                   "Maintenance",
                   "Crude Transportation", 
                   "Gas Disposal", 
                   "Other", 
                   "Offsite Operations",
                   "CO2 Sequestration", 
                   "Midstream Transmission")

# Assign colors to each process using a color palette
colors <- brewer.pal(12, "Paired")
names(colors) <- process_names

process_ids <- c("exploration_gCO2_MJ", 
                 "drilling_dev_gCO2_MJ", 
                 "crude_prod_extra_gCO2_MJ",
                 "surface_gCO2_MJ", 
                 "lng_gCO2_MJ", 
                 "maintenance_gCO2_MJ",
                 "crude_transport_gCO2_MJ", 
                 "gas_dis_gCO2_MJ", 
                 "other_gCO2_MJ",
                 "offsite_gCO2_MJ", 
                 "CO2_seq_gCO2_MJ", 
                 "field_mid_em_factor_gCO2_MJ")

# Prepare the data
plot_data <- df_viz_basin %>%
  st_drop_geometry() %>%
  arrange(desc(Annual_Gas)) %>%
  slice_head(n=20) %>%
  pivot_longer(cols = c("exploration_gCO2_MJ", 
                        "drilling_dev_gCO2_MJ", 
                        "crude_prod_extra_gCO2_MJ",
                        "surface_gCO2_MJ", 
                        "lng_gCO2_MJ", 
                        "maintenance_gCO2_MJ",
                        "crude_transport_gCO2_MJ", 
                        "gas_dis_gCO2_MJ", 
                        "other_gCO2_MJ",
                        "offsite_gCO2_MJ", 
                        "CO2_seq_gCO2_MJ", 
                        "field_mid_em_factor_gCO2_MJ"),
               names_to = "Process",
               values_to = "CI Value") %>%
  mutate(Process = factor(Process, levels = process_ids, labels = process_names))

# Create the plot
ggplot(plot_data, aes(x = reorder(Basin, Annual_Gas), y = `CI Value`, fill = Process)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Exploration" = "#A6CEE3", 
                               "Drilling & Development" = "#1F78B4", 
                               "Crude Production & Extraction" = "#B2DF8A",
                               "Surface Operations" = "#33A02C", 
                               "LNG Operations" = "#FB9A99", 
                               "Maintenance" = "#E31A1C",
                               "Crude Transportation"="#FDBF6F", 
                               "Gas Disposal"="#FF7F00", 
                               "Other"="#CAB2D6", 
                               "Offsite Operations"="#6A3D9A",
                               "CO2 Sequestration" = "#FFFF99", 
                               "Midstream Transmission" = "orange")) +
  coord_flip() +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        panel.grid = element_blank()) + 
  labs(x = "Basin", y = "Carbon Intensity (gCO2e/MJ)") +
  geom_hline(yintercept = total_mean, linetype = "dotted", color = "black", size = 0.5)



# Figure 2 violin plot showing variation by basin ---------------------------

df_viz %>%
  filter(CI_gCO2_MJ < 50 & Basin %in% top_15_basin) %>%
  ggplot(aes(x = Basin, y = CI_gCO2_MJ, fill = Basin)) +
  geom_violin() + 
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  #scale_fill_viridis(discrete = TRUE) +
  #theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )


# Figure 4 base map showing regional states ----

## 4.2 basemap for interregional transmission ----
# pacific
state_pacific = c("WA", "OR", "CA")
# rocky mountain
state_rockymountain = c("ID", "MT", "WY", "NV", "UT", "CO")
# southwest
state_southwest = c("AZ", "NM", "TX", "OK")
# midwest
state_midwest = c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH")
# northeast
state_northeast = c("VT", "ME", "NH", "MA", "RI", "CT", "NY", "PA", "NJ")
# southeast
state_southeast = c("AR", "LA", "MS", "TN", "KY", "AL", "FL", "GA", "SC", "NC", "VA", "WV", "DC","DE", "MD")

library(RColorBrewer)
states = states %>% mutate(region = case_when(
  STUSPS %in% state_pacific ~ "Pacific",
  STUSPS %in% state_rockymountain ~ "Rocky Mountain",
  STUSPS %in% state_southeast ~ "Southeast",
  STUSPS %in% state_southwest ~ "Southwest",
  STUSPS %in% state_northeast ~ "Northeast",
  STUSPS %in% state_midwest ~ "Midwest",
  STUSPS == "GOM" ~ "Offshore"
))

states_with_offshore <- states %>%
  rbind(gom_sf_final)

states_with_offshore <- bind_rows(states, gom_sf_final %>% 
                                    mutate(STUSPS = "GOM",
                                           NAME = "Offshore Gulf of Mexico",
                                           region = "Offshore") %>%
                                    st_set_crs(st_crs(states)))  %>% 
                                    mutate(region = case_when(
                                      NAME == "Alaska" ~ "Pacific",
                                      T ~ region
                                    ))
View(states_with_offshore)

states_with_offshore$region <- factor(states_with_offshore$region, levels = c("Pacific", 
                                                  "Rocky Mountain",
                                                  "Southwest",
                                                  "Midwest",
                                                  "Southeast",
                                                  "Northeast",
                                                  "Offshore"))
# Generate seven colors from the "Blues" palette
blues_palette <- brewer.pal(n = 7, name = "Blues")
# Print the color codes
print(blues_palette)

state_map <-
  ggplot() +
  geom_sf(data = states_with_offshore %>% select(STUSPS, NAME, region), aes(fill = region), color = "white", lwd = 0.2) +  # Plot US boundaries+
  geom_sf(data = gom_sf_final, fill = "#084594", lwd= 0) + 
  scale_size(range = c(1, 20)) +  # Adjust size range based on your data
  scale_fill_manual(values = blues_palette,
                    name = "Region") +
  #scale_fill_oil_gas() + 
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="right",
        legend.direction =  "vertical"
        ) + 
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude") 

ggsave(filename = paste0(PATH_SAVE_VIZ, "state_map.png"), 
       plot= state_map,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")

## 4.2 alaska ----
al_map <-
  ggplot() +
  geom_sf(data = states_with_offshore %>% select(STUSPS, NAME, region), aes(fill = region), color = "white", lwd = 0.2) +  # Plot US boundaries+
  geom_sf(data = gom_sf_final, fill = "#084594", lwd= 0) + 
  scale_size(range = c(1, 20)) +  # Adjust size range based on your data
  scale_fill_manual(values = blues_palette,
                    name = "Region") +
  #scale_fill_oil_gas() + 
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="right",
        legend.direction =  "vertical"
  )  + 
  coord_sf(expand = FALSE,xlim = c(-180, -128), ylim = c(49, 73))  

ggsave(filename = paste0(PATH_SAVE_VIZ, "alaska_map.png"), 
       plot= al_map,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")

## 4.1/3 CI state map ----

state_ci_map <-
  ggplot() +
  geom_sf(data = states, fill = "white", color = "black",, lwd = 0.2) +
  geom_sf(data = df_viz_basin %>% mutate(
                                total_CI = 
                                  CI_gCO2_MJ + field_mid_em_factor_gCO2_MJ,
                                total_CI_gCO2_MJ = as.numeric(total_CI),
                                total_CI_cate = case_when(
                                  total_CI < 5 ~ "0-5",
                                  total_CI < 10 ~ "5-10",
                                  total_CI < 15 ~ "10-15",
                                  total_CI < 20 ~ "15-20",
                                  total_CI < 30 ~ "20-30",
                                  TRUE ~ ">30"
                                ),
                                total_CI_cate = fct_reorder(total_CI_cate,
                                                            total_CI_gCO2_MJ),
                                CI_gCO2_MJ = as.numeric(CI_gCO2_MJ),
                                    CI_cate = case_when(
                                      CI_gCO2_MJ < 5 ~ "0-5",
                                      CI_gCO2_MJ < 10 ~ "5-10",
                                      CI_gCO2_MJ < 15 ~ "10-15",
                                      CI_gCO2_MJ < 20 ~ "15-20",
                                      CI_gCO2_MJ < 30 ~ "20-30",
                                      TRUE ~ ">30"
                                    ),
                                CI_cate = fct_reorder(CI_cate, CI_gCO2_MJ)),
          aes(fill = CI_cate, alpha= Annual_Gas), color = "white", lwd = 0) +  
  #geom_sf(data = gom_sf_final, fill = "#084594", lwd= 0) + 
  scale_size(range = c(1, 20)) +  # Adjust size range based on your data
  scale_fill_manual(values = c("0-5" = "lightgreen",
                               "5-10" = "green", 
                               "10-15" = "yellow",
                               "15-20" = "gold",
                               "20-30" = "orange",
                               "100-500" = "coral",
                               ">30" = "red"),
                    name = "Carbon Intensity Range \n gCO2e MJ^-1") +
  #scale_fill_oil_gas() + 
  scale_alpha_continuous(range = c(0.3, 1)) +
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="right",
        legend.direction =  "vertical"
  ) + 
  coord_sf(crs = st_crs(states), xlim = c(-125, -66), ylim = c(24, 50)) +  # Focus on the US
  labs(x = "Longitude", y = "Latitude") 

ggsave(filename = paste0(PATH_SAVE_VIZ, "basin_ci_upstream_map.png"), 
       plot= state_ci_map,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")

## 4.1 4.3 alaska ci----
alaska_plot <- 
  ggplot() +
  geom_sf(data = alaska_sf, fill = "white", color = "black", lwd = 0.7) + 
  geom_sf(data = df_viz_basin %>% filter(Basin == "Cook Inlet Basin") %>% mutate(
    total_CI = 
      CI_gCO2_MJ + field_mid_em_factor_gCO2_MJ,
    total_CI_gCO2_MJ = as.numeric(total_CI),
    total_CI_cate = case_when(
      total_CI < 5 ~ "0-5",
      total_CI < 10 ~ "5-10",
      total_CI < 15 ~ "10-15",
      total_CI < 20 ~ "15-20",
      total_CI < 30 ~ "20-30",
      TRUE ~ ">30"
    ),
    total_CI_cate = fct_reorder(total_CI_cate,
                                total_CI_gCO2_MJ),
    CI_gCO2_MJ = as.numeric(CI_gCO2_MJ),
    CI_cate = case_when(
      CI_gCO2_MJ < 5 ~ "0-5",
      CI_gCO2_MJ < 10 ~ "5-10",
      CI_gCO2_MJ < 15 ~ "10-15",
      CI_gCO2_MJ < 20 ~ "15-20",
      CI_gCO2_MJ < 30 ~ "20-30",
      TRUE ~ ">30"
    ),
    CI_cate = fct_reorder(CI_cate, CI_gCO2_MJ)),
    aes(fill = total_CI_cate, alpha= Annual_Gas), color = "white", lwd = 0) +  
  #geom_sf(data = gom_sf_final, fill = "#084594", lwd= 0) + 
  scale_size(range = c(1, 20)) +  # Adjust size range based on your data
  scale_fill_manual(values = c("0-5" = "lightgreen",
                               "5-10" = "green", 
                               "10-15" = "yellow",
                               "15-20" = "gold",
                               "20-30" = "orange",
                               "100-500" = "coral",
                               ">30" = "red"),
                    name = "Carbon Intensity Range \n gCO2e MJ^-1") +
  #scale_fill_oil_gas() + 
  scale_alpha_continuous(range = c(0.3, 1)) +
  theme_NAG_publication() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none"
  ) + 
  coord_sf(expand = FALSE,xlim = c(-180, -128), ylim = c(49, 73))  

ggsave(filename = paste0(PATH_SAVE_VIZ, "ci_map_alaska_total.png"), 
       plot= alaska_plot,
       #width = 100,
       #height = 50,
       dpi = 300,
       units = "cm",
       bg = "transparent")


# Policy run ----

clean_result = function(PATH, 
                        gas_file, oil_file, offshore_file, 
                        save_name, PATH_save){
  
  # # for debug
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
  df_viz_gas <- CI_by_process(clean_and_join_input(gas_results,US_GAS_WITH_COMP_scaled))
  df_viz_oil <- CI_by_process(clean_and_join_input(oil_results,US_OIL))
  df_viz_offshore <- CI_by_process_offshore(offshore_results, offshore_input, gom_sf_final) 
  
  # # for debug
  # v(df_viz_gas)
  
  df_base = rbind(df_viz_gas %>% mutate(Production_Type = "GAS"), df_viz_oil%>% mutate(Production_Type = "OIL")) %>%
    rbind(df_viz_offshore) %>%
    filter(Annual_Gas > 0 & !is.na(CI_gCO2_MJ))
  
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

df_base = clean_result(PATH_POLICY,
                       "gas_run_updated_opgee_0719.xlsx",
                       "oil_run_updated_opgee_0719.xlsx", 
                       "offshore_base.xlsx",
                       "df_base",
                       PATH_POLICY_Save)
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

df = df_base %>% st_drop_geometry() %>%
  left_join(df_flare_25 %>%
              select(FIPS, Production_Type, flare_25 = CI_gCO2_MJ) %>% st_drop_geometry()) %>%
  left_join(df_flare_5 %>%
              select(FIPS, Production_Type, flare_5 = CI_gCO2_MJ)%>% st_drop_geometry()) %>%
  left_join(df_extreme %>%
              select(FIPS, Production_Type, extreme = CI_gCO2_MJ)%>% st_drop_geometry())
  
# Updated base run results w uncertainty ----
df_up = clean_result(PATH_POLICY,
                           "gas_up.xlsx",
                           "oil_up.xlsx", 
                           "offshore_up.xlsx",
                           "base_up",
                           PATH_POLICY_Save)
df_lo = clean_result(PATH_POLICY,
                     "gas_lo.xlsx",
                     "oil_lo.xlsx", 
                     "offshore_lo.xlsx",
                     "base_lo",
                          PATH_POLICY_Save)

colnames(df_up)


df_base_full = df_base %>%
  st_drop_geometry() %>%
  left_join(df_lo %>%
              st_drop_geometry() %>%
              select(GEOID,Production_Type, CI_lo = CI_gCO2_MJ)) %>%
  left_join(df_up %>%
              st_drop_geometry() %>%
              select(GEOID,Production_Type, CI_up = CI_gCO2_MJ))

View(df_base_full)

save_local(df_base_full, "df_base_full")

A_field_midstream = read.csv(paste0(PATH_SAVE_VIZ, "/A_field_midstream_no_coords.csv"))

df_base_with_midstream = df_base_full %>%
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

View(df_base_with_midstream)


# Process breakdown ----
raw1 = read.xlsx(paste0(PATH_POLICY, "gas_run_updated_opgee_0719.xlsx"))
raw2 = read.xlsx(paste0(PATH_POLICY, "oil_run_updated_opgee_0719.xlsx"))
raw3 = read.xlsx(paste0(PATH_POLICY, "offshore_base.xlsx"))
df_gas = clean_opgee_remote_run_result(raw1)
df_oil = clean_opgee_remote_run_result(raw2)


df_gas_with_input = clean_and_join_input(raw1,US_GAS_WITH_COMP_scaled)
View(df_gas_with_input)

df_oil_with_input = clean_and_join_input(raw2,US_OIL)
View(df_oil_with_input)

View(CI_by_process(df_oil_with_input))
View(CI_by_process(df_gas_with_input))

save_local(df_gas_with_input,"df_gas_with_input")
save_local(df_oil_with_input,"df_oil_with_input")

df_viz_oil = CI_by_process(df_oil_with_input) %>%
  mutate(Production_Type = "OIL")
df_viz_gas = CI_by_process(df_gas_with_input) %>%
  mutate(Production_Type = "GAS")
df_viz_offshore <- CI_by_process_offshore(raw3, offshore_input, gom_sf_final) 

save_local(rbind(df_viz_oil, df_viz_gas) %>% filter(check == "OK"), "df_viz_updated")
save_local(df_viz_offshore, "df_viz_offshore_updated")

