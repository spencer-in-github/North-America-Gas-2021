# Load the OPGEE remote run results and clean it into a nice data frame

# Author          Spencer Zhang
# Date            September 26 2023
# Last Updated    Dec 3 2023

----------------------------------------
#---         1 Package              ----       
----------------------------------------
  
## Install Packages ----
# run this ONLY ONCE if you don't have the packages installed on your machine
# install.packages()
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("mapview")
# install.packages("sf")
# install.packages("tigris")


## Load Packages ----
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



----------------------------------------
#---      2 Global Variables        ----     
----------------------------------------

PATH_RESULT           = "~/GitHub/PhD/North-America-Gas-2021/Result Data/"
PATH_RESULT_SHEET     = "~/GitHub/PhD/North-America-Gas-2021/Result Data/RESULT_US_GAS_2022_20230925.xlsx"

PATH_INPUT_GAS        = paste0(PATH_RESULT, "us_gas_sf.rds")
PATH_INPUT_FLARE      = paste0(PATH_RESULT, "UScounty_withflaring2022_gas.csv")

ACTIVITY_FACTOR       = 4 # gCO2/MMSCF-km

----------------------------------------
#---        3 Function              ----     
----------------------------------------

clean_opgee_remote_run_result <- function(df){
  #df = RESULT_GAS
  colnames(df) <- seq(ncol(df))
  df = df %>% 
    mutate(`1` = if_else(grepl("Notes", `1`), NA, `1`)) %>%
    fill(`1`, `2`)
  
  sheet <- df %>%
    unite(., Var, 1,2,3,4,5,6,7,sep = " ", remove = T, na.rm = T)
  
  data1 = as.data.frame(t(sheet[8:nrow(sheet),]))
  colnames(data1) = data1[1,]
  data1 = data1[2:nrow(data1),]
  
  return(data1)
}
  
----------------------------------------
#---       4 Processing             ----     
----------------------------------------
  
RESULT_GAS = read_excel(PATH_RESULT_SHEET)
sheet = clean_opgee_remote_run_result(RESULT_GAS)
write_csv(sheet, paste0(PATH_RESULT, "US_GAS_2022_RESULT_cleaned.csv"))

## 4.1 Calc Vol.Wtd.Ave by oil volume----

vol_wtd_ci <- sheet %>%
  select(`Field properties Field name NA`, 
         `Field properties Oil production volume bbl/d`,
         `Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ`)

weighted.mean(as.numeric(vol_wtd_ci$`Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ`), as.numeric(vol_wtd_ci$`Field properties Oil production volume bbl/d`))
# 6.220844

## 4.2 Inspect NA CIs? ----
input_gas = read_rds(PATH_INPUT_GAS)
ci_with_input = input_gas %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(vol_wtd_ci %>%
              mutate(GEOID = as.numeric(`Field properties Field name NA`)), by = c("GEOID"="GEOID")) %>%
  mutate(CI = as.numeric(`Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ`)) 
  #%>%
  #filter(is.na(CI)) %>%
  #.[,c(1:38,85:97)]

## 4.2 vol wtd ave by gas vol -----

weighted.mean(as.numeric(ci_with_input$CI), as.numeric(ci_with_input$Annual_Gas))
# 6.491061

----------------------------------------
#---       5 Plotting             ----     
----------------------------------------

input_gas = read_rds(PATH_INPUT_GAS)
plot_df = input_gas %>%
  left_join(vol_wtd_ci, by = c("GEOID"="Field properties Field name NA")) %>%
  mutate(CI = as.numeric(`Lifecycle GHG emissions Total CO2 sequestered gCO2eq/MJ`))

## 5.1 Plot county CI  ----
ci_with_input$normalized_gas = scale(ci_with_input$Annual_Gas)

# Define breaks for your categories
breaks <- c(0, 10, 50, 100, 1000)
# Create categories based on the breaks
ci_with_input$ci_category <- cut(ci_with_input$CI, 
                                 breaks = breaks, 
                                 labels = c("0-10",
                                            "10-50",
                                            "50-100",
                                            "100-1000"))
# Define a color palette with distinct colors for each category
color_palette <- colorRampPalette(c("darkgreen", "greenyellow", "yellow", "red"))(length(breaks) - 1)

m = mapview(ci_with_input, zcol = "ci_category", 
        layer.name= "Field CI Range (gCO2e per MJ)",
        col.regions = color_palette,
        alpha.regions = "Annual_Gas",
        popup = popupTable(ci_with_input,
                           zcol = c("County_Name.x",
                                    "CI",
                                    "Annual_Gas",
                                    "Annual_Oil",
                                    "BCM2022")))
        #alpha.regions = "normalized_gas")
mapshot(m, paste0(PATH_VIZ, "/NAG_CI_2022_with_production_alpha_channel.html"))

## 5.2 Plot flare points ------
# Define breaks for your categories
breaks <- c(-0.1, 0.02, 0.04, 0.06, 0.08, 1.11)
# Create categories based on the breaks
plot_df = RAW_FLARE_2022
RAW_FLARE_2022$category <- cut(RAW_FLARE_2022$`BCM 2022`, 
                                 breaks = breaks, 
                                 labels = c("0-0.02",
                                            "0.02-0.04",
                                            "0.04-0.06",
                                            "0.06-0.08",
                                            "0.08-0.10"))
# Define a color palette with distinct colors for each category
color_palette <- colorRampPalette(c("darkgreen", "greenyellow", "yellow", "orange", "red"))(length(breaks) - 1)
mapview(ci_with_input, col.regions = "orange", layer.name = "Gas Zone") +
  mapview(RAW_FLARE_2022, 
          alpha = 0.5, 
          zcol = "category",
          #cex = .45,
          col.regions = color_palette, 
          layer.name = "Flare 2022")

## 5.3 -----
mapview(US_FIELD_INPUT_2022, zcol = "classified.type") + 
  mapview(RAW_FLARE_2022, zcol = "BCM2022")

mapview(US_FIELD_INPUT_2022, zcol = "classified.type", alpha.regions = "BCM2022")

--------------------------------------------
#---  6 Midstream (allocate back to fields)   ----     
--------------------------------------------  

## 6.1 Read raw matrix -----
PATH_OPTIM = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Midstream/Optimized_Flow_updated.xlsx"
PATH_LEON = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Midstream/North America Leontief Results 2021.xlsx"


## 6.2 Plant state to R&D state matrix ----

plant_to_rnd <- read_excel(PATH_LEON, sheet = "Results") 

A_plant_to_rnd <- plant_to_rnd[2:nrow(plant_to_rnd), 2:ncol(plant_to_rnd)] %>%
  mutate(plant = substr(`...2`, start=6, stop=7)) %>%
  select(-`...2`) %>%
  pivot_longer(!plant, names_to = "R&D", values_to = "throughput") %>%
  mutate(throughput = as.numeric(throughput)) %>%
  group_by(plant) %>%
  mutate(perc = throughput/sum(throughput))

# find state-to-state distance

unique_states = unique(c(unique(A_plant_to_rnd$plant), unique(A_plant_to_rnd$`R&D`)))

sf_states <- states()

unique(A_plant_to_rnd$plant)[!unique(A_plant_to_rnd$plant) %in% sf_states$STUSPS]

### Flag: We missing Canada ----

A_distance <- sf_states %>%
  st_centroid() %>%
  st_distance() %>%
  as.data.frame()

colnames(A_distance) <- sf_states$STUSPS
A_distance$state_start <- sf_states$STUSPS

A_distance <- A_distance %>%
  pivot_longer(!state_start, names_to = "state_end", values_to = "distance") %>%
  mutate(distance_km = as.numeric(distance))

A_midstream_emission <- A_plant_to_rnd %>%
  left_join(A_distance %>% select(-distance), by = c("plant" = "state_start", "R&D" = "state_end"))

## 6.3 Field-to-Plant ----

optim = read_excel(PATH_OPTIM)
plants <- colnames(optim)


sum(plants %in% optim_plant_state$Plant)    # 98%
length(plants)                              # 593
length(optim_plant_state$Plant)             # 612
sum(gsub("\\..*", "", plants[!plants %in% optim_plant_state$Plant]) 
    %in% optim_plant_state$Plant)

plants[!plants %in% optim_plant_state$Plant] 

out_counties <- optim$...1
prod_counties <- ci_with_input$County_Name.x

sum(prod_counties %in% out_counties)  # 566
length(prod_counties)                 # 622

missed_counties <- prod_counties[!prod_counties %in% out_counties]

### 6.3.1 simple state to state ver -----
A_optim_plant_state <- read_excel(PATH_OPTIM, sheet = "ProsStates")
A_optim_plant_state <- rename(A_optim_plant_state, c("Plant"= "...1"))

A_field_plant <- A_optim_plant_state %>%
  pivot_longer(!c("Plant", "State"), 
               names_to = "Field County", 
               values_to = "throughput") %>%
  mutate(`Field County` = gsub("\\..*" , "", `Field County`))

# check how the field county names in the matrix match with what I have in the OPGEE results
sum(unique(A_field_plant$`Field County`) %in% ci_with_input$County_Name.x )         # 457 in the results
length(ci_with_input$County_Name.x)                                                 # 622 county names
sum(unique(ci_with_input$County_Name.x) %in% unique(A_field_plant$`Field County`))  # 457
# check which counties in the result sheets dont show up in the matrix
unique(ci_with_input$County_Name.x)[!unique(ci_with_input$County_Name.x) %in% unique(A_field_plant$`Field County`)]
# They really are just not in there -- let's group by states

## 6.4 Field-to-plant by state level ----

dict_field_county_state <- read_excel(PATH_OPTIM, sheet = "Fields_Data") %>%
  select(NAME, STATE_NAME, STATE_FIPS, CNTY_FIPS, FIPS)

sum(unique(A_field_plant$`Field County`) %in% dict_field_county_state$NAME) # 612
length(unique(A_field_plant$`Field County`)) # 614
unique(A_field_plant$`Field County`)[!unique(A_field_plant$`Field County`) %in% dict_field_county_state$NAME] # "St" "Sum" -> just ignore

A_field_plant <- A_field_plant %>%
  left_join(dict_field_county_state, by = c("Field County" = "NAME"))

colnames(A_field_plant) <- c("proc","proc_state","prod_field","throughput","prod_state","prod_state_fips","prod_cnty_fips","prod_fips")

A_field_plant_states <- A_field_plant %>%
  mutate(proc_state = gsub("\\_.*", "", proc_state)) %>%
  group_by(proc_state, prod_state, prod_state_fips) %>%
  summarize(throughput = sum(throughput)) %>%
  drop_na(.) %>%
  group_by(proc_state) %>%
  mutate(prec_proc_denominator = throughput/sum(throughput)) %>%
  group_by(prod_state) %>%
  mutate(perc_prod_denominator = throughput/sum(throughput))
  

unique(A_field_plant_states %>%
  filter(is.na(throughput)) %>%
  .$proc_state) # only AB_pros was in here with no throughput -- its in Canada its fine


## 6.5 Calc Midstream Emissions on Plant State ----
plant_state_midstream_emission = A_midstream_emission %>%
  mutate(em_factor = as.numeric(perc)*as.numeric(distance_km)) %>%
  group_by(plant) %>%
  summarise(em_factor_perc_km = sum(em_factor, na.rm = T))

## 6.6 Gas volume allocated to plant state ----
states_production = ci_with_input %>%
  st_drop_geometry(.) %>%
  group_by(STATEFP) %>%
  summarise(Annual_Gas = sum(Annual_Gas, na.rm  =T),
            Annual_Oil = sum(Annual_Oil, na.rm  =T)) %>%
  left_join(states %>% select(STATEFP, STUSPS, NAME) %>%
              st_drop_geometry(.)) 
            #CI = weighted.mean(CI, Annual_Gas, na.rm  =T))


A_plant_proc_gas_mcf = A_field_plant_states %>%
  left_join(states_production %>% select(NAME, Annual_Gas), by = c("prod_state" = "NAME")) %>%
  mutate(Annual_Gas = coalesce(Annual_Gas, 0)) %>%
  mutate(proc_gas_mcf = Annual_Gas*perc_prod_denominator) %>%
  group_by(proc_state) %>%
  summarize(proc_gas_mcf = sum(proc_gas_mcf))

A_midstream_emission_with_prod <- A_midstream_emission %>%
  left_join(A_plant_proc_gas_mcf, by =c("plant" = "proc_state")) %>%
  mutate(proc_gas_mcf = coalesce(proc_gas_mcf,0)) %>%
  filter(!is.na(distance_km)) %>%
  mutate(midstream_emission_gCO2_per_MMCF = distance_km * perc * ACTIVITY_FACTOR) %>%
  group_by(plant) %>%
  summarize(proc_gas_mcf = mean(proc_gas_mcf),
            midstream_emission_gCO2_per_MMCF = sum(midstream_emission_gCO2_per_MMCF))
  
A_field_midstream <- A_field_plant_states %>%
  left_join(A_midstream_emission_with_prod, by = c("proc_state" = "plant"))

A_field_midstream_result <- A_field_midstream %>%
  mutate(midstream_emission = midstream_emission_gCO2_per_MMCF * proc_gas_mcf/1000 * prec_proc_denominator) %>%
  group_by(prod_state) %>%
  summarize(midstream_emission_gCO2 = sum(midstream_emission)) %>%
  left_join(ci_with_input %>%
              st_drop_geometry() %>%
              select(STATE_NAME, Annual_Gas, Annual_Oil, CI) %>%
              group_by(STATE_NAME) %>%
              summarize(Annual_Gas = sum(Annual_Gas),
                        Annual_Oil = sum(Annual_Oil)),
                        #CI = weighted.mean(CI, Annual_Oil)),
            by = c("prod_state" = "STATE_NAME")) %>%
  mutate(midstream_emission_gCO2_per_MMCF = midstream_emission_gCO2/Annual_Gas * 1000) %>%
  mutate(mid_em_gCO2_per_MJ = midstream_emission_gCO2_per_MMCF/1055.05585)

# 7 Midstream factor calculation gCO2/MMCF ----

ACTIVITY_FACTOR = 4 # kg CO2/MMCF-km 
MMCF_to_MJ = 1055055.85 # MJ/MMCF
ACTIVITY_FACTOR_gCO2_MJ_km = ACTIVITY_FACTOR * 1000/MMCF_to_MJ #gCO2e/MJ


proc_centroid = read_excel("/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Midstream/Processing_centroid.xlsx")
dlry_centroid = read_excel("/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/Data/Midstream/Delivery_centroid.xlsx")

# add match names to the two centroid dataframe (e.g. AL, CA, etc)
state_names <- sf_states %>%
  st_drop_geometry() %>%
  select("STUSPS", "NAME")
View(state_names)

sf_proc <- proc_centroid %>% 
  left_join(state_names, by = c("State" = "NAME")) %>%
    mutate(STUSPS = case_when(State == "Federal Offshore - GoM" ~ "GOM",
                            TRUE ~ STUSPS)) %>%
    filter(!is.na(STUSPS)) %>%
    st_as_sf(coords = c("Longitude","Latitude"), crs = st_crs(states))

sf_dlry <- dlry_centroid %>% 
        left_join(state_names, by = c("Location" = "NAME")) %>%
        mutate(STUSPS = case_when(Location == "Federal Offshore - GoM" ~ "GOM",
                                  TRUE ~ STUSPS)) %>%
        filter(!is.na(STUSPS))%>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = st_crs(states))


mapview(sf_dlry, color= "green") + mapview(sf_proc, color = "red")

sf_midstream_distance <- st_distance(sf_proc, sf_dlry) %>%
  as.data.frame()

View(sf_midstream_distance)

colnames(sf_midstream_distance) <- sf_dlry$STUSPS
sf_midstream_distance$state_start <- sf_proc$STUSPS

A_distance <- sf_midstream_distance %>%
  pivot_longer(!state_start, names_to = "state_end", values_to = "distance") %>%
  mutate(distance_km = as.numeric(distance)/1000,
         #proc_start_w_offshore = state_start,
         proc_start = case_when(state_start == "GOM" ~ "LA",
                                 TRUE ~ state_start)) %>%
  rename(proc_start_w_offshore = state_start)

View(A_distance)

## 7.1 midstream factor to processing state ----

plant_to_rnd <- read_excel(PATH_LEON, sheet = "Results") 

A_plant_to_rnd <- plant_to_rnd[2:nrow(plant_to_rnd), 2:ncol(plant_to_rnd)] %>%
  mutate(plant = substr(`...2`, start=6, stop=7)) %>%
  select(-`...2`) %>%
  pivot_longer(!plant, names_to = "R&D", values_to = "throughput") %>%
  mutate(throughput = as.numeric(throughput)) %>%
  group_by(plant) %>%
  mutate(perc_proc_denominator = throughput/sum(throughput)) 

A_plant_to_rnd_w_offshore <- A_plant_to_rnd %>%
  rbind(A_plant_to_rnd %>%
          filter(plant == "LA") %>%
          mutate(plant = "GOM"))
View(A_plant_to_rnd_w_offshore)

A_plant_RnD_midstream <- A_plant_to_rnd_w_offshore %>%
  left_join(A_distance %>% select(plant = proc_start_w_offshore, `R&D` = state_end, distance_km), by = c("plant" = "plant", "R&D" = "R&D")) %>%
  filter(!is.na(distance_km)) %>%
  mutate(midstream_factor_kgCO2_MMCF = ACTIVITY_FACTOR * perc_proc_denominator * distance_km)

View(A_plant_RnD_midstream)
s(A_plant_RnD_midstream, "A_plant_RnD_midstream")

A_plant_midstream <- A_plant_RnD_midstream %>%
  group_by(plant) %>%
  summarize(plant_midstream_factor_kgCO2_MMCF = sum(midstream_factor_kgCO2_MMCF)) %>%
  mutate(plant_midstream_factor_gCO2_MJ = plant_midstream_factor_kgCO2_MMCF * 1000 / MMCF_to_MJ)

View(A_plant_midstream)
s(A_plant_midstream, "A_plant_midstream")

## 7.2 mid factor to production state ----

cn(A_plant_midstream)
# "state_start_w_offshore"             
# "plant_mid_em_factor_kgCO2_per_MMCF" "plant_mid_em_factor_gCO2_MJ"       
# "plant" 

A_field_plant_w_offshore <- A_field_plant_states %>%
  rbind(
    data.frame(
      proc_state = "GOM",
      prod_state = "GOM",
      prod_state_fips = 0,
      throughput = 1,
      prec_proc_denominator = 1,
      perc_prod_denominator = 1
    )
  )
View(A_field_plant_w_offshore)

s(A_field_plant_w_offshore, "A_field_plant_w_offshore")

A_field_midstream <- A_field_plant_w_offshore %>%
  left_join(A_plant_midstream, by =c("proc_state" = "plant")) %>%
  mutate(production_midstream_factor = perc_prod_denominator * plant_midstream_factor_gCO2_MJ) %>%
  group_by(prod_state) %>%
  summarize(field_midstream_factor_gCO2_MJ = sum(production_midstream_factor)) %>%
  mutate(prod_state = case_when(prod_state == "GOM" ~ "Offshore Gulf of Mexico",
                                T ~ prod_state))

View(A_field_midstream)

write_csv(A_plant_midstream, paste0(PATH_RESULT, "/PLANT STATE MID EM FACTOR w GOM.csv"))
write_csv(A_field_midstream, paste0(PATH_RESULT, "/PRODUCTION STATE MID EM FACTOR w GOM.csv"))

s(A_field_midstream, "A_field_midstream")
A_field_midstream <- readRDS(paste0(PATH_SAVE_VIZ, "A_field_midstream.rds"))

## 7.3 connect with upstream CI ----
A_upstream_ci = ci_with_input %>%
  st_drop_geometry() %>%
  mutate(m = Annual_Gas * CI) %>%
  group_by(STATE_NAME) %>%
  summarise(CI_wtd_gas = sum(m)/sum(Annual_Gas),
            Annual_Gas = sum(Annual_Gas),
            Annual_Oil = sum(Annual_Oil))

A_field_up_midstream <- A_field_midstream %>%
  left_join(A_upstream_ci, by = c("prod_state" = "STATE_NAME"))

write.xlsx(A_field_up_midstream, paste0(PATH_RESULT, "UP+MID EMISSION by field state.xlsx"))

