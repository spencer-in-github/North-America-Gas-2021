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
