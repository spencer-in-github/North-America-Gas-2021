# This script is used to plot the delivery centroid of the industrial natural gas demand in the U.S.
# Author: Spencer Zhang
# Date: June 23, 2025

import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt

# Load your CSV file
df = pd.read_csv("revision_data/IndustrialNaturalGasDemand.csv")

# Create GeoDataFrame from Latitude and Longitude
gdf = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df['Longitude'], df['Latitude']), crs="EPSG:4326")

# Load US states base map
us_states = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
us_states = us_states[us_states['name'] == 'United States']
us_states = us_states.explode(index_parts=True)  # handle multipart geometry

# Plot
fig, ax = plt.subplots(figsize=(12, 8))
us_states.boundary.plot(ax=ax, linewidth=1)
gdf.plot(ax=ax, color='red', markersize=10, alpha=0.6)

# Add labels
ax.set_title("Industrial Natural Gas Demand Facilities in the U.S.", fontsize=14)
ax.set_xlabel("Longitude")
ax.set_ylabel("Latitude")
plt.tight_layout()
plt.show()
