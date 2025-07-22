import os
import glob
import geopandas as gpd
import matplotlib.pyplot as plt

shapefile_dir = "US field shapefile"
shp_files = glob.glob(os.path.join(shapefile_dir, "*.shp"))

for shp_file in shp_files:
    print(f"Reading and plotting: {shp_file}")
    gdf = gpd.read_file(shp_file)
    gdf.plot(figsize=(12, 8))
    plt.title(f"Shapefile: {os.path.basename(shp_file)}")
    plt.xlabel("Longitude")
    plt.ylabel("Latitude")
    plt.tight_layout()
    plt.show()
