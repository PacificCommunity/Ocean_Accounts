##
##    Programme:  DEP_Play_Code.py
##
##    Objective:   python -m pip install Geopandas pandas seaborn geodatasets matplotlib pystac odc odc-stac pywin32
##                  https://odc-stac.readthedocs.io/en/latest/notebooks/stac-load-e84-aws.html
##
##    Author:     James Hogan, started 3 September 2026
##
##

##
##      Clear the memory
##

for name in dir():
    if not name.startswith('__'):  # Exclude built-in names and special attributes
        del globals()[name]
##
##  load some functionality
##
import pandas
import seaborn
import matplotlib
import os
from pystac.client import Client
from odc.stac import load

pandas.options.display.max_columns = None
pandas.options.display.max_rows = 1000
pandas.options.display.width = 1500

current_directory = os.getcwd()
print(f"Current Working Directory: {current_directory}")

##
##  Try out some of Sachin's code
##
catalog = "https://stac.digitalearthpacific.org"  # DE Pacific STAC Catalog
collection = "dep_s2_mangroves"  # Collection for mangroves

# Define Coordinates, in lat, lon
lower_left = (-10.590125, 149.844629)
upper_right = (-10.360110, 150.195631)

bbox = (lower_left[1], lower_left[0], upper_right[1], upper_right[0])

# Find STAC Items
client = Client.open(catalog)
items = client.search(collections=[collection], bbox=bbox).item_collection()
print(f"Found {len(items)} items")

# Load STAC Items
data = load(items, bbox=bbox, bands=["mangroves"], chunks={})

# Plot Mangroves
data.mangroves.plot.imshow(
    col="time",
    col_wrap=4,
    levels=[0, 1, 2, 3],
    colors=["white", "yellow", "green", "darkgreen"],
)
Graphics_Path = str(current_directory)
Graphics_Path = Graphics_Path.replace("\\Programmes","\\Data_Spatial\\")

# Save Mangroves as COG Tiles
for time in data.time.values:
    year = time.astype("datetime64[Y]")
    data_year = data.sel(time=year)
    Graphics_Path1 = Graphics_Path + str(collection) + "_" + str(year) + ".tif"
    print(Graphics_Path1)
    data_year.mangroves.odc.write_cog(Graphics_Path1, overwrite = True)

