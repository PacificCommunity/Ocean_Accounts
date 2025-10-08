##
##    Programme:  DEP_Play_Code_version2.py
##
##    Objective:   python -m pip install Geopandas pandas seaborn geodatasets matplotlib pystac odc odc-stac pywin32
##                  https://odc-stac.readthedocs.io/en/latest/notebooks/stac-load-e84-aws.html
##                  https://digitalearthpacific.github.io/data-access/
##                  https://data.digitalearthpacific.org/#
##
##
##                  Lets see if we can pull in other types of data
##
##
##
##
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
##  load some functionality
##
Spatial_Path = str(current_directory)
Spatial_Path = Spatial_Path.replace("\\Programmes","\\Data_Spatial\\")

Bounded_Boxes = pandas.read_csv(Spatial_Path + "/BBX.csv")

print(Bounded_Boxes.head())

##
##  Loop it around all the islands
##
catalog = "https://stac.digitalearthpacific.org"  # DE Pacific STAC Catalog
collection = "dep_s2_geomad"  # Collection for GeoMAD files

for Counting_Variable in range(0,len(Bounded_Boxes)):
#for Counting_Variable in range(0,1):
    bbox = (Bounded_Boxes.iloc[Counting_Variable,0], Bounded_Boxes.iloc[Counting_Variable,1], Bounded_Boxes.iloc[Counting_Variable,2],Bounded_Boxes.iloc[Counting_Variable,3])
    print(bbox)
    
    # Find STAC Items
    client = Client.open(catalog)
    items = client.search(collections=[collection], bbox=bbox).item_collection()
    print(f"Found {len(items)} items")

     # Load STAC Items
    data = load(items, bbox=bbox, bands=["blue", "red", "green"], chunks={})

    # Plot Mangroves
    #data.blue.plot.imshow(
    #    col="time",
    #    col_wrap=4,
    #    levels=[0, 1, 2, 3],
    #    colors=["white", "yellow", "green", "darkgreen"],
    #)
    print(f"Found {len(items)} items here")

    Polygon_ID = str(int(Bounded_Boxes.iloc[Counting_Variable,4]))

    print(Polygon_ID)
    
    # Save blue as COG Tiles
    for time in data.time.values:
        year = time.astype("datetime64[Y]")
        data_year = data.sel(time=year)
        Graphics_Path1 = Spatial_Path + str(collection) + "_red_" + str(year) + "_PolygonID_" + Polygon_ID +".tif"
        Graphics_Path2 = Spatial_Path + str(collection) + "_green_" + str(year) + "_PolygonID_" + Polygon_ID +".tif"
        Graphics_Path3 = Spatial_Path + str(collection) + "_blue_" + str(year) + "_PolygonID_" + Polygon_ID +".tif"
        print(Graphics_Path1)
        data_year.red.odc.write_cog(Graphics_Path1, overwrite = True)
        data_year.green.odc.write_cog(Graphics_Path2, overwrite = True)
        data_year.blue.odc.write_cog(Graphics_Path3, overwrite = True)
    
