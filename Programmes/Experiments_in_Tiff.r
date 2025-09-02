##
##    Programme:  Experiments_in_Tiff.r
##
##    Objective:  Refamiliarizing myself with working with Tiff files
##
##                The data is New Caledonia, downloaded from here: https://livingatlas.arcgis.com/landcoverexplorer/#mapCenter=39.18600%2C9.04200%2C10.00&mode=step&timeExtent=2017%2C2022&year=2020&downloadMode=true
##
##                This is a useful site: https://rspatial.org/rs/1-introduction.html
##                and this https://rspatial.org/rs/4-unsupclassification.html
##                and this https://datacarpentry.github.io/r-raster-vector-geospatial/01-raster-structure.html
##
##                Converting from UTM to Latitude/Longitude: https://stackoverflow.com/questions/30018098/how-to-convert-utm-coordinates-to-lat-and-long-in-r
##
##                The Living Atlas website is going to serve as the primary data source for this analysis.
##                It provides from free a 10meter x 10meter land use coded value which will work fine for a prototype.
##                I'll read the raster into the R. There's WAY too much data for holding in memory all at once, so it will need
##                chopped down into parallel processed chunks. The Z value of the raster is the land use measure
##
##
##    Author:     James Hogan, Senior Marine Resource Economist, 1 September 2025
##
##
   ##
   ##    Clear the memory
   ##

      rm(list=ls(all=TRUE))

      NewCal <- rast("Data_Spatial/58K_20240101-20241231.tif")

      #describe("Data_Spatial/58K_20240101-20241231.tif")
      
      subNewCal <- crop(NewCal, ext(178910, (178910+100), 7342530, (7342530+100)))
      subNewCal <- as.points(subNewCal, values = TRUE, na.rm = FALSE)
      y <- project(subNewCal, "+proj=longlat +datum=WGS84")
      lonlat <- st_as_sf(y)   



      NewCal <- as.points(rast("Data_Spatial/58K_20240101-20241231.tif"), values = TRUE, na.rm = FALSE)
      NewCal <- project(NewCal, "+proj=longlat +datum=WGS84")
      lonlat <- st_as_sf(y)   
