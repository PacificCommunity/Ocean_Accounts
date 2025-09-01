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








      
      xmin(subNewCal) <- 620000
      xmax(subNewCal) <- 700000
      
      ymin(subNewCal) <- 7520000
      ymax(subNewCal) <- 7600000
      
      plot(NewCal, ylim=c(7600000, 7520000), xlim=c(620000, 700000))
      
   ##
   ##    Chop it back while I figure this new library out
   ##
      
      

    download.file("https://geodata.ucdavis.edu/rspatial/rs.zip", dest = "Data_Spatial/rs.zip")
    unzip("data/rs.zip", exdir="data")



      NewCal <- raster("Data_Spatial/58K_20240101-20241231.tif")



##
##    And we're done
##

rm(list=ls(all=TRUE))

NewCal <- rast("Data_Spatial/58K_20240101-20241231.tif")

#describe("Data_Spatial/58K_20240101-20241231.tif")
subNewCal <- crop(NewCal, ext(178910, (178910+100), 7342530, (7342530+100)))
subNewCal <- as.points(subNewCal, values = TRUE, na.rm = FALSE)
y <- project(subNewCal, "+proj=longlat +datum=WGS84")
lonlat <- geom(y)[, c("x", "y")]
head(lonlat, 100)


NewCal <- vect("Data_Spatial/58K_20240101-20241231.tif")


