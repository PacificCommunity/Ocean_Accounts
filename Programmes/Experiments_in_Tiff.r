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
##    Author:     James Hogan, Senior Marine Resource Economist, 1 September 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))

      NewCal <- rast("Data_Spatial/58K_20240101-20241231.tif")
      
      describe("Data_Spatial/58K_20240101-20241231.tif")
      #summary(values(NewCal))   #very data intensive. A less intensive is summary(NewCal) which takes a sample
      NewCal_df <- as.data.frame(NewCal, xy = TRUE)
      
      
      
      subNewCal <- NewCal
      
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



NewCal <- rast("Data_Spatial/58K_20240101-20241231.tif",nrows=180, ncols=360


##
##    And we're done
##
