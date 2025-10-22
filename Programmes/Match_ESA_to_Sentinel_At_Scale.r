##
##    Programme:  Match_ESA_to_Sentinel_At_Scale.r
##
##    Objective:  I've downloaded all of the Sentinel-2 spatial data, now I need to 
##                match that to the overarching ESA data, at scale for all the countries.
##
##                Ok... how to..?
##
##                Options: 
##                   (1) Take all of the ESA data, and express the pacific as a series of extents
##                       with x-min, x-max, y-min, y-max values. Each extent inherits the Land Cover value.
##                       REFINEMENT: this could just be the x-min and y-min values since the x-max and y-max 
##                       values are just the next set of x-min and y-min values up. That creates a three dimensional array
##                       of x-min, y-min, land cover value.
##
##                       Take the Sentinel-2 data and do the same exercise. This will create a six dimensional array which
##                       will be data intensive... I can reduce the RGB back to a single (16581375 x 3) matrix as a master
##                       look-up table. Then, all of the Sentinel-2 data can be stored in another three dimensional array 
##                       of x-min, y-min, lookup index.
##
##                       Complications - I think I need to express the 300m x 300m and the 10m x 10m as lat/longs to make 
##                       the location spatially compatible.
##
##                       I feel there's a trick with SQL which can help here (because its set-based langauge)... 
##
##    Author:     James Hogan, Senior Marine Resource Economist, 22 October 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Read in the ESA data
   ##
      ESA       <- raster("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
      getValues(ESA, row=10)[1:10]
                     
   ##
   ##    Read in a Sentinel-2 file...
   ##
      Sentinel_blue  <- raster("Data_Spatial/_blue_20177.tif")
      Sentinel_green <- raster("Data_Spatial/_green_20177.tif")
      Sentinel_red   <- raster("Data_Spatial/_red_20177.tif")
      
      freq(Sentinel_blue)
      Z <- getValues(Sentinel_blue)
      
      summary(getValues(Sentinel_blue))
      summary(getValues(Sentinel_green))
      summary(getValues(Sentinel_red))


   ##
   ##    Put them on the same CRS
   ##
      crs(Sentinel_blue) <- crs(ESA)




   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##



matrix(data = values(r), nrow = 18, ncol = 36, byrow = TRUE)



r <- raster(ncols=36, nrows=18)
values(r) <- rnorm(ncell(r)) *3
breaks <- -2:2 * 3
rc <- cut(r, breaks=breaks)


