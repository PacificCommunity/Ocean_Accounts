##
##    Programme:  Draw_Samples_Version2.r
##
##    Objective:  This programme builds on the thinking that we can use ESA data to pre-train
##                areas using a logistic regression. This programme makes the regression and
##                using parallel processing.
##                
##                To save time, the code creates two mutually exclusive samples: one for the 
##                regression, and the other for the test. This programme then goes off and 
##                processes the regression dataset, leaving the companion processing "Create_Test_Set.r"
##                to separately estimate the test set on another machine saving time.
##
##                Finally, both the regression and the test sets feed into an third program 
##                which does the logistic regressions, and the estimation of the land use in 
##                the test_set.
##
##
##    Changes to: This programme now reads in the Pacific Coastlines data set from https://geonode.pacificdata.org/catalogue/uuid/9f63ff8d-b475-418c-90ba-d8fab43aabc1
##    Version 1 : and uses its to subset the raster and exclude the sea.
##                
##
##    Recommendations: The country shapefile might benefit from having a boarder around it. The regression is struggling
##                     to identify water.
##
##
##    Author:     James Hogan, Senior Marine Resource Economist, 8 September 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions
   ##
      source("R/functions.r")

   ##
   ##    Read in the ESA data
   ##
      ESA   <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
      
   ##
   ##    Load spatial coastline data
   ##
      Countries_Dir <- "Data_Spatial/Pacific Coastlines/pacific_coastlines.shp"
      Countries <- st_read(dsn = Countries_Dir, layer = "pacific_coastlines")
      Countries <- st_rotate(Countries)
     
   ##
   ##    Read in the Sentinel-2 data
   ##
   ##       Define Coordinates, in lat, lon
   ##       lower_left = (-22.351142, 166.173569)
   ##       upper_right = (-21.895303, 166.932652)
   ##

      Red   <- rast("Data_Spatial/dep_s2_geomad_red_2017.tif")
      Green <- rast("Data_Spatial/dep_s2_geomad_green_2017.tif")
      Blue  <- rast("Data_Spatial/dep_s2_geomad_blue_2017.tif")
   ##
   ##    Move the sentinel data to the same co-ordinate reference system
   ##
      Red   <- project(Red,   crs(ESA))
      Green <- project(Green, crs(ESA))
      Blue  <- project(Blue,  crs(ESA))

      Extent <- ext(Red)
      ESA    <- crop(ESA, Extent)

   ##
   ##    Crop the Sentinel-2 data to the Coastlines
   ##
      New_Caledonia <- st_union(st_make_valid(Countries[Countries$territory1 == "New Caledonia",]))
      ##
      ##    Make New_Caledonia a SpatVector
      ##
         New_Caledonia_SpatVector <- vect(New_Caledonia)
         
      ##
      ##    I can't subset a raster into anything other than a rectangle (thats in the documentation),
      ##       but I can set all of the raster values outside a spatial vector to NA with the mask
      ##       function. This means, when I draw a sample, I can exclude NA values, and draw the sample
      ##       of defined size exclusively from land areas.
      ##
         New_Caledonia_Red   <- mask(Red,   New_Caledonia_SpatVector)
         New_Caledonia_Green <- mask(Green, New_Caledonia_SpatVector)
         New_Caledonia_Blue  <- mask(Blue,  New_Caledonia_SpatVector)
      
         New_Caledonia_ESA   <- mask(ESA,  New_Caledonia_SpatVector)
      
      
         plot(New_Caledonia_Red) # :) - Yay!
         plot(New_Caledonia_ESA) # :) - Yay!
      

   ##
   ##    Crop the ESA data to the Sentinel-2 extract
   ##
      
         png(filename = "Graphical_Output/ESA_Land_Use_Example.png", bg = "transparent", height =(1.0*16.13), width = (1.0*20.66), res = 600, units = "cm")
            plot(New_Caledonia_ESA)
         dev.off()
      
         png(filename = "Graphical_Output/Sentinel-2_Example.png", bg = "transparent", height =(1.0*16.13), width = (1.0*20.66), res = 600, units = "cm")
            plot(New_Caledonia_Red)
         dev.off()

   ##
   ## Draw a sample from the Sentinel-2 data
   ## 
      ##
      ##    I want a 20% sample of observations from cells which are valid
      ##
         ##
         ## Shuffle the data to make a random sample
         ##
            Observations <- sample(1:(nrow(New_Caledonia_Red)*ncol(New_Caledonia_Red)), ((nrow(New_Caledonia_Red)*ncol(New_Caledonia_Red))))
            Sample_Size  <- ((nrow(New_Caledonia_Red)*ncol(New_Caledonia_Red))) * 0.2
            
         ##
         ## Find all of the non-NA observations across all three layers
         ##
            Valid <- which((!is.na(New_Caledonia_Red[Observations][1])) &
                           (!is.na(New_Caledonia_Green[Observations][1])) &
                           (!is.na(New_Caledonia_Blue[Observations][1])))
            # New_Caledonia_Red[Observations[head(Valid)]][1]

         ##
         ## Subset the first sample size as the regression set, and the next sample size as the test set
         ##
            Regression_Sample <- Observations[Valid[1:Sample_Size]]
            Test_Sample       <- Observations[Valid[(Sample_Size+1):(2*(Sample_Size))]]

   ##
   ## Save - Fuck the above is elegant! :D
   ##
      save(Regression_Sample, file = "Data_Intermediate/Regression_Sample.rda")
      save(Test_Sample,       file = "Data_Intermediate/Test_Sample.rda")

##
## And we're done
##

