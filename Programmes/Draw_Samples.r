##
##    Programme:  Draw_Samples
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
##    Author:     James Hogan, Senior Marine Resource Economist, 8 September 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))

   ##
   ##    Read in the ESA data
   ##
      ESA   <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
     
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
      describe("Data_Spatial/dep_s2_geomad_red_2017.tif")

   ##
   ##    Move the sentinel data to the same co-ordinate reference system
   ##
      Red    <- project(Red,   crs(ESA))
      Green  <- project(Green, crs(ESA))
      Blue   <- project(Blue,  crs(ESA))

   ##
   ##    Crop the ESA data to the Sentinel-2 extract
   ##
      Extent <- ext(Red)
      ESA <- crop(ESA, Extent)
      png(filename = "Graphical_Output/ESA_Land_Use_Example.png", bg = "transparent", height =(1.0*16.13), width = (1.0*20.66), res = 600, units = "cm")
         plot(ESA)
      dev.off()
      
      png(filename = "Graphical_Output/Sentinel-2_Example.png", bg = "transparent", height =(1.0*16.13), width = (1.0*20.66), res = 600, units = "cm")
         plot(Red)
      dev.off()
         
   ##
   ## Draw a sample from the Sentinel-2 data
   ##
      Regression_Sample <- sample(1:(nrow(Red)*ncol(Red)), ((nrow(Red)*ncol(Red))*.2))
      
   ##
   ## Exclude the regression obs and draw a test sample
   ##
      Regression_Sample <- sample(1:(nrow(Red)*ncol(Red)), ((nrow(Red)*ncol(Red))*.2))
      Not_Regressed     <-       (1:(nrow(Red)*ncol(Red)))[-unique(Regression_Sample)]
      Test_Sample       <- sample(Not_Regressed, length(Not_Regressed) * .2)

      save(Regression_Sample, "Data_Intermediate/Regression_Sample.rda")
      save(Test_Sample,       "Data_Intermediate/Test_Sample.rda")

##
## And we're done
##

