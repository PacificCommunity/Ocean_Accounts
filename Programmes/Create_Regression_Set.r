##
##    Programme:  Create_Regression_Set
##
##    Objective:  This programme builds on the thinking that we can use ESA data to pre-train
##                areas using a logistic regression. This programme makes the regression data set from the Sentinel-2 data
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
   ##    Read in the regression sample
   ##
      load("Data_Intermediate/Regression_Sample.rda")

   ##
   ##    Read in the Peg_Me function used for parallel processing
   ##
      source("R/functions.r")

   ##
   ##    Read in the spatial data
   ##
      ESA   <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
      Red   <- rast("Data_Spatial/dep_s2_geomad_red_2017.tif")
      Green <- rast("Data_Spatial/dep_s2_geomad_green_2017.tif")
      Blue  <- rast("Data_Spatial/dep_s2_geomad_blue_2017.tif")

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
      
   ##
   ##    Try with parallel processing
   ##         
      Size_of_Loops <- ceiling(length(Regression_Sample) / 10)
      
      cl <- makeCluster(13)
      clusterEvalQ(cl, { c(library(terra), library(sf)) }) 

      for(Parcycle in 0:9)
        {
        
          Grab_Target_Samples <- Regression_Sample[(Parcycle*Size_of_Loops + 1):min(((Parcycle+1)*Size_of_Loops),  length(Regression_Sample))]

          sp <- parallel::clusterSplit(cl, 1:length(Grab_Target_Samples))
          
          clusterExport(cl, c("sp", "Grab_Target_Samples", "Peg_Me"))  # each worker is a new environment. Peg_Me comes from "R/functions.r"
         
          tic(print(paste("Starting to process loop", Parcycle)))
            system.time(ll <- parallel::parLapplyLB(cl, sp, Peg_Me))
            New_Parcels <- do.call(rbind, ll)
         
            assign(paste0("Regression_SetXX", Parcycle),  New_Parcels)
            save(list = paste0("Regression_SetXX", Parcycle), 
                 file = paste0("Parallel/Regression_SetXX", Parcycle,".rda"))                              
            rm(list = paste0("Regression_SetXX",Parcycle))
          toc()
         }

      stopCluster(cl)
    
##
##    Collect them all back up
##   
      Contents <- as.data.frame(list.files(path = "Parallel/",  pattern = "*.rda"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents <- Contents[str_detect(Contents$DataFrames, "Regression_SetXX"),]

      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Parallel/", File))  
                           X <- get(str_split_fixed(File, "\\.", n = 2)[,1])
                           rm(list=c(as.character(File) ))
                           return(X)})
      Regression_Set <- do.call(rbind, All_Data)
      Regression_Set <- st_as_sf(Regression_Set)
                  
      names(Regression_Set)[names(Regression_Set) == "dep_s2_geomad_red_2017"]   = "Red_Values"
      names(Regression_Set)[names(Regression_Set) == "dep_s2_geomad_green_2017"] = "Green_Values"
      names(Regression_Set)[names(Regression_Set) == "dep_s2_geomad_blue_2017"]  = "Blue_Values"
      
      Regression_Set <- Regression_Set[!is.nan(Regression_Set$Blue_Values),]
      Regression_Set <- Regression_Set[!is.nan(Regression_Set$Green_Values),]
      Regression_Set <- Regression_Set[!is.nan(Regression_Set$Red_Values),]
      
##
##    Save and we're done
##   
      save(Regression_Set, file = "Data_Spatial/Regression_Set.rda")
