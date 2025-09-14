##
##    Programme:  Create_Test_Set
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
      Red   <- rast("Data_Spatial/dep_s2_geomad_red_2017.tif")
      Green <- rast("Data_Spatial/dep_s2_geomad_green_2017.tif")
      Blue  <- rast("Data_Spatial/dep_s2_geomad_blue_2017.tif")

   ##
   ##    Read in the test sample
   ##
      load("Data_Intermediate/Test_Sample.rda")

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
   ## Step 3: Pull out a test set with parallel processing
   ##
      
      Size_of_Loops <- ceiling(length(Test_Sample) / 10)
      cl <- makeCluster(13)
      clusterEvalQ(cl, { c(library(terra), library(sf)) }) 


      for(Parcycle in 0:9)
        {
        
          Grab_Target_Samples <- Test_Sample[(Parcycle*Size_of_Loops + 1):min(((Parcycle+1)*Size_of_Loops),  length(Test_Sample))]

          Peg_Me <- function(Fistful)
               {
               
                  if(!exists("ESA")){ 
                     ESA <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
                     }
                     
                  if(!exists("Red")){ 
                     Red    <- rast("Data_Spatial/dep_s2_geomad_red_2017.tif")
                     Red    <- project(Red,   crs(ESA))
                     }
                  if(!exists("Green")){ 
                     Green  <- rast("Data_Spatial/dep_s2_geomad_green_2017.tif")
                     Green  <- project(Green, crs(ESA))
                     }
                     
                  if(!exists("Blue")){ 
                     Blue   <- rast("Data_Spatial/dep_s2_geomad_blue_2017.tif")
                     Blue   <- project(Blue,  crs(ESA))
                     }
                     
                  Extent <- ext(Red)
                  ESA    <- crop(ESA, Extent)

                 return(
                        Regression_Set <- do.call(rbind,
                                                 lapply(Fistful, 
                                                         function(x){
                                                                     return(
                                                                              tryCatch({
                                                                                         st_sf(data.frame(ESA_Value       = values(crop(ESA, ext(Red[Grab_Target_Samples[x], drop=FALSE])))[1],
                                                                                                          Red_Cell_Number = Grab_Target_Samples[x],
                                                                                                          Red_Values      = Red[Grab_Target_Samples[x]][1],
                                                                                                          Green_Values    = Green[Grab_Target_Samples[x]][1],
                                                                                                          Blue_Values     = Blue[Grab_Target_Samples[x]][1]),
                                                                                                          geom            = st_as_sf(as.polygons(Red[Grab_Target_Samples[x], drop=FALSE]))$geometry)
                                                                                       },
                                                                                   warning = function(w) {}, 
                                                                                   error   = function(e) {NULL}, 
                                                                                   finally = {})
                                                                              
                                                                           )
                                                                     }
                                                       )
                                            )
                       )
               }
            sp <- parallel::clusterSplit(cl, 1:length(Grab_Target_Samples))
            clusterExport(cl, c("sp", "Grab_Target_Samples", "Peg_Me"))  # each worker is a new environment, you will need to export variables/functions to
         
          tic(print(paste("Starting to process loop", Parcycle)))
            system.time(ll <- parallel::parLapplyLB(cl, sp, Peg_Me))
            New_Parcels <- do.call(rbind, ll)
         
            assign(paste0("Test_SetXX", Parcycle),  New_Parcels)
            save(list = paste0("Test_SetXX", Parcycle), 
                 file = paste0("Parallel/Test_SetXX", Parcycle,".rda"))                              
            rm(list = paste0("Test_SetXX",Parcycle))
          toc()
         }

      stopCluster(cl)
    
##
##    Collect them all back up
##   
      Contents <- as.data.frame(list.files(path = "Parallel/",  pattern = "*.rda"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents <- Contents[str_detect(Contents$DataFrames, "Test_SetXX"),]

      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Parallel/", File))  
                           X <- get(str_split_fixed(File, "\\.", n = 2)[,1])
                           rm(list=c(as.character(File) ))
                           return(X)})
      Test_Set <- do.call(rbind, All_Data)
      Test_Set <- st_as_sf(Test_Set)
                  
      names(Test_Set)[names(Test_Set) == "dep_s2_geomad_red_2017"]   = "Red_Values"
      names(Test_Set)[names(Test_Set) == "dep_s2_geomad_green_2017"] = "Green_Values"
      names(Test_Set)[names(Test_Set) == "dep_s2_geomad_blue_2017"]  = "Blue_Values"
      
      Test_Set <- Test_Set[!is.nan(Test_Set$Blue_Values),]
      Test_Set <- Test_Set[!is.nan(Test_Set$Green_Values),]
      Test_Set <- Test_Set[!is.nan(Test_Set$Red_Values),]
      
##
##    Save, and we're done
##   
      save(Test_Set, file = "Data_Spatial/Test_Set.rda")
