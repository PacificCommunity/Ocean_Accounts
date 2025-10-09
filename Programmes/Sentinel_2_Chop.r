##
##    Programme:  Sentinel_2_Chop.r
##
##    Objective:  Chop out the sentinel-2 (S2) data into the shape of the islands, together with a 1 km buffer around the land mass. This excess RASTER data is not needed and is file bloat.
##
##    Author:     James Hogan, Senior Marine Resource Economist, 9 October 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      
   ##
   ##    Load the country shape files created in "Estimate_Bounded_Boxes.r". These do not have a 1km buffer around them.
   ##
      load('Data_Spatial/Countries.rda')
                     
   ##
   ##    Use the Countries dataset as the template for controling a loop that sequentially reads in each 
   ##       island, and chops the raster back to the Countries shapefile, plus a 1 km buffer.
   ##
      ##
      ##    Get a list of all of the S2 rasters
      ##
         Contents <- as.data.frame(list.files(path = "Data_Spatial/",  pattern = "*.tif"))
         names(Contents) = "DataFrames"
         Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
         Contents <- Contents[str_detect(Contents$DataFrames, "dep_s2"),]











      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Parallel/", File))  
                           X <- get(str_split_fixed(File, "\\.", n = 2)[,1])
                           return(X)})
#      rm(list=ls(pattern="Obs_split*"))
      New_VMS <- do.call(rbind, All_Data)
      New_VMS_Remainder2024 <- st_as_sf(New_VMS)
      save(New_VMS_Remainder2024, file = "Data_Spatial/New_Caledonia_Spatial.rda")




   ##
   ## Step 2: xxxxxxxxxxx
   ##
   
   
   ##
   ## Step 3: xxxxxxxxxxx
   ##



   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
