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
      load('Data_Spatial/BBX.rda')
   ##
   ##    Load some generic functions
   ##
      source("R/functions.r")

   ##
   ##    Read in the ESA data
   ##
      ESA   <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
                     
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
         Contents$Polygon_ID <- as.numeric(str_split_fixed(Contents$Dframe, "PolygonID_",2)[,2])
         
         

         Countries <- st_rotate(Countries)
         st_bbox(st_buffer(Countries[Countries$Polygon_ID == 1,], 1000))
      
      
      
      plot(Countries[Countries$NAME_EN == "Fiji","Polygon_ID"], 
                xlim = as.numeric(st_bbox(Countries[Countries$Polygon_ID == 6,"Polygon_ID"])[c(1, 3)]),
                ylim = as.numeric(st_bbox(Countries[Countries$Polygon_ID == 6,"Polygon_ID"])[c(2, 4)]),
                main = "The Fiji Island of Vanua Levu",
                key.pos = NULL,
                border = 10)
      ##
      ##    Take a snapshot to show the limits of the work
      ##
      png(filename = "Graphical_Output/Vanua_Levu_as_Shapefile.png", bg = "transparent", res=200, width = 1024, height = 768)
         plot(Countries[Countries$NAME_EN == "Fiji","Polygon_ID"], 
                xlim = as.numeric(st_bbox(Countries[Countries$Polygon_ID == 6,"Polygon_ID"])[c(1, 3)]),
                ylim = as.numeric(st_bbox(Countries[Countries$Polygon_ID == 6,"Polygon_ID"])[c(2, 4)]),
                main = "The Fiji Island of Vanua Levu",
                key.pos = NULL,
                border = 10)
      dev.off()
       
       plot(Countries[Countries$NAME_EN == "Fiji","Polygon_ID"],col="red",
            xlim = as.numeric(st_bbox(Countries[Countries$NAME_EN == "Fiji","Polygon_ID"])[c(1, 3)]),
            ylim = as.numeric(st_bbox(Countries[Countries$NAME_EN == "Fiji","Polygon_ID"])[c(2, 4)]),
            add = TRUE) 



st_drop_geometry(Countries[Countries$NAME_EN == "Fiji",])

      
     
   ##
   ##    Read in the Sentinel-2 data
   ##
   ##       Define Coordinates, in lat, lon
   ##       lower_left = (-22.351142, 166.173569)
   ##       upper_right = (-21.895303, 166.932652)
   ##

      Red   <- rast("Data_Spatial/dep_s2_geomad_Red_2017_PolygonID_1.tif")
      Green <- rast("Data_Spatial/dep_s2_geomad_green_2017_PolygonID_1.tif")
      Blue  <- rast("Data_Spatial/dep_s2_geomad_blue_2017_PolygonID_1.tif")
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
      New_Caledonia <- st_union(st_make_valid(Countries[Countries$Polygon_ID == 1,]))
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
