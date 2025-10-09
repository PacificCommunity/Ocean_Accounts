##
##    Programme:  Estimate_bounded_boxes.r
##
##    Objective:  The EEZ are sourced from here: https://www.marineregions.org/downloads.php
##                Countries from here: https://datacatalog.worldbank.org/search/dataset/0038272
##
##                This programme will estimate bounded boxes for Fiji, Palau, Cook Islands and New Caledonia,
##                including all of their islands. I want to do this island by island so I don't get this huge
##                glob of water filling up space in the satellite images.
##
##    Author:     James Hogan, Senior Marine Resource Economist, 8 October 2025
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
   ##    Read in the EEZs 
   ##
      EEZ_Dir       <- "Data_Spatial/World_EEZ_v12_20231025/eez_v12.shp"
      Countries_Dir <- "Data_Spatial/WB_countries_Admin0_10m/WB_countries_Admin0_10m.shp"
      
      EEZ     <- st_read(dsn = EEZ_Dir, layer = "eez_v12")
      #EEZ     <- st_rotate(EEZ)
      
      Countries <- st_read(dsn = Countries_Dir, layer = "WB_countries_Admin0_10m")
      #Countries <- st_rotate(Countries)

   ##
   ##    Subset the EEZs for the countries we care about 
   ##
      EEZ       <- EEZ[EEZ$TERRITORY1 %in% c("Fiji", "Palau", "Cook Islands", "New Caledonia"),]
      Countries <- Countries[Countries$NAME_EN %in% c("Fiji", "Palau", "Cook Islands", "New Caledonia"),]

   ##
   ##   Move to common CRS
   ##
      st_crs(EEZ) <- st_crs(Countries)              


   ##
   ##    Lets try breaking Fiji apart... ok that worked
   ## 
      # Fiji <- Countries[Countries$NAME_EN %in% c("Fiji"),]
      # FijiFiji <- st_sf(st_cast(Fiji, "POLYGON"))
      # FijiFiji$Polygon_ID <- 1:nrow(FijiFiji)
   
   ##
   ##       Scale it up for all landmasses of all countries
   ## 
      Countries <- lapply(unique(Countries$NAME_EN), function(X){
                           x <- Countries[Countries$NAME_EN == X,]
                           x <- st_sf(st_cast(x, "POLYGON"))
                     })
      Countries <- do.call(rbind, Countries)
      Countries$Area <- st_area(Countries)
      Countries$Polygon_ID <- 1:nrow(Countries)                     

      #plot(Countries[Countries$NAME_EN %in% c("Fiji"),"Polygon_ID"])

   ##
   ##   Estimate the bounded boxes for each landmass - add a 1 km buffer
   ##
      BBX <- lapply(unique(Countries$Polygon_ID), function(X){
                    return(data.frame(xmin = st_bbox(st_buffer(Countries[Countries$Polygon_ID == X,], 1000))[1],
                                      ymin = st_bbox(st_buffer(Countries[Countries$Polygon_ID == X,], 1000))[2],
                                      xmax = st_bbox(st_buffer(Countries[Countries$Polygon_ID == X,], 1000))[3],
                                      ymax = st_bbox(st_buffer(Countries[Countries$Polygon_ID == X,], 1000))[4],
                                      Polygon_ID = X))
                     })
      BBX <- do.call(rbind, BBX)
      rownames(BBX) = NULL
      
   ##
   ## Save files our produce some final output of something
   ##
      save(EEZ,       file = 'Data_Spatial/EEZ.rda')
      save(Countries, file = 'Data_Spatial/Countries.rda')
      save(BBX,       file = 'Data_Spatial/BBX.rda')
      write.table(BBX,file = 'Data_Spatial/BBX.csv', row.names=FALSE, sep =",")
##
##    And we're done
##

load('Data_Spatial/Countries.rda')
plot(Countries[Countries$Polygon_ID == 48,"Polygon_ID"])


