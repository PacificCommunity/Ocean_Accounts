##
##    Programme:  Estimate_bounded_boxes_Version2.r
##
##    Objective:  The EEZ are sourced from here: https://www.marineregions.org/downloads.php
##                Countries from here: https://datacatalog.worldbank.org/search/dataset/0038272
##
##                This programme will estimate bounded boxes for Fiji, Palau, Cook Islands and New Caledonia,
##                including all of their islands. I want to do this island by island so I don't get this huge
##                glob of water filling up space in the satellite images.
##
##                Version 1 of this programme sought to break all of the countries apart into their islands.                
##                However, the shapefiles are not that smart, and countries were arbitarily split down the 180 line.
##
##                Secondly, the worldbank shapefiles missed a lot of spatial complexity - see "Graphical_Output/Vanua_Levu_as_Shapefile.png"
##                and "Graphical_Output/Vanua_Levu_in_Google_Maps.png", "Graphical_Output/Vanua_Levu_Missing_Spatial_Complexity.png".
##                That MIGHT exist in the ESA data, but spliting the countries into their islands turned out to be a bad idea, but this 
##                spatial complexity got omitted when the shapefiles were relied upon.
##
##                So this time, I'm going to union the countries before I boundry box them, and just accept, I'm 
##                going to be pulling a lot of empty water and blowing hard drive space :(
##
##                Drop Fiji for the moment
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
      EEZ     <- st_rotate(EEZ)
      
      Countries <- st_read(dsn = Countries_Dir, layer = "WB_countries_Admin0_10m")
      #Countries <- st_rotate(Countries)

   ##
   ##    Subset the EEZs for the countries we care about 
   ##
      EEZ       <- EEZ[EEZ$TERRITORY1 %in% c("Fiji", "Palau", "Cook Islands", "New Caledonia"),]
      Countries <- Countries[Countries$NAME_EN %in% c("Palau", "Cook Islands", "New Caledonia"),]

   ##
   ##   Move to common CRS
   ##
      st_crs(EEZ) <- st_crs(Countries)              

   ##
   ##       Union all of the countries together with 1 kilometer buffer
   ## 
      Hold <- as.character(unique(Countries$NAME_EN))
      Countries <- lapply(unique(Countries$NAME_EN), function(X){
                           x <- Countries[Countries$NAME_EN == X,]
                           x <- st_as_sf(st_union(st_buffer(st_make_valid(x), 1000)))
                     })
      Countries <- do.call(rbind, Countries)
      Countries$Name <- Hold


      st_bbox(Countries[1,])
      st_bbox(Countries[2,])
      st_bbox(Countries[3,])
      st_bbox(Countries[4,])
      
   ##
   ##   Estimate the bounded boxes for each landmass - add a 1 km buffer
   ##
      BBX <- lapply(unique(Countries$Name), function(X){
                    return(data.frame(xmin = st_bbox(Countries[Countries$Name == X,])[1],
                                      ymin = st_bbox(Countries[Countries$Name == X,])[2],
                                      xmax = st_bbox(Countries[Countries$Name == X,])[3],
                                      ymax = st_bbox(Countries[Countries$Name == X,])[4],
                                      Name = X))
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

 