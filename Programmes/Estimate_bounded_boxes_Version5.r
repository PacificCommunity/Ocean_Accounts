##
##    Programme:  Estimate_bounded_boxes_Version5.r
##
##    Objective:  The EEZ are sourced from here: https://www.marineregions.org/downloads.php
##                Countries are derived from DEP Coastlines
##
##                This programme will estimate bounded boxes for Fiji, Palau, Cook Islands and New Caledonia,
##                including all of their islands. I want to do this island by island so I don't get this huge
##                glob of water filling up space in the satellite images.
##
##                Version 1 of this programme sought to break all of the countries apart into their islands.                
##                However, the shapefiles are not that smart, and countries were arbitarily split down the 180 line.
##
##                Secondly, previous versions of this programme used the worldbank shapefiles which missed a lot of spatial complexity
##                - see "Graphical_Output/Vanua_Levu_as_Shapefile.png" and "Graphical_Output/Vanua_Levu_in_Google_Maps.png", 
##                "Graphical_Output/Vanua_Levu_Missing_Spatial_Complexity.png".
##
##                This version is a major departure because I'm going to use the DEP coastline data to define "land"
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
      
      EEZ     <- st_read(dsn = EEZ_Dir, layer = "eez_v12")
      EEZ     <- st_rotate(EEZ)

   ##
   ##    Read in the DEP shorelines project
   ##
      g <- geopackage("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg")
      Shorelines <- gpkg_table(g, "shorelines_annual")
      Country_Codes = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select distinct eez_territory
                                                                                     from shorelines_annual")
      ##
      ##    eez_territory turns out to be an ISO code
      ##
      
      Country_Codes <-  merge(Country_Codes,
                              unique(st_drop_geometry(EEZ[,c("TERRITORY1", "ISO_TER1")])),
                              by.x = "eez_territory",
                              by.y = "ISO_TER1",
                              all.x = TRUE)  
      Country_Codes <- Country_Codes[!is.na(Country_Codes$eez_territory),]
      Country_Codes <- Country_Codes[Country_Codes$eez_territory != "KIR",]
      Country_Codes <- rbind(Country_Codes,
                             data.frame(eez_territory = "KIR",
                                        TERRITORY1 = "Kiribati"))

   ##
   ##    Extract the Palau, Cook Islands, Fiji and New Caledonian coastlines
   ##
      Target_Countries = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select * 
                                                                                        from shorelines_annual
                                                                                        where eez_territory in ('FJI', 'COK', 'PLW','NCL')")
                                                                                        
      Target_Countries <- merge(Target_Countries[(Target_Countries$year == max(Target_Countries$year)) & 
                                                 (Target_Countries$certainty == "good"),],
                                Country_Codes,
                                by = "eez_territory")

   ##
   ##   Move to common CRS and 
   ##
      Target_Countries      <- st_transform(Target_Countries, st_crs(EEZ))
      Target_Countries      <- st_shift_longitude(st_polygonize(Target_Countries))
      Target_Countries$Area <- st_area(Target_Countries)/1000000
      Target_Countries      <- Target_Countries[as.numeric(Target_Countries$Area) > 0,]
      
      Target_Countries$Polygon_ID <- 1:nrow(Target_Countries)     
            
   ##
   ##       Scale it up for all landmasses of all countries
   ## 
      Countries <- lapply(unique(Target_Countries$TERRITORY1), function(X){
                           x <- Target_Countries[(Target_Countries$TERRITORY1 == X),]
                           x <- st_as_sf(st_convex_hull(st_union(x)))
                           x$Country <- X
                           return(x)
                     })
      Countries <- st_shift_longitude(do.call(rbind, Countries))
      Countries <- st_sf(st_cast(Countries, "POLYGON"))
      
      Countries$Area <- st_area(Countries)/1000000
      Countries$Polygon_ID <- 1:nrow(Countries)     
      
      ##
      ##    These are the chunks of DEP GeoMAD we want to cut out
      ##
         plot(Countries[,1])
      
   ##
   ## Save files our produce some final output of something
   ##
      save(EEZ,              file = 'Data_Spatial/EEZ.rda')
      save(Target_Countries, file = 'Data_Spatial/Target_Countries.rda')
      save(Countries,        file = 'Data_Spatial/Countries.rda')
##
##    And we're done
##
