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
##                https://stacspec.org/en/tutorials/1-download-data-using-r/
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
   ##    I want to merge all of these Coastlines together - coastlines dont close - add a 1 meter buffer
   ##
      # New_Caledonia <- Target_Countries[(Target_Countries$TERRITORY1 == "New Caledonia"),]
      # New_Caledonia <- st_as_sf(st_union(New_Caledonia))
      # New_Caledonia$Country <- "New Caledonia" 
      # New_Caledonia <- st_make_valid(st_buffer(New_Caledonia, 1))
      # New_Caledonia <- st_cast(New_Caledonia, "MULTIPOLYGON")                          
      
      # New_Caledonia      <- st_transform(New_Caledonia,      st_crs(4326))
      # Noumea <- st_union(st_crop(New_Caledonia, c(ymin = -23, xmin = 163.5, ymax = -19.45, xmax = 168.4)))
      # New_Caledonia_Hull <- st_as_sf(st_convex_hull(st_union(Noumea)))
      # New_Caledonia_BBox <- st_bbox(Noumea)
      # New_Caledonia_Hull <- st_transform(New_Caledonia_Hull, st_crs(4326))
      # New_Caledonia_BBox <- st_transform(New_Caledonia_BBox, st_crs(4326))

   ##
   ##    I want to merge all of these Coastlines together - coastlines dont close - add a 1 meter buffer
   ##
      All_Countries <- lapply(unique(Target_Countries$TERRITORY1), function(x){
                                    X <- st_as_sf(st_union(Target_Countries[(Target_Countries$TERRITORY1 == x),]))
                                    X$Country <- x
                                    X <- st_make_valid(st_buffer(X, 1))
                                    X <- st_transform(X, st_crs(4326))
                                    
                             return(X)
                     })
      All_Countries <- st_shift_longitude(do.call(rbind, All_Countries))
      plot(All_Countries[,1])
      
   rm(g)
   rm(Shorelines)


##
##    Try the intersect again
##
   for(Country in c("Cook Islands", "Fiji", "Palau", "New Caledonia"))
   {
      New_Caledonia_Hull <- st_as_sf(st_convex_hull(All_Countries[All_Countries$Country == Country,]))
      New_Caledonia_Hull <- st_transform(New_Caledonia_Hull, st_crs(4326))

      s_obj <- stac("https://stac.digitalearthpacific.org")
      
      Search <- stac_search(q = s_obj,
                           limit = 999,
                            collections= "dep_s2_geomad")
      
      Filter <- ext_filter(q = Search,
                           s_intersects(geometry, {{New_Caledonia_Hull}}))
                                                   
      Results <- get_request(Filter)
      Items <- assets_select(Results,
                             asset_names = c("blue", "red", "green"))  
                             
      ToDownload <- assets_url(Items)
      
      ##
      ##    Try terra - YUP!!!
      ##
      
      for(colour in c("_red","_green","_blue"))
      {
         for(year in c("_2022","_2023","_2024"))
         {
            print(paste0("Country is: ", Country, " Colour is: ", colour, " Year is: ", year))
            BringDown <- ToDownload[str_detect(ToDownload, year)]
            BringDown <- BringDown[str_detect(BringDown, colour)]
            count = 1
            Wonder <- lapply(BringDown, function(x){
                              print(paste0("Bringing down ", count, " of ", length(BringDown)))
                              count <<- count + 1
                              Y <- rast(x)
                              Y <- aggregate(Y,fact=5, cores = 10)
                              return(Y)
                              })
            Wonder <- sprc(Wonder)
            r <- mosaic(Wonder)
            rp = project(r,"epsg:4326")
            writeRaster(rp, filename =paste0("Data_Spatial/", Country,"_Rast", colour, year,".tif"), gdal=c("COMPRESS=DEFLATE"), overwrite=TRUE)
         }
      }
   }


      rast_NC <- rast("Data_Spatial/New_Caledonia_Rast.tif")


      Noumea <- st_union(st_crop(New_Caledonia, c(ymin = -23, xmin = 163.5, ymax = -19.45, xmax = 168.4)))
      
                             

   ##
   ##    Load up one
   ##
      rast_NC <- rast("Data_Spatial/New_Caledonia_Rast.tif")
   ##
   ##    cut it to the coastline
   ##
      
      As_Vect   <- vect(New_Caledonia)
      Only_Land <- intersect(rast_NC, As_Vect)
      plot(Only_Land)

      New_Caledonia = st_transform(New_Caledonia, st_crs(4326))

      Subset1 <- st_union(st_crop(st_make_valid(New_Caledonia), c(ymin = -22.351142, xmin = 166.173569, ymax = -21.895303, xmax = 166.932652)))
      Subset1 <- vect(Subset1)

      rp = project(rast_NC,"epsg:4326")
      Subset2 <- crop(rp,Subset1)

      plot(Subset2)
      
      Test_Set <- mask(Subset2, Subset1)
      plot(Test_Set)
      
##
##    Aggregate up to 1000m x 1000m just for processing
##      
      Lower_Resolution <- aggregate(rast_NC, fact=100, cores = 10)
      writeRaster(Lower_Resolution, filename ="Data_Spatial/New_Caledonia_Rast_Lower_Resolution.tif", gdal=c("COMPRESS=DEFLATE"), overwrite=TRUE)

      tif=read_stars("Data_Spatial/New_Caledonia_Rast_Lower_Resolution.tif")
      sf=st_as_sf(tif)
      sf

      New_Caledonia <- Target_Countries[(Target_Countries$TERRITORY1 == "New Caledonia"),]
      New_Caledonia <- st_as_sf(st_union(New_Caledonia))
      New_Caledonia$Country <- "New Caledonia" 
      New_Caledonia = st_transform(New_Caledonia, st_crs(4326))
 
      Coastlines <- st_union(st_line_merge(New_Caledonia))
      Coastlines <- st_cast(st_make_valid(Coastlines), "MULTIPOLYGON")                          
      
      Subset2 <- st_contains(sf,Coastlines)


      plot(Coastlines)


