

##
##    Clear the memory
##
rm(list=ls(all=TRUE))
Sys.unsetenv('CURL_CA_BUNDLE')

##
##    Read in the DEP shorelines project (1.97gig): https://s3.us-west-2.amazonaws.com/dep-public-data/dep_ls_coastlines/dep_ls_coastlines_0-7-0-55.gpkg
##
g <- geopackage("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg")
Shorelines <- gpkg_table(g, "shorelines_annual")

##
##    Extract the Palau, Cook Islands, Fiji and New Caledonian coastlines
##
   ##
   ##    Fiji
   ##
      Target_Countries = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select * 
                                                                                        from shorelines_annual
                                                                                        where eez_territory in ('FJI')")
      Target_Countries <- st_transform(Target_Countries, crs = "epsg:4326")
      Target_Countries <- Target_Countries[(Target_Countries$year == max(Target_Countries$year)) & (Target_Countries$certainty == "good"),]
                                                 
      X <- st_as_sf(st_union(Target_Countries))
      X <- st_shift_longitude(st_transform(X, st_crs(4326)))

      X$Country = "Fiji"
      plot(X)

      fiji_concave <- st_concave_hull(X, ratio = .01)
      plot(fiji_concave[,1])


      # Creating a STAC obj
      s_obj <- rstac::stac("https://stac.digitalearthpacific.org")

      ##
      ##    Concave
      ##
      search <- rstac::stac_search(
          q = s_obj,
          limit = 9999
      ) |> rstac::ext_filter(
          collection == "dep_s2_geomad" && s_intersects(geometry, {{fiji_concave}})
      )
      # Requesting items
      items <- rstac::post_request(search)
      items
      ToDownload <- assets_url(items)
      
      ##
      ##    Try terra - YUP!!!
      ##
      
      for(colour in c("_red","_green","_blue"))
      {
         for(year in c("_2022","_2023","_2024"))
         {
            print(paste0("Country is: Fiji Colour is: ", colour, " Year is: ", year))
            BringDown <- ToDownload[str_detect(ToDownload, year)]
            BringDown <- BringDown[str_detect(BringDown, colour) & !str_detect(BringDown, "rededge")]
            count = 1
            Wonder <- lapply(BringDown, function(x){
                              print(paste0("Bringing down ", count, " of ", length(BringDown)))
                              count <<- count + 1
                              Y <- rast(x)
                              Y <- aggregate(Y,fact=5, cores = 10)
#                              Y <- aggregate(Y,fact=2, cores = 10)
                              return(Y)
                              })
            Wonder <- sprc(Wonder)
            r <- mosaic(Wonder)
            #rp <- rotate(project(r,"epsg:4326"))
            #Cut_Me <- mask(rp, fiji_concave[,1])
            writeRaster(Cut_Me, filename =paste0("Data_Spatial/Fiji_Rast", colour, year,".tif"), gdal=c("COMPRESS=DEFLATE"), overwrite=TRUE)
         }
      }
            

   ##
   ##    Cook Islands
   ##
      Target_Countries = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select * 
                                                                                        from shorelines_annual
                                                                                        where eez_territory in ('COK')")
      Target_Countries <- st_transform(Target_Countries, crs = "epsg:4326")
      Target_Countries <- Target_Countries[(Target_Countries$year == max(Target_Countries$year)) & (Target_Countries$certainty == "good"),]
                                                 
      X <- st_as_sf(st_union(Target_Countries))
      X <- st_transform(X, st_crs(4326))

      X$Country = "Cook Islands"
      plot(X)

      fiji_concave <- st_concave_hull(X, ratio = .01)
      plot(fiji_concave[,1])


      # Creating a STAC obj
      s_obj <- rstac::stac("https://stac.digitalearthpacific.org")

      ##
      ##    Concave
      ##
      search <- rstac::stac_search(
          q = s_obj,
          limit = 9999
      ) |> rstac::ext_filter(
          collection == "dep_s2_geomad" && s_intersects(geometry, {{fiji_concave}})
      )
      # Requesting items
      items <- rstac::post_request(search)
      items
      ToDownload <- assets_url(items)
      
      ##
      ##    Try terra - YUP!!!
      ##
      
      for(colour in c("_red","_green","_blue"))
      {
         for(year in c("_2022","_2023","_2024"))
         {
            print(paste0("Country is: Cook Islands Colour is: ", colour, " Year is: ", year))
            BringDown <- ToDownload[str_detect(ToDownload, year)]
            BringDown <- BringDown[str_detect(BringDown, colour) & !str_detect(BringDown, "rededge")]
            count = 1
            Wonder <- lapply(BringDown, function(x){
                              print(paste0("Bringing down ", count, " of ", length(BringDown)))
                              count <<- count + 1
                              Y <- rast(x)
                              Y <- aggregate(Y,fact=2, cores = 10)
                              return(Y)
                              })
            Wonder <- sprc(Wonder)
            r <- mosaic(Wonder)
            rp = project(r,"epsg:4326")
            Cut_Me <- mask(rp, fiji_concave[,1])
            writeRaster(Cut_Me, filename =paste0("Data_Spatial/Cook_Islands_Rast", colour, year,".tif"), gdal=c("COMPRESS=DEFLATE"), overwrite=TRUE)
         }
      }

   ##
   ##    New Caledonian
   ##
      Target_Countries = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select * 
                                                                                        from shorelines_annual
                                                                                        where eez_territory in ('NCL')")
      Target_Countries <- st_transform(Target_Countries, crs = "epsg:4326")
      Target_Countries <- Target_Countries[(Target_Countries$year == max(Target_Countries$year)) & (Target_Countries$certainty == "good"),]
                                                 
      X <- st_as_sf(st_union(Target_Countries))
      X <- st_transform(X, st_crs(4326))

      X$Country = "New Caledonian"
      plot(X)

      fiji_concave <- st_concave_hull(X, ratio = .01)
      plot(fiji_concave[,1])


      # Creating a STAC obj
      s_obj <- rstac::stac("https://stac.digitalearthpacific.org")

      ##
      ##    Concave
      ##
      search <- rstac::stac_search(
          q = s_obj,
          limit = 9999
      ) |> rstac::ext_filter(
          collection == "dep_s2_geomad" && s_intersects(geometry, {{fiji_concave}})
      )
      # Requesting items
      items <- rstac::post_request(search)
      items
      ToDownload <- assets_url(items)
      
      ##
      ##    Try terra - YUP!!!
      ##
      
      for(colour in c("_red","_green","_blue"))
      {
         for(year in c("_2022","_2023","_2024"))
         {
            print(paste0("Country is: New Caledonian Colour is: ", colour, " Year is: ", year))
            BringDown <- ToDownload[str_detect(ToDownload, year)]
            BringDown <- BringDown[str_detect(BringDown, colour) & !str_detect(BringDown, "rededge")]
            count = 1
            Wonder <- lapply(BringDown, function(x){
                              print(paste0("Bringing down ", count, " of ", length(BringDown)))
                              count <<- count + 1
                              Y <- rast(x)
                              Y <- aggregate(Y,fact=2, cores = 10)
                              return(Y)
                              })
            Wonder <- sprc(Wonder)
            r <- mosaic(Wonder)
            rp = project(r,"epsg:4326")
            Cut_Me <- mask(rp, fiji_concave[,1])
            writeRaster(Cut_Me, filename =paste0("Data_Spatial/New_ Caledonian_Rast", colour, year,".tif"), gdal=c("COMPRESS=DEFLATE"), overwrite=TRUE)
         }
      }


   ##
   ##    Palau
   ##
      Target_Countries = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select * 
                                                                                        from shorelines_annual
                                                                                        where eez_territory in ('PLW')")
      Target_Countries <- st_transform(Target_Countries, crs = "epsg:4326")
      Target_Countries <- Target_Countries[(Target_Countries$year == max(Target_Countries$year)) & (Target_Countries$certainty == "good"),]
                                                 
      X <- st_as_sf(st_union(Target_Countries))
      X <- st_transform(X, st_crs(4326))

      X$Country = "Palau"
      plot(X)

      fiji_concave <- st_concave_hull(X, ratio = .01)
      plot(fiji_concave[,1])


      # Creating a STAC obj
      s_obj <- rstac::stac("https://stac.digitalearthpacific.org")

      ##
      ##    Concave
      ##
      search <- rstac::stac_search(
          q = s_obj,
          limit = 9999
      ) |> rstac::ext_filter(
          collection == "dep_s2_geomad" && s_intersects(geometry, {{fiji_concave}})
      )
      # Requesting items
      items <- rstac::post_request(search)
      items
      ToDownload <- assets_url(items)
      
      ##
      ##    Try terra - YUP!!!
      ##
      
      for(colour in c("_red","_green","_blue"))
      {
         for(year in c("_2022","_2023","_2024"))
         {
            print(paste0("Country is: Palau Colour is: ", colour, " Year is: ", year))
            BringDown <- ToDownload[str_detect(ToDownload, year)]
            BringDown <- BringDown[str_detect(BringDown, colour) & !str_detect(BringDown, "rededge")]
            count = 1
            Wonder <- lapply(BringDown, function(x){
                              print(paste0("Bringing down ", count, " of ", length(BringDown)))
                              count <<- count + 1
                              Y <- rast(x)
                              Y <- aggregate(Y,fact=2, cores = 10)
                              return(Y)
                              })
            Wonder <- sprc(Wonder)
            r <- mosaic(Wonder)
            rp = project(r,"epsg:4326")
            Cut_Me <- mask(rp, fiji_concave[,1])
            writeRaster(Cut_Me, filename =paste0("Data_Spatial/Palau_Rast", colour, year,".tif"), gdal=c("COMPRESS=DEFLATE"), overwrite=TRUE)
         }
      }

