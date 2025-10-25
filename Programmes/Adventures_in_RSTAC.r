##
##    Programme:  Adventures_in_RSTAC.r
##
##    Objective:  What is this programme designed to do?
##
##    Author:     <PROGRAMMER>, <TEAM>, <DATE STARTED>
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      load('Data_Spatial/EEZ.rda')
      load('Data_Spatial/Target_Countries.rda')
      load('Data_Spatial/Countries.rda')
      load('Data_Spatial/BBX.rda')      

   ##
   ##    Open a link to the catalog and collection 
   ##

      New_Caledonia <- st_as_sf(Countries[3,])
      plot(st_geometry(New_Caledonia))
      New_Caledonia_BBox <- st_bbox(New_Caledonia)
      
      
      s_obj <- stac("https://stac.digitalearthpacific.org")

      
      Search <- stac_search(q = s_obj,
                            collections= "dep_s2_geomad",
                            bbox = New_Caledonia_BBox,
                            limit = 5)
      Results <- get_request(Search)
      
     
      stac_query <- stac_search(q = s_obj,
                                collections= "dep_s2_geomad",
                                bbox = Cook_Islands_BBox)      
      stac_query
      
      ##
      ##    Lets just 
      ##


      s_obj <- stac("https://stac.digitalearthpacific.org")
      
      Search <- stac_search(q = s_obj,
                            collections= "dep_s2_geomad")
      
      Filter <- ext_filter(q = Search,
                           s_intersects(geometry, {{New_Caledonia}}))
                                                   
      Results <- get_request(Filter)
      Items <- assets_select(Results,
                             asset_names = c("blue", "red", "green"))  
                             
      Wonder <- items_as_sfc(assets_download(Items,  
                            output_dir = "Data_Spatial/New_Caledonia",
                            overwrite  = TRUE))

sf <- items_as_sf(stac_items)

# create a function to plot a map
plot_map <- function(x) {
  library(tmap)
  library(leaflet)
  current.mode <- tmap_mode("view")
  tm_basemap(providers[["Stamen.Watercolor"]]) +
    tm_shape(x) + 
    tm_borders()
}

plot_map(sf)      
      
      
      ##
      ##    Lets just 
      ##
      s_obj <- stac("https://stac.digitalearthpacific.org")
      
      Search <- stac_search(q = s_obj,
                            collections= "dep_s2_geomad")
      
      Filter <- ext_filter(q = Search,
                           s_intersects(geometry, {{New_Caledonia}}))
      Items <- assets_select(Filter,
                             asset_names = c("blue", "red", "green"))  
                                                   
      Results <- get_request(Items)
      Geospatial <- items_as_sf(Results)      
      
      
      WhatsThis <- st_as_sf(Geospatial[1,])
      
      Y <- st_as_sf(data.frame(st_drop_geometry(WhatsThis)), geometry = WhatsThis$geometry)
      
      
##
##    This is starting to work!!!
##
      s_obj <- stac("https://stac.digitalearthpacific.org")
      Search <- stac_search(q = s_obj,
                            collections= "dep_s2_geomad")
      Filter <- ext_filter(q = Search,
                           s_intersects(geometry, {{New_Caledonia}}))
      Request <- post_request(Filter)
      items_assets(Request)
      
      selected_item <- Request$features[[1]]
      ToDownload <- assets_url(selected_item, asset_names = c("blue", "green", "red"), append_gdalvsi = TRUE)
      
      Wonder <- read_stars(ToDownload)
      Wonder
      plot(Wonder, axes = TRUE)


##
##    Lets try reading all of New Caledonia in
##
      s_obj <- stac("https://stac.digitalearthpacific.org")
      Search <- stac_search(q = s_obj,
                            collections= "dep_s2_geomad")
      Filter <- ext_filter(q = Search,
                           s_intersects(geometry, {{New_Caledonia}}))
      Request <- post_request(Filter)
      Items <- assets_select(Request,
#                             asset_names = c("blue", "green", "red"))
                             asset_names = c("blue"))
      ToDownload <- assets_url(Items, append_gdalvsi = TRUE)
      ToDownload
      ##
      ##    Didn't work - stars wants to merge them
      ##
      Wonder <- read_stars(ToDownload, along = "new_dimensions")
      Wonder
      plot(Wonder, axes = TRUE)
      ##
      ##    Try terra - YUP!!!
      ##
      Wonder <- sprc(lapply(ToDownload, rast))
      r <- mosaic(Wonder)


   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##




x <- rast(xmin=-110, xmax=-60, ymin=40, ymax=70, res=1, vals=1)
y <- rast(xmin=-95,  xmax=-45, ymax=60, ymin=30, res=1, vals=2)
z <- rast(xmin=-80,  xmax=-30, ymax=50, ymin=20, res=1, vals=3)

m1 <- mosaic(x, y, z)

m2 <- mosaic(z, y, x)

# with many SpatRasters, make a SpatRasterCollection from a list
rlist <- list(x, y, z)
rsrc <- sprc(rlist)

m <- mosaic(rsrc)

plot(x)
plot(y, add = TRUE)
plot(z, add = TRUE)



