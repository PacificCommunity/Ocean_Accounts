##
##    Programme:  Experiments_in_Raster_DEM.R
##
##    Objective:  Regionalisation of the GHG metrics requires understanding hillcountry slope. Through
##                OpenTopography, I've found NASA's Digital Elevation Model (DEM) for New Zealand:
##                https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.2
##
##                And https://nceas.github.io/oss-lessons/spatial-data-gis-law/3-mon-intro-gis-in-r.html
##
##                Lets see if we can estimate the land surface profile from it, and isolate it to farms.
##
##    Author:     James Hogan, Green House Gas Inventory, Policy & Trade, 19 September 2022
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load data from somewhere
   ##
      library(raster)
      library(rgdal)
      library(lidR)      
      library(rasterVis)
      library(terra)
   ##
   ##    Grab some spatial shapes for the different regions
   ##
      RegionalCouncil_Dir  <- "Data_Spatial/regional-council-2022-clipped-generalised/regional-council-2022-clipped-generalised.gdb"
      Regional_Council     <- st_read(dsn = RegionalCouncil_Dir, layer = "Regional_Council_2022_Clipped__generalised_")
      #Regional_Council     <- st_transform(Regional_Council,  2193 )
      Regional_Council$One <- 1

   ##
   ##    Grab one of these raster tiles
   ##       Code stolen from here: https://www.neonscience.org/resources/learning-hub/tutorials/raster-data-r
   ##
      DEM_Waikato  <- raster("G:/Greenhouse_Gas_Inventory_Team/Ag GHG - R/Data_Spatial/NASADEM/Waikato_output_NASADEM.tif")
      DEM_Manawatu <- raster("G:/Greenhouse_Gas_Inventory_Team/Ag GHG - R/Data_Spatial/NASADEM/Manawatu_output_NASADEM.tif")
      DEM <- raster("G:/Greenhouse_Gas_Inventory_Team/Ag GHG - R/Data_Spatial/NASADEM/output_NASADEM.tif")
      
        
      x <- terrain(DEM_Manawatu, opt=c('slope', 'aspect'), unit='degrees')
      plot(x)
      
      plot(DEM_Manawatu,
           box = FALSE,
           axes = FALSE)
      
      DEM_Waikato_Change  <- projectRaster(DEM_Waikato, 
                                           crs = crs(Regional_Council))
      DEM_Manawatu_Change <- projectRaster(DEM_Manawatu, 
                                           crs = crs(Regional_Council))
      v <- vect(Regional_Council)

      plot(DEM_Manawatu_Change)
      plot(v, add = TRUE)

      plot(v)
      plot(DEM_Manawatu_Change, add = TRUE)
      plot(DEM_Waikato_Change, add = TRUE)

      i <- intersect(DEM_Manawatu_Change,Regional_Council[Regional_Council$REGC2022_V1_00_NAME == "Hawke's Bay Region",])
      plot(v)
      plot(i, add = TRUE)

      i <- crop(DEM_Manawatu_Change,Regional_Council[Regional_Council$REGC2022_V1_00_NAME == "Hawke's Bay Region",],
                snap = "in")
      plot(v)
      plot(i, add = TRUE)
      
      i <- mask(DEM_Manawatu_Change,Regional_Council[Regional_Council$REGC2022_V1_00_NAME == "Hawke's Bay Region",])
      plot(v[v$REGC2022_V1_00_NAME == "Hawke's Bay Region",])
      plot(i, add = TRUE)
      
      x <- terrain(DEM_Manawatu, opt=c('slope', 'aspect'), unit='degrees')
      plot(x)
      
      x <- terrain(i, opt=c('slope', 'aspect'), unit='degrees')
      plot(x)
##
##    Works from here up
##      
      plot(v)
      plot(DEM_Manawatu_Change, add = TRUE)
      
      
      x <- terrain(DEM, opt=c('slope', 'aspect'), unit='degrees')
      plot(x)
   
      ##
      ##    
      ##
      terraspat <- rast("G:/Greenhouse_Gas_Inventory_Team/Ag GHG - R/Data_Spatial/NASADEM/NASADEM_HGT_n44w116.tif")
      terraspat_Waikato <- rast("G:/Greenhouse_Gas_Inventory_Team/Ag GHG - R/Data_Spatial/NASADEM/Waikato_output_NASADEM.tif")
      crs(terraspat_Waikato) <- st_crs(Regional_Council)$proj4string
      crs(terraspat_Waikato) <- st_crs(Regional_Council)$proj4string
      
      terra_terrain <- terrain(terraspat, v = 'slope')
      plot(terra_terrain)
 
 
      v <- vect(Regional_Council)
      crs(v) <- crs(terraspat_Waikato)
      
      plot(v)
      plot(terraspat_Waikato,add=TRUE)

 
     crs(DEM) <- st_crs(Regional_Council)$proj4string    
 


      i <- intersect(terraspat_Waikato,v)
 
      spplot(DEM)
      plot(Regional_Council[,c("One")], col = "white",add=TRUE)
      
 

 
     
      

     
      gplot(terraspat_Waikato) + geom_tile(aes(fill = value)) +
          facet_wrap(~ variable) +
          scale_fill_gradient(low = 'white', high = 'blue') +
          coord_equal()
      
      
      plot(Regional_Council[,c("One")], col = "white")
      plot(DEM,add=TRUE)
      
      test_spdf <- as(DEM, "SpatialPixelsDataFrame")
      intersection <- intersect(test_spdf, Regional_Council)
 

   
	b <- as(DEM_Manawatu@extent, 'SpatialPolygons')
	crs(b) <- st_crs(Regional_Council)$proj4string 
	plot(v)
	plot(b, add=TRUE, col='red', lwd=4)



   
	i <- intersect(Regional_Council, b)
   
   plot(Regional_Council[,c("One")])
	plot(intersection, add=TRUE, col='blue', lwd=2)  


   
       
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
intersect(e1, e2)

#SpatialPolygons
if (require(rgdal) & require(rgeos)) {
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
	projection(b) <- projection(p)
	i <- intersect(p, b)
	plot(p)
	plot(b, add=TRUE, col='red')
	plot(i, add=TRUE, col='blue', lwd=2)
}       
       
       
       
       
      Raster_Box <- st_bbox(DEM@extent)

      ggplot() +  
        geom_sf(data = Regional_Council) +
        geom_sf(data = Raster_Box) 

      
      test_spdf <- as(DEM, "SpatialPixelsDataFrame")
      test_df <- as.data.frame(test_spdf)
      colnames(test_df) <- c("value", "x", "y")

      
      ggplot() +  
        geom_sf(data = Regional_Council) +
        geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8) 



        
      x <- terrain(DEM, opt=c('slope', 'aspect'), unit='degrees')
      plot(x)

      
      
   ##
   ##    Express the regional council shapes on the same CRS as the DEM
   ##
      DEM2 <- as(DEM, "SpatialPixelsDataFrame")
      coordinates(DEM)
      
      
      DEM2 <- st_sfc(st_polygon(DEM2))
      DEM2 <- st_sf(a = 15, geometry = DEM2)
      st_crs(DEM2) <- st_crs(Regional_Council)              
      Target_Properties   <- Small_Site_3_Bedroom      
      
      
      
   ##
   ## Step 1: Where abouts is THIS specific Raster tile?
   ##
      test_spdf <- as(DEM, "SpatialPixelsDataFrame")
      test_df <- as.data.frame(test_spdf)
      colnames(test_df) <- c("value", "x", "y")
   
   
   ggplot() +  
     geom_sf(data = Regional_Council) +
     geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
     geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
                  fill=NA, color="grey50", size=0.25) +
     scale_fill_viridis() +
     coord_equal() +
     theme_map() +
     theme(legend.position="bottom") +
     theme(legend.key.width=unit(2, "cm"))   

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
