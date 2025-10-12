##    Programme:  functions.r
##
##    Objective:  Generic functions for doing something
##
##      Author:   
##

##
##    Put functions in here
##
 Peg_Me <- function(Fistful)
      {
         ##
         ##    Peg_Me is the parallel processing function used in Create_Regression_Set.r and Create_Test_Set.r
         ##
         ##       The same function, twice.. I saved here once and read twice
         ##
      
         if(!exists("ESA")){ ESA <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif") }
            
         if(!exists("Red")){ 
            Red    <- rast("Data_Spatial/dep_s2_geomad_red_2017.tif")
            Red    <- project(Red,   crs(ESA))}
            
         if(!exists("Green")){ 
            Green  <- rast("Data_Spatial/dep_s2_geomad_green_2017.tif")
            Green  <- project(Green, crs(ESA))}
            
         if(!exists("Blue")){ 
            Blue   <- rast("Data_Spatial/dep_s2_geomad_blue_2017.tif")
            Blue   <- project(Blue,  crs(ESA))}
            
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

##
##    This function is used to make a pacific-centric map
##
st_rotate <- function(x){
  x2 <- (sf::st_geometry(x) + c(360,90)) %% c(360) - c(0,90) 
  x3 <- sf::st_wrap_dateline(sf::st_set_crs(x2 - c(180,0), 4326)) + c(180,0)
  x4 <- sf::st_set_crs(x3, 4326)
  
  x <- sf::st_set_geometry(x, x4)
  
  return(x)
}


IPCC_Land_Categories <- data.frame(
IPCC_Classes = c("1. Agriculture","1. Agriculture","1. Agriculture","1. Agriculture","1. Agriculture","1. Agriculture","2. Forest","2. Forest","2. Forest","2.Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","3. Grassland","3. Grassland","4. Wetland","5. Settlement","6.Other","6.Other","6.Other","6.Other","6.Other","6.Other","6.Other","6.Other","6.Other","6.Other","6.Other","6.Other"),

IPCC_Classes_Detailed = c("1. Agriculture","1. Agriculture","1. Agriculture","1. Agriculture","1. Agriculture","1. Agriculture","2. Forest","2. Forest","2.Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","2. Forest","3. Grassland","3.Grassland","4. Wetland","5. Settlement","6.1 Shrubland","6.1 Shrubland","6.1 Shrubland","6.2 Sparse Vegetation","6.2 Sparse Vegetation","6.2 Sparse Vegetation","6.2 Sparse Vegetation","6.2 Sparse Vegetation","6.3 Bare Area","6.3 Bare Area","6.3 Bare Area","6.4 Water"),

Land_Cover = c(10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,160,170,110,130,180,190,120,121,122,140,150,151,152,153,200,201,202,210),
Description = c("Rainfed cropland","Rainfed cropland","Rainfed cropland","Irrigated cropland","Mosaic cropland (>50%) / natural vegetation (tree, shrub,herbaceouscover) (<50%)","Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (< 50%)","Tree cover, broadleaved, evergreen, closed to open (>15%)","Tree cover, broadleaved, deciduous, closed to open (> 15%)","Tree cover, broadleaved, deciduous, closed to open (> 15%)","Tree cover,broadleaved, deciduous, closed to open (> 15%)","Tree cover, needleleaved, evergreen, closed to open (> 15%)","Tree cover, needleleaved, evergreen, closed to open (> 15%)","Tree cover, needleleaved, evergreen, closed to open (> 15%)","Tree cover, needleleaved, deciduous, closed to open (> 15%)","Tree cover,needleleaved, deciduous, closed to open (> 15%)","Tree cover, needleleaved, deciduous, closed to open (> 15%)","Tree cover, mixed leaf type (broadleaved andneedleleaved)","Mosaic tree and shrub (>50%) / herbaceous cover (< 50%)","Tree cover, flooded, fresh or brakish water","Tree cover, flooded, saline water","Mosaic herbaceous cover (>50%) / tree and shrub (<50%)","Grassland","Shrub or herbaceous cover, flooded, fresh-saline or brakish water","Urban","Shrubland","Shrubland","Shrubland","Lichens and mosses","Sparse vegetation (tree, shrub, herbaceous cover)","Sparse vegetation (tree, shrub, herbaceous cover)","Sparsevegetation (tree, shrub, herbaceous cover)","Sparse vegetation (tree, shrub, herbaceous cover)","Bare areas","Bare areas","Bare areas","Water"))



