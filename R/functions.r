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



