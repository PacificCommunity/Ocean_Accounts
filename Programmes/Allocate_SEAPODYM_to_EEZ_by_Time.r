##
##    Programme:  Allocate_SEAPODYM_to_EEZ_by_Time.r
##
##    Objective:  We've got SEAPODYM in as raster files. Now allocate the biomass to the EEZs over time.
##
##    Author:     James Hogan, Senior Marine Resource Economist, 20 October 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      source("R/functions.r")
      
   ##
   ##    Load data from somewhere
   ##
      load("Data_Spatial/EEZ.rda")
      SEAPODYM_Skipjack_Adults <- rast('Data_Spatial/SEAPODYM_Skipjack_Adults.tif')


      plot(EEZ[,2])

      Fiji <- mask(SEAPODYM_Skipjack_Adults, EEZ[4,])

      plot(Fiji, "1989-07-15" )
      plot(EEZ[4,2], add=TRUE)

                     
   ##
   ## Step 1: xxxxxxxxxxx
   ##


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



Fiji <- mask(SEAPODYM_Skipjack_Adults, EEZ[4,])

Names <- unique(names(SEAPODYM_Skipjack_Adults))

Test_Fiji <- lapply(Names, function(x){
                    print(x)

                    Number_of_Fish <- as.numeric(as.data.frame(as.polygons(subset(Fiji, x), round=FALSE, na.all = TRUE, na.rm=FALSE))[,1])
                    Number_of_Fish[is.nan(Number_of_Fish)] <- 0
                    
                    Fiji_Poly <- as.polygons(Fiji, round=FALSE, na.all = TRUE, na.rm=FALSE)

                    Fiji_sf <- st_as_sf(as.polygons(Fiji_Poly))

                    X <- st_rotate(st_intersection(Fiji_sf, st_make_valid(EEZ[4,])))
                    X$Area <- st_area(X)/1000000
                    X$Number_of_Fish <- Number_of_Fish
                    X$Weighted_Fish <- X$Number_of_Fish * X$Area

                    return(data.frame(Date = x,
                                      Weighted_Fish = as.numeric(sum(X$Weighted_Fish))))
              })
Test_Fiji <- do.call(rbind, Test_Fiji)






Fiji <- mask(SEAPODYM_Skipjack_Adults, EEZ[4,])
Fiji_Poly <- as.polygons(Fiji, round=FALSE)

Fiji_sf <- st_as_sf(as.polygons(Fiji_Poly))

x <- st_rotate(st_intersection(Fiji_sf, st_make_valid(EEZ[4,])))
x$Area <- st_area(x)/1000000
x$Weighted_Fish <- x$`X1960.01.15` * x$Area


plot(x[,1])
plot(EEZ[4,2], add=TRUE, alpha = 0.3)
sum(st_area(x))
sum(st_area(st_make_valid(EEZ[4,])))

sum(x$Weighted_Fish)



x = "1960-01-15"
as.polygons(subset(Fiji, x), round=FALSE, na.all = TRUE, na.rm=FALSE)

x = "1962-02-15"
as.polygons(subset(Fiji, x), round=FALSE, na.all = TRUE, na.rm=FALSE)



plot( as.polygons(subset(Fiji, x), round=FALSE, na.all = TRUE, na.rm=FALSE), xlim=c(170, 187), ylim=c(-30,-8) )




Fiji <- mask(SEAPODYM_Skipjack_Adults, EEZ[4,])
Fiji_Poly <- as.polygons(Fiji, round=FALSE)

Fiji_sf <- st_as_sf(as.polygons(Fiji_Poly))

x <- st_rotate(st_intersection(Fiji_sf, st_make_valid(EEZ[4,])))
x$Area <- st_area(x)/1000000
x$Weighted_Fish <- x$`X1960.01.15` * x$Area


plot(x[,1])
sum(st_area(x))
sum(st_area(st_make_valid(EEZ[4,])))

sum(x$Weighted_Fish)

