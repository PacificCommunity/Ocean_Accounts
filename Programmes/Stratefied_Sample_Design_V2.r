##
##    Programme:  Stratefied_Sample_Design.r
##
##    Objective:  
##
##    Author:     <PROGRAMMER>, <TEAM>, <DATE STARTED>
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load all of the country data
   ##
      ESA <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
      ##
      ##    Cook Islands
      ##
         Red   <- rast("Data_Spatial/Cook_Islands_Rast_Red_2022.tif")
         Green <- rast("Data_Spatial/Cook_Islands_Rast_Green_2022.tif")
         Blue  <- rast("Data_Spatial/Cook_Islands_Rast_Blue_2022.tif")
         
         NC_ESA <- crop(ESA,Red)
         NC_ESA <- resample(NC_ESA, Red, method = "mode")
         
         Cook_Islands <- c(NC_ESA, Red, Green, Blue)
         names(Cook_Islands) <- c("ESA", "Red", "Green", "Blue")
      ##
      ##    New Caledonia
      ##
         Red   <- rast("Data_Spatial/New_Caledonian_Rast_Red_2022.tif")
         Green <- rast("Data_Spatial/New_Caledonian_Rast_Green_2022.tif")
         Blue  <- rast("Data_Spatial/New_Caledonian_Rast_Blue_2022.tif")
         
         NC_ESA <- crop(ESA,Red)
         NC_ESA <- resample(NC_ESA, Red, method = "mode")
         
         New_Caledonian <- c(NC_ESA, Red, Green, Blue)
         names(New_Caledonian) <- c("ESA", "Red", "Green", "Blue")
      ##
      ##    Palau
      ##
         Red   <- rast("Data_Spatial/Palau_Rast_Red_2022.tif")
         Green <- rast("Data_Spatial/Palau_Rast_Green_2022.tif")
         Blue  <- rast("Data_Spatial/Palau_Rast_Blue_2022.tif")
         
         NC_ESA <- crop(ESA,Red)
         NC_ESA <- resample(NC_ESA, Red, method = "mode")
         
         Palau <- c(NC_ESA, Red, Green, Blue)
         names(Palau) <- c("ESA", "Red", "Green", "Blue")

      ##
      ##    Damn you Fiji!
      ##
      ##
         ##
         ##    Load up Fiji again
         ##
            Red   <- rast("Data_Spatial/Fiji_Rast_Red_2022.tif")
            Green <- rast("Data_Spatial/Fiji_Rast_Green_2022.tif")
            Blue  <- rast("Data_Spatial/Fiji_Rast_Blue_2022.tif")

            
            #plot(Blue,  xlim=c(3000000, 3500000), ylim=c(-1200000, -2200000))

            #Red   <- project(Red,"epsg:4326")
            #Green <- project(Green,"epsg:4326")
            #Blue  <- project(Blue,"epsg:4326")

         ##
         ##    Extract the Palau, Cook Islands, Fiji and New Caledonian coastlines
         ##
            ##
            ##    Fiji
            ##
               g <- geopackage("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg")
               Shorelines <- gpkg_table(g, "shorelines_annual")

               Target_Countries = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select * 
                                                                                                 from shorelines_annual
                                                                                                 where eez_territory in ('FJI')")
#               Target_Countries <- st_transform(Target_Countries, crs = "epsg:4326")

               Target_Countries_Blue <- st_transform(Target_Countries, crs = crs(Blue))
               Target_Countries_Red  <- st_transform(Target_Countries, crs = crs(Red))
               Target_Countries_Green<- st_transform(Target_Countries, crs = crs(Green))
               
               Target_Countries_Blue  <- Target_Countries_Blue[ (Target_Countries_Blue$year  == max(Target_Countries_Blue$year))  & (Target_Countries_Blue$certainty  == "good"),]
               Target_Countries_Red   <- Target_Countries_Red[  (Target_Countries_Red$year   == max(Target_Countries_Red$year))   & (Target_Countries_Red$certainty   == "good"),]
               Target_Countries_Green <- Target_Countries_Green[(Target_Countries_Green$year == max(Target_Countries_Green$year)) & (Target_Countries_Green$certainty == "good"),]
                                                          
               X_Blue  <- st_as_sf(st_union(Target_Countries_Blue))
               X_Red   <- st_as_sf(st_union(Target_Countries_Red))
               X_Green <- st_as_sf(st_union(Target_Countries_Green))

               fiji_concave <- st_concave_hull(X_Blue, ratio = .01)
               Blue  <- mask(Blue, fiji_concave)
               
               fiji_concave <- st_concave_hull(X_Red, ratio = .01)
               Red   <- mask(Red,  fiji_concave)

               fiji_concave <- st_concave_hull(X_Green, ratio = .01)
               Green <- mask(Green,fiji_concave)

               plot(Red,   xlim=c(177, 185), ylim=c(-12, -20))
               plot(Green, xlim=c(177, 185), ylim=c(-12, -20))
               plot(Blue,  xlim=c(177, 185), ylim=c(-12, -20))
   
               Green  <- project(Green,  crs(Blue))
               Red    <- project(Red,    crs(Blue))
               
               NC_ESA   <- project(ESA,Blue)
               
               NC_ESA <- project(NC_ESA, "epsg:4326")
               Red    <- project(Red,    "epsg:4326")
               Green  <- project(Green,  "epsg:4326")
               Blue   <- project(Blue,   "epsg:4326")
               
               NC_ESA <- resample(NC_ESA, Red, method = "mode")
               
               Fiji <- c(NC_ESA, Red, Green, Blue)
               names(Fiji) <- c("ESA", "Red", "Green", "Blue")


   ##
   ##    Ok, pull all of the land data out
   ##
      All_Together <- rbind(data.frame(New_Caledonian[New_Caledonian$ESA < 210,]),
                            data.frame(Palau[Palau$ESA < 210,]),
                            data.frame(Cook_Islands[Cook_Islands$ESA < 210,]),
                            data.frame(Fiji[Fiji$ESA   < 210,]))



      NC <- data.frame(New_Caledonian[New_Caledonian$ESA < 210,])
      PL <- data.frame(Palau[Palau$ESA < 210,])
      CI <- data.frame(Cook_Islands[Cook_Islands$ESA < 210,])
      FJ <- data.frame(Fiji[Fiji$ESA   < 210,])




   ##
   ##    run a logistic model
   ##
      list=ls(all=TRUE)
      rm(list= list[!(list %in% c("list", "New_Caledonia","Survey_Design"))])

      New_Caledonia$Is_Agriculture <- 0
      New_Caledonia$Is_Forest      <- 0
      New_Caledonia$Is_Grassland   <- 0
      New_Caledonia$Is_Wetland     <- 0
      New_Caledonia$Is_Settlement  <- 0
      New_Caledonia$Is_Water       <- 0
      New_Caledonia$Is_Other       <- 0

      values(New_Caledonia$"Is_Agriculture")[which(values(New_Caledonia$"ESA") %in% c(10,11,12,20,30,40))]  <- 1
      values(New_Caledonia$"Is_Forest")     [which(values(New_Caledonia$"ESA") %in% c(50,60,61,62,70,71,72,80,81,82,90,100,160,170))]  <- 1
      values(New_Caledonia$"Is_Grassland")  [which(values(New_Caledonia$"ESA") %in% c(110,130))]  <- 1
      values(New_Caledonia$"Is_Wetland")    [which(values(New_Caledonia$"ESA")  == 180)]  <- 1
      values(New_Caledonia$"Is_Settlement") [which(values(New_Caledonia$"ESA")  == 190)]  <- 1
      values(New_Caledonia$"Is_Water")      [which(values(New_Caledonia$"ESA")  == 210)]  <- 1
      values(New_Caledonia$"Is_Other")      [which(values(New_Caledonia$"ESA")  %in% c(120,121,122,140,150,151,152,153,200,201,202))]  <- 1
     
   ##
   ##    Use the survey Design to pull some random samples from the strata
   ##
     
      Random_Sample <- lapply(1:nrow(Survey_Design), function(x){
                              Layer <- New_Caledonia[values(New_Caledonia$ESA == Survey_Design$value[x])]
                              Random_Sample <- Layer[sample(1:nrow(Layer), Survey_Design$Sample_Size[x]),]
                              return(Random_Sample)
                              })
      Random_Sample <- do.call(rbind, Random_Sample)
      

      Model_Agriculture  <- glm(Is_Agriculture  ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      Model_Forest       <- glm(Is_Forest       ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      Model_Grassland    <- glm(Is_Grassland    ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      Model_Wetland      <- glm(Is_Wetland      ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      Model_Settlement   <- glm(Is_Settlement   ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      Model_Water        <- glm(Is_Water        ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      Model_Other        <- glm(Is_Other        ~ Red + Green + Blue, family = binomial, data = Random_Sample)

      New_Caledonia$"Probability_Agriculture" <- predict(New_Caledonia, Model_Agriculture, type="response", se.fit=FALSE)
      New_Caledonia$"Probability_Forest"      <- predict(New_Caledonia, Model_Forest,      type="response", se.fit=FALSE)
      New_Caledonia$"Probability_Grassland"   <- predict(New_Caledonia, Model_Grassland,   type="response", se.fit=FALSE)
      New_Caledonia$"Probability_Wetland"     <- predict(New_Caledonia, Model_Wetland,     type="response", se.fit=FALSE)
      New_Caledonia$"Probability_Settlement"  <- predict(New_Caledonia, Model_Settlement,  type="response", se.fit=FALSE)
      New_Caledonia$"Probability_Water"       <- predict(New_Caledonia, Model_Water,       type="response", se.fit=FALSE)
      New_Caledonia$"Probability_Other"       <- predict(New_Caledonia, Model_Other,       type="response", se.fit=FALSE)

   ##
   ## Find the highest probability
   ##

      X = tapp(New_Caledonia, index = c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1), which.max)
      New_Caledonia$"Predicted_Value"<- X$'X1'
      plot(New_Caledonia$"Predicted_Value")
   ##
   ## Save files our produce some final output of something
   ##
      writeRaster(New_Caledonia_2022, filename =paste0("Data_Spatial/", Country, "_2022.tif"), gdal=c("COMPRESS=DEFLATE"), overwrite=TRUE)
##
##    And we're done
##
rm(list=ls(all=TRUE))

##
##    Can we truncate this to the coastline?
##
New_Caledonia_2022 <- rast("Data_Spatial/New_Caledonia_2022.tif")
   ##
   ##    Read in the DEP shorelines project
   ##
      g <- geopackage("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg")
      Shorelines <- gpkg_table(g, "shorelines_annual")
      NewCal_Coast = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select * 
                                                                                    from shorelines_annual
                                                                                    where eez_territory in ('NCL')
                                                                                     and year = 2022
                                                                                     and certainty = 'good'")
NC <- st_transform(NewCal_Coast, crs = "epsg:4326")

NewCal_Coast <- st_union(NC)
NewCal_Coast <- st_concave_hull(NewCal_Coast, ratio =  0.0009765625)  ## THIS IS THE PERFECT NUMBER

NC <- vect(NewCal_Coast)
plet(NC)

plot(NC, background = "yellow")

Wonder <- mask(New_Caledonia_2022, NC)
plot(Wonder)





st_is_valid(NewCal_Coast)

plot(st_polygonize(NewCal_Coast), background = "yellow")


