##
##    Programme:  Stratified_Sample_Design_V2.r
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
      New_Caledonia <- rast("Data_Spatial/New_Caledonia.tif")
      Fiji          <- rast("Data_Spatial/Fiji.tif")
      Palau         <- rast("Data_Spatial/Palau.tif")
      Cook_Islands  <- rast("Data_Spatial/Cook_Islands.tif")


   ##
   ##    Ok, pull all of the land data out
   ##
      Frequency <- freq(New_Caledonia,digits=2, bylayer = TRUE)
   
      NC <- data.frame(New_Caledonia[New_Caledonia$ESA < 210,])
      PL <- data.frame(Palau[Palau$ESA < 210,])
      CI <- data.frame(Cook_Islands[Cook_Islands$ESA < 210,])
      FJ <- data.frame(Fiji[Fiji$ESA   < 210,])



New_Caledonia <- Fiji
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
      

      Model_Agriculture  <- glm(Is_Agriculture  ~ Red + Green + Blue, family = binomial, data = New_Caledonia)
      Model_Forest       <- glm(Is_Forest       ~ Red + Green + Blue, family = binomial, data = New_Caledonia)
      Model_Grassland    <- glm(Is_Grassland    ~ Red + Green + Blue, family = binomial, data = New_Caledonia)
      Model_Wetland      <- glm(Is_Wetland      ~ Red + Green + Blue, family = binomial, data = New_Caledonia)
      Model_Settlement   <- glm(Is_Settlement   ~ Red + Green + Blue, family = binomial, data = New_Caledonia)
      Model_Water        <- glm(Is_Water        ~ Red + Green + Blue, family = binomial, data = New_Caledonia)
      Model_Other        <- glm(Is_Other        ~ Red + Green + Blue, family = binomial, data = New_Caledonia)

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


