##
##    Programme:  Stratefied_Sample_Design.r
##
##    Objective:  Stars is an R library designed for working with satelite data. Lets see what it can do
##
##    Author:     <PROGRAMMER>, <TEAM>, <DATE STARTED>
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load data from somewhere
   ##
      Country = "Cook Islands"
      
      Red   <- rast(paste0("Data_Spatial/", Country,"_Rast_red_2022.tif"))
      Green <- rast(paste0("Data_Spatial/", Country,"_Rast_green_2022.tif"))
      Blue  <- rast(paste0("Data_Spatial/", Country,"_Rast_blue_2022.tif"))
      
      ESA <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
      NC_ESA <- crop(ESA,Red)
   
      NC_ESA_Redim <- resample(NC_ESA, Red, method = "mode")
      
      New_Caledonia <- c(NC_ESA_Redim, Red, Green, Blue)
      names(New_Caledonia) <- c("ESA", "Red", "Green", "Blue")




##
##    Ok, that worked good. Can we stratify the sampling process?
##
   Frequency <- freq(New_Caledonia,digits=2, bylayer = TRUE)
   Land_Cover <- Frequency[Frequency$layer == 1,]
   Land_Cover$Population_Total <- sum(Land_Cover$count)
   Land_Cover$Proportion <- Land_Cover$count / Land_Cover$Population_Total

   ##
   ##    If the things that vary by Land_Cover are the red/blue/green colours, then estimate how these vary by Land_Cover
   ##
   Land_Cover_Data_NC <- data.frame(New_Caledonia)
   
   ##
   ##    Estimate the variance
   ##
      
      Variance_Colours <- with(Land_Cover_Data_NC,
                       aggregate(list(Variance_Red   = Red,
                                      Variance_Green = Green,
                                      Variance_Blue  = Blue,
                                      Average_Variance = (Red + Green + Blue)/3),
                               list(value = ESA),
                               var, 
                               na.rm = TRUE))   
      Variance_Colours
   ##
   ##    Combine it with the Land_Cover totals - because water makes up 93% of the sample, lets exclude it
   ##       when calculating the stratum sample size, and then pop it back in at the end.
   ##
      Survey_Design <- merge(Land_Cover,
                             Variance_Colours,
                             by = c("value"))
      
      Survey_Design$Weighted_Variance <- Survey_Design$Average_Variance * Survey_Design$count
      
      Survey_Design_Excl_Water <- Survey_Design[Survey_Design$value != 210,]
      Survey_Design_Excl_Water$Sample_Size <- round(((Survey_Design_Excl_Water$Weighted_Variance)/sum(Survey_Design_Excl_Water$Weighted_Variance))*1000000)
      for(i in 1:nrow(Survey_Design_Excl_Water))
      {
         Survey_Design_Excl_Water$Sample_Size[i] <- ifelse(Survey_Design_Excl_Water$Sample_Size[i] > Survey_Design_Excl_Water$count[i], Survey_Design_Excl_Water$count[i], Survey_Design_Excl_Water$Sample_Size[i])
         Survey_Design_Excl_Water$Sample_Size[i] <- ifelse(Survey_Design_Excl_Water$Sample_Size[i] < 100, 100, Survey_Design_Excl_Water$Sample_Size[i])
      }
      Survey_Design <- rbind.fill(Survey_Design_Excl_Water,
                                  Survey_Design[Survey_Design$value == 210,])
                                  
      Survey_Design$Sample_Size[Survey_Design$value == 210] <- Survey_Design$count[Survey_Design$value == 210] * 0.05
   
     sum(Survey_Design$Sample_Size)

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


