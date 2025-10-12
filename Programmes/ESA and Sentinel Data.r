##
##    Programme:  <SAMPLE_PROGRAMME.r>
##
##    Objective:  What is this programme designed to do?
##                That's a very interesting approach. I'll be very curious to hear your results, whether good or bad.
##                
##                Good luck, 
##                Philippine
##                ________________________________________
##                From: James Hogan <jamesh@spc.int>
##                Sent: Monday, 8 September 2025 14:09
##                To: Philippine Laroche <philippinel@spc.int>; Sachindra Singh <sachindras@spc.int>
##                Subject: RE: DEP - Land Use Models 
##                 
##                Regarding the ESA product in the last link, the aim of my work is to develop something more tailored 
##                to our region and at a higher resolution. The ESA product is designed at a global scale, so it doesn’t 
##                capture the specific characteristics of the Pacific.
##                 
##                Nope, it doesn’t – not automatically.  
##                 
##                But if you think of the two data sources as a regression where the 300m ESA is an aggregation of the 
##                underlying 10m Sentinel data with error, then a nice big random sample of the ESA data for pacific 
##                countries, regressed against the underlying Sentinel data should create an unbiased estimate of the 
##                underlying sentinel data that equates to the ESA groups.
##                 
##                ESA = a + b * Sentinel + error
##                 
##                I’ll see if I can make that work… 
##                 
##                I might be completely wrong and it might not 😊
##                
##                 
##                James
##                 
##                 
##                –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––-
##                 
##                James Hogan
##                Senior Marine Resource Economist
##                Économiste principal des ressources marines
##                Pacific Community | Communauté du Pacifique
##                CPS – B.P. D5 | 98848 Noumea, New Caledonia | Nouméa, Nouvelle-Calédonie
##                Tel: (+64) 275 997 999
##                E: jamesh@spc.int | Website | Twitter | LinkedIn | Facebook | YouTube | Instagram
##                
##                
##                –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––-
##                As part of our emissions reduction strategy, please only print this email if necessary
##                Dans le cadre de notre stratégie de réduction des émissions, merci d'imprimer cet e-mail uniquement si nécessaire
##                 
##                
##                From: Philippine Laroche <philippinel@spc.int> 
##                Sent: Monday, 8 September 2025 2:57 pm
##                To: James Hogan <jamesh@spc.int>; Sachindra Singh <sachindras@spc.int>
##                Subject: Re: DEP - Land Use Models
##                 
##                I had a quick look at the links. For the first two, it seems the available labels are quite limited. I only 
##                see buildings and vegetation, whereas we would also need croplands, mangroves, bare soil, etc., to ensure a 
##                representative land cover classification.
##                Regarding the ESA product in the last link, the aim of my work is to develop something more tailored to our 
##                region and at a higher resolution. The ESA product is designed at a global scale, so it doesn’t capture the 
##                specific characteristics of the Pacific.
##                You can find attached, the notebook I used and an geopackage file with label points. At the beginning you can 
##                choose the Area of Interest (AOI), select the training area over fiji to match the gpkg file : # over Viti Levu, 
##                Fiji : left=177.2, bottom=-18.3, right=178.8, top=-17.2.  Then you can run the notebook, until the part "for later maybe".
##                I also have gpkg for marshall, cook and palau. My final training dataset is a mixed of the four islands.
##                 
##                Kind regards, 
##                Philippine
##                ________________________________________
##                From: James Hogan <jamesh@spc.int>
##                Sent: Monday, 8 September 2025 12:56
##                To: Philippine Laroche <philippinel@spc.int>; Sachindra Singh <sachindras@spc.int>
##                Subject: RE: DEP - Land Use Models 
##                 
##                Great – thanks Philippine 😊 Sharing the notebook would be great.
##                 
##                I was thinking the same thing around how to creating a training set for how the Sentinel-2 data relates to land use. 
##                 
##                My thinking was overlaying the raster data with information from here: https://pacific-data.sprep.org/search?f%5B0%5D=content_type%3Adataset.dataset&f%5B1%5D=format%3Ageojson. 
##                 
##                Like this: https://pacific-data.sprep.org/dataset/grassland-vegetationnauru to carve out 
##                Or from here: https://maps.elie.ucl.ac.be/CCI/viewer/
##                 
##                It means, we’re training on their version of the truth, but it should be enough to cut a training set of spatial 
##                areas and labels for training on the Sentinel-2 data.
##                 
##                Yeah? Nah?
##                 
##                 
##                Best regards,
##                James
##                
##                
##                
##
##    Author:     James Hogan, Senior Marine Resource Economist, 8 September 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))

   ##
   ##    Read in the ESA data
   ##
      ESA   <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
      describe("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
     
   ##
   ##    Read in the Sentinel-2 data
   ##
   ##       Define Coordinates, in lat, lon
   ##       lower_left = (-22.351142, 166.173569)
   ##       upper_right = (-21.895303, 166.932652)
   ##

      Red   <- rast("Data_Spatial/dep_s2_geomad_red_2017.tif")
      Green <- rast("Data_Spatial/dep_s2_geomad_green_2017.tif")
      Blue  <- rast("Data_Spatial/dep_s2_geomad_blue_2017.tif")
      describe("Data_Spatial/dep_s2_geomad_red_2017.tif")

   ##
   ##    Move the sentinel data to the same co-ordinate reference system
   ##
      Red    <- project(Red,   crs(ESA))
      Green  <- project(Green, crs(ESA))
      Blue   <- project(Blue,  crs(ESA))

   ##
   ##    Crop the ESA data to the Sentinel-2 extract
   ##
      Extent <- ext(Red)
      ESA <- crop(ESA, Extent)
      png(filename = "Graphical_Output/ESA_Land_Use_Example.png", bg = "transparent", height =(1.0*16.13), width = (1.0*20.66), res = 600, units = "cm")
         plot(ESA)
      dev.off()
      
      png(filename = "Graphical_Output/Sentinel-2_Example.png", bg = "transparent", height =(1.0*16.13), width = (1.0*20.66), res = 600, units = "cm")
         plot(Red)
      dev.off()
         
   ##
   ## Draw a sample from ESA - start with one
   ##
   ##    YESSS!!! This has taken me 2 hours!!
   ##
      Regression_Set <- lapply(sample(1:(nrow(ESA)*ncol(ESA)), (nrow(ESA)*ncol(ESA))*.5), function(x){
      
                               One_Cell    <- ESA[x, drop=FALSE]
                               Red_Cells   <- crop(Red,   ext(One_Cell))
                               Green_Cells <- crop(Green, ext(One_Cell))
                               Blue_Cells  <- crop(Blue,  ext(One_Cell)) 
                              
                               X <- data.frame(Red_Values   = values(Red_Cells),
                                               Green_Values = values(Green_Cells),
                                               Blue_Values  = values(Blue_Cells))
                                               
                               X$ESA_Value          <- rep(values(One_Cell), nrow(X))
                               X$Observation_Number <- x
                               
                               return(X)
                              })
      Regression_Set <- do.call(rbind, Regression_Set)
      
      names(Regression_Set)[names(Regression_Set) == "dep_s2_geomad_red_2017"]   = "Red_Values"
      names(Regression_Set)[names(Regression_Set) == "dep_s2_geomad_green_2017"] = "Green_Values"
      names(Regression_Set)[names(Regression_Set) == "dep_s2_geomad_blue_2017"]  = "Blue_Values"
      
      Regression_Set <- Regression_Set[!is.nan(Regression_Set$Blue_Values),]
      Regression_Set <- Regression_Set[!is.nan(Regression_Set$Green_Values),]
      Regression_Set <- Regression_Set[!is.nan(Regression_Set$Red_Values),]
      length(unique(Regression_Set$ESA_Value))

   ##
   ## Reorder the regression set and generate categorical variables (the hard way)
   ##
      Regression_Set <- Regression_Set[sample(1:nrow(Regression_Set), nrow(Regression_Set)),]
      Regression_Set$Is_10  <- ifelse(Regression_Set$ESA_Value ==  10, 1,0)
      Regression_Set$Is_11  <- ifelse(Regression_Set$ESA_Value ==  11, 1,0)
      Regression_Set$Is_12  <- ifelse(Regression_Set$ESA_Value ==  12, 1,0)
      Regression_Set$Is_20  <- ifelse(Regression_Set$ESA_Value ==  20, 1,0)
      Regression_Set$Is_30  <- ifelse(Regression_Set$ESA_Value ==  30, 1,0)
      Regression_Set$Is_40  <- ifelse(Regression_Set$ESA_Value ==  40, 1,0)
      Regression_Set$Is_50  <- ifelse(Regression_Set$ESA_Value ==  50, 1,0)
      Regression_Set$Is_80  <- ifelse(Regression_Set$ESA_Value ==  80, 1,0)
      Regression_Set$Is_100 <- ifelse(Regression_Set$ESA_Value == 100, 1,0)
      Regression_Set$Is_110 <- ifelse(Regression_Set$ESA_Value == 110, 1,0)
      Regression_Set$Is_120 <- ifelse(Regression_Set$ESA_Value == 120, 1,0)
      Regression_Set$Is_121 <- ifelse(Regression_Set$ESA_Value == 121, 1,0)
      Regression_Set$Is_130 <- ifelse(Regression_Set$ESA_Value == 130, 1,0)
      Regression_Set$Is_150 <- ifelse(Regression_Set$ESA_Value == 150, 1,0)
      Regression_Set$Is_160 <- ifelse(Regression_Set$ESA_Value == 160, 1,0)
      Regression_Set$Is_170 <- ifelse(Regression_Set$ESA_Value == 170, 1,0)
      Regression_Set$Is_190 <- ifelse(Regression_Set$ESA_Value == 190, 1,0)
      Regression_Set$Is_210 <- ifelse(Regression_Set$ESA_Value == 210, 1,0)
      
   ##
   ## Step 2: Run a regression
   ##
      model_10  <- glm(Is_10  ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_11  <- glm(Is_11  ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_12  <- glm(Is_12  ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_20  <- glm(Is_20  ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_30  <- glm(Is_30  ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_40  <- glm(Is_40  ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_50  <- glm(Is_50  ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_80  <- glm(Is_80  ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_100 <- glm(Is_100 ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_110 <- glm(Is_110 ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_120 <- glm(Is_120 ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_121 <- glm(Is_121 ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_130 <- glm(Is_130 ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_150 <- glm(Is_150 ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_160 <- glm(Is_160 ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_170 <- glm(Is_170 ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_190 <- glm(Is_190 ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      model_210 <- glm(Is_210 ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)
      

   ##
   ## Step 3: Pull out a test set
   ##
      Not_Regressed <- (1:(nrow(ESA)*ncol(ESA)))[-unique(Regression_Set$Observation_Number)]
      
      Test_Set <- lapply(sample(Not_Regressed, length(Not_Regressed) *.2), function(x){
      
                               One_Cell    <- ESA[x, drop=FALSE]
                               Red_Cells   <- crop(Red,   ext(One_Cell))
                               Green_Cells <- crop(Green, ext(One_Cell))
                               Blue_Cells  <- crop(Blue,  ext(One_Cell)) 
                              
                               X <- data.frame(Red_Values   = values(Red_Cells),
                                               Green_Values = values(Green_Cells),
                                               Blue_Values  = values(Blue_Cells))
                                               
                               X$ESA_Value          <- rep(values(One_Cell), nrow(X))
                               X$Observation_Number <- x
                               
                               return(X)
                              })
      Test_Set <- do.call(rbind, Test_Set)
      Test_Set$ESA_Value_Factor <- as.factor(Test_Set$ESA_Value)
      
      names(Test_Set)[names(Test_Set) == "dep_s2_geomad_red_2017"]   = "Red_Values"
      names(Test_Set)[names(Test_Set) == "dep_s2_geomad_green_2017"] = "Green_Values"
      names(Test_Set)[names(Test_Set) == "dep_s2_geomad_blue_2017"]  = "Blue_Values"
      
      Test_Set <- Test_Set[!is.nan(Test_Set$Blue_Values),]
      Test_Set <- Test_Set[!is.nan(Test_Set$Green_Values),]
      Test_Set <- Test_Set[!is.nan(Test_Set$Red_Values),]
      

   ##
   ## Reorder the test set
   ##
      Test_Set <- Test_Set[sample(1:nrow(Test_Set), nrow(Test_Set)),]
      Test_Set$Is_10  <- ifelse(Test_Set$ESA_Value ==  10, 1,0)
      Test_Set$Is_11  <- ifelse(Test_Set$ESA_Value ==  11, 1,0)
      Test_Set$Is_12  <- ifelse(Test_Set$ESA_Value ==  12, 1,0)
      Test_Set$Is_20  <- ifelse(Test_Set$ESA_Value ==  20, 1,0)
      Test_Set$Is_30  <- ifelse(Test_Set$ESA_Value ==  30, 1,0)
      Test_Set$Is_40  <- ifelse(Test_Set$ESA_Value ==  40, 1,0)
      Test_Set$Is_50  <- ifelse(Test_Set$ESA_Value ==  50, 1,0)
      Test_Set$Is_80  <- ifelse(Test_Set$ESA_Value ==  80, 1,0)
      Test_Set$Is_100 <- ifelse(Test_Set$ESA_Value == 100, 1,0)
      Test_Set$Is_110 <- ifelse(Test_Set$ESA_Value == 110, 1,0)
      Test_Set$Is_120 <- ifelse(Test_Set$ESA_Value == 120, 1,0)
      Test_Set$Is_121 <- ifelse(Test_Set$ESA_Value == 121, 1,0)
      Test_Set$Is_130 <- ifelse(Test_Set$ESA_Value == 130, 1,0)
      Test_Set$Is_150 <- ifelse(Test_Set$ESA_Value == 150, 1,0)
      Test_Set$Is_160 <- ifelse(Test_Set$ESA_Value == 160, 1,0)
      Test_Set$Is_170 <- ifelse(Test_Set$ESA_Value == 170, 1,0)
      Test_Set$Is_190 <- ifelse(Test_Set$ESA_Value == 190, 1,0)
      Test_Set$Is_210 <- ifelse(Test_Set$ESA_Value == 210, 1,0)
      

      Test_Set$Predict_Is_50  <- stats::predict(model_50,  newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_11  <- stats::predict(model_11,  newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_12  <- stats::predict(model_12,  newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_20  <- stats::predict(model_20,  newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_30  <- stats::predict(model_30,  newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_40  <- stats::predict(model_40,  newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_50  <- stats::predict(model_50,  newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_80  <- stats::predict(model_80,  newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_100 <- stats::predict(model_100, newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_110 <- stats::predict(model_110, newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_120 <- stats::predict(model_120, newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_121 <- stats::predict(model_121, newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_130 <- stats::predict(model_130, newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_150 <- stats::predict(model_150, newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_160 <- stats::predict(model_160, newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_170 <- stats::predict(model_170, newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_190 <- stats::predict(model_190, newdata = Test_Set, type = "response")
      Test_Set$Predict_Is_210 <- stats::predict(model_210, newdata = Test_Set, type = "response")

      Test_Set$Predicted_Value = NA
      
      
      Values <- as.matrix(Test_Set[, names(Test_Set)[str_detect(names(Test_Set), "Predict_Is_")]])
      namez  <- as.numeric(str_replace_all(names(Test_Set)[str_detect(names(Test_Set), "Predict_Is_")], "Predict_Is_",""))
      
      Test_Set$Predicted_Value <- apply(Values, 1, function(x){
                                        namez[which(x == max(x))]
                                        })
      Test_Set$Total <- 1
      
      Estimated_Predicted <- with(Test_Set,
                             aggregate(list(Correct   = ifelse(Predicted_Value == ESA_Value, 1,0),
                                            InCorrect = ifelse(Predicted_Value != ESA_Value, 1,0),
                                            Total = Total),
                                       list(Observation_Number = Observation_Number,
                                            ESA_Value = ESA_Value),
                                     sum, 
                                     na.rm = TRUE))
                   
      Test_Set[Test_Set$Observation_Number == 368,]
      sum(Estimated_Predicted$Correct)/sum(Estimated_Predicted$Total)

##
##    Save stuff
##
   save(Test_Set, file = "Data_Intermediate/Test_Set.rda")
   save(Regression_Set, file = "Data_Intermediate/Regression_Set.rda")
   write.table(Estimated_Predicted, file = "Data_Output/Estimated_Predicted.csv", sep=",", row.names = FALSE)
   write.table(head(Test_Set[Test_Set$Observation_Number == 368,]), file = "Data_Output/Example_Final_Predictions.csv", sep=",", row.names = FALSE)
   
   

plot(ESA[368])





One_Cell    <- ESA[34350, drop=FALSE]
Red_Cells   <- crop(Red,   ext(One_Cell))
Green_Cells <- crop(Green, ext(One_Cell))
Blue_Cells  <- crop(Blue,  ext(One_Cell)) 
   
sapply(as.polygons(Red_Cells)
  
   
   
plot(ESA, main="Overlay Example")
points(as.polygons(ESA[34350, drop=FALSE]), col="red", pch=16, cex=1, add=TRUE)

##
##    Find urban
##
Urban <- Test_Set[Test_Set$Predicted_Value == 190,]



One_Cell    <- ESA[34350, drop=FALSE]
Red_Cells   <- crop(Red,   ext(One_Cell))
Green_Cells <- crop(Green, ext(One_Cell))
Blue_Cells  <- crop(Blue,  ext(One_Cell)) 
   
plot(ESA, main="Big Picture")
for(i in 1:(31*32))
{
   points(as.polygons(Red_Cells[i, drop=FALSE]), col="red", pch=16, cex=1, add=TRUE)
}

##
##    Zoom in
##
lower_left = c(-22.351142, 166.173569)
upper_right = c(-21.895303, 166.932652)

bbox = c(lower_left[1], upper_right[1], lower_left[2], upper_right[2])

Zoom_In <- crop(ESA, ext(bbox))


plot(Zoom_In, main="Zoom In")
for(i in 1:(31*32))
{
   points(as.polygons(Red_Cells[i, drop=FALSE]), col="red", pch=16, cex=.1, add=TRUE)
}


   ##       Define Coordinates, in lat, lon
   ##       lower_left = (-22.351142, 166.173569)
   ##       upper_right = (-21.895303, 166.932652)


