##
##    Programme:  Create_a_TimeSeries.r
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
   ##
   ##    Load some generic functions or colour palattes, depending on what you're doing.
   ##
      source("R/functions.r")
      source("R/themes.r")
   ##
   ##    Load in the Models
   ##
      load("Data_Output/model_10.rda")
      load("Data_Output/model_11.rda")
      load("Data_Output/model_12.rda")
      load("Data_Output/model_20.rda")
      load("Data_Output/model_30.rda")
      load("Data_Output/model_40.rda")
      load("Data_Output/model_50.rda")
      load("Data_Output/model_80.rda")
      load("Data_Output/model_100.rda")
      load("Data_Output/model_110.rda")
      load("Data_Output/model_120.rda")
      load("Data_Output/model_121.rda")
      load("Data_Output/model_130.rda")
      load("Data_Output/model_150.rda")
      load("Data_Output/model_160.rda")
      load("Data_Output/model_170.rda")
      load("Data_Output/model_190.rda")
      load("Data_Output/model_210.rda")

   ##
   ##    Load in the physical data
   ##
      Years <- c(2017, 2019:2024)
      Land_Cover <- lapply(Years, function(i)
                           {
                             tic(print(paste("Starting to process year ", i)))
                             
                              Red   <- rast(paste0("Data_Spatial/dep_s2_geomad_red_",   i,".tif"))
                              Green <- rast(paste0("Data_Spatial/dep_s2_geomad_green_", i,".tif"))
                              Blue  <- rast(paste0("Data_Spatial/dep_s2_geomad_blue_",  i,".tif"))


                              Wonder <- data.frame(Red_Values   = Red, 
                                                   Green_Values = Green, 
                                                   Blue_Values  = Blue)
                              names(Wonder) <- c("Red_Values", "Green_Values", "Blue_Values")
                              
                              Wonder$Predict_Is_50  <- stats::predict(model_50,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_11  <- stats::predict(model_11,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_12  <- stats::predict(model_12,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_20  <- stats::predict(model_20,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_30  <- stats::predict(model_30,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_40  <- stats::predict(model_40,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_50  <- stats::predict(model_50,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_80  <- stats::predict(model_80,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_100 <- stats::predict(model_100, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_110 <- stats::predict(model_110, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_120 <- stats::predict(model_120, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_121 <- stats::predict(model_121, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_130 <- stats::predict(model_130, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_150 <- stats::predict(model_150, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_160 <- stats::predict(model_160, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_170 <- stats::predict(model_170, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_190 <- stats::predict(model_190, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_210 <- stats::predict(model_210, newdata = Wonder, type = "response")
                              
                           ##
                           ## Find the highest probability - this is memory efficient
                           ##
                              Values <- as.matrix(Wonder[, names(Wonder)[str_detect(names(Wonder), "Predict_Is_")]])
                              namez  <- as.numeric(str_replace_all(names(Wonder)[str_detect(names(Wonder), "Predict_Is_")], "Predict_Is_",""))
                              Land_Cover <- namez[max.col(Values)]
                             toc()
                              return(data.frame(table(Land_Cover)))
                           })
                              



   ##
   ## Find the highest probability - this needs to be more memory efficient
   ##

      Wonder$Predicted_Value = NA
      
      Values <- as.matrix(Wonder[, names(Wonder)[str_detect(names(Wonder), "Predict_Is_")]])
      namez  <- as.numeric(str_replace_all(names(Wonder)[str_detect(names(Wonder), "Predict_Is_")], "Predict_Is_",""))
      
      Wonder$Predicted_Value <- apply(Values, 1, function(x){
                                        namez[which(x == max(x))]
                                        })
      Wonder$Area <- 10
      
##
##
##
   Play_Values <- Values[1:100,]
   Play_Wonder <- max.col(Play_Values)




##
##
##      
      
      
      
      
      
      
      
      Testy <- Wonder
      Testy$Predicted_Value <- sapply(1:nrow(Testy), function(x) as.numeric(Wonder$Predicted_Value[[x]][[1]]))
      Wonder <- Testy

      Estimated_Area_2017 <- with(Wonder,
                             aggregate(list(Area = Area),
                                       list(Predicted_Value = Predicted_Value),
                                     sum, 
                                     na.rm = TRUE))
      

save(Estimated_Area_2017, file = "Data_Intermediate/Estimated_Area_2017.rda")


                           
                           

st_sf(data.frame(ESA_Value       = values(crop(ESA, ext(Red[Grab_Target_Samples[x], drop=FALSE])))[1],
                                                                                                 Red_Cell_Number = Grab_Target_Samples[x],
                                                                                                 Red_Values      = Red[Grab_Target_Samples[x]][1],
                                                                                                 Green_Values    = Green[Grab_Target_Samples[x]][1],
                                                                                                 Blue_Values     = Blue[Grab_Target_Samples[x]][1]),
                                                                                                 geom            = st_as_sf(as.polygons(Red[Grab_Target_Samples[x], drop=FALSE]))$geometry)

      Red_2019   <- rast("Data_Spatial/dep_s2_geomad_red_2019.tif")
      Green_2019 <- rast("Data_Spatial/dep_s2_geomad_green_2019.tif")
      Blue_2019  <- rast("Data_Spatial/dep_s2_geomad_blue_2019.tif")

      Red_2020   <- rast("Data_Spatial/dep_s2_geomad_red_2020.tif")
      Green_2020 <- rast("Data_Spatial/dep_s2_geomad_green_2020.tif")
      Blue_2020  <- rast("Data_Spatial/dep_s2_geomad_blue_2020.tif")

      Red_2021   <- rast("Data_Spatial/dep_s2_geomad_red_2021.tif")
      Green_2021 <- rast("Data_Spatial/dep_s2_geomad_green_2021.tif")
      Blue_2021  <- rast("Data_Spatial/dep_s2_geomad_blue_2021.tif")

      Red_2022   <- rast("Data_Spatial/dep_s2_geomad_red_2022.tif")
      Green_2022 <- rast("Data_Spatial/dep_s2_geomad_green_2022.tif")
      Blue_2022  <- rast("Data_Spatial/dep_s2_geomad_blue_2022.tif")

      Red_2023   <- rast("Data_Spatial/dep_s2_geomad_red_2023.tif")
      Green_2023 <- rast("Data_Spatial/dep_s2_geomad_green_2023.tif")
      Blue_2023  <- rast("Data_Spatial/dep_s2_geomad_blue_2023.tif")

      Red_2024   <- rast("Data_Spatial/dep_s2_geomad_red_2024.tif")
      Green_2024 <- rast("Data_Spatial/dep_s2_geomad_green_2024.tif")
      Blue_2024  <- rast("Data_Spatial/dep_s2_geomad_blue_2024.tif")

      


   ##
   ##    Load in the models
   ##
      load("Data_Output/model_10.rda")
      load("Data_Output/model_11.rda")
      load("Data_Output/model_12.rda")
      load("Data_Output/model_20.rda")
      load("Data_Output/model_30.rda")
      load("Data_Output/model_40.rda")
      load("Data_Output/model_50.rda")
      load("Data_Output/model_80.rda")
      load("Data_Output/model_100.rda")
      load("Data_Output/model_110.rda")
      load("Data_Output/model_120.rda")
      load("Data_Output/model_121.rda")
      load("Data_Output/model_130.rda")
      load("Data_Output/model_150.rda")
      load("Data_Output/model_160.rda")
      load("Data_Output/model_170.rda")
      load("Data_Output/model_190.rda")
      load("Data_Output/model_210.rda")









                     
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
