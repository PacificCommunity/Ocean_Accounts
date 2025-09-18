##
##    Programme:  Logistic_Regression_to_Test.r
##
##    Objective:  The regression set has a random sample of the red/green/blue sentinel-2 data, 
##                measured at a 10 meter by 10 meter level of disaggregation. These colours are, 
##                in theory, associated with land use, but the only idea of the land use we have comes from 
##                ESA data, which is 300 meter by 300 meter resolution. So the land use values in ESA are
##                related to the 10m * 10m sentinel-2 rgb colours with error. 
##                
##                The theory is that if I draw a large enough random sample, then a logistic regression of
##                the 10 * 10 rgb values will be an unbiased estimator of the true land use associated with 
##                the specific 10 * 10 rgb values. We should be able to chop out all the "noise" associated
##                with ESA being measured at the 300m by 300m level if the sample is both big enough and
##                a random sample.
##                
##                Thats what this programme does.
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
   ##    Read in the models
   ##
      load("Data_Spatial/Test_Set.rda") 
      
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
   ## Generate categorical variables (the hard way)
   ##
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
      
      
   ##
   ## Predict the low level land use on the test set
   ##

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

   ##
   ## Find the highest probability
   ##

      Test_Set$Predicted_Value = NA
      
      Values <- as.matrix(st_drop_geometry(Test_Set[, names(Test_Set)[str_detect(names(Test_Set), "Predict_Is_")]]))
      namez  <- as.numeric(str_replace_all(names(Test_Set)[str_detect(names(Test_Set), "Predict_Is_")], "Predict_Is_",""))
      
      Test_Set$Predicted_Value <- apply(Values, 1, function(x){
                                        namez[which(x == max(x))]
                                        })
      Test_Set$Total <- 1
      
      Testy <- Test_Set
      Testy$Predicted_Value <- sapply(1:nrow(Testy), function(x) as.numeric(Test_Set$Predicted_Value[[x]][[1]]))
      Test_Set <- Testy
      
   ##
   ## Estimate closeness
   ##
   
      Estimated_Predicted <- with(st_drop_geometry(Test_Set),
                             aggregate(list(Correct   = ifelse(Predicted_Value == ESA_Value, 1,0),
                                            InCorrect = ifelse(Predicted_Value != ESA_Value, 1,0),
                                            Total = Total),
                                       list(ESA_Value = ESA_Value),
                                     sum, 
                                     na.rm = TRUE))
                   
      Test_Set[Test_Set$Observation_Number == 368,]
      sum(Estimated_Predicted$Correct)/sum(Estimated_Predicted$Total)
   
   save(Test_Set, file = "Data_Spatial/Test_Set.rda")
   save(Regression_Set, file = "Data_Spatial/Regression_Set.rda")
   
   write.table(Estimated_Predicted, file = "Data_Output/Estimated_Predicted.csv", sep=",", row.names = FALSE)
   
##
##    And we're done
##
