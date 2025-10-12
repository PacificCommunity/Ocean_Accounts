##
##    Programme:  Logistic_Regression
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
   ##    Read in the ESA data
   ##
      load("Data_Spatial/Regression_Set.rda")   
      
   ##
   ## Generate categorical variables (the hard way)
   ##
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
   ## Estimate the low level land use from the regression set
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
      
   save(model_10,  file = "Data_Output/model_10.rda")
   save(model_11,  file = "Data_Output/model_11.rda")
   save(model_12,  file = "Data_Output/model_12.rda")
   save(model_20,  file = "Data_Output/model_20.rda")
   save(model_30,  file = "Data_Output/model_30.rda")
   save(model_40,  file = "Data_Output/model_40.rda")
   save(model_50,  file = "Data_Output/model_50.rda")
   save(model_80,  file = "Data_Output/model_80.rda")
   save(model_100, file = "Data_Output/model_100.rda")
   save(model_110, file = "Data_Output/model_110.rda")
   save(model_120, file = "Data_Output/model_120.rda")
   save(model_121, file = "Data_Output/model_121.rda")
   save(model_130, file = "Data_Output/model_130.rda")
   save(model_150, file = "Data_Output/model_150.rda")
   save(model_160, file = "Data_Output/model_160.rda")
   save(model_170, file = "Data_Output/model_170.rda")
   save(model_190, file = "Data_Output/model_190.rda")
   save(model_210, file = "Data_Output/model_210.rda")
   
##
##    And we're done
##
