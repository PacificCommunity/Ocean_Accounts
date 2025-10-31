##
##    Programme:  Stars_Based_Analysis.r
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
      Red   <- rast("Data_Spatial/New_Caledonia_Rast_red_2022.tif")
      Green <- rast("Data_Spatial/New_Caledonia_Rast_green_2022.tif")
      Blue  <- rast("Data_Spatial/New_Caledonia_Rast_blue_2022.tif")
      
      ESA <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
      NC_ESA <- crop(ESA,Red)
   
      NC_ESA_Redim <- resample(ESA, Red, method = "mode")
      
      New_Caledonia <- c(NC_ESA_Redim, Red, Green, Blue)
      names(New_Caledonia) <- c("ESA", "Red", "Green", "Blue")
   ##
   ##    run a logistic model
   ##
      unique(values(New_Caledonia$ESA))
      
      New_Caledonia$Is_10  <- 0
      New_Caledonia$Is_11  <- 0
      New_Caledonia$Is_12  <- 0
      New_Caledonia$Is_20  <- 0
      New_Caledonia$Is_30  <- 0
      New_Caledonia$Is_40  <- 0
      New_Caledonia$Is_50  <- 0
      New_Caledonia$Is_80  <- 0
      New_Caledonia$Is_100 <- 0
      New_Caledonia$Is_110 <- 0
      New_Caledonia$Is_120 <- 0
      New_Caledonia$Is_121 <- 0
      New_Caledonia$Is_130 <- 0
      New_Caledonia$Is_150 <- 0
      New_Caledonia$Is_160 <- 0
      New_Caledonia$Is_170 <- 0
      New_Caledonia$Is_190 <- 0
      New_Caledonia$Is_210 <- 0

      values(New_Caledonia$"Is_10")[which(values(New_Caledonia$"ESA")  == 10)]  <- 1
      values(New_Caledonia$"Is_11")[which(values(New_Caledonia$"ESA")  == 11)]  <- 1
      values(New_Caledonia$"Is_12")[which(values(New_Caledonia$"ESA")  == 12)]  <- 1
      values(New_Caledonia$"Is_20")[which(values(New_Caledonia$"ESA")  == 20)]  <- 1
      values(New_Caledonia$"Is_30")[which(values(New_Caledonia$"ESA")  == 30)]  <- 1
      values(New_Caledonia$"Is_40")[which(values(New_Caledonia$"ESA")  == 40)]  <- 1
      values(New_Caledonia$"Is_50")[which(values(New_Caledonia$"ESA")  == 50)]  <- 1
      values(New_Caledonia$"Is_80")[which(values(New_Caledonia$"ESA")  == 80)]  <- 1
      values(New_Caledonia$"Is_100")[which(values(New_Caledonia$"ESA") == 100)] <- 1
      values(New_Caledonia$"Is_110")[which(values(New_Caledonia$"ESA") == 110)] <- 1
      values(New_Caledonia$"Is_120")[which(values(New_Caledonia$"ESA") == 120)] <- 1
      values(New_Caledonia$"Is_121")[which(values(New_Caledonia$"ESA") == 121)] <- 1
      values(New_Caledonia$"Is_130")[which(values(New_Caledonia$"ESA") == 130)] <- 1
      values(New_Caledonia$"Is_150")[which(values(New_Caledonia$"ESA") == 150)] <- 1
      values(New_Caledonia$"Is_160")[which(values(New_Caledonia$"ESA") == 160)] <- 1
      values(New_Caledonia$"Is_170")[which(values(New_Caledonia$"ESA") == 170)] <- 1
      values(New_Caledonia$"Is_190")[which(values(New_Caledonia$"ESA") == 190)] <- 1
      values(New_Caledonia$"Is_210")[which(values(New_Caledonia$"ESA") == 210)] <- 1
      
      Random_Sample <- extract(New_Caledonia, sample(1:(nrow(New_Caledonia) * ncol(New_Caledonia)), (nrow(New_Caledonia) * ncol(New_Caledonia))*0.10))

      model_10  <- glm(Is_10  ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_11  <- glm(Is_11  ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_12  <- glm(Is_12  ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_20  <- glm(Is_20  ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_30  <- glm(Is_30  ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_40  <- glm(Is_40  ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_50  <- glm(Is_50  ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_80  <- glm(Is_80  ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_100 <- glm(Is_100 ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_110 <- glm(Is_110 ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_120 <- glm(Is_120 ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_121 <- glm(Is_121 ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_130 <- glm(Is_130 ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_150 <- glm(Is_150 ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_160 <- glm(Is_160 ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_170 <- glm(Is_170 ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_190 <- glm(Is_190 ~ Red + Green + Blue, family = binomial, data = Random_Sample)
      model_210 <- glm(Is_210 ~ Red + Green + Blue, family = binomial, data = Random_Sample)

      model_10se   <- predict(New_Caledonia, model_10,  type="response", se.fit=TRUE, cores = 1)
      model_11se   <- predict(New_Caledonia, model_11,  type="response", se.fit=TRUE, cores = 1)
      model_12se   <- predict(New_Caledonia, model_12,  type="response", se.fit=TRUE, cores = 1)
      model_20se   <- predict(New_Caledonia, model_20,  type="response", se.fit=TRUE, cores = 1)
      model_30se   <- predict(New_Caledonia, model_30,  type="response", se.fit=TRUE, cores = 1)
      model_40se   <- predict(New_Caledonia, model_40,  type="response", se.fit=TRUE, cores = 1)
      model_50se   <- predict(New_Caledonia, model_50,  type="response", se.fit=TRUE, cores = 1)
      model_80se   <- predict(New_Caledonia, model_80,  type="response", se.fit=TRUE, cores = 1)
      model_100se  <- predict(New_Caledonia, model_100, type="response", se.fit=TRUE, cores = 1)
      model_110se  <- predict(New_Caledonia, model_110, type="response", se.fit=TRUE, cores = 1)
      model_120se  <- predict(New_Caledonia, model_120, type="response", se.fit=TRUE, cores = 1)
      model_121se  <- predict(New_Caledonia, model_121, type="response", se.fit=TRUE, cores = 1)
      model_130se  <- predict(New_Caledonia, model_130, type="response", se.fit=TRUE, cores = 1)
      model_150se  <- predict(New_Caledonia, model_150, type="response", se.fit=TRUE, cores = 1)
      model_160se  <- predict(New_Caledonia, model_160, type="response", se.fit=TRUE, cores = 1)
      model_170se  <- predict(New_Caledonia, model_170, type="response", se.fit=TRUE, cores = 1)
      model_190se  <- predict(New_Caledonia, model_190, type="response", se.fit=TRUE, cores = 1)
      model_210se  <- predict(New_Caledonia, model_210, type="response", se.fit=TRUE, cores = 1)


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
   
   
   
   Variance_Colours <- with(Land_Cover_Data_NC,
                    aggregate(list(Variance_Red   = Red,
                                   Variance_Green = Green,
                                   Variance_Blue  = Blue),
                            list(ESA = ESA),
                            var, 
                            na.rm = TRUE))   
   Variance_Colours


   StdDev_Colours <- with(Land_Cover_Data_NC,
                       aggregate(list(Variance_Red   = Red,
                                      Variance_Green = Green,
                                      Variance_Blue  = Blue),
                               list(ESA = ESA),
                               sd, 
                               na.rm = TRUE))   
   StdDev_Colours



   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##



nrow(New_Caledonia)

