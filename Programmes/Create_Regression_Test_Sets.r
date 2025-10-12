##
##    Programme:  Create_Regression
##
##    Objective:  This programme builds on the thinking that we can use ESA data to pre-train
##                areas using a logistic regression. This programme makes the regression and
##                test data sets using parallel processing.
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
#      describe("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
     
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
   ## Draw a sample from the Sentinel-2 data
   ##
      Sample <- sample(1:(nrow(Red)*ncol(Red)), ((nrow(Red)*ncol(Red))*.2))

   ##
   ##    Try with parallel processing
   ##         
      Size_of_Loops <- ceiling(length(Sample) / 10)
      
      cl <- makeCluster(13)
#      cl <- makeCluster(detectCores())
      
      clusterEvalQ(cl, { c(library(terra), library(sf)) }) 
      ##
      ##    Load up the clusters with data
      ##
      Load_Me <- function(){
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
      }
   clusterExport(cl, c("Load_Me")) 
#   system.time(parallel::clusterCall(cl, Load_Me))

      for(Parcycle in 0:9)
        {
        
          Grab_Target_Samples <- Sample[(Parcycle*Size_of_Loops + 1):min(((Parcycle+1)*Size_of_Loops),  length(Sample))]

          Peg_Me <- function(Fistful)
               {
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
            sp <- parallel::clusterSplit(cl, 1:length(Grab_Target_Samples))
            clusterExport(cl, c("sp", "Grab_Target_Samples", "Peg_Me"))  # each worker is a new environment, you will need to export variables/functions to
         
          tic(print(paste("Starting to process loop", Parcycle)))
            system.time(ll <- parallel::parLapplyLB(cl, sp, Peg_Me))
            New_Parcels <- do.call(rbind, ll)
         
            assign(paste0("Regression_SetXX", Parcycle),  New_Parcels)
            save(list = paste0("Regression_SetXX", Parcycle), 
                 file = paste0("Parallel/Regression_SetXX", Parcycle,".rda"))                              
            rm(list = paste0("Regression_SetXX",Parcycle))
          toc()
         }

      stopCluster(cl)
    
##
##    Collect them all back up
##   
      Contents <- as.data.frame(list.files(path = "Parallel/",  pattern = "*.rda"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents <- Contents[str_detect(Contents$DataFrames, "Regression_SetXX"),]

      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Parallel/", File))  
                           X <- get(str_split_fixed(File, "\\.", n = 2)[,1])
                           rm(list=c(as.character(File) ))
                           return(X)})
      Regression_Set <- do.call(rbind, All_Data)
      Regression_Set <- st_as_sf(Regression_Set)
                  
      names(Regression_Set)[names(Regression_Set) == "dep_s2_geomad_red_2017"]   = "Red_Values"
      names(Regression_Set)[names(Regression_Set) == "dep_s2_geomad_green_2017"] = "Green_Values"
      names(Regression_Set)[names(Regression_Set) == "dep_s2_geomad_blue_2017"]  = "Blue_Values"
      
      Regression_Set <- Regression_Set[!is.nan(Regression_Set$Blue_Values),]
      Regression_Set <- Regression_Set[!is.nan(Regression_Set$Green_Values),]
      Regression_Set <- Regression_Set[!is.nan(Regression_Set$Red_Values),]
      save(Regression_Set, file = "Data_Spatial/Regression_Set.rda")
      load("Data_Spatial/Regression_Set.rda")

   ##
   ## Step 3: Pull out a test set with parallel processing
   ##
      Not_Regressed <- (1:(nrow(Red)*ncol(Red)))[-unique(Regression_Set$Red_Cell_Number)]

      Sample <- sample(Not_Regressed, length(Not_Regressed) * .2)
      
      Size_of_Loops <- ceiling(length(Sample) / 10)
      
      #cl <- makeCluster(detectCores())
      cl <- makeCluster(13)
      
      clusterEvalQ(cl, { c(library(terra), library(sf)) }) 


      for(Parcycle in 0:9)
        {
        
          Grab_Target_Samples <- Sample[(Parcycle*Size_of_Loops + 1):min(((Parcycle+1)*Size_of_Loops),  length(Sample))]

          Peg_Me <- function(Fistful)
               {
               
                  if(!exists("ESA")){ 
                     ESA <- rast("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
                     }
                     
                  if(!exists("Red")){ 
                     Red    <- rast("Data_Spatial/dep_s2_geomad_red_2017.tif")
                     Red    <- project(Red,   crs(ESA))
                     }
                  if(!exists("Green")){ 
                     Green  <- rast("Data_Spatial/dep_s2_geomad_green_2017.tif")
                     Green  <- project(Green, crs(ESA))
                     }
                     
                  if(!exists("Blue")){ 
                     Blue   <- rast("Data_Spatial/dep_s2_geomad_blue_2017.tif")
                     Blue   <- project(Blue,  crs(ESA))
                     }
                     
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
            sp <- parallel::clusterSplit(cl, 1:length(Grab_Target_Samples))
            clusterExport(cl, c("sp", "Grab_Target_Samples", "Peg_Me"))  # each worker is a new environment, you will need to export variables/functions to
         
          tic(print(paste("Starting to process loop", Parcycle)))
            system.time(ll <- parallel::parLapplyLB(cl, sp, Peg_Me))
            New_Parcels <- do.call(rbind, ll)
         
            assign(paste0("Test_SetXX", Parcycle),  New_Parcels)
            save(list = paste0("Test_SetXX", Parcycle), 
                 file = paste0("Parallel/Test_SetXX", Parcycle,".rda"))                              
            rm(list = paste0("Test_SetXX",Parcycle))
          toc()
         }

      stopCluster(cl)
    
##
##    Collect them all back up
##   
      Contents <- as.data.frame(list.files(path = "Parallel/",  pattern = "*.rda"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents <- Contents[str_detect(Contents$DataFrames, "Test_SetXX"),]

      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Parallel/", File))  
                           X <- get(str_split_fixed(File, "\\.", n = 2)[,1])
                           rm(list=c(as.character(File) ))
                           return(X)})
      Test_Set <- do.call(rbind, All_Data)
      Test_Set <- st_as_sf(Test_Set)
                  
      names(Test_Set)[names(Test_Set) == "dep_s2_geomad_red_2017"]   = "Red_Values"
      names(Test_Set)[names(Test_Set) == "dep_s2_geomad_green_2017"] = "Green_Values"
      names(Test_Set)[names(Test_Set) == "dep_s2_geomad_blue_2017"]  = "Blue_Values"
      
      Test_Set <- Test_Set[!is.nan(Test_Set$Blue_Values),]
      Test_Set <- Test_Set[!is.nan(Test_Set$Green_Values),]
      Test_Set <- Test_Set[!is.nan(Test_Set$Red_Values),]
      save(Test_Set, file = "Data_Spatial/Test_Set.rda")

      load("Data_Spatial/Regression_Set.rda")
      load("Data_Spatial/Test_Set.rda")

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
   ## Reorder the test set
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
      
      
      Values <- as.matrix(st_drop_geometry(Test_Set[, names(Test_Set)[str_detect(names(Test_Set), "Predict_Is_")]]))
      namez  <- as.numeric(str_replace_all(names(Test_Set)[str_detect(names(Test_Set), "Predict_Is_")], "Predict_Is_",""))
      
      Test_Set$Predicted_Value <- apply(Values, 1, function(x){
                                        namez[which(x == max(x))]
                                        })
      Test_Set$Total <- 1
      
      Testy <- Test_Set
      Testy$Predicted_Value <- sapply(1:nrow(Testy), function(x) as.numeric(Test_Set$Predicted_Value[[x]][[1]]))
      Test_Set <- Testy
      
      Estimated_Predicted <- with(st_drop_geometry(Test_Set),
                             aggregate(list(Correct   = ifelse(Predicted_Value == ESA_Value, 1,0),
                                            InCorrect = ifelse(Predicted_Value != ESA_Value, 1,0),
                                            Total = Total),
                                       list(ESA_Value = ESA_Value),
                                     sum, 
                                     na.rm = TRUE))
                   
      Test_Set[Test_Set$Observation_Number == 368,]
      sum(Estimated_Predicted$Correct)/sum(Estimated_Predicted$Total)

##
##    Save stuff
##
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
   save(Test_Set, file = "Data_Intermediate/Test_Set.rda")
   save(Regression_Set, file = "Data_Intermediate/Regression_Set.rda")
   write.table(Estimated_Predicted, file = "Data_Output/Estimated_Predicted.csv", sep=",", row.names = FALSE)



   write.table(head(Test_Set[Test_Set$Observation_Number == 368,]), file = "Data_Output/Example_Final_Predictions.csv", sep=",", row.names = FALSE)
   

load("Data_Intermediate/Regression_Set.rda")   
load("Data_Intermediate/Test_Set.rda")   
load("Data_Output/Test_Set.rda")   

plot(Test_Set[Test_Set$ESA_Value == 190,])





One_Cell    <- ESA[34350, drop=FALSE]
Red_Cells   <- crop(Red,   ext(One_Cell))
Green_Cells <- crop(Green, ext(One_Cell))
Blue_Cells  <- crop(Blue,  ext(One_Cell)) 
   
   
png(filename = "Graphical_Output/One_ESA_Urban_Cell.png", bg = "transparent", height =(1.0*16.13), width = (1.0*20.66), res = 600, units = "cm")
   plot(ESA, main="One ESA Cell\n")
   points(as.polygons(ESA[34350, drop=FALSE]), col="red", add=TRUE)
dev.off()

##
##    Find urban
##
Urban <- Test_Set[Test_Set$Predicted_Value == 190,]

One_Cell    <- ESA[34350, drop=FALSE]
Red_Cells   <- crop(Red,   ext(One_Cell))
Green_Cells <- crop(Green, ext(One_Cell))
Blue_Cells  <- crop(Blue,  ext(One_Cell)) 
 

 
plot(ESA, main="Big Picture")
plot(as.points(Test_Set[Test_Set$ESA_Value == 190,1], col="red", add=TRUE)


for(i in 1:(31*32))
{
   points(as.polygons(Red_Cells[i, drop=FALSE]), col="red", pch=16, cex=1, add=TRUE)
}

##
##    Zoom in
##

plot(ESA, main="Zoom In",xlim=c(166.4, 166.5),ylim=c(-22.225, -22.32))
plot(Test_Set[Test_Set$ESA_Value == 50,1], col="red", add=TRUE)



for(i in 1:(31*32))
{
   points(as.polygons(Red_Cells[i, drop=FALSE]), col="red", pch=16, cex=.1, add=TRUE)
}



##
##    Flip it around, and look at the predicted
##

plot(ESA, main="Zoom In",xlim=c(166.4, 166.5),ylim=c(-22.225, -22.3))

for(i in 1:(31*32))
{
   points(as.polygons(Red_Cells[i, drop=FALSE]), col="red", pch=16, cex=.1, add=TRUE)
}







Red    <- project(Red,   crs(ESA))
Green  <- project(Green, crs(ESA))
Blue   <- project(Blue,  crs(ESA))




One_Cell    <- ESA[34350, drop=FALSE]
Red_Cells   <- intersect(Red,   ext(ESA[34350, drop=FALSE]))
Green_Cells <- intersect(Green, ext(ESA[34350, drop=FALSE]))
Blue_Cells  <- intersect(Blue,  ext(ESA[34350, drop=FALSE])) 

X <- st_as_sf(as.polygons(Red_Cells))
X$Green_Values = values(Green_Cells)
X$Blue_Values  = values(Blue_Cells)
              
X$ESA_Value          <- rep(values(One_Cell), nrow(X))
X$Observation_Number <- 34350


 
 
 

 X <- st_as_sf(as.polygons(Red_Cells))
 X <- st_as_sf(as.polygons(Green_Cells))
 X <- st_as_sf(as.polygons(Blue_Cells))
           
           
length(   values(Green_Cells))
length(   values(Red_Cells))
length(   values(Blue_Cells))


Wonder <- do.call(rbind,lapply(1:10, function(x){
                                 
                                 return(st_sf(data.frame(ESA_Value = values(crop(ESA, ext(X)))[1],
                                                       Red_Cell_Number = x,
                                                       Red_Values   = Red[x][1],
                                                       Green_Values = Green[x][1],
                                                       Blue_Values  = Blue[x][1]),
                                          geom = st_as_sf(as.polygons(Red[x, drop=FALSE]))$geometry))

                              })
                  )
        
           