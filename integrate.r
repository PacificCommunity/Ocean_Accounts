##
##    Programme:  Oceans Accounts.r
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
##    Hot Tips:   (1)   I trained the regression dataset on the full raster including the sea - I got a lot of observations of the sea.
##    for next:         Next time, subset the rasters on the Pacific Coastline dataset from the Pacific Data Hub so that all the samples
##    time    :         are of land.
##                
##                (2)   Spend a lot more effort cutting out the land component of the data. The next iteration of this programme is going 
##                      to bring all of the Pacific land together and sample from that, rather than just train on a subset of New Caledonia
##                      and mostly sampling the ocean.
##                
##                
##                
##                
##                
##                
##                
##                
##                
##                
##                
##                
##                
##                
##                
##                
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
      options(max.print=999999)
      options(scipen = 100)

   ##
   ##    Core libraries
   ##
      library(ggplot2)
      library(plyr)
      library(stringr)
      library(reshape2)
      library(lubridate)
      library(calibrate)
      library(Hmisc)
      library(RColorBrewer)
      library(stringi)
      library(sqldf)
      library(extrafont)
      library(scales)
      library(RDCOMClient)
      library(extrafont)
      library(tictoc)   
      
      library(sysfonts)
      library(showtext)

   ##
   ##    Special Libraries
   ##      
      library(tensorflow)
      library(GPUmatrix)
      library(Matrix)
      library(keras3)
      library(terra)
      library(raster)
      library(sf)
      library(parallel)   
      library(stats)   
      library(MASS)

   ##
   ##    Set working directory
   ##
      setwd("c:\\Git_Projects\\Ocean_Accounts")
      setwd("C:\\From BigDisk\\GIT\\Ocean_Accounts")

      # From: James Hogan 
      # Sent: Tuesday, 9 September 2025 5:39 pm
      # To: Philippine Laroche <philippinel@spc.int>; Sachindra Singh <sachindras@spc.int>
      # Subject: RE: DEP - Land Use Models

      # I got it! 😊

      # Here’s a couple of pieces of code. The first, DEP_Play_Code_Version2.py is a little piece of python that goes to the Sentinel-2 data and pulls out the south province of New Caledonia (ish), in red, green and blue. It saves it to a Data_Spatial folder in R.

      # ESA data came from here: https://maps.elie.ucl.ac.be/CCI/viewer/download.php  and is their ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif file which is their world land use for 2015

      # The second programme is in R. It:
      # 1.	Reads in the ESA and New Caledonia raster files, and crops the world ESA file to New Caledonia data extents.
      # 2.	Takes a 10% random sample of ESA data values at the 300*300m dimension to estimate a regression dataset
      # 3.	Another separate 10 random sample of ESA data is extracted for a test dataset.
      # 4.	The code then extracts the underlying 10*10 m cel values for the red/green/blue layers from the Sentinel-2 data for each 300*300 ESA value and returns the low-level Sentinel-2 colour data, together with the high level ESA land use value. There’s approximately about 990 Sentinel-2 observations for each ESA observation.
      # 5.	The code then generates a number of dummy variables on each of different the ESA land use values. In the random sample, there were 16 different land use values (50, 150,  30, 210, 120,  40, 170, 160, 190,  10,  11,  20, 110, 100,  12,  80) representing: 

       
      # 6.	Separate logistic regressions were estimated for each of the land use dummy variables against the red/green/blue sentinel colours:
      # glm(Is_50  ~ Red_Values + Green_Values + Blue_Values, family = binomial, data = Regression_Set)

      # 16 regressions were estimated separately using the regression_set data.
      # 7.	In the separate Test_Set, dummy variables were data were estimated as in the regression set.
      # 8.	The different logistic models were applied to data in the Test_Set to estimate the probability that the test_set green/red/blue values created a Land Use value of the type estimated by the different models. 
      # 9.	The outcome of step 8 is a range of probabilities from the logistic regression models that the Sentinel-2 data had a specific land use value.
      # 10.	The highest probability for each of the predicted land value from the individual regressions were chosen as the most likely land use value for the sentinel-2 green/blue/red 10m*10m cell.

      # To test the accuracy of the model:
      # 1.	The number of predicted land-use value for each of the individual Sentinel-2 cells was compared to the actual ESA land use value at the ESA raster observation level. If the number of the Sentinel-2 cell equalled the actual ESA land use value, then a variable called “Correct” was given a value of 1. 
      # 2.	If the number of the predicted land-use value for each of the individual Sentinel-2 cells did not equal the ESA value, then a variable called “Incorrect” was given a value of 1.
      # 3.	The number of correct and incorrect sentinel-data based predictions was then summed up for each ESA raster observation number
      # 4.	70.14% of the Sentinel-2 cell estimates were correctly predicted

      # Waddyathink?

      # At this level, we can now generate a training set for your models Philippine


      # Regards,
      # James

   ##
   ##    Run "Programmes/DEP_Play_Code_Version2.py" in Python.
   ##    It will download some code shape files into the Data_Spatial directory
   ##
#      source("Programmes/ESA and Sentinel Data_Version3.r") # This was the prototype programme for the regression analysis

#      source("Programmes/Draw_Samples.r")         # This draws two mutually exclusive random samples of the Sentinel-2 data for regression and testing
      source("Programmes/Draw_Samples_Version2.r") # This draws two mutually exclusive random samples of the Sentinel-2 data for regression and testing, but now uses the Pacific Coastline data to exclude the sea
      
      source("Programmes/Create_Test_Set.r")       # This parallel processes a sample of sentinel-2 data against the ESA data to identify potential ESA land use for testing the model.
      source("Programmes/Create_Regression_Set.r") # This parallel processes a sample of sentinel-2 data against the ESA data to identify potential ESA land use for regression.
   
      source("Programmes/Logistic_Regression.r")           # This estimates the Sentinel-2 RGB values associated with ESA land use. It estimates using the regression set, and tests on the test data.
      source("Programmes/Logistic_Regression_to_Test.r")   # This applies the Logistic_Regression models to the test data. I separated this from the above programme because I split the regression
                                                           # and test dataset creation across machines. So the machine that makes the regression data can carry on and estimate the models while the 
                                                           # machine that makes the test data can finish and then run this programme.
      source("Programmes/Explore_Logistic_Results.r")      # Creates a picture of land use using the test data

      source("Programmes/Create_a_TimeSeries.r")           # Apply the logistic regression models to timeseries of land to estimate area.



