##
##    Programme:  Experiments_in_Tiff.r
##
##    Objective:  Builds on output derived from Dep_Play_Code_Version2.py
##
##                GeoMAD is the Digital Earth Africa (DE Africa) surface reflectance geomedian and triple 
##                Median Absolute Deviation data service. It is a cloud-free composite of satellite data compiled over specific timeframes. 
##                The geomedian component combines measurements collected over the specified timeframe to produce one representative, 
##                multispectral measurement for every pixel unit of the African continent. The end result is a comprehensive dataset 
##                that can be used to generate true-colour images for visual inspection of anthropogenic or natural landmarks. The full 
##                spectral dataset can be used to develop more complex algorithms. 
##
##                For each pixel, invalid data is discarded, and remaining 
##                observations are mathematically summarised using the geomedian statistic. Flyover coverage provided by collecting data 
##                over a period of time also helps scope intermittently cloudy areas. Variations between the geomedian and the individual 
##                measurements are captured by the three Median Absolute Deviation (MAD) layers. These are higher-order statistical 
##                measurements calculating variation relative to the geomedian. 
##
##                The MAD layers can be used on their own or together with 
##                geomedian to gain insights about the land surface, and understand change over time. Calculating GeoMAD over different 
##                timeframes and sensors provides a range of insights to the environment. An annual timeframe allows better correction 
##                for cloud cover and reduces artifacts for comparison over multiple years. A semiannual timeframe, for example six-month 
##                blocks, better captures seasonal variation within one year, but can also be used to compare equivalent periods from 
##                different years. Likewise, Landsat sensors allows full utility of the surface reflectance archive dating back to 1984, 
##                while more recent Sentinel-2 data provides higher-frequency flyovers and better resolution. The Digital Earth Africa 
##                GeoMAD service currently provides annual and six-month semiannual datasets, with separate services for Landsat and 
##                Sentinel-2 sensors.
##
##                https://registry.opendata.aws/deafrica-geomad/b
##
##    Author:     James Hogan, Senior Marine Resource Economist, 1 September 2025
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
      library(tensorflow)
      library(GPUmatrix)
      library(Matrix)
      
      library(tictoc)
      library(terra)
      library(sf)
      library(parallel)   

   ##
   ##    Set working directory
   ##
      setwd("c:\\Git_Projects\\Ocean_Accounts")

      NewCal <- rast("Data_Spatial/dep_s2_geomad_2017.tif")
      describe("Data_Spatial/dep_s2_geomad_2017.tif")
      plot(NewCal)
      head(values(NewCal))

      