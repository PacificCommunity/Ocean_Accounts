##    Programme:  Ocean_Accounts.r
##
##    Objective:  This project is about developing Ocean Accounts according to the System of 
##                Environment-Economic Accounts - Central Framework using remote sensing data. 
##
##    Plan of  :  The original plan was to use Digital Earth Pacific, but I've now got doubts
##    Attack   :  about it. I'm not getting any traction getting access to it, and its making 
##                me suspicious about its production-level status. I haven't seen it in github
##                either, so I'm thinking I will try an alternative data source, and build this
##                outside of DEP.
##
##                The data source for this work is here: https://livingatlas.arcgis.com/landcoverexplorer/#mapCenter=39.18600%2C9.04200%2C10.00&mode=step&timeExtent=2017%2C2022&year=2020&downloadMode=true
##                More Useful links: https://www.arcgis.com/home/item.html?id=cfcb7609de5f478eb7666240902d4d3d
##                                   https://www.impactobservatory.com/maps-for-good/
##
##                The AI which does the classifying is here: https://www.impactobservatory.com/maps-for-good/ 
##                and they offer different types of maps with different resolutions. I'm going to 
##                start building with their free version "Maps for Good" which gives 9 classes of 
##                land use.
##
##                Impact Observatory offers two other different levels of resolution but at cost.
##                If the pilot works, then this might be an option as a data source.
##
##                This code is being stored here: https://github.com/JamesHoganNZ/Ocean_Accounts
##
##
##                This piece of analysis will build off the spatial analysis work delivered for 
##                Sense Partners.
##
##    Important:  The documentation for this project is written here: S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Ocean_Accounts\\Product_Output\\Pacific Regional Environmental Accounts.docx,
##    Linkages :  created in the programme Intergrate_Project_Documentation.r
##
##    Author   :  James Hogan, Senior Marine Resource Economist, 1 September 2025
##
##    Peer     :  <PROGRAMMER>, <TEAM>, <PEER REVIEWED COMPLETED>
##    Reviewer :
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
   ##
   ##    Project-specific libraries
   ##
      library(raster)
      library(tmap)
      library(tmaptools)
      library(lidR)
      library(RStoolbox)
      library(sf)
      library(parallel)   
      library(data.table)
      library(XML)
      library(RCurl)
      library(RJSONIO)
      library(elevatr)

      library(Rcpp)
      library(RcppParallel)
      library(inline) 
      
      library(quadmesh)
      library(rgl)
      library(REdaS)
      
      library(RSelenium)
      library(XML)
      library(rlas)

   ##
   ##    Set working directory
   ##
      #setwd("S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Ocean_Accounts")
      setwd("c:\\From BigDisk\\GIT\\Ocean_Accounts")


      source("Programmes/Experiments_in_Tiff.r") # Builds of the programme Experiements in Lidar.r


      DTM_pre  <- raster("Data_Spatial/wellington-city-lidar-1m-dem-2019-2020/DEM_BQ31_2019_1000_2035.tif")







##
##   End of programme
##
