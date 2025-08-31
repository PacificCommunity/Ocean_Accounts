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
   ##
   ##    Clear the decks and load up some functionality
   ##
      rm(list=ls(all=TRUE))
      options(scipen = 999)
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
      library(xxxx)
      library(xxxx)
      library(xxxx)

   ##
   ##    Set working directory
   ##
      setwd("C:\\SOMEWHERE\\SOMEPROJECT")
      # setwd("C:\\From BigDisk\\GIT\\R_Fundamentals_Training\\Fundamentals Graphics and Reporting")
   ##
   ##   Modular-programmed code from here down. Each following programme
   ##      needs to be able to 'stand alone' in the sense that it starts of 
   ##       reading all the data it needs, it processes it, and it concludes
   ##       either writing data for a following programme, or creating a final
   ##       output. 
   ##
      ##
      ##    STEP 1:
      ##
         source("Programmes/xxxxxxxx.r") # This does blah blah blah

      ##
      ##    STEP 2:
      ##
         source("Programmes/xxxxxxxx.r") # This does blah blah blah
         source("Programmes/xxxxxxxx.r") # This does blah blah blah
         source("Programmes/xxxxxxxx.r") # This does blah blah blah
         source("Programmes/xxxxxxxx.r") # This does blah blah blah
      ##
      ##    STEP 3:
      ##
         source("Programmes/xxxxxxxx.r") # This does blah blah blah
      ##
      ##    STEP 4:
      ##
         source("Programmes/xxxxxxxx.r") # This does blah blah blah
      ##
      ##    STEP x: Final output
      ##
         source("Programmes/xxxxxxxx.r") # This does blah blah blah

      ##
      ##    Write up of results
      ##      
         rmarkdown::render("Programmes/SSAP_Value_Report.rmd", output_file = "C:\\From BigDisk\\GIT\\R_Fundamentals_Training\\Fundamentals Graphics and Reporting\\Product_Output\\Value of Good Science.docx")                


##
##   End of programme
##
