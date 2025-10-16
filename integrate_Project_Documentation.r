##    Programme:  Oceans_Accounts_Project_Documentation.r
##
##    Objective:  
##                
##                
##
##    Plan of  :  
##    Attack   :  
##
##
##    Important:  
##    Linkages :  
##
##
##    Author   :  James Hogan, Senior Marine Resource Economist, FAME, 11 July 2025
##
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
      library(RefManageR)

      library(sysfonts)
      library(showtext)
      library(viridis)                  
      
   ##
   ##    Project-specific libraries
   ##
      library(sf)
      library(sp)
      library(ggridges)
      library(dym)         # This library is derived from here: https://osf.io/hgfjq/files/5kqyf
      
   ##
   ##    Set working directory
   ##
      setwd("S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Ocean_Accounts")
      setwd("C:\\GIT_Projects\\Ocean_Accounts")
      setwd("C:\\From BigDisk\\GIT\\Ocean_Accounts")
      
      ##
      ##    This is the content that went to Palau back in November 2024 
      ##
      ##    The project died a death after this - it wasnt a priority, and now, in July 2025, the project's gone no further
      ##
         #rmarkdown::render("Programmes/Marine_Spatial_Planning_Economics_Version.rmd", output_file = "S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Ocean_Accounts\\Product_Output\\Marine_Spatial_Planning_Economics_Version.docx")                
      ##
      ##    
      ##
        # rmarkdown::render("Programmes/Day1_Thinking.rmd",                           output_file = "S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Ocean_Accounts\\Product_Output\\Day1_Thinking.docx")                
          rmarkdown::render("Programmes/Pacific Regional Environmental Accounts.Rmd", output_file = "S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Ocean_Accounts\\Product_Output\\Pacific Regional Environmental Accounts.docx")                
          rmarkdown::render("Programmes/Pacific Regional Environmental Accounts_Version2.Rmd", output_file = "C:\\GIT_Projects\\Ocean_Accounts\\Product_Output\\Pacific Regional Environmental Accounts.docx")                

      ##
      ##    Write some documentation for the first attempt using ESA Land Use metrics
      ##
         
          rmarkdown::render("Programmes/Version1_Documentation.Rmd", output_file = "C:\\From BigDisk\\GIT\\Ocean_Accounts\\Product_Output\\Generating_EnvironAsset_Accounts - Documentation.docx")                
      

##
##   End of programme
##




