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
   ##
   ##    Set working directory
   ##
      setwd("S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Ocean_Accounts")
<<<<<<< HEAD
      
      source("Programmes/Adhoc_Data.r")                  # Th
      source("Programmes/Organise_Input_Spatial_Data.r") # This calculates the ratio of land to EEZ internationally and makes a picture
=======
      setwd("C:\\GIT_Projects\\Ocean_Accounts")
   source("Programmes/Organise_Input_Spatial_Data.r") # This calculates the ratio of land to EEZ internationally and makes a picture
<<<<<<< HEAD
>>>>>>> Version2MoreCountries
=======
>>>>>>> HeavyLifterVersion
>>>>>>> 47417a3a0dc9c0b09654725f1264a995ca9909f7
      
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
<<<<<<< HEAD
          rmarkdown::render("Programmes/Pacific Regional Environmental Accounts.Rmd", output_file = "S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Ocean_Accounts\\Product_Output\\Pacific Regional Environmental Accounts.docx")                
=======
          rmarkdown::render("Programmes/Pacific Regional Environmental Accounts_Version2.Rmd", output_file = "C:\\GIT_Projects\\Ocean_Accounts\\Product_Output\\Pacific Regional Environmental Accounts.docx")                


<<<<<<< HEAD
>>>>>>> Version2MoreCountries
=======
>>>>>>> HeavyLifterVersion
>>>>>>> 47417a3a0dc9c0b09654725f1264a995ca9909f7


##
##   End of programme
##




