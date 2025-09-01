##
##    Programme:  Experiments_in_Tiff.r
##
##    Objective:  Refamiliarizing myself with working with Tiff files
##
##                The data is New Caledonia, downloaded from here: https://livingatlas.arcgis.com/landcoverexplorer/#mapCenter=39.18600%2C9.04200%2C10.00&mode=step&timeExtent=2017%2C2022&year=2020&downloadMode=true
##
##    Author:     James Hogan, Senior Marine Resource Economist, 1 September 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))

      NewCal  <- raster("Data_Spatial/58K_20240101-20241231.tif")


      terrain(NewCal, opt = "slope",  units = "radians")








##
##    And we're done
##
