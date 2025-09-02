##
##    Programme:  Parallel_Process_Tiff.r
##
##    Objective:  The data is New Caledonia, downloaded from here: https://livingatlas.arcgis.com/landcoverexplorer/#mapCenter=39.18600%2C9.04200%2C10.00&mode=step&timeExtent=2017%2C2022&year=2020&downloadMode=true
##
##                This is a useful site: https://rspatial.org/rs/1-introduction.html
##                and this https://rspatial.org/rs/4-unsupclassification.html
##                and this https://datacarpentry.github.io/r-raster-vector-geospatial/01-raster-structure.html
##
##                Converting from UTM to Latitude/Longitude: https://stackoverflow.com/questions/30018098/how-to-convert-utm-coordinates-to-lat-and-long-in-r
##
##                The Living Atlas website is going to serve as the primary data source for this analysis.
##                It provides from free a 10meter x 10meter land use coded value which will work fine for a prototype.
##                I'll read the raster into the R. There's WAY too much data for holding in memory all at once, so it will need
##                chopped down into parallel processed chunks. The Z value of the raster is the land use measure
##
##
##    Author:     James Hogan, Senior Marine Resource Economist, 2 September 2025
##
##
5,705,961,954

   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      options(max.print=999999)
      options(scipen = 100)
     
   ##
   ##    Core libraries
   ##
      library(tictoc)
      library(terra)
      library(sf)
      library(parallel)   

   ##
   ##    Set working directory
   ##
      setwd("c:\\Git_Projects\\Ocean_Accounts")

   ##
   ##    Grab some data which we want to make spatial
   ##

      NewCal <- rast("Data_Spatial/58K_20240101-20241231.tif")
      
      subNewCal <- crop(NewCal, ext(178910, (178910+100), 7342530, (7342530+100)))
      subNewCal <- as.points(subNewCal, values = TRUE, na.rm = FALSE)
      y <- project(subNewCal, "+proj=longlat +datum=WGS84")
      lonlat <- st_as_sf(y)   

      NewCal <- as.points(rast("Data_Spatial/58K_20240101-20241231.tif"), values = TRUE, na.rm = FALSE)
      NewCal <- project(NewCal, "+proj=longlat +datum=WGS84")
      lonlat <- st_as_sf(y)   


   ##
   ##    find locations already processed
   ##
         Contents <- as.data.frame(list.files(path = "Parallel/",  pattern = "*.rda"))
         names(Contents) = "DataFrames"
         Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
         Contents$Dframe <- str_replace_all(Contents$Dframe, "Obs_split_", "")
         Contents <- Contents[str_detect(Contents$DataFrames, "XXNewCal_Split"),]
         
         To_Process <- 0:999
         Already_Processed <- as.numeric(str_split_fixed(Contents$Dframe, "XX", n = 2)[,1])

         To_Process <- To_Process[!(To_Process %in% Already_Processed)]
         
   ##
   ##    Restart the parallel process if it crashed
   ##

      e <- ext(NewCal)
   
      Size_of_Loops <- ceiling((as.numeric(e[2]) - as.numeric(e[1]))/ 1000)

#      cl <- makeCluster(detectCores())
#      cl <- makeCluster(20)
#      clusterEvalQ(cl, { c(library(sf), library(terra)) }) 

      Start_Min_X <- as.numeric(e[1])-1
      Start_Max_X <- as.numeric(e[2])
      
      Start_Min_Y <- as.numeric(e[3])
      Start_Max_Y <- as.numeric(e[4])

for(i in 0:99)
{
Start = ((i*Size_of_Loops*10) + Start_Min_X + 1) 
Finish = min(((((i+1)*Size_of_Loops)*10)  + Start_Min_X ), (Start_Min_X + 1 + (as.numeric(e[2]) - as.numeric(e[1]))))
print(paste(Start, Finish, Start_Min_Y, Start_Max_Y))
}



      subNewCal <- crop(NewCal, ext(Start_Min_X, (178910+100), Start_Min_Y, Start_Max_Y))


[(i*Size_of_Loops + 1):min(((i+1)*Size_of_Loops), nrow(RAWDATA_XXVMS_2024)),]

      
      for(i in To_Process)
      {
         i = 0
      
         (i*Size_of_Loops + 1)
      
         New_Y <- Start_Min_Y + ceiling(Size_of_Loops / (Max_X - Min_X))
         New_X <- Start_Min_X + (Size_of_Loops %% (Max_X - Min_X))

(New_X - Start_Min_X) * (New_Y - Start_Min_Y)

      subNewCal <- crop(NewCal, ext(Start_Min_X, New_X, Start_Min_Y, New_Y))



      
         ext(Min_X, (178910+100), 7342530, (7342530+100))
      
      
      
       Grab_Obs <- crop(NewCal, ext(178910, (178910+100), 7342530, (7342530+100)))
      
       Grab_Obs <- RAWDATA_XXVMS_2024[(i*Size_of_Loops + 1):min(((i+1)*Size_of_Loops), nrow(RAWDATA_XXVMS_2024)),]
       Grab_Obs$longitude <- as.numeric(Grab_Obs$longitude)
       Grab_Obs$latitude  <- as.numeric(Grab_Obs$latitude)
       
        Peg_Me <- function(Fistful)
         {
            Fistful_of_Data <- Grab_Obs[Fistful,]
            Fistful_of_Data <- Fistful_of_Data[as.numeric(Fistful_of_Data$longitude) > -190,]
            
            coordinates(Fistful_of_Data) <- ~ longitude + latitude
            proj4string(Fistful_of_Data) <- CRS("+proj=longlat +datum=WGS84")
            
            Fistful_of_Data <- st_as_sf(Fistful_of_Data)               
            Fistful_of_Data <- st_rotate(Fistful_of_Data)
            
            return(Fistful_of_Data)
         }
         sp <- parallel::clusterSplit(cl, 1:nrow(Grab_Obs))
         clusterExport(cl, c("sp", "Grab_Obs", "Peg_Me", "st_rotate"))  # each worker is a new environment, you will need to export variables/functions to
         
       tic(print(paste("Starting to process loop", i)))
         system.time(ll <- parallel::parLapply(cl, sp, Peg_Me))
         New_VMS <- do.call(rbind, ll)

         assign(paste0("Obs_split_", (i+50), "XXNewCal_Split"), New_VMS)
         save(list = paste0("Obs_split_", (i+50), "XXNewCal_Split"), 
              file = paste0("Parallel/Obs_split_", (i+50), "XXNewCal_Split.rda"))
       toc()
      }
      stopCluster(cl)
    
##
##    Collect them all back up
##   
      Contents <- as.data.frame(list.files(path = "Parallel/",  pattern = "*.rda"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents <- Contents[str_detect(Contents$DataFrames, "XXNewCal_Split"),]

      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Parallel/", File))  
                           X <- get(str_split_fixed(File, "\\.", n = 2)[,1])
                           return(X)})
#      rm(list=ls(pattern="Obs_split*"))
      New_VMS <- do.call(rbind, All_Data)
      New_VMS_Remainder2024 <- st_as_sf(New_VMS)
      save(New_VMS_Remainder2024, file = "Data_Spatial/New_Caledonia_Spatial.rda")



##
##    Show me a map of 
##  
     showtext_auto()

   ggplot() + 
     geom_sf(data = PNA_EEZ,  color = "red", fill = SPCColours("Light_Blue")) +
     geom_sf(data = Vessel_Ever_Fished_PNA, size = 0.05, alpha = 0.05) +
     coord_sf(datum = NA) +
     theme_bw(base_size=12, base_family =  "Calibri") %+replace%
     theme(legend.title.align=0.5,
           plot.margin = unit(c(1,3,1,1),"mm"),
           panel.border = element_blank(),
           strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
           strip.text = element_text(colour = "white", 
                                     size   = 13,
                                     family = "MyriadPro-Bold",
                                     margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
           panel.spacing = unit(1, "lines"),                                              
           legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
           plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
           plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
           plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
           plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
           axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
           axis.text.x   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
           axis.text.y   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
           legend.key.width = unit(1, "cm"),
           legend.spacing.y = unit(1, "cm"),
           legend.margin = margin(10, 10, 10, 10),
           legend.position  = "bottom")
      
##
##    And we're done
##
