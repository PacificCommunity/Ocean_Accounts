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

   ##
   ##    find locations already processed
   ##
         # Contents <- as.data.frame(list.files(path = "Parallel/",  pattern = "*.rda"))
         # names(Contents) = "DataFrames"
         # Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
         # Contents$Dframe <- str_replace_all(Contents$Dframe, "Obs_split_", "")
         # Contents <- Contents[str_detect(Contents$DataFrames, "XXNewCal_Split"),]
         
         # To_Process <- 0:99
         # Already_Processed <- as.numeric(str_split_fixed(Contents$Dframe, "XX", n = 2)[,1])

         # To_Process <- To_Process[!(To_Process %in% Already_Processed)]
         
   ##
   ##    Restart the parallel process if it crashed
   ##

      e <- ext(NewCal)
   
      Size_of_Loops <- ceiling((as.numeric(e[2]) - as.numeric(e[1]))/ 1000)

#      cl <- makeCluster(detectCores())

      Start_Min_X <- as.numeric(e[1])-1
      Start_Max_X <- as.numeric(e[2])
      
      Start_Min_Y <- as.numeric(e[3])
      Start_Max_Y <- as.numeric(e[4])

      cl <- makeCluster(10)
      clusterEvalQ(cl, { c(library(sf), library(terra)) }) 

      
#      for(i in To_Process)
      for(i in 0:1)
      {
       Start = ((i*Size_of_Loops*10) + Start_Min_X + 1) 
       Finish = min(((((i+1)*Size_of_Loops)*10)  + Start_Min_X ), (Start_Min_X + 1 + (as.numeric(e[2]) - as.numeric(e[1]))))
       print(paste(i,Start, Finish, Start_Min_Y, Start_Max_Y))
      
       #Grab_Obs <- crop(NewCal, ext(Start, Finish, Start_Min_Y, Start_Max_Y))
             
        Peg_Me <- function(Fistful)
         {
            # The error message "external pointer is not valid" in R typically indicates an issue with an object that relies on an external resource 
               # or compiled code, particularly when that object is being used in a context where its connection to that resource or code is broken 
               # or not properly maintained. This often occurs in parallel processing, when saving/loading R objects, or when dealing with specific 
               # packages that manage external connections.
               
            # Common Scenarios and Solutions:
            # Parallel Processing (e.g., parallel, future, foreach):
            # Problem: When using parallel workers, objects with external pointers (like database connections, compiled models, or certain data structures 
            # from packages like arrow or udpipe) are not automatically transferred or re-initialized in the new processes. The pointer becomes invalid 
            # in the new environment.
            
            # Solutions:
            # Explicitly export or re-create objects: Ensure that any objects with external pointers are either explicitly exported to the parallel 
            # workers or re-created within each worker's environment.
            
            # Pass data, not objects: If possible, pass the underlying data to the parallel function and re-create the object within the function on each worker.
            # Use model_dir argument: For packages like udpipe, specify the model_dir argument to point to the model files on disk, allowing each worker 
            # to load the model independently.         
                     
            NewCal <- rast("Data_Spatial/58K_20240101-20241231.tif")

            Fistful_of_Data <- crop(NewCal, ext(min(Fistful), max(Fistful), Start_Min_Y, Start_Max_Y))
            #Fistful_of_Data <- crop(NewCal, ext(178910, 178910+10, 7342530, 8231060))
            
            subNewCal <- as.points(Fistful_of_Data, values = TRUE, na.rm = FALSE)
            y <- project(subNewCal, "+proj=longlat +datum=WGS84")
            lonlat <- st_as_sf(y)   
            
            return(lonlat)
         }
         sp <- parallel::clusterSplit(cl, Start:Finish)
#         clusterExport(cl, c("sp", "Grab_Obs", "Peg_Me", "Start_Min_Y", "Start_Max_Y"))  # each worker is a new environment, you will need to export variables/functions to
         clusterExport(cl, c("sp", "Peg_Me", "Start_Min_Y", "Start_Max_Y"))  # each worker is a new environment, you will need to export variables/functions to
         
       tic(print(paste("Starting to process loop", i)))
         system.time(ll <- parallel::parLapply(cl, sp, Peg_Me))
         New_VMS <- do.call(rbind, ll)

         assign(paste0("Obs_split_", i, "XXNewCal_Split"), New_VMS)
         save(list = paste0("Obs_split_", i, "XXNewCal_Split"), 
              file = paste0("Parallel/Obs_split_", i, "XXNewCal_Split.rda"))
          rm(list=c(paste0("Obs_split_", i, "XXNewCal_Split")))
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



        Peg_Me <- function(Fistful)
         {
            Fistful_of_Data <- crop(Grab_Obs, ext(min(Fistful), max(Fistful), Start_Min_Y, Start_Max_Y))
            
            subNewCal <- as.points(Fistful_of_Data, values = TRUE, na.rm = FALSE)
            y <- project(subNewCal, "+proj=longlat +datum=WGS84")
            lonlat <- st_as_sf(y)   
            
            return(lonlat)
         }
         
wonder <- Peg_Me(sp[[1]])



