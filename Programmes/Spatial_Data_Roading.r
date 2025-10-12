##
##    Programme:  Spatial_Data_Roading.r
##
##    Objective:  This was related to a comment in the Sapere report where roading
##                might be seen as a cost quantity driver
##
##    Author:     James Hogan, Sense Partners, 9 March 2020
##
##

   rm(list=ls(all=TRUE))
   source("R/themes.r")
   
   ##
   ##    Read in the other shape data
   ##
   
   Roads_Dir         <- "Data_Spatial/nz-roads-addressing"
   Road_Sections_Dir <- "Data_Spatial/nz-roads-road-section-geometry"
   TLA_Dir           <- "Data_Spatial/territorial-authority-2019-clipped-generalised/territorial-authority-2019-clipped-generalised.shp"
   TLA2010_Dir       <- "Data_Spatial/statsnzterritorial-authority-2010-SHP/territorial-authority-2010.shp"

   ##
   ##    Grab the geometry for each parcel
   ##
   Roads <- st_read(dsn = Road_Sections_Dir,  layer = "nz-roads-road-section-geometry")
   Roads <- st_transform(Roads, 2193)
   
   TLAs <- st_read(dsn = TLA_Dir,   layer = "territorial-authority-2019-clipped-generalised")
   TLAs <- TLAs[!(TLAs$TA2019_V_1 %in% c("Area Outside Territorial Authority", "Chatham Islands Territory")),]
   TLAs <- st_transform(TLAs, 2193)

   TLA2010 <- st_read(dsn = TLA2010_Dir,   layer = "territorial-authority-2010")
   TLA2010 <- TLA2010[!(TLA2010$TA2010_V_1 %in% c("Area Outside Territorial Authority", "Chatham Islands Territory")),]
   TLA2010 <- st_transform(TLA2010, 2193)

   ##
   ##    Set it up for parallelisation - tried parallelising the TLAs but that was still too much 
   ##       data. This time, I'll unlease all of the cores onto each TLA sequentially.
   ##

   Size_of_Loops <- ceiling(nrow(Roads) / 100)

   cl <- makeCluster(detectCores())
   clusterEvalQ(cl, { c(library(sf), library(rmapshaper)) }) 
   clusterExport(cl, c("TLA2010", "TLAs"))
      
   
   for(i in 0:99)
   {
   
    Grab_Obs <- Roads[(i*Size_of_Loops + 1):min(((i+1)*Size_of_Loops), nrow(Roads)),]
    
     Peg_Me <- function(Fistful)
      {
         Fistful_of_Data <- Grab_Obs[Fistful,]
         ##
         ##    Grab their TLAs
         ##
         Wonder <- st_intersection(Fistful_of_Data, st_buffer(TLAs, 0))
         Wonder <- st_intersection(Wonder, st_buffer(TLA2010, 0))
         Wonder$Road_Length <- st_length(Wonder)
         return(Wonder)
      }
      sp <- parallel::clusterSplit(cl, 1:nrow(Grab_Obs))
      clusterExport(cl, c("sp", "Grab_Obs", "Peg_Me"))  # each worker is a new environment, you will need to export variables/functions to
      
    tic(print(paste("Starting to process loop", i)))
      system.time(ll <- parallel::parLapply(cl, sp, Peg_Me))
      New_Parcels <- do.call(rbind, ll)

      assign(paste0("Obs_split_", i, "XXRoading_Split"), New_Parcels)
      save(list = paste0("Obs_split_", i, "XXRoading_Split"), 
           file = paste0("Parallel/Obs_split_", i, "XXRoading_Split.rda"))
    toc()
   }
      

      stopCluster(cl)

##
##    Collect them all back up
##   
      Contents <- as.data.frame(list.files(path = "Parallel/",  pattern = "*Roading_Split.rda"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents <- Contents[str_detect(Contents$DataFrames, "XX"),]

      All_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Parallel/", File))  
                           X <- get(str_split_fixed(File, "\\.", n = 2)[,1])
                           rm(list=c(as.character(File) ))
                           return(X)})
      Mapped_Roads <- do.call(rbind, All_Data)
      Mapped_Roads <- st_as_sf(Mapped_Roads)
      save(Mapped_Roads, file = "Data_Spatial/Mapped_Roads.rda")
      
  
  ##
  ##     Make a picture
  ##
load("Data_Spatial/Mapped_Roads.rda")
   
   p <-  ggplot() + 
           geom_sf(data =  Mapped_Roads[Mapped_Roads$TA2010_V_1 %in% c("Auckland City"),],    color="red")  +
           geom_sf(data =  Mapped_Roads[Mapped_Roads$TA2010_V_1 %in% c("North Shore City"),], color="blue")  +
           geom_sf(data =  Mapped_Roads[Mapped_Roads$TA2010_V_1 %in% c("Franklin District"),], color="green")  +
           geom_sf(data =  Mapped_Roads[Mapped_Roads$TA2010_V_1 %in% c("Manukau City"),],      color="brown")  +
           geom_sf(data =  Mapped_Roads[Mapped_Roads$TA2010_V_1 %in% c("Rodney District"),], color="purple")  +
           geom_sf(data =  Mapped_Roads[Mapped_Roads$TA2010_V_1 %in% c("Waitakere City"),], color="orange")  +
           geom_sf(data = TLAs[TLAs$TA2019_V_1 %in% c("Auckland"),], fill = NA) +
           theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
           theme(legend.title.align=0.5,
                 plot.margin = unit(c(1,3,1,1),"mm"),
                panel.border = element_blank(),
                strip.background =  element_rect(fill   = SenseColours("LightBlue")),
                strip.text = element_text(colour = "white", 
                                          size=11,
                                          family = "Open Sans Semibold",
                                          margin = margin(1.25,0,1.25,0, unit = "mm")),
                panel.spacing = unit(1, "lines"),                                              
                legend.text   = element_text(size=12, family = "Open Sans Light"),
                plot.title    = element_text(size = 24, colour = SenseColours("Blue"),                   family = "Raleway SemiBold"),
                plot.subtitle = element_text(size = 14, colour = SenseColours("LightBlue"), hjust = 0.5, family = "Raleway SemiBold"),
                plot.caption  = element_text(size = 11, colour = SenseColours("Black"),     hjust = 1.0, family = "Open Sans"),
                plot.tag      = element_text(size =  9, colour = SenseColours("Black"),     hjust = 0.0, face = "italic" ),
                axis.title    = element_text(size = 12, colour = SenseColours("Blue")),
                axis.text.x   = element_text(size =  9, colour = SenseColours("Blue"), angle = 90),
                legend.key.width = unit(1, "cm"),
                legend.spacing.y = unit(1, "cm"),
                legend.margin = margin(0, 0, 0, 0),
                legend.position  = "bottom")
                
                   
      png(file='Output/Roads_Old_Councils_versus_New.png', h =8.3, w = 15.7 ,  res = 600, units = "in")
         print(p)
      dev.off()
      
      
      
      

  
      