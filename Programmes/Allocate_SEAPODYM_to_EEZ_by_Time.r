##
##    Programme:  Allocate_SEAPODYM_to_EEZ_by_Time.r
##
##    Objective:  We've got SEAPODYM in as raster files. Now allocate the biomass to the EEZs over time.
##
##                The complication with this is that the SEAPODYM is on different timescales and extent definitions.
##
##
##    Author:     James Hogan, Senior Marine Resource Economist, 20 October 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions or colour palattes, depending on what you're doing.
   ##
      source("R/functions.r")
      source("R/themes.r")
      
   ##
   ##    Load data from somewhere
   ##
      load("Data_Spatial/EEZ.rda")
      SEAPODYM_Skipjack_Adults  <- rast('Data_Spatial/SEAPODYM_Skipjack_Adults.tif')
      SEAPODYM_Bigeye_Adults    <- rast('Data_Spatial/SEAPODYM_Bigeye_Adults.tif')
      SEAPODYM_Yellowfin_Adults <- rast('Data_Spatial/SEAPODYM_Yellowfin_Adults.tif')
      SEAPODYM_Albacore_Adults  <- rast('Data_Spatial/SEAPODYM_Albacore_Adults.tif')

   ##
   ##    Generate the country cuts - function EEZ_Fish_Stocks stored in R/functions.r
   ##
      ##
      ##    Cook Islands
      ##
         ##
         ##    Skipjack
         ##
            Select_EEZ <- EEZ[1,]# EEZ[1,] is Cook Islands
            
            Country   <- mask(SEAPODYM_Skipjack_Adults, Select_EEZ)  
            Skipjack <- do.call(rbind, 
                                     lapply(unique(names(SEAPODYM_Skipjack_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Bigeye
         ##
            Country   <- mask(SEAPODYM_Bigeye_Adults, Select_EEZ)
            Bigeye <-  do.call(rbind, 
                                    lapply(unique(names(SEAPODYM_Bigeye_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Yellowfin
         ##
            Country   <- mask(SEAPODYM_Yellowfin_Adults, Select_EEZ) 
            Yellowfin <-  do.call(rbind, 
                                       lapply(unique(names(SEAPODYM_Yellowfin_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Albacore
         ##
            Country   <- mask(SEAPODYM_Albacore_Adults, Select_EEZ)
            Albacore <-  do.call(rbind, 
                                      lapply(unique(names(SEAPODYM_Albacore_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))

         ##
         ##    Stick them all together
         ##
            CookIslands_Tuna_Stocks <- merge(Skipjack,
                                             Bigeye,
                                             by = "Date",
                                             all = TRUE)
                                      
            CookIslands_Tuna_Stocks <- merge(CookIslands_Tuna_Stocks,
                                             Yellowfin,
                                             by = "Date",
                                             all = TRUE)
                                      
            CookIslands_Tuna_Stocks <- merge(CookIslands_Tuna_Stocks,
                                             Albacore,
                                             by = "Date",
                                             all = TRUE)
                                      
            names(CookIslands_Tuna_Stocks) <- c("Date", "Skipjack", "Bigeye", "Yellowfin", "Albacore")
      ##
      ##    New Caledonia
      ##
         ##
         ##    Skipjack
         ##
            Select_EEZ <- EEZ[2,]# EEZ[2,] is New Caledonia
            
            Country   <- mask(SEAPODYM_Skipjack_Adults, Select_EEZ)  
            Skipjack <- do.call(rbind, 
                                     lapply(unique(names(SEAPODYM_Skipjack_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Bigeye
         ##
            Country   <- mask(SEAPODYM_Bigeye_Adults, Select_EEZ)  
            Bigeye <-  do.call(rbind, 
                                    lapply(unique(names(SEAPODYM_Bigeye_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Yellowfin
         ##
            Country   <- mask(SEAPODYM_Yellowfin_Adults, Select_EEZ)  
            Yellowfin <-  do.call(rbind, 
                                       lapply(unique(names(SEAPODYM_Yellowfin_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Albacore
         ##
            Country   <- mask(SEAPODYM_Albacore_Adults, Select_EEZ) 
            Albacore <-  do.call(rbind, 
                                      lapply(unique(names(SEAPODYM_Albacore_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))

         ##
         ##    Stick them all together
         ##
            NewCal_Tuna_Stocks <- merge(Skipjack,
                                        Bigeye,
                                        by = "Date",
                                        all = TRUE)
                                      
            NewCal_Tuna_Stocks <- merge(NewCal_Tuna_Stocks,
                                        Yellowfin,
                                        by = "Date",
                                        all = TRUE)
                                      
            NewCal_Tuna_Stocks <- merge(NewCal_Tuna_Stocks,
                                        Albacore,
                                        by = "Date",
                                        all = TRUE)
                                      
            names(NewCal_Tuna_Stocks) <- c("Date", "Skipjack", "Bigeye", "Yellowfin", "Albacore")
      ##
      ##    Palau
      ##
         ##
         ##    Skipjack
         ##
            Select_EEZ <- EEZ[3,]# EEZ[3,] is Palau
            
            Country   <- mask(SEAPODYM_Skipjack_Adults, Select_EEZ)  
            Skipjack <- do.call(rbind, 
                                     lapply(unique(names(SEAPODYM_Skipjack_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Bigeye
         ##
            Country   <- mask(SEAPODYM_Bigeye_Adults, Select_EEZ)  
            Bigeye <-  do.call(rbind, 
                                    lapply(unique(names(SEAPODYM_Bigeye_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Yellowfin
         ##
            Country   <- mask(SEAPODYM_Yellowfin_Adults, Select_EEZ)  
            Yellowfin <-  do.call(rbind, 
                                       lapply(unique(names(SEAPODYM_Yellowfin_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Albacore
         ##
            Country   <- mask(SEAPODYM_Albacore_Adults, Select_EEZ) 
            Albacore <-  do.call(rbind, 
                                      lapply(unique(names(SEAPODYM_Albacore_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))

         ##
         ##    Stick them all together
         ##
            Palau_Tuna_Stocks <- merge(Skipjack,
                                        Bigeye,
                                        by = "Date",
                                        all = TRUE)
                                      
            Palau_Tuna_Stocks <- merge(Palau_Tuna_Stocks,
                                        Yellowfin,
                                        by = "Date",
                                        all = TRUE)
                                      
            Palau_Tuna_Stocks <- merge(Palau_Tuna_Stocks,
                                        Albacore,
                                        by = "Date",
                                        all = TRUE)
                                      
            names(Palau_Tuna_Stocks) <- c("Date", "Skipjack", "Bigeye", "Yellowfin", "Albacore")
            
      ##
      ##    Fiji
      ##
         ##
         ##    Skipjack
         ##
            Select_EEZ <- EEZ[4,]# EEZ[4,] is Fiji
            
            Country   <- mask(SEAPODYM_Skipjack_Adults, Select_EEZ)  
            Skipjack <- do.call(rbind, 
                                     lapply(unique(names(SEAPODYM_Skipjack_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Bigeye
         ##
            Country   <- mask(SEAPODYM_Bigeye_Adults, Select_EEZ)  
            Bigeye <-  do.call(rbind, 
                                    lapply(unique(names(SEAPODYM_Bigeye_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Yellowfin
         ##
            Country   <- mask(SEAPODYM_Yellowfin_Adults, Select_EEZ) 
            Yellowfin <-  do.call(rbind, 
                                       lapply(unique(names(SEAPODYM_Yellowfin_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))
         ##
         ##    Albacore
         ##
            Country   <- mask(SEAPODYM_Albacore_Adults, Select_EEZ)  
            Albacore <-  do.call(rbind, 
                                      lapply(unique(names(SEAPODYM_Albacore_Adults)), EEZ_Fish_Stocks, Country = Country, EEZ = Select_EEZ))

         ##
         ##    Stick them all together
         ##
            Fiji_Tuna_Stocks <- merge(Skipjack,
                                      Bigeye,
                                      by = "Date",
                                      all = TRUE)
                                      
            Fiji_Tuna_Stocks <- merge(Fiji_Tuna_Stocks,
                                      Yellowfin,
                                      by = "Date",
                                      all = TRUE)
                                      
            Fiji_Tuna_Stocks <- merge(Fiji_Tuna_Stocks,
                                      Albacore,
                                      by = "Date",
                                      all = TRUE)
                                      
            names(Fiji_Tuna_Stocks) <- c("Date", "Skipjack", "Bigeye", "Yellowfin", "Albacore")

   ##
   ##    Bring them all together and lets have a look
   ##
      CookIslands_Tuna_Stocks <- reshape2::melt(CookIslands_Tuna_Stocks,
                                                id.var = "Date")
      NewCal_Tuna_Stocks      <- reshape2::melt(NewCal_Tuna_Stocks,
                                                id.var = "Date")
      Palau_Tuna_Stocks       <- reshape2::melt(Palau_Tuna_Stocks,
                                                id.var = "Date")
      Fiji_Tuna_Stocks        <- reshape2::melt(Fiji_Tuna_Stocks,
                                                id.var = "Date")


      CookIslands_Tuna_Stocks$Country  <- "Cook Islands"
      NewCal_Tuna_Stocks$Country       <- "New Caledonia"
      Palau_Tuna_Stocks$Country        <- "Palau"
      Fiji_Tuna_Stocks$Country         <- "Fiji"

      All_Together <- rbind(CookIslands_Tuna_Stocks,
                            NewCal_Tuna_Stocks,
                            Palau_Tuna_Stocks,
                            Fiji_Tuna_Stocks)

      showtext_begin()

      ggplot(All_Together, 
             aes(x = Date, 
                 y = value,
                 colour = Country))     +
             geom_smooth(size =2, se = FALSE) +
             geom_point(size =1, alpha = 0.1) +
             facet_wrap(~variable, scales="free") +
             scale_y_continuous(labels = comma, breaks = seq(from = 0, to = 300000, by =10000)) +
             scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
             labs(title="Adult Tuna Biomass\n",
                  subtitle = "Data Source: SEAPODYM\n", 
             caption = "FAME\nThe Pacific Community (SPC)") +
             scale_colour_manual(values = SPCColours()) +
             xlab("\nTime Period") +
             ylab("Metric Tonnes") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,3,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 12,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.0,1.0,1.0,1.0, unit = "mm")),
                   panel.spacing = unit(4, "lines"),                                              
                   legend.text   = element_text(size = 14, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 20, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 16, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 10, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 8, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(2, "cm"),
                   legend.spacing.y = unit(2, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "bottom")          
      showtext_end()                   
           
      ggsave("Graphical_Output/SEAPODYM Biomass.png", height =(2.5)*10.13, width = (2)*20.66, dpi = 300, units = c("cm"))      
                   

   ##
   ## Save files our produce some final output of something
   ##
      save(CookIslands_Tuna_Stocks, file = 'Data_Intermediate/CookIslands_Tuna_Stocks.rda')
      save(NewCal_Tuna_Stocks,      file = 'Data_Intermediate/NewCal_Tuna_Stocks.rda')
      save(Palau_Tuna_Stocks,       file = 'Data_Intermediate/Palau_Tuna_Stocks.rda')
      save(Fiji_Tuna_Stocks,        file = 'Data_Intermediate/Fiji_Tuna_Stocks.rda')

##
## And we're done
##
                   
                   