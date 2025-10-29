##
##    Programme: Take a sample
##
##    Objective:  Sample from the largest and smallest properties
##
##    Author:     James Hogan, 3 August 2021
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
    source("R/themes.r")
  

      TA_2019_Dir  <- "Data_Spatial/territorial-authority-2019-clipped-generalised"
      TA_2019 <- st_read(dsn = TA_2019_Dir, layer = "territorial-authority-2019-clipped-generalised")
      TA_2019 <- st_transform(TA_2019, 2193 )
      
   ##
   ##    Auckland
   ##
      load("Data_Spatial/PWC_Properties_Auckland_with_Addresses.rda")
      
      Spatial_Catalogue <- readLAScatalog("e:/Auckland_North/")
      Wonder <- st_as_sf(Spatial_Catalogue, crs = st_crs(PWC_Properties_Auckland_with_Addresses))
      Wonder <- st_transform(Wonder, 2193 )
      #PWC_Properties_Auckland_with_Addresses <- PWC_Properties_Auckland_with_Addresses[st_within(PWC_Properties_Auckland_with_Addresses, st_union(Wonder),sparse = FALSE),]
      PWC_Properties_Auckland_with_Addresses$Land_Area <- st_area(PWC_Properties_Auckland_with_Addresses)
      
      Deciles <- data.frame(Value = quantile(PWC_Properties_Auckland_with_Addresses$Land_Area, prob = seq(0, 1, length = 11), type = 5, na.rm = TRUE))
      Deciles$Decile_Group <- row.names(Deciles)
      Deciles$ID <- 1:nrow(Deciles)
      Deciles$MatchID = Deciles$ID - 1
      Deciles <- merge(Deciles,
                      Deciles,
                      by.x = c("ID"),
                      by.y = c("MatchID"))
      Deciles$DGroup <- paste(Deciles$Decile_Group.x, Deciles$Decile_Group.y, sep = " - ")
      
      for(i in 1:nrow(PWC_Properties_Auckland_with_Addresses))
      {
         PWC_Properties_Auckland_with_Addresses$Decile[i] <- Deciles$DGroup[((PWC_Properties_Auckland_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                             (PWC_Properties_Auckland_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
         PWC_Properties_Auckland_with_Addresses$DecileGroup[i] <- Deciles$ID[((PWC_Properties_Auckland_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                              (PWC_Properties_Auckland_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
      }  
      Frequency <- aggregate(DecileGroup ~ Decile,
                            data = PWC_Properties_Auckland_with_Addresses,
                            FUN  = length,
                            na.action = NULL)
                            
      Auckland_Decile_Stats <- merge(Deciles,
                                     Frequency,
                                     by.x = c("DGroup"),
                                     by.y = c("Decile"))

      ##
      ##    Draw a stratified random sample of 100 properties
      ##
         Auckland_Sample <- lapply(1:10, function(x){
            A_Decile <- PWC_Properties_Auckland_with_Addresses[PWC_Properties_Auckland_with_Addresses$DecileGroup == x,]
            return(A_Decile[sample(1:nrow(A_Decile), 10),])
         })
         Auckland_Stratfied_Random_Sample <- do.call(rbind, Auckland_Sample)

      png("Graphical_Output/Auckland_Coverage.png", h =29.7, w = 21.0 ,  res = 600, units = "cm")
         ggplot() + 
              geom_sf(data = TA_2019[TA_2019$TA2019_V_1 == "Auckland",],  fill = SenseColours("ReallyLightBlue")) +
              geom_sf(data = PWC_Properties_Auckland_with_Addresses, colour = 'yellow') +
              geom_sf(data = Auckland_Stratfied_Random_Sample,  color = 'red', size = 2) +
                   theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SenseColours("LightBlue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 13,
                                                   family = "Open Sans Semibold",
                                                   margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 14, family = "Open Sans Light"),
                         plot.title    = element_text(size = 24, colour = SenseColours("Blue"),                   family = "Raleway SemiBold"),
                         plot.subtitle = element_text(size = 14, colour = SenseColours("LightBlue"), hjust = 0.5, family = "Raleway SemiBold"),
                         plot.caption  = element_text(size = 11, colour = SenseColours("Black"),     hjust = 1.0, family = "Open Sans"),
                         plot.tag      = element_text(size =  9, colour = SenseColours("Black"),     hjust = 0.0, face = "italic" ),
                         axis.title    = element_text(size = 14, colour = SenseColours("Blue")),
                         axis.text.x   = element_text(size = 12, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 12, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      dev.off() 
         ##
         ##    Save the decile information so it can be recombined at the end.
         ##
            save(Auckland_Decile_Stats, file = "Data_Intermediate/Auckland_Decile_Stats.rda")
            save(Auckland_Stratfied_Random_Sample, file = "Data_Intermediate/Auckland_Stratfied_Random_Sample.rda")
            write.table(Auckland_Stratfied_Random_Sample$full_add_1, file = "Data_Output/Sampled_Auckland_Properties.csv", sep = ",", row.names = FALSE,col.names = FALSE)

   ##
   ##    Hamilton
   ##
      load("Data_Spatial/PWC_Properties_Hamilton_with_Addresses.rda")
      
      Spatial_Catalogue <- readLAScatalog("e:/Hamilton/")
      Wonder <- st_as_sf(Spatial_Catalogue, crs = st_crs(PWC_Properties_Hamilton_with_Addresses))
      Wonder <- st_transform(Wonder, 2193 )
      PWC_Properties_Hamilton_with_Addresses <- PWC_Properties_Hamilton_with_Addresses[st_within(PWC_Properties_Hamilton_with_Addresses, st_union(Wonder),sparse = FALSE),]
      PWC_Properties_Hamilton_with_Addresses$Land_Area <- st_area(PWC_Properties_Hamilton_with_Addresses)

      Deciles <- data.frame(Value = quantile(PWC_Properties_Hamilton_with_Addresses$Land_Area, prob = seq(0, 1, length = 11), type = 5, na.rm = TRUE))
      Deciles$Decile_Group <- row.names(Deciles)
      Deciles$ID <- 1:nrow(Deciles)
      Deciles$MatchID = Deciles$ID - 1
      Deciles <- merge(Deciles,
                      Deciles,
                      by.x = c("ID"),
                      by.y = c("MatchID"))
      Deciles$DGroup <- paste(Deciles$Decile_Group.x, Deciles$Decile_Group.y, sep = " - ")
      
      for(i in 1:nrow(PWC_Properties_Hamilton_with_Addresses))
      {
         PWC_Properties_Hamilton_with_Addresses$Decile[i]  <- Deciles$DGroup[((PWC_Properties_Hamilton_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                              (PWC_Properties_Hamilton_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
         PWC_Properties_Hamilton_with_Addresses$DecileGroup[i] <- Deciles$ID[((PWC_Properties_Hamilton_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                              (PWC_Properties_Hamilton_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
      }  
      Frequency <- aggregate(DecileGroup ~ Decile,
                            data = PWC_Properties_Hamilton_with_Addresses,
                            FUN  = length,
                            na.action = NULL)
                            
      Hamilton_Decile_Stats <- merge(Deciles,
                                     Frequency,
                                     by.x = c("DGroup"),
                                     by.y = c("Decile"))

      ##
      ##    Draw a stratified random sample of 100 properties
      ##
         Auckland_Sample <- lapply(1:10, function(x){
            A_Decile <- PWC_Properties_Hamilton_with_Addresses[PWC_Properties_Hamilton_with_Addresses$DecileGroup == x,]
            return(A_Decile[sample(1:nrow(A_Decile), 10),])
         })
         Hamilton_Stratfied_Random_Sample <- do.call(rbind, Auckland_Sample)
         ##
         ##    Save the decile information so it can be recombined at the end.
         ##
            save(Hamilton_Decile_Stats, file = "Data_Intermediate/Hamilton_Decile_Stats.rda")
            save(Hamilton_Stratfied_Random_Sample, file = "Data_Intermediate/Hamilton_Stratfied_Random_Sample.rda")

            
      png("Graphical_Output/Hamilton_AllCoverage.png", h =29.7, w = 21.0 ,  res = 600, units = "cm")
            ggplot() + 
              geom_sf(data = TA_2019[TA_2019$TA2019_V_1 %in% c("Waikato District","Hamilton City","Waipa District"),],  fill = SenseColours("ReallyLightBlue")) +
              geom_sf(data = PWC_Properties_Hamilton_with_Addresses, colour ='yellow') +
              geom_sf(data = Hamilton_Stratfied_Random_Sample,  color = 'red', size = 2) +
                   theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SenseColours("LightBlue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 13,
                                                   family = "Open Sans Semibold",
                                                   margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 14, family = "Open Sans Light"),
                         plot.title    = element_text(size = 24, colour = SenseColours("Blue"),                   family = "Raleway SemiBold"),
                         plot.subtitle = element_text(size = 14, colour = SenseColours("LightBlue"), hjust = 0.5, family = "Raleway SemiBold"),
                         plot.caption  = element_text(size = 11, colour = SenseColours("Black"),     hjust = 1.0, family = "Open Sans"),
                         plot.tag      = element_text(size =  9, colour = SenseColours("Black"),     hjust = 0.0, face = "italic" ),
                         axis.title    = element_text(size = 14, colour = SenseColours("Blue")),
                         axis.text.x   = element_text(size = 12, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 12, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      dev.off() 
         
    png("Graphical_Output/Hamilton_Restricted_Coverage.png", h =29.7, w = 21.0 ,  res = 600, units = "cm")
            ggplot() + 
              geom_sf(data = TA_2019[TA_2019$TA2019_V_1 %in% c("Hamilton City"),],  fill = SenseColours("ReallyLightBlue")) +
              geom_sf(data = PWC_Properties_Hamilton_with_Addresses, colour = 'yellow') +
              geom_sf(data = Hamilton_Stratfied_Random_Sample,  color ='red' , size = 2) +
        coord_sf(xlim = c(1790712, 1807023),
                 ylim = c(5808500, 5825652),
                 expand = FALSE) +
                   theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SenseColours("LightBlue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 13,
                                                   family = "Open Sans Semibold",
                                                   margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 14, family = "Open Sans Light"),
                         plot.title    = element_text(size = 24, colour = SenseColours("Blue"),                   family = "Raleway SemiBold"),
                         plot.subtitle = element_text(size = 14, colour = SenseColours("LightBlue"), hjust = 0.5, family = "Raleway SemiBold"),
                         plot.caption  = element_text(size = 11, colour = SenseColours("Black"),     hjust = 1.0, family = "Open Sans"),
                         plot.tag      = element_text(size =  9, colour = SenseColours("Black"),     hjust = 0.0, face = "italic" ),
                         axis.title    = element_text(size = 14, colour = SenseColours("Blue")),
                         axis.text.x   = element_text(size = 12, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 12, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      dev.off() 
         
   ##
   ##    Tauranga
   ##
      load("Data_Spatial/PWC_Properties_Tauranga_with_Addresses.rda")
      Spatial_Catalogue <- readLAScatalog("e:/Tauranga/")

      Wonder <- st_as_sf(Spatial_Catalogue, crs = st_crs(PWC_Properties_Tauranga_with_Addresses))
      Wonder <- st_transform(Wonder, 2193 )
      PWC_Properties_Tauranga_with_Addresses <- PWC_Properties_Tauranga_with_Addresses[st_within(PWC_Properties_Tauranga_with_Addresses, st_union(Wonder),sparse = FALSE),]
      PWC_Properties_Tauranga_with_Addresses$Land_Area <- st_area(PWC_Properties_Tauranga_with_Addresses)

      Deciles <- data.frame(Value = quantile(PWC_Properties_Tauranga_with_Addresses$Land_Area, prob = seq(0, 1, length = 11), type = 5, na.rm = TRUE))
      Deciles$Decile_Group <- row.names(Deciles)
      Deciles$ID <- 1:nrow(Deciles)
      Deciles$MatchID = Deciles$ID - 1
      Deciles <- merge(Deciles,
                      Deciles,
                      by.x = c("ID"),
                      by.y = c("MatchID"))
      Deciles$DGroup <- paste(Deciles$Decile_Group.x, Deciles$Decile_Group.y, sep = " - ")
      
      for(i in 1:nrow(PWC_Properties_Tauranga_with_Addresses))
      {
         PWC_Properties_Tauranga_with_Addresses$Decile[i]  <- Deciles$DGroup[((PWC_Properties_Tauranga_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                              (PWC_Properties_Tauranga_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
         PWC_Properties_Tauranga_with_Addresses$DecileGroup[i] <- Deciles$ID[((PWC_Properties_Tauranga_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                              (PWC_Properties_Tauranga_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
      }  
      Frequency <- aggregate(DecileGroup ~ Decile,
                            data = PWC_Properties_Tauranga_with_Addresses,
                            FUN  = length,
                            na.action = NULL)
                            
      Tauranga_Decile_Stats <- merge(Deciles,
                                     Frequency,
                                     by.x = c("DGroup"),
                                     by.y = c("Decile"))

      ##
      ##    Draw a stratified random sample of 100 properties
      ##
         Auckland_Sample <- lapply(1:10, function(x){
            A_Decile <- PWC_Properties_Tauranga_with_Addresses[PWC_Properties_Tauranga_with_Addresses$DecileGroup == x,]
            return(A_Decile[sample(1:nrow(A_Decile), 10),])
         })
         Tauranga_Stratfied_Random_Sample <- do.call(rbind, Auckland_Sample)
         ##
         ##    Save the decile information so it can be recombined at the end.
         ##
            save(Tauranga_Decile_Stats, file = "Data_Intermediate/Tauranga_Decile_Stats.rda")
            save(Tauranga_Stratfied_Random_Sample, file = "Data_Intermediate/Tauranga_Stratfied_Random_Sample.rda")

      png("Graphical_Output/Tauranga_AllCoverage.png", h =29.7, w = 21.0 ,  res = 600, units = "cm")
            ggplot() + 
              geom_sf(data = TA_2019[TA_2019$TA2019_V_1 %in% c("Western Bay of Plenty District","Tauranga City"),],  fill = SenseColours("ReallyLightBlue")) +
              geom_sf(data = PWC_Properties_Tauranga_with_Addresses, colour ='yellow') +
              geom_sf(data = Tauranga_Stratfied_Random_Sample,  color = 'red', size = 1.5) +
#        coord_sf(xlim = c(1734082, 1778993),
#                 ylim = c(5881395, 5945464),
#                 datum = NA,
#                 expand = FALSE) +
                   theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SenseColours("LightBlue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 13,
                                                   family = "Open Sans Semibold",
                                                   margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 14, family = "Open Sans Light"),
                         plot.title    = element_text(size = 24, colour = SenseColours("Blue"),                   family = "Raleway SemiBold"),
                         plot.subtitle = element_text(size = 14, colour = SenseColours("LightBlue"), hjust = 0.5, family = "Raleway SemiBold"),
                         plot.caption  = element_text(size = 11, colour = SenseColours("Black"),     hjust = 1.0, family = "Open Sans"),
                         plot.tag      = element_text(size =  9, colour = SenseColours("Black"),     hjust = 0.0, face = "italic" ),
                         axis.title    = element_text(size = 14, colour = SenseColours("Blue")),
                         axis.text.x   = element_text(size = 12, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 12, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      dev.off() 
 


   ##
   ##    Wellington
   ##
      load("Data_Spatial/PWC_Properties_Wellington_with_Addresses.rda")
      Spatial_Catalogue <- readLAScatalog("e:/Wellington/")

      Wonder <- st_as_sf(Spatial_Catalogue, crs = st_crs(PWC_Properties_Wellington_with_Addresses))
      Wonder <- st_transform(Wonder, 2193 )
      PWC_Properties_Wellington_with_Addresses <- PWC_Properties_Wellington_with_Addresses[st_within(PWC_Properties_Wellington_with_Addresses, st_union(Wonder),sparse = FALSE),]
      PWC_Properties_Wellington_with_Addresses$Land_Area <- st_area(PWC_Properties_Wellington_with_Addresses)

      Deciles <- data.frame(Value = quantile(PWC_Properties_Wellington_with_Addresses$Land_Area, prob = seq(0, 1, length = 11), type = 5, na.rm = TRUE))
      Deciles$Decile_Group <- row.names(Deciles)
      Deciles$ID <- 1:nrow(Deciles)
      Deciles$MatchID = Deciles$ID - 1
      Deciles <- merge(Deciles,
                      Deciles,
                      by.x = c("ID"),
                      by.y = c("MatchID"))
      Deciles$DGroup <- paste(Deciles$Decile_Group.x, Deciles$Decile_Group.y, sep = " - ")
      
      for(i in 1:nrow(PWC_Properties_Wellington_with_Addresses))
      {
         PWC_Properties_Wellington_with_Addresses$Decile[i]  <- Deciles$DGroup[((PWC_Properties_Wellington_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                                (PWC_Properties_Wellington_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
         PWC_Properties_Wellington_with_Addresses$DecileGroup[i] <- Deciles$ID[((PWC_Properties_Wellington_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                                (PWC_Properties_Wellington_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
      }  
      Frequency <- aggregate(DecileGroup ~ Decile,
                            data = PWC_Properties_Wellington_with_Addresses,
                            FUN  = length,
                            na.action = NULL)
                            
      Wellington_Decile_Stats_Not_LH <- merge(Deciles,
                                       Frequency,
                                       by.x = c("DGroup"),
                                       by.y = c("Decile"))

      ##
      ##    Draw a stratified random sample of 100 properties
      ##
         Auckland_Sample <- lapply(1:10, function(x){
            A_Decile <- PWC_Properties_Wellington_with_Addresses[PWC_Properties_Wellington_with_Addresses$DecileGroup == x,]
            return(A_Decile[sample(1:nrow(A_Decile), 10),])
         })
         Wellington_Stratfied_Random_Sample_Not_LH <- do.call(rbind, Auckland_Sample)


         ##
         ##    Save the decile information so it can be recombined at the end.
         ##
            save(Wellington_Decile_Stats_Not_LH, file = "Data_Intermediate/Wellington_Decile_Stats_Not_LH.rda")
            save(Wellington_Stratfied_Random_Sample_Not_LH, file = "Data_Intermediate/Wellington_Stratfied_Random_Sample_Not_LH.rda")
            
            
      png("Graphical_Output/Wellington_City.png", h =29.7, w = 21.0 ,  res = 600, units = "cm")
            ggplot() + 
              geom_sf(data = TA_2019[TA_2019$TA2019_V_1 %in% c("Wellington City"),],  fill = SenseColours("ReallyLightBlue")) +
              geom_sf(data = PWC_Properties_Wellington_with_Addresses, colour ='yellow') +
              geom_sf(data = Wellington_Stratfied_Random_Sample_Not_LH,  color = 'red', size = 1.5) +
#        coord_sf(xlim = c(1734082, 1778993),
#                 ylim = c(5881395, 5945464),
#                 datum = NA,
#                 expand = FALSE) +
                   theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SenseColours("LightBlue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 13,
                                                   family = "Open Sans Semibold",
                                                   margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 14, family = "Open Sans Light"),
                         plot.title    = element_text(size = 24, colour = SenseColours("Blue"),                   family = "Raleway SemiBold"),
                         plot.subtitle = element_text(size = 14, colour = SenseColours("LightBlue"), hjust = 0.5, family = "Raleway SemiBold"),
                         plot.caption  = element_text(size = 11, colour = SenseColours("Black"),     hjust = 1.0, family = "Open Sans"),
                         plot.tag      = element_text(size =  9, colour = SenseColours("Black"),     hjust = 0.0, face = "italic" ),
                         axis.title    = element_text(size = 14, colour = SenseColours("Blue")),
                         axis.text.x   = element_text(size = 12, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 12, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      dev.off()     

            
            
   ##
   ##    Lower Hutt
   ##
      load("Data_Spatial/PWC_Properties_Wellington_with_Addresses.rda")
      Spatial_Catalogue <- readLAScatalog("e:/Lower_Hutt/")

      Wonder <- st_as_sf(Spatial_Catalogue, crs = st_crs(PWC_Properties_Wellington_with_Addresses))
      Wonder <- st_transform(Wonder, 2193 )
      PWC_Properties_Wellington_with_Addresses <- PWC_Properties_Wellington_with_Addresses[st_within(PWC_Properties_Wellington_with_Addresses, st_union(Wonder),sparse = FALSE),]
      PWC_Properties_Wellington_with_Addresses$Land_Area <- st_area(PWC_Properties_Wellington_with_Addresses)

      Deciles <- data.frame(Value = quantile(PWC_Properties_Wellington_with_Addresses$Land_Area, prob = seq(0, 1, length = 11), type = 5, na.rm = TRUE))
      Deciles$Decile_Group <- row.names(Deciles)
      Deciles$ID <- 1:nrow(Deciles)
      Deciles$MatchID = Deciles$ID - 1
      Deciles <- merge(Deciles,
                      Deciles,
                      by.x = c("ID"),
                      by.y = c("MatchID"))
      Deciles$DGroup <- paste(Deciles$Decile_Group.x, Deciles$Decile_Group.y, sep = " - ")
      
      for(i in 1:nrow(PWC_Properties_Wellington_with_Addresses))
      {
         PWC_Properties_Wellington_with_Addresses$Decile[i]  <- Deciles$DGroup[((PWC_Properties_Wellington_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                                (PWC_Properties_Wellington_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
         PWC_Properties_Wellington_with_Addresses$DecileGroup[i] <- Deciles$ID[((PWC_Properties_Wellington_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                                (PWC_Properties_Wellington_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
      }  
      Frequency <- aggregate(DecileGroup ~ Decile,
                            data = PWC_Properties_Wellington_with_Addresses,
                            FUN  = length,
                            na.action = NULL)
                            
      Wellington_Decile_Stats <- merge(Deciles,
                                       Frequency,
                                       by.x = c("DGroup"),
                                       by.y = c("Decile"))

      ##
      ##    Draw a stratified random sample of 100 properties
      ##
         Auckland_Sample <- lapply(1:10, function(x){
            A_Decile <- PWC_Properties_Wellington_with_Addresses[PWC_Properties_Wellington_with_Addresses$DecileGroup == x,]
            return(A_Decile[sample(1:nrow(A_Decile), 10),])
         })
         Wellington_Stratfied_Random_Sample <- do.call(rbind, Auckland_Sample)
         ##
         ##    Save the decile information so it can be recombined at the end.
         ##
            save(Wellington_Decile_Stats, file = "Data_Intermediate/Wellington_Decile_Stats.rda")
            save(Wellington_Stratfied_Random_Sample, file = "Data_Intermediate/Wellington_Stratfied_Random_Sample.rda")
        
   ##
   ##    Create pictures of coverage
   ##
      png("Graphical_Output/Lower_Hutt_Coverage.png", h =29.7, w = 21.0 ,  res = 600, units = "cm")
            ggplot() + 
              geom_sf(data = TA_2019[TA_2019$TA2019_V_1 ==  "Lower Hutt City",],  fill = SenseColours("ReallyLightBlue")) +
              geom_sf(data = PWC_Properties_Wellington_with_Addresses, colour = 'yellow') +
              geom_sf(data = Wellington_Stratfied_Random_Sample_Not_LH,  color = 'red', size = 2) +
#        coord_sf(xlim = c(1734082, 1778993),
#                 ylim = c(5881395, 5945464),
#                 datum = NA,
#                 expand = FALSE) +
                   theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SenseColours("LightBlue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 13,
                                                   family = "Open Sans Semibold",
                                                   margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 14, family = "Open Sans Light"),
                         plot.title    = element_text(size = 24, colour = SenseColours("Blue"),                   family = "Raleway SemiBold"),
                         plot.subtitle = element_text(size = 14, colour = SenseColours("LightBlue"), hjust = 0.5, family = "Raleway SemiBold"),
                         plot.caption  = element_text(size = 11, colour = SenseColours("Black"),     hjust = 1.0, family = "Open Sans"),
                         plot.tag      = element_text(size =  9, colour = SenseColours("Black"),     hjust = 0.0, face = "italic" ),
                         axis.title    = element_text(size = 14, colour = SenseColours("Blue")),
                         axis.text.x   = element_text(size = 12, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 12, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      dev.off() 


   ##
   ##    Create pictures of regional coverage
   ##
      png("Graphical_Output/Wellington_AllCoverage.png", h =29.7, w = 21.0 ,  res = 600, units = "cm")
            ggplot() + 
              geom_sf(data = TA_2019[TA_2019$TA2019_V_1 %in% c("Wellington City","Lower Hutt City","Porirua City","Upper Hutt City"),],  fill = SenseColours("ReallyLightBlue")) +
              geom_sf(data = PWC_Properties_Wellington_with_Addresses, colour = 'yellow') +
              geom_sf(data = Wellington_Stratfied_Random_Sample,  color = 'red', size = 2) +
              geom_sf(data = Wellington_Stratfied_Random_Sample_Not_LH,  color = 'red', size = 2) +
#        coord_sf(xlim = c(1734082, 1778993),
#                 ylim = c(5881395, 5945464),
#                 datum = NA,
#                 expand = FALSE) +
                   theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SenseColours("LightBlue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 13,
                                                   family = "Open Sans Semibold",
                                                   margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 14, family = "Open Sans Light"),
                         plot.title    = element_text(size = 24, colour = SenseColours("Blue"),                   family = "Raleway SemiBold"),
                         plot.subtitle = element_text(size = 14, colour = SenseColours("LightBlue"), hjust = 0.5, family = "Raleway SemiBold"),
                         plot.caption  = element_text(size = 11, colour = SenseColours("Black"),     hjust = 1.0, family = "Open Sans"),
                         plot.tag      = element_text(size =  9, colour = SenseColours("Black"),     hjust = 0.0, face = "italic" ),
                         axis.title    = element_text(size = 14, colour = SenseColours("Blue")),
                         axis.text.x   = element_text(size = 12, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 12, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      dev.off() 




        
   ##
   ##    Christchurch
   ##
      load("Data_Spatial/PWC_Properties_Christchurch_with_Addresses.rda")
      Spatial_Catalogue <- readLAScatalog("e:/Christchurch")

      Wonder <- st_as_sf(Spatial_Catalogue, crs = st_crs(PWC_Properties_Christchurch_with_Addresses))
      Wonder <- st_transform(Wonder, 2193 )
      PWC_Properties_Christchurch_with_Addresses <- PWC_Properties_Christchurch_with_Addresses[st_within(PWC_Properties_Christchurch_with_Addresses, st_union(Wonder),sparse = FALSE),]
      PWC_Properties_Christchurch_with_Addresses$Land_Area <- st_area(PWC_Properties_Christchurch_with_Addresses)

      Deciles <- data.frame(Value = quantile(PWC_Properties_Christchurch_with_Addresses$Land_Area, prob = seq(0, 1, length = 11), type = 5, na.rm = TRUE))
      Deciles$Decile_Group <- row.names(Deciles)
      Deciles$ID <- 1:nrow(Deciles)
      Deciles$MatchID = Deciles$ID - 1
      Deciles <- merge(Deciles,
                      Deciles,
                      by.x = c("ID"),
                      by.y = c("MatchID"))
      Deciles$DGroup <- paste(Deciles$Decile_Group.x, Deciles$Decile_Group.y, sep = " - ")
      
      for(i in 1:nrow(PWC_Properties_Christchurch_with_Addresses))
      {
         PWC_Properties_Christchurch_with_Addresses$Decile[i]  <- Deciles$DGroup[((PWC_Properties_Christchurch_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                                  (PWC_Properties_Christchurch_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
         PWC_Properties_Christchurch_with_Addresses$DecileGroup[i] <- Deciles$ID[((PWC_Properties_Christchurch_with_Addresses$Land_Area[i] >= Deciles$Value.x ) &
                                                                                  (PWC_Properties_Christchurch_with_Addresses$Land_Area[i] <= Deciles$Value.y ))]
      }  
      Frequency <- aggregate(DecileGroup ~ Decile,
                            data = PWC_Properties_Christchurch_with_Addresses,
                            FUN  = length,
                            na.action = NULL)
                            
      Christchurch_Decile_Stats <- merge(Deciles,
                                       Frequency,
                                       by.x = c("DGroup"),
                                       by.y = c("Decile"))

      ##
      ##    Draw a stratified random sample of 100 properties
      ##
         Auckland_Sample <- lapply(1:10, function(x){
            A_Decile <- PWC_Properties_Christchurch_with_Addresses[PWC_Properties_Christchurch_with_Addresses$DecileGroup == x,]
            return(A_Decile[sample(1:nrow(A_Decile), 10),])
         })
         Christchurch_Stratfied_Random_Sample <- do.call(rbind, Auckland_Sample)
         ##
         ##    Save the decile information so it can be recombined at the end.
         ##
            save(Christchurch_Decile_Stats, file = "Data_Intermediate/Christchurch_Decile_Stats.rda")
            save(Christchurch_Stratfied_Random_Sample, file = "Data_Intermediate/Christchurch_Stratfied_Random_Sample.rda")

   ##
   ##    Create pictures of coverage
   ##
      png("Graphical_Output/Christchurch_AllCoverage.png", h =29.7, w = 21.0 ,  res = 600, units = "cm")
            ggplot() + 
              geom_sf(data = TA_2019[TA_2019$TA2019_V_1 %in% c("Waimakariri District","Christchurch City","Selwyn District"),],  fill = SenseColours("ReallyLightBlue")) +
              geom_sf(data = PWC_Properties_Christchurch_with_Addresses, colour = 'yellow') +
              geom_sf(data = Christchurch_Stratfied_Random_Sample,  color = 'red', size = 2) +
#        coord_sf(xlim = c(1734082, 1778993),
#                 ylim = c(5881395, 5945464),
#                 datum = NA,
#                 expand = FALSE) +
                   theme_bw(base_size=12, base_family =  "Open Sans") %+replace%
                   theme(legend.title.align=0.5,
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SenseColours("LightBlue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 13,
                                                   family = "Open Sans Semibold",
                                                   margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                         panel.spacing = unit(1, "lines"),                                              
                         legend.text   = element_text(size = 14, family = "Open Sans Light"),
                         plot.title    = element_text(size = 24, colour = SenseColours("Blue"),                   family = "Raleway SemiBold"),
                         plot.subtitle = element_text(size = 14, colour = SenseColours("LightBlue"), hjust = 0.5, family = "Raleway SemiBold"),
                         plot.caption  = element_text(size = 11, colour = SenseColours("Black"),     hjust = 1.0, family = "Open Sans"),
                         plot.tag      = element_text(size =  9, colour = SenseColours("Black"),     hjust = 0.0, face = "italic" ),
                         axis.title    = element_text(size = 14, colour = SenseColours("Blue")),
                         axis.text.x   = element_text(size = 12, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 12, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      dev.off() 


