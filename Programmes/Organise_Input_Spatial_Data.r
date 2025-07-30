##
##    Programme: Organise_Input_Data.r
##
##    Objective:  EEZ shapes sourced from https://www.marineregions.org/downloads.php
##                and countries data came from here: https://datacatalog.worldbank.org/search/dataset/0038272
##
##    Author:      James Hogan, FAME - The Pacific Community (SPC)
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions
   ##
      source("R/functions.r")
      source("R/themes.r")
   ##
   ##    Read in the EEZs and ocean shapes
   ##
      EEZ_Dir       <- "Data_Spatial/World_EEZ_v12_20231025/eez_v12.shp"
      Countries_Dir <- "Data_Spatial/WB_countries_Admin0_10m/WB_countries_Admin0_10m.shp"

      ##
      ##   Load the EEZ and Ocean spatial files data
      ##
         #st_layers(Countries_Dir)
         EEZ       <- st_read(dsn = EEZ_Dir, layer = "eez_v12")
         Countries <- st_read(dsn = Countries_Dir, layer = "WB_countries_Admin0_10m")
         
         EEZ       <- st_make_valid(EEZ)
         Countries <- st_make_valid(Countries)
         
         EEZ$GEONAME[is.na(EEZ$GEONAME)] <- EEZ$TERRITORY1[is.na(EEZ$GEONAME)]
         Countries$FORMAL_EN[is.na(Countries$FORMAL_EN)] <- Countries$NAME_EN[is.na(Countries$FORMAL_EN)]
         
         
         EEZ_Size <- with(EEZ,
                       aggregate(list(EEZ_Area = st_area(EEZ)/(1000*1000)),
                                 list(Country = ifelse(str_detect(GEONAME,"Kiribati"), "Kiribati", 
                                                ifelse(str_detect(GEONAME,"Guam"), "Guam", 
                                                ifelse(str_detect(GEONAME,"Comores"), "Comoros", 
                                                ifelse(str_detect(GEONAME,"Falkland / Malvinas Islands"), "Falkland Islands", 
                                                ifelse(str_detect(GEONAME,"Northern Mariana Islands"), "Northern Mariana Islands", 
                                                ifelse(str_detect(GEONAME,"French Exclusive Economic Zone (New Caledonia)"), "New Caledonia", 
                                                ifelse(str_detect(GEONAME,"American Samoa"), "American Samoa", 
                                                ifelse(str_detect(GEONAME,"Republic of Mauritius"), "Mauritius", 
                                                ifelse(str_detect(GEONAME,"Tokelau"), "Tokelau", 
                                                ifelse(str_detect(GEONAME,"United States Exclusive Economic Zone"), "United States of America", 
                                                ifelse(str_detect(GEONAME,"Pitcairn"), "Pitcairn Islands", TERRITORY1)))))))))))),
                                 sum, 
                                 na.rm = TRUE))         

         Countries$CONTINENT <- ifelse(Countries$CONTINENT == "Seven seas (open ocean)", "Seven Seas*", str_wrap(Countries$CONTINENT, 8))

         Countries[Countries$CONTINENT == "Seven seas",]
         
         Land_Size <- with(Countries,
                       aggregate(list(Land_Area = st_area(Countries)/(1000*1000)),
                                 list(Continent = CONTINENT,
                                      Country = ifelse(str_detect(FORMAL_EN,"Federated States of Micronesia"), "Micronesia", 
                                                ifelse(str_detect(FORMAL_EN,"United States"), "United States of America",
                                                ifelse(str_detect(FORMAL_EN,"People's Republic of China"), "China",
                                                ifelse(str_detect(FORMAL_EN,"Federal Republic of Somalia"), "Somalia",                                                
                                                ifelse(str_detect(FORMAL_EN,"Faroe Islands"), "Faeroe",
                                                ifelse(str_detect(FORMAL_EN,"Sint Maarten"), "Sint-Maarten", NAME_EN))))))),
                               sum, 
                               na.rm = TRUE))        

         Land_to_EEZ <- merge(Land_Size,
                              EEZ_Size,
                              by = "Country",
                              all = TRUE)
         Land_to_EEZ$Ratio <- as.numeric(Land_to_EEZ$Land_Area / Land_to_EEZ$EEZ_Area)

         write.table(Land_to_EEZ, file = "Data_Output/Land to EEZ size.csv", row.names = FALSE, sep =",")
         Plot_Me <- Land_to_EEZ[!is.na(Land_to_EEZ$Ratio),]
         
         
         
         # showtext_begin()
         # ggplot(Plot_Me, 
                # aes(fill = Continent,
                    # x = Ratio)) +
                # scale_x_log10() +
                # geom_density(alpha = 0.70, linetype = 2, bw=0.2) +         
                
                # scale_y_continuous(labels = percent) +                
                # scale_y_continuous(labels = comma) +                
                
                # xlab("Ratio of Land Area to EEZ\nLog") +
                # ylab("\nRelative Frequency") +                
                # labs(title="Difference in Geographies - PICTS and Non PICTS\n")  +         
         
                # theme_bw(base_size=12, base_family =  "Calibri") %+replace%
                # theme(legend.title.align=0.5,
                      # plot.margin = unit(c(1,3,1,1),"mm"),
                      # panel.border = element_blank(),
                      # strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                      # strip.text = element_text(colour = "white", 
                                                # size   = 13,
                                                # family = "MyriadPro-Bold",
                                                # margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                      # panel.spacing = unit(1, "lines"),                                              
                      # legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                      # plot.title    = element_text(size = 44, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                      # plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                      # plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                      # plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                      # axis.title    = element_text(size = 24, colour = SPCColours("Dark_Blue")),
                      # axis.text.x   = element_text(size = 22, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                      # axis.text.y   = element_text(size = 22, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                      # legend.key.width = unit(1, "cm"),
                      # legend.spacing.y = unit(1, "cm"),
                      # legend.margin = margin(10, 10, 10, 10),
                      # legend.position  = "bottom")
               
         # ggsave("Graphical_Output/Relative Land Area to EEZ.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))

   
         showtext_begin()
         ggplot(Plot_Me, 
                aes(fill = Continent,
                    y = Continent,
                    x = Ratio)) +
                geom_density_ridges(jittered_points = TRUE) +         
                geom_density_ridges(alpha=0.3,
                                    quantile_lines = TRUE,
                                    quantiles = 2,
                                    linewidth = 1.5,
                                    colour = SPCColours("Red"),
                                    linetype = 1) +         
                geom_density_ridges(alpha=0.5,
                                    linewidth = 1,
                                    colour = "white") +         
                scale_fill_manual(values = c(SPCColours("Light_Blue"), 
                                             SPCColours("Light_Blue"),
                                             SPCColours("Light_Blue"),
                                             SPCColours("Light_Blue"),
                                             SPCColours("Gold"),
                                             SPCColours("Light_Blue"),
                                             SPCColours("Light_Blue"))) +
                scale_x_log10() +
                xlab("\nRatio of Land Area to EEZ\nLog Scale\n") +
                ylab("") +      
                theme_bw(base_size=12, base_family =  "Calibri") %+replace%
                theme(legend.title.align=0.5,
                      plot.margin = unit(c(6,6,6,6),"mm"),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      plot.background  = element_blank(),
                      strip.background = element_blank(),
#                      panel.background = element_rect(fill = "grey90", colour = "black", linewidth = 1),
#                      plot.background  = element_rect(fill = "grey90", colour = "black", linewidth = 1),
#                      strip.background = element_rect(fill = SPCColours("Light_Blue")),
                      strip.text = element_text(colour = "white", 
                                                size   = 13,
                                                family = "MyriadPro-Bold",
                                                margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                      panel.spacing = unit(1, "lines"),                                              
                      legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                      plot.title    = element_text(size = 44, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                      plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                      plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                      plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                      axis.title    = element_text(size = 20, colour = SPCColours("Dark_Blue")),
                      axis.text.x   = element_text(size = 20, colour = SPCColours("Dark_Blue"), angle = 00, hjust = 0.5),
                      axis.text.y   = element_text(size = 20, colour = SPCColours("Dark_Blue"), angle = 00, hjust = 1.0),
                      legend.key.width = unit(1, "cm"),
                      legend.spacing.y = unit(1, "cm"),
                      legend.margin = margin(10, 10, 10, 10),
                      legend.position  = "none")
               
         ggsave("Graphical_Output/Relative Land Area to EEZ.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))
   ##
   ## And we're done
   ##
