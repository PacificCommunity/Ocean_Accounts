##
##    Programme:  Estimate_Shadow_Costs.r
##
##    Objective:  Uses R Selenium to get property values
##
##    Author:     James Hogan, 3 August 2021
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions or colour palattes, depending on what you're doing.
   ##
      source("R/themes.r")
      source("R/functions.r")
   ##
   ##    Load data from somewhere
   ##
      load("Data_Intermediate/Auckland_Decile_Stats.rda")
      load("Data_Intermediate/Hamilton_Decile_Stats.rda")
      load("Data_Intermediate/Tauranga_Decile_Stats.rda")
      load("Data_Intermediate/Christchurch_Decile_Stats.rda")
      load("Data_Intermediate/Wellington_Decile_Stats.rda")
      load("Data_Intermediate/Wellington_Decile_Stats_Not_LH.rda")
      
      load("Data_Intermediate/Auckland_Stratfied_Random_Sample.rda")
      load("Data_Intermediate/Hamilton_Stratfied_Random_Sample.rda")
      load("Data_Intermediate/Tauranga_Stratfied_Random_Sample.rda")
      load("Data_Intermediate/Christchurch_Stratfied_Random_Sample.rda")
      load("Data_Intermediate/Wellington_Stratfied_Random_Sample.rda")
      load("Data_Intermediate/Wellington_Stratfied_Random_Sample_Not_LH.rda")

      load("Data_Spatial/PWC_Properties_Auckland_with_Addresses.rda")
      load("Data_Spatial/PWC_Properties_Hamilton_with_Addresses.rda")
      load("Data_Spatial/PWC_Properties_Tauranga_with_Addresses.rda")
      load("Data_Spatial/PWC_Properties_Christchurch_with_Addresses.rda")
      load("Data_Spatial/PWC_Properties_Wellington_with_Addresses.rda")
      
      load("Data_Spatial/Auckland_Shadow_Costs.rda")
      load("Data_Spatial/Hamilton_Shadow_Costs.rda")
      load("Data_Spatial/Tauranga_Shadow_Costs.rda")
      load("Data_Spatial/Christchurch_Shadow_Costs.rda")
      load("Data_Spatial/Wellington_Shadow_Costs.rda")
      load("Data_Spatial/Wellington_Shadow_Costs_Not_LH.rda")

      #load("Data_spatial/ICARUS.rda")
      
   ##
   ##    Land size differences
   ##
      Auckland_Decile_Stats$Source <- "Auckland"
      Hamilton_Decile_Stats$Source <- "Hamilton"
      Tauranga_Decile_Stats$Source <- "Tauranga"
      Christchurch_Decile_Stats$Source <- "Christchurch"
      Wellington_Decile_Stats$Source   <- "Lower Hutt"
      Wellington_Decile_Stats_Not_LH$Source   <- "Wellington"

      Decile_Differences <- rbind(Auckland_Decile_Stats,
                                  Hamilton_Decile_Stats,
                                  Tauranga_Decile_Stats,
                                  Christchurch_Decile_Stats,
                                  Wellington_Decile_Stats_Not_LH,
                                  Wellington_Decile_Stats)
                                  
      Decile_Differences$Value <- as.numeric(Decile_Differences$Value.y)
                                 
      p <-  ggplot(Decile_Differences[Decile_Differences$DGroup != "90% - 100%" & (Decile_Differences$Source != "Lower Hutt"),],
                   aes(x = DGroup, y = Value, colour = Source, group = Source)) +
                   geom_line(size = 1.25)  +
                   geom_point(size = 3.25, alpha = 0.30)  +
                   scale_colour_manual(values = SenseColours()) + 
                   scale_fill_manual(values = SenseColours()) + 
            #       labs(title = "Average Property Land Area\n",  
            #            subtitle = "by Decile Group\n",
            #            caption = "Sense Partners\nData  Logic  Action") +
                   ylab("Land Size\n(m2)\n") +
                   xlab("\nDecile Group\n") +
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
                         axis.text.x   = element_text(size = 14, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 14, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      png(file='Graphical_Output/Average_Property_Land_Area.png', h =7.7, w = 14.57 ,  res = 600, units = "in")
         print(p)
      dev.off()                     
                   
   ##
   ##    Compare Differences
   ##
      Auckland_Shadow_Costs          <- st_drop_geometry(Auckland_Shadow_Costs)
      Hamilton_Shadow_Costs          <- st_drop_geometry(Hamilton_Shadow_Costs)
      Tauranga_Shadow_Costs          <- st_drop_geometry(Tauranga_Shadow_Costs)
      Christchurch_Shadow_Costs      <- st_drop_geometry(Christchurch_Shadow_Costs)
      Wellington_Shadow_Costs        <- st_drop_geometry(Wellington_Shadow_Costs)
      Wellington_Shadow_Costs_Not_LH <- st_drop_geometry(Wellington_Shadow_Costs_Not_LH)
      
      Auckland_Shadow_Costs <- merge(Auckland_Shadow_Costs,
                                     Auckland_Decile_Stats[,c("DGroup","ID")],
                                     by.x = c("DecileGroup"),
                                     by.y = c("ID"))
      names(Auckland_Shadow_Costs)[names(Auckland_Shadow_Costs) == "DGroup"] <- "Decile"
      
      Auckland_Shadow_Costs$Source <- "Auckland"
      Hamilton_Shadow_Costs$Source <- "Hamilton"
      Tauranga_Shadow_Costs$Source <- "Tauranga"
      Christchurch_Shadow_Costs$Source <- "Christchurch"
      Wellington_Shadow_Costs$Source   <- "Lower Hutt"
      Wellington_Shadow_Costs_Not_LH$Source   <- "Wellington"


      All_Data <- rbind(Auckland_Shadow_Costs, 
                        Hamilton_Shadow_Costs,
                        Tauranga_Shadow_Costs,
                        Christchurch_Shadow_Costs,
                        Wellington_Shadow_Costs_Not_LH,
                        Wellington_Shadow_Costs)
                        
      # All_Data <- ICARUS                 
      All_Data$Average_Shade_per_Year_With_Construction <- ifelse(All_Data$Average_Shade_per_Year_With_Construction < All_Data$Average_Existing_Shade_per_Year,  All_Data$Average_Existing_Shade_per_Year, All_Data$Average_Shade_per_Year_With_Construction)
      
      All_Data$Hours_Increase_in_Shade <- All_Data$Average_Shade_per_Year_With_Construction - All_Data$Average_Existing_Shade_per_Year

   ##
   ##    Compare Average shade increase
   ##
      Average_Sun <- with(All_Data,
                       aggregate(list(Average_Shade_per_Year_With_Construction = Average_Shade_per_Year_With_Construction,
                                      Average_Existing_Shade_per_Year = Average_Existing_Shade_per_Year,
                                      Average_Increase_in_Shade = (Average_Shade_per_Year_With_Construction - Average_Existing_Shade_per_Year),
                                      Average_Neighbour_Property_Values = Calculation_Price,
                                      Average_Shadow_Costs = Restimated_Shadow),
                               list(Source = Source,
                                    Decile =  Decile),
                               mean, 
                               na.rm = TRUE))   

   ##
   ##    Estimate the strata variance
   ##

      Variance_Sun <- with(All_Data,
                       aggregate(list(Variance_Shade_per_Year_With_Construction = Average_Shade_per_Year_With_Construction,
                                      Variance_Existing_Shade_per_Year = Average_Existing_Shade_per_Year,
                                      Variance_Increase_in_Shade = (Average_Shade_per_Year_With_Construction - Average_Existing_Shade_per_Year),
                                      Variance_Neighbour_Property_Values = Calculation_Price,
                                      Variance_Shadow_Costs = Restimated_Shadow),
                               list(Source = Source,
                                    Decile =  Decile),
                               var, 
                               na.rm = TRUE))   

      Plot_Me <- reshape2::melt(Average_Sun,
                                id.vars = c("Source", "Decile"),
                                measure.vars = c("Average_Shadow_Costs"))

      Plot_Me$value[is.nan(Plot_Me$value)] <- NA
      Plot_Me <- Plot_Me[,c('Source','Decile','value')]
      Plot_Me$Decile <- as.numeric(as.factor(Plot_Me$Decile))
     
      p <-  ggplot(Plot_Me[(Plot_Me$Source != "Lower Hutt"),],
                   aes(x = Decile, y = value, colour = Source)) +
                   #geom_point()  +
                   geom_smooth(se = FALSE, span = 1)  +
                   scale_colour_manual(values = SenseColours()) + 
                   scale_fill_manual(values = SenseColours()) + 
                   scale_y_continuous(breaks = seq(from = 1000, to = 25000, by =2000), labels = scales::dollar) +
                   scale_x_continuous(breaks = seq(from = 1, to = 10, by =1)) +
                   # labs(title = "Average Shadow Costs from New Construction\n",  
                        # subtitle = "by Decile Group\n",
                        # caption = "Sense Partners\nData  Logic  Action") +
                   ylab("Shade Cost\n") +
                   xlab("\nDecile Group\n") +
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
                         axis.text.x   = element_text(size = 14, colour = SenseColours("Blue"),  angle = 00),
                         axis.text.y   = element_text(size = 14, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      png(file='Graphical_Output/Average_Shade_Costs.png', h =7.7, w = 14.57 ,  res = 600, units = "in")
         print(p)
      dev.off()                                                    


      Plot_Me <- reshape2::melt(Average_Sun,
                                id.vars = c("Source", "Decile"),
                                measure.vars = c("Average_Neighbour_Property_Values"))

      Plot_Me$value[is.nan(Plot_Me$value)] <- NA
      Plot_Me <- Plot_Me[,c('Source','Decile','value')]
      Plot_Me$Decile <- as.numeric(as.factor(Plot_Me$Decile))

      p <-  ggplot(Plot_Me[(Plot_Me$Source != "Lower Hutt"),],
                   aes(x = Decile, y = value, colour = Source)) +
                   #geom_point()  +
                   geom_smooth(se = FALSE, span = 1)  +
                   scale_colour_manual(values = SenseColours()) + 
                   scale_fill_manual(values = SenseColours()) + 
                   scale_y_continuous(breaks = seq(from = 0, to = 3400000, by =300000), labels = scales::dollar) +
                   scale_x_continuous(breaks = seq(from = 1, to = 10, by =1)) +
                   # labs(title = "Average Neighbouring Property Values\n",  
                        # subtitle = "by Decile Group\n",
                        # caption = "Sense Partners\nData  Logic  Action") +
                   ylab("$Dollars\n") +
                   xlab("\nDecile Group\n") +
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
                         axis.text.x   = element_text(size = 14, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 14, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      png(file='Graphical_Output/Average_Neighbouring_Prices.png', h =7.7, w = 14.57 ,  res = 600, units = "in")
         print(p)
      dev.off()                                                    

      Plot_Me <- reshape2::melt(Average_Sun,
                                id.vars = c("Source", "Decile"),
                                measure.vars = c("Average_Increase_in_Shade"))

      Plot_Me$value[is.nan(Plot_Me$value)] <- NA
      Plot_Me <- Plot_Me[,c('Source','Decile','value')]
      Plot_Me$Decile <- as.numeric(as.factor(Plot_Me$Decile))

      p <-  ggplot(Plot_Me[(Plot_Me$Source != "Lower Hutt"),],
                   aes(x = Decile, y = value*60, colour = Source)) +
                   #geom_point()  +
                   geom_smooth(se = FALSE, span = 1)  +
                   scale_colour_manual(values = SenseColours()) + 
                   scale_fill_manual(values = SenseColours()) + 
                   scale_y_continuous(breaks = seq(from = 0, to = 30, by =1)) +
                   scale_x_continuous(breaks = seq(from = 1, to = 10, by =1)) +
                   # labs(title = "Average Shadow Costs\n",  
                        # subtitle = "by Decile Group\n",
                        # caption = "Sense Partners\nData  Logic  Action") +
                   ylab("Increased Shade Minutes\n") +
                   xlab("\nDecile Group\n") +
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
                         axis.text.x   = element_text(size = 14, colour = SenseColours("Blue"),  angle = 00),
                         axis.text.y   = element_text(size = 14, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      png(file='Graphical_Output/Average_Increase_in_Shade_Minutes.png', h =7.7, w = 14.57 ,  res = 600, units = "in")
         print(p)
      dev.off()                                                    


      Total_Costs <- with(All_Data,
                       aggregate(list(Shadow_Costs = Restimated_Shadow),
                               list(Source = Source,
                                    Decile =  Decile),
                               mean, 
                               na.rm = TRUE))   

      Total_Costs$Population_Weight <- as.numeric(ifelse(Total_Costs$Source == "Auckland", 2698, 
                                                   ifelse(Total_Costs$Source == "Hamilton", 388, 
                                                    ifelse(Total_Costs$Source == "Tauranga", 403,
                                                     ifelse(Total_Costs$Source == "Christchurch", 881,
                                                      ifelse(Total_Costs$Source == "Lower Hutt", 190,
                                                       ifelse(Total_Costs$Source == "Wellington", 268, NA)))))))

      Total_Costs_Plot <- with(Total_Costs,
                       aggregate(list(Shadow_Costs = Shadow_Costs * Population_Weight),
                               list(Source = Source,
                                    Decile =  Decile),
                               sum, 
                               na.rm = TRUE))   

      Plot_Me <- reshape2::melt(Total_Costs_Plot,
                                id.vars = c("Source", "Decile"),
                                measure.vars = c("Shadow_Costs"))

      Plot_Me$value[is.nan(Plot_Me$value)] <- NA
      Plot_Me <- Plot_Me[,c('Source','Decile','value')]
      Plot_Me$Decile <- as.numeric(as.factor(Plot_Me$Decile))

      p <-  ggplot(Plot_Me[(Plot_Me$Source != "Lower Hutt"),],
                   aes(x = Decile, y = value, colour = Source)) +
                   #geom_point()  +
                   geom_smooth(se = FALSE, span = 1)  +
                   scale_colour_manual(values = SenseColours()) + 
                   scale_fill_manual(values = SenseColours()) + 
                   scale_y_continuous(breaks = seq(from = 0, to = 50000000, by =2000000), labels = scales::dollar) +
                   scale_x_continuous(breaks = seq(from = 1, to = 10, by =1)) +
                   # labs(title = "Total Shadow Costs\n",  
                        # subtitle = "by Decile Group\n",
                        # caption = "Sense Partners\nData  Logic  Action") +
                   ylab("$Dollars\n") +
                   xlab("\nDecile Group\n") +
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
                         axis.text.x   = element_text(size = 14, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 14, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      png(file='Graphical_Output/Total_Shadow_Costs.png', h =7.7, w = 14.57 ,  res = 600, units = "in")
         print(p)
      dev.off()                                                    


      Total_City_Costs <- with(Total_Costs_Plot,
                       aggregate(list(Shadow_Costs = Shadow_Costs),
                               list(Source = Source),
                               sum, 
                               na.rm = TRUE))  
Total_City_Costs
sum(Total_City_Costs$Shadow_Cost)
   ##
   ##    Do a decomposition
   ##    Total Cost = f(Number_of_Neighours, Average_House_Price, Average_Section_Size, Population_Size        )
   ##
                                        
      Fixed_Cat <- unique(data.frame(Source      = All_Data$Source,
                                     DecileGroup = All_Data$DecileGroup,
                                     full_add_2  = All_Data$full_add_2,
                                     PWCs_Cl_ID  = All_Data$PWCs_Cl_ID,
                                     Neighbour_Price = All_Data$Calculation_Price,
                                     Count       = 1))
 Fixed_Cat[Fixed_Cat$PWCs_Cl_ID == 252154800,]
                                     
      
      Total_Number_of_Neighbours <- with(Fixed_Cat,
                                      aggregate(list(Total_Number_of_Neighbours = Count),
                                                list(Source = Source,
                                                     PWCs_Cl_ID = PWCs_Cl_ID,
                                                     DecileGroup = DecileGroup),
                                              sum, 
                                              na.rm = TRUE))  
                                              
      Total_Number_of_Neighbours$Number_of_PWCs <- 1

      Total_Number_of_Neighbours <- with(Total_Number_of_Neighbours,
                                      aggregate(list(Total_Number_of_Neighbours = Total_Number_of_Neighbours,
                                                     Total_Number_of_PWCs = Number_of_PWCs),
                                                list(Source = Source,
                                                     DecileGroup = DecileGroup),
                                              sum, 
                                              na.rm = TRUE))  
      Total_Number_of_Neighbours$Density <- Total_Number_of_Neighbours$Total_Number_of_Neighbours /10

                                              
      Average_Neighbour_Price <- with(Fixed_Cat,
                                      aggregate(list(Average_Neighbour_Price = Neighbour_Price),
                                                list(Source = Source,
                                                     DecileGroup = DecileGroup),
                                              mean, 
                                              na.rm = TRUE))  
                                        
      Fixed_Cat <- unique(data.frame(Source      = All_Data$Source,
                                     DecileGroup = All_Data$DecileGroup,
                                     PWCs_Cl_ID  = All_Data$PWCs_Cl_ID,
                                     Count       = 1))
                                     
      Total_Number_of_Target_Properties <- with(Fixed_Cat,
                                      aggregate(list(Total_Number_of_Targets = Count),
                                              list(Source = Source,
                                                   DecileGroup = DecileGroup),
                                              sum, 
                                              na.rm = TRUE))  
      Density <- merge(Total_Number_of_Target_Properties,
                       Total_Number_of_Neighbours,
                       by = c("Source", "DecileGroup"))
      Density$Density <- with(Density, (Total_Number_of_Neighbours / 10))


##
##    Bring it all together
##
   Analytical_Frame <- merge(Decile_Differences[, c("DGroup", "ID", "Value.x","DecileGroup", "Source")],
                             Average_Neighbour_Price,
                             by.x = c("Source", "ID"),
                             by.y = c("Source", "DecileGroup"))
   names(Analytical_Frame) = c("Source", "DecileGroup", "DGroup", "Average_Land_Size", "Target_Population_Size", "Average_Neighbour_Price")
   Analytical_Frame <- merge(Analytical_Frame,
                             Density[,c("Source", "DecileGroup","Density")],
                             by = c("Source", "DecileGroup"))
   Analytical_Frame <- merge(Analytical_Frame,
                             Total_Costs_Plot,
                             by.x = c("Source", "DGroup"),
                             by.y = c("Source", "Decile"))
   Analytical_Frame <- merge(Analytical_Frame,
                             Average_Sun[,c("Source", "Decile","Average_Increase_in_Shade")],
                             by.x = c("Source", "DGroup"),
                             by.y = c("Source", "Decile"))

   Analytical_Frame <- Analytical_Frame[,c("Shadow_Costs", "Average_Increase_in_Shade", "Density", "Average_Neighbour_Price", "Average_Land_Size", "Target_Population_Size", "Source", "DecileGroup", "DGroup")]
   Analytical_Frame$Average_Land_Size <- as.numeric(Analytical_Frame$Average_Land_Size)
   Analytical_Frame <- Analytical_Frame[order(Analytical_Frame$Source, Analytical_Frame$DecileGroup),]


      p <-  ggplot(Analytical_Frame[(Analytical_Frame$Source != "Lower Hutt"),],
                   aes(x = Density, y = Average_Increase_in_Shade, colour = Source)) +
                   geom_point()  +
                   geom_smooth(se = FALSE, method = 'lm')  +
                   scale_colour_manual(values = SenseColours()) + 
                   scale_fill_manual(values = SenseColours()) + 
                   # labs(title = "Total Shadow Costs\n",  
                        # subtitle = "by Decile Group\n",
                        # caption = "Sense Partners\nData  Logic  Action") +
                   xlab("Average Density\n") +
                   ylab("\nAverage Increase in Shade\n") +
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
                         axis.text.x   = element_text(size = 14, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 14, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
      png(file='Graphical_Output/Shadows_and_Density.png', h =7.7, w = 14.57 ,  res = 600, units = "in")
         print(p)
      dev.off()                                                    



      p <-  ggplot(Analytical_Frame[(Analytical_Frame$Source != "Lower Hutt"),],
                   aes(x = Density, y = Average_Neighbour_Price, colour = Source)) +
                   geom_point()  +
                   geom_smooth(se = FALSE, method = 'lm')  +
                   scale_colour_manual(values = SenseColours()) + 
                   scale_fill_manual(values = SenseColours()) + 
                   # labs(title = "Total Shadow Costs\n",  
                        # subtitle = "by Decile Group\n",
                        # caption = "Sense Partners\nData  Logic  Action") +
                   xlab("Density\n") +
                   ylab("\nAverage Neighbour Price\n") +
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
                         axis.text.x   = element_text(size = 14, colour = SenseColours("Blue"),  angle = 90),
                         axis.text.y   = element_text(size = 14, colour = SenseColours("Black"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(0, 0, 0, 0),
                         legend.position  = "bottom")
   
   ##
   ##    Of course, this regression is garbage because Average_Increase_in_Shade, Average_Neighbour_Price and Target_Population_Size completely explain all the variation in Shadow_Costs.
   ##       Thats how shadow costs are constructed
   ##
#   OLS <- lm(log(Shadow_Costs) ~ log(Average_Increase_in_Shade) + log(Density) + log(Average_Neighbour_Price) + log(Average_Land_Size) + log(Target_Population_Size),
#             data = Analytical_Frame)
#   summary(OLS)
   



   PWC_Properties_Auckland_with_Addresses$Source      = "Auckland"
   PWC_Properties_Christchurch_with_Addresses$Source  = "Christchurch"
   PWC_Properties_Hamilton_with_Addresses$Source      = "Hamilton"
   PWC_Properties_Tauranga_with_Addresses$Source      = "Tauranga"
   PWC_Properties_Wellington_with_Addresses$Source    = "Wellington"

   PWC_All <- rbind(PWC_Properties_Auckland_with_Addresses[,c('full_add_2','Cl_ID','Source')],
                    PWC_Properties_Christchurch_with_Addresses[,c('full_add_2','Cl_ID','Source')],
                    PWC_Properties_Hamilton_with_Addresses[,c('full_add_2','Cl_ID','Source')],
                    PWC_Properties_Tauranga_with_Addresses[,c('full_add_2','Cl_ID','Source')],
                    PWC_Properties_Wellington_with_Addresses[,c('full_add_2','Cl_ID','Source')])
   PWC_All$Land_Area <- st_area(PWC_All)
   PWC_All$Count = 1
   
   Source <- unique(PWC_All$Source)
   
   Larger_Population <- data.frame()
   
   Decile_Differences$Value.x[Decile_Differences$ID == 1] <- 0
   Decile_Differences$Value.y[Decile_Differences$ID == 10] <- 10000000
   
   for(a in 1:length(Source))
   {
      Hold_Me <- PWC_All[PWC_All$Source == Source[a],]

      for(i in 1:nrow(Hold_Me))
      {
         Hold_Me$DGroup[i] <- tryCatch({Decile_Differences$DGroup[((Decile_Differences$Source == Source[a]) &
                                                         (Hold_Me$Land_Area[i] >= Decile_Differences$Value.x ) &
                                                         (Hold_Me$Land_Area[i] <= Decile_Differences$Value.y ))]
                                       }, warning = function(w) {
                                                             }, error = function(e) {
                                                             }, finally = {
                                                             })
                                                             
         Hold_Me$DecileGroup[i] <- tryCatch({Decile_Differences$ID[((Decile_Differences$Source == Source[a]) &
                                                          (Hold_Me$Land_Area[i] >= Decile_Differences$Value.x ) &
                                                          (Hold_Me$Land_Area[i] <= Decile_Differences$Value.y ))]
                                            }, warning = function(w) {
                                                             }, error = function(e) {
                                                             }, finally = {
                                                             })
      }
      Larger_Population <- rbind(Larger_Population, Hold_Me)   
   }

      Frequency <- aggregate(DecileGroup ~ DGroup + Source,
                            data = Larger_Population,
                            FUN  = length,
                            na.action = NULL)
      ##
      ##    Replace the DecileGroup Column
      ##
         Decile_Differences <- Decile_Differences[,names(Decile_Differences) != 'DecileGroup']
         Decile_Differences <- merge(Decile_Differences,
                                     Frequency,
                                     by = c("DGroup", "Source"),
                                     all = TRUE)


##
##    Create some standard errors
##



   Statification_Stats <- merge(Decile_Differences[,c("Source","DGroup", "DecileGroup")],
                                Average_Sun[,c("Source","Decile", "Average_Shadow_Costs")],
                                by.x = c("Source","DGroup"),
                                by.y = c("Source","Decile"))

   Statification_Stats <- merge(Statification_Stats,
                                Variance_Sun[,c("Source","Decile", "Variance_Shadow_Costs")],
                                by.x = c("Source","DGroup"),
                                by.y = c("Source","Decile"))
                                
   Statification_Stats$Sample_Size <- 10

   Statification_Stats$Stratum_Total_Costs <- with(Statification_Stats, (Average_Shadow_Costs * DecileGroup))
   Statification_Stats$N2    <- with(Statification_Stats, (DecileGroup * DecileGroup))
   Statification_Stats$s2hnh <- with(Statification_Stats, (Variance_Shadow_Costs / Sample_Size))
   Statification_Stats$nhNh  <- with(Statification_Stats, (1 - (Sample_Size/DecileGroup)))
   Statification_Stats$Adjusted_Variance <- with(Statification_Stats, nhNh*N2*s2hnh)

   Survey_Stats <- with(Statification_Stats,
                    aggregate(list(Total_Shade_Costs = Stratum_Total_Costs,
                                   Adjusted_Variance = Adjusted_Variance),
                            list(Source = Source),
                            sum, 
                            na.rm = TRUE))  
   Survey_Stats$Adjusted_Variance <- sqrt( Survey_Stats$Adjusted_Variance)
   Survey_Stats$Upper_Bound <- Survey_Stats$Total_Shade_Costs + qt(.95, 1000000) * Survey_Stats$Adjusted_Variance
   Survey_Stats$Lower_Bound <- Survey_Stats$Total_Shade_Costs - qt(.95, 1000000) * Survey_Stats$Adjusted_Variance
   
   Survey_Stats <- Survey_Stats[,c("Source", "Lower_Bound", "Total_Shade_Costs", "Upper_Bound")]
   Survey_Stats <- Survey_Stats[order(-Survey_Stats$Total_Shade_Costs),]

   Survey_Stats <- rbind(Survey_Stats,
                         data.frame(Source = "All Main Urban Areas",
                                    Lower_Bound = sum(Survey_Stats[,2]),
                                    Total_Shade_Costs = sum(Survey_Stats[,3]),
                                    Upper_Bound = sum(Survey_Stats[,4])))
   ##
   ##    Adjust for Lower Hutt
   ##
   Survey_Stats$Lower_Bound[Survey_Stats$Source == 'Wellington'] <- Survey_Stats$Lower_Bound[Survey_Stats$Source == 'Wellington']  +
                                                                    Survey_Stats$Lower_Bound[Survey_Stats$Source == 'Lower Hutt'] 
   Survey_Stats$Total_Shade_Costs[Survey_Stats$Source == 'Wellington'] <- Survey_Stats$Total_Shade_Costs[Survey_Stats$Source == 'Wellington']  +
                                                                          Survey_Stats$Total_Shade_Costs[Survey_Stats$Source == 'Lower Hutt'] 
   Survey_Stats$Upper_Bound[Survey_Stats$Source == 'Wellington'] <- Survey_Stats$Upper_Bound[Survey_Stats$Source == 'Wellington']  +
                                                                    Survey_Stats$Upper_Bound[Survey_Stats$Source == 'Lower Hutt'] 
   Survey_Stats <- Survey_Stats[Survey_Stats$Source != 'Lower Hutt',]                                                              
   
   Survey_Stats$Lower_Bound <- format(Survey_Stats$Lower_Bound/1000000,big.mark   = ",", justify = c("centre"),digits = 3)
   Survey_Stats$Total_Shade_Costs <- format(Survey_Stats$Total_Shade_Costs/1000000,big.mark   = ",", justify = c("centre"),digits = 3)
   Survey_Stats$Upper_Bound <- format(Survey_Stats$Upper_Bound/1000000,big.mark   = ",", justify = c("centre"),digits = 3)
   
   
   names(Survey_Stats) = c('Urban Areas', 'Lower Bound (\\$Mill) 90% CI', 'Total Shadow Cost (\\$Mill)', 'Upper Bound (\\$Mill) 90% CI')
  
  
##
##    And Save
##
   save(Survey_Stats, file= "Data_Output/Survey_Stats.rda")
   save(Analytical_Frame, file = "Data_Output/Analytical_Frame.rda")
   save(Decile_Differences, file = "Data_Output/Decile_Differences.rda")
   save(All_Data, file = "Data_Output/All_Data.rda")

##
##    And we're done
##
