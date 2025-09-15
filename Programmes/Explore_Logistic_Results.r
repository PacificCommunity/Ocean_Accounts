##
##    Programme:  Explore_Logistic_Results.r
##
##    Objective:  This programme has estimate the expected relationship between 10m x 10m Sentinel-2
##                red/green/blue data and land use. How well did it do?
##
##                What's a test?
##                   - Pull down New Caledonia, and estimate the land use?
##                   - Pull down a shape file of New Caledonia, and colour it with estimated land use?
##                   - Pull down all of the member countries, and colour their land use?
##
##
##    Author:     James Hogan, Senior Marine Resource Economist, 8 September 2025
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
   ##
   ##    Load spatial data
   ##
      Countries_Dir <- "Data_Spatial/Pacific Coastlines/pacific_coastlines.shp"
      Countries <- st_read(dsn = Countries_Dir, layer = "pacific_coastlines")
      Countries <- st_rotate(Countries)
   ##
   ##    Load the test data
   ##
      load("Data_Spatial/Test_Set.rda")

      Countries <- st_union(st_crop(st_make_valid(Countries), c(ymin = -22.351142, xmin = 166.173569, ymax = -21.895303, xmax = 166.932652)))
      plot(Countries[,1])
      
      Test_Set <- st_crop(Test_Set, Countries)

      ggplot() + 
      geom_sf(data = Countries, size = 3) +
      geom_sf(data = Test_Set[Test_Set$Predicted_Value == 190,1], color = "red", fill = "Blue") + 
      coord_sf(xlim=c(166.0, 167),ylim=c(-22.0, -22.4))





      ##
      ##    Pull out New Caledonia
      ##
        # X <- unique(Countries$territory1)
        # X[order(X)]
        # plot(Countries[Countries$territory1 == "New Caledonia",])
        
      ggplot() + 
      geom_sf(data = Countries[Countries$territory1 == "New Caledonia",], size = 3) +
      geom_sf(data = Test_Set[Test_Set$Predicted_Value == 190,1], color = "red",   fill = "Blue") + 
      geom_sf(data = Test_Set[Test_Set$Predicted_Value == 50,1],  color = "green", fill = "Blue") +
      coord_sf(xlim=c(166.38, 166.61),ylim=c(-22.16, -22.32))

      ggplot() + 
      geom_sf(data = Countries[Countries$territory1 == "New Caledonia",], size = 3) +
      geom_sf(data = Test_Set[Test_Set$Predicted_Value == 190,1], color = "red", fill = "Blue") + 
      coord_sf(xlim=c(166.0, 167),ylim=c(-22.0, -22.4))



      +
        coord_sf(datum = NA) +
          labs(title = "Pacific VMS Data Generated Port Locations", 
             subtitle = paste0("\nCountry = ",i),
             caption = "FAME\nThe Pacific Community (SPC)") +
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
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
