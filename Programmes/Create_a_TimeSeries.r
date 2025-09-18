##
##    Programme:  Create_a_TimeSeries.r
##
##    Objective:  What is this programme designed to do?
##
##    Author:     <PROGRAMMER>, <TEAM>, <DATE STARTED>
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
   ##    Load in the Models
   ##
      load("Data_Output/model_10.rda")
      load("Data_Output/model_11.rda")
      load("Data_Output/model_12.rda")
      load("Data_Output/model_20.rda")
      load("Data_Output/model_30.rda")
      load("Data_Output/model_40.rda")
      load("Data_Output/model_50.rda")
      load("Data_Output/model_80.rda")
      load("Data_Output/model_100.rda")
      load("Data_Output/model_110.rda")
      load("Data_Output/model_120.rda")
      load("Data_Output/model_121.rda")
      load("Data_Output/model_130.rda")
      load("Data_Output/model_150.rda")
      load("Data_Output/model_160.rda")
      load("Data_Output/model_170.rda")
      load("Data_Output/model_190.rda")
      load("Data_Output/model_210.rda")

   ##
   ##    Load in the physical data - about 17 mins per loop :(
   ##       Total area is 2770102579 [m^2] from shapefile
   ##
      Years <- c(2017, 2019:2024)
      Land_Cover_matrix <- lapply(Years, function(i)
                           {
                             tic(print(paste("Starting to process year", i)))
                             
                              Red   <- rast(paste0("Data_Spatial/dep_s2_geomad_red_",   i,".tif"))
                              Green <- rast(paste0("Data_Spatial/dep_s2_geomad_green_", i,".tif"))
                              Blue  <- rast(paste0("Data_Spatial/dep_s2_geomad_blue_",  i,".tif"))


                              Wonder <- data.frame(Red_Values   = Red, 
                                                   Green_Values = Green, 
                                                   Blue_Values  = Blue)
                              #Wonder <- Wonder[1:10000,]
                              names(Wonder) <- c("Red_Values", "Green_Values", "Blue_Values")
                              
                              Wonder$Predict_Is_11  <- stats::predict(model_11,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_12  <- stats::predict(model_12,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_20  <- stats::predict(model_20,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_30  <- stats::predict(model_30,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_40  <- stats::predict(model_40,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_50  <- stats::predict(model_50,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_80  <- stats::predict(model_80,  newdata = Wonder, type = "response")
                              Wonder$Predict_Is_100 <- stats::predict(model_100, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_110 <- stats::predict(model_110, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_120 <- stats::predict(model_120, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_121 <- stats::predict(model_121, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_130 <- stats::predict(model_130, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_150 <- stats::predict(model_150, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_160 <- stats::predict(model_160, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_170 <- stats::predict(model_170, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_190 <- stats::predict(model_190, newdata = Wonder, type = "response")
                              Wonder$Predict_Is_210 <- stats::predict(model_210, newdata = Wonder, type = "response")
                              
                           ##
                           ## Find the highest probability - this is memory efficient
                           ##
                              Values <- as.matrix(Wonder[, names(Wonder)[str_detect(names(Wonder), "Predict_Is_")]])
                              namez  <- as.numeric(str_replace_all(names(Wonder)[str_detect(names(Wonder), "Predict_Is_")], "Predict_Is_",""))

                              Land_Cover <- namez[max.col(Values)]
                              print(toc())
                              return(data.frame(table(Land_Cover)))
                           })
names(Land_Cover_matrix) <- Years                       
save(Land_Cover_matrix, file = "Data_Intermediate/Land_Cover_matrix.rda")                           
          
results <- do.call(rbind,lapply(names(Land_Cover_matrix), function(x){
                                X <- Land_Cover_matrix[[x]]
                                X$Year <- x
                                return(X)})
                  )
Peek <- reshape2::dcast(results,
                        Land_Cover ~ Year,
                        value.var = "Freq")
         
Peek <- merge(IPCC_Land_Categories,
              Peek,
              by = "Land_Cover")
              
Peek2 <- reshape2::melt(Peek,
                       id.vars = "IPCC_Classes",
                       measure.vars = c("2017","2019","2020","2021","2022","2023","2024"),
                       "Year")

Predicted <- with(Peek2,
              aggregate(list(value = value),
                        list(IPCC_Classes = IPCC_Classes,
                              Year = Year),
                      sum, 
                      na.rm = TRUE))
Predicted2 <- reshape2::dcast(Predicted,
                        IPCC_Classes ~ Year,
                        value.var = "value")

ggplot(Predicted, aes(x=as.numeric(as.character(Year)), y=value, colour=IPCC_Classes))     +
       geom_line(size =1, alpha = 0.6) +
       geom_point(size =2) +
       labs(title="Land Cover Estimate\n",
            subtitle = "Not Yet Accurate\n") +
       ylab("Number of 10m x 10m squares\n") +
       scale_colour_manual(values = SPCColours(), name="IPCC_Classes") +
       scale_y_continuous(labels = comma) +
       scale_x_continuous(breaks = seq(from = 2017, to = 2024, by =1)) +
       xlab("Time Period\n") +
       facet_wrap(~IPCC_Classes, scales="free") +
       theme_bw(base_size=12, base_family =  "Calibri") %+replace%
       theme(legend.title.align=0.5,
             plot.margin = unit(c(1,3,1,1),"mm"),
             panel.border = element_blank(),
             strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
             strip.text = element_text(colour = "white", 
                                       size   = 12,
                                       family = "MyriadPro-Bold",
                                       margin = margin(1.0,1.0,1.0,1.0, unit = "mm")),
             panel.spacing = unit(1, "lines"),                                              
             legend.text   = element_text(size = 14, family = "MyriadPro-Regular"),
             plot.title    = element_text(size = 20, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
             plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
             plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
             plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
             axis.title    = element_text(size = 16, colour = SPCColours("Dark_Blue")),
             axis.text.x   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
             axis.text.y   = element_text(size = 12, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
             legend.key.width = unit(1, "cm"),
             legend.spacing.y = unit(1, "cm"),
             legend.margin = margin(10, 10, 10, 10),
             legend.position  = "bottom")                        
            
      ggsave("Graphical_Output/Experimental_Land_Area.png", height =(2)*10.13, width = (1.5)*20.66, dpi = 300, units = c("cm"))      
                        
##
## what's the spatial area?
##
      Countries_Dir <- "Data_Spatial/Pacific Coastlines/pacific_coastlines.shp"
      Countries <- st_read(dsn = Countries_Dir, layer = "pacific_coastlines")
      Countries <- st_rotate(Countries)
      New_Caledonia <- st_union(st_crop(st_make_valid(Countries[Countries$territory1 == "New Caledonia",]), c(ymin = -22.351142, xmin = 166.173569, ymax = -21.895303, xmax = 166.932652)))
      st_area(New_Caledonia)
      # plot(New_Caledonia[,1])
                       
   ##
   ## Find the highest probability - this needs to be more memory efficient
   ##

      Wonder$Predicted_Value = NA
      
      Values <- as.matrix(Wonder[, names(Wonder)[str_detect(names(Wonder), "Predict_Is_")]])
      namez  <- as.numeric(str_replace_all(names(Wonder)[str_detect(names(Wonder), "Predict_Is_")], "Predict_Is_",""))
      
      Wonder$Predicted_Value <- apply(Values, 1, function(x){
                                        namez[which(x == max(x))]
                                        })
      Wonder$Area <- 10
      


##
##
##      
      
      
      

   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
