##
##    Programme:  Scale_Colours_to_Common_255.r
##
##    Objective:  It turns out that the red / green / blue can have over 9000 different shades. 
##                This programme scales all the different shades back to a common 0 - 255 range which is calibrated to each red / green / blue colour.
##
##    Author:     James Hogan, Senior Marine Resource Economist, 22 October 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Read in the ESA data
   ##
      ESA       <- raster("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
      getValues(ESA, row=10)[1:10]
                     
   ##
   ##    Nice... the individual layers can range in colour from 1 - over 7000... :-(  This needs scaled back. grrr
   ##
   
      # Sentinel_blue  <- raster("Data_Spatial/_blue_20177.tif")
      # Sentinel_green <- raster("Data_Spatial/_green_20177.tif")
      # Sentinel_red   <- raster("Data_Spatial/_red_20177.tif")
      
      # X <- data.frame(freq(Sentinel_blue))
      # Z <- getValues(Sentinel_blue)
      
      # summary(getValues(Sentinel_blue))
      # summary(getValues(Sentinel_green))
      # summary(getValues(Sentinel_red))


##
##    Read in all of the tiffs so I can pull out all of their colour values.
##   
      Contents <- as.data.frame(list.files(path = "Data_Spatial/",  pattern = "*.tif"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents <- Contents[str_detect(Contents$DataFrames, "_red_") | str_detect(Contents$DataFrames, "_green_") | str_detect(Contents$DataFrames, "_blue_"),]
      Contents$Country <- str_replace_all(str_replace_all(str_replace_all(Contents$Dframe, "_red_", ""), "_green_", ""), "_blue_","")
      Contents$Year    <- Contents$Country
      Contents$Country <- str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(Contents$Country, "2017", ""), "2018", ""), "2019",""), "2020",""), "2021",""), "2022",""), "2023",""), "2024","")
      Contents$Year    <- str_sub(Contents$Year, start = 1, end = str_length(Contents$Year) - str_length(Contents$Country))
      Contents$Colour  <- ifelse(str_detect(Contents$Dframe, "_red_"),1,
                          ifelse(str_detect(Contents$Dframe, "_green_"),2,3))
      
      Contents$Country <- as.numeric(Contents$Country)
      Contents$Year    <- as.numeric(Contents$Year)
      
      Contents <- Contents[order(Contents$Country,Contents$Year),]
      rownames(Contents) <- NULL

      All_Data <- lapply(1:nrow(Contents), function(File){
                           tryCatch({#print(Contents$DataFrames[File])
                                       X <- raster(paste0("Data_Spatial/", Contents$DataFrames[File]))
                                       Z <- data.frame(freq(X))
                                       Z$Country <- Contents$Country[File]
                                       Z$Year    <- Contents$Year[File]
                                       Z$Colour  <- Contents$Colour[File]
                                       rm(list=c("X"))
                                       return(Z[!is.na(Z$value),])
                                    },
                                warning = function(w) {}, 
                                error   = function(e) {NULL}, 
                                finally = {})})
      Colour_Breadth <- do.call(rbind, All_Data)
      
      Colour_Frequency <- with(Colour_Breadth,
                             aggregate(list(count = count),
                                       list(value = value,
                                            Colour  = Colour),    # I've made a deliberate choice to drop the country measure so the colour scales are country invariant
                                       sum,
                                       na.rm = TRUE))

      for(Colour in 1:3)
      {
            Frequency_Data <- Colour_Frequency[Colour_Frequency$Colour == Colour,]

            Frequency_Data$Cumulative_Total <- Frequency_Data$count[1]      
                                     
            for(i in 2:nrow(Frequency_Data))
            {
               Frequency_Data$Cumulative_Total[i] <- Frequency_Data$Cumulative_Total[(i-1)] + Frequency_Data$count[i]
            }
            Frequency_Data$Proportion <- Frequency_Data$Cumulative_Total / sum(Frequency_Data$count)      


            Deciles <- data.frame(Value = seq(from = 0, to = 1, by = 1/255))
            Deciles$Decile_Group <- as.numeric(row.names(Deciles))-1
            Deciles$ID <- 1:nrow(Deciles)
            Deciles$MatchID = Deciles$ID - 1
            Deciles <- merge(Deciles,
                            Deciles,
                            by.x = c("ID"),
                            by.y = c("MatchID"))
            Deciles$DGroup <- paste(Deciles$Decile_Group.x, Deciles$Decile_Group.y, sep = " - ")
            
            for(i in 1:nrow(Frequency_Data))
            {
               Frequency_Data$Decile[i] <- Deciles$DGroup[((Frequency_Data$Proportion[i] >= Deciles$Value.x ) &
                                                           (Frequency_Data$Proportion[i] <= Deciles$Value.y ))]
                                                           
               Frequency_Data$DecileGroup[i] <- Deciles$ID[((Frequency_Data$Proportion[i] >= Deciles$Value.x ) &
                                                            (Frequency_Data$Proportion[i] <= Deciles$Value.y ))]
            }  
            
            Frequency <- aggregate(DecileGroup ~ Decile,
                                   data = Frequency_Data,
                                   FUN  = length,
                                   na.action = NULL)
                                  
            Test_Decile_Stats <- merge(Deciles,
                                       Frequency,
                                       by.x = c("DGroup"),
                                       by.y = c("Decile"))
            Test_Decile_Stats <- Test_Decile_Stats[order(Test_Decile_Stats$ID),]

            Range <- with(Frequency_Data,
                       aggregate(list(value  = value ),
                                 list(DecileGroup = DecileGroup),
                               range, 
                               na.rm = TRUE))
            if(Colour == 1) {
               Range_Red       <- Range
               DecileStats_Red <- Test_Decile_Stats
            } else if(Colour == 2) {
               Range_Green       <- Range
               DecileStats_Green <- Test_Decile_Stats
            } else {
               Range_Blue       <- Range
               DecileStats_Blue <- Test_Decile_Stats
            }

      }

   save(Range_Red,   file = 'Data_Intermediate/Range_Red.rda')
   save(Range_Green, file = 'Data_Intermediate/Range_Green.rda')
   save(Range_Blue,  file = 'Data_Intermediate/Range_Blue.rda')

   save(DecileStats_Red,   file = 'Data_Intermediate/DecileStats_Red.rda')
   save(DecileStats_Green, file = 'Data_Intermediate/DecileStats_Green.rda')
   save(DecileStats_Blue,  file = 'Data_Intermediate/DecileStats_Blue.rda')



##
##    And we're done
##

