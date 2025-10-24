##
##    Programme:  Match_ESA_to_Sentinel_At_Scale.r
##
##    Objective:  I've downloaded all of the Sentinel-2 spatial data, now I need to 
##                match that to the overarching ESA data, at scale for all the countries.
##
##                Ok... how to..?
##
##                Options: 
##                   (1) Take all of the ESA data, and express the pacific as a series of extents
##                       with x-min, x-max, y-min, y-max values. Each extent inherits the Land Cover value.
##                       REFINEMENT: this could just be the x-min and y-min values since the x-max and y-max 
##                       values are just the next set of x-min and y-min values up. That creates a three dimensional array
##                       of x-min, y-min, land cover value.
##
##                       Take the Sentinel-2 data and do the same exercise. This will create a six dimensional array which
##                       will be data intensive... I can reduce the RGB back to a single (16581375 x 3) matrix as a master
##                       look-up table. Then, all of the Sentinel-2 data can be stored in another three dimensional array 
##                       of x-min, y-min, lookup index.
##
##                       Complications - I think I need to express the 300m x 300m and the 10m x 10m as lat/longs to make 
##                       the location spatially compatible.
##
##                       I feel there's a trick with SQL which can help here (because its set-based langauge)... 
##
##    Author:     James Hogan, Senior Marine Resource Economist, 22 October 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Read in the spatial data
   ##
      load("Data_Spatial/EEZ_All.rda")
      load("Data_Spatial/BBX.rda")
      load("Data_Spatial/Countries.rda")
      
      ESA       <- raster("Data_Spatial/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
      getValues(ESA, row=10)[1:10]
                     
   ##
   ##    Nice... the individual layers can range in colour from 1 - over 7000... :-(  This needs scaled back. grrr
   ##
      ##
      ##    Steal data from the shorelines project
      ##
         g <- geopackage("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg")
         Shorelines <- gpkg_table(g, "shorelines_annual")
         Country_Codes = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select distinct eez_territory
                                                                                        from shorelines_annual")
         ##
         ##    eez_territory turns out to be an ISO code
         ##
         
         Country_Codes <-  merge(Country_Codes,
                                 unique(st_drop_geometry(EEZ_All[,c("TERRITORY1", "ISO_TER1")])),
                                 by.x = "eez_territory",
                                 by.y = "ISO_TER1",
                                 all.x = TRUE)  
         Country_Codes <- Country_Codes[!is.na(Country_Codes$eez_territory),]
         Country_Codes <- Country_Codes[Country_Codes$eez_territory != "KIR",]
         Country_Codes <- rbind(Country_Codes,
                                data.frame(eez_territory = "KIR",
                                           TERRITORY1 = "Kiribati"))
         
         Country_Mapping <- unique(st_drop_geometry(Countries[,c("NAME_EN", "ISO_A3", "Polygon_ID")]))

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
      
      Contents <- merge(Contents,
                        Country_Mapping,
                        by.x = "Country",
                        by.y = "Polygon_ID")
      
      Contents <- Contents[order(Contents$Country,Contents$Year),]
      rownames(Contents) <- NULL







#      All_Data <- lapply(1:nrow(Contents), function(File){
      All_Data <- lapply(1:200, function(File){
      
      
                           tryCatch({
                           
File <- 337                           
print(Contents$DataFrames[File])
X <- rast(paste0("Data_Spatial/", Contents$DataFrames[File]))

##
##    What can we do about the shorelines?
##
   g <- geopackage("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg")
   Shorelines <- gpkg_table(g, "shorelines_annual")
   Country_Codes = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select distinct eez_territory
                                                                                  from shorelines_annual")
                                                                                  
   Country_Codes <-  merge(Country_Codes,
                           unique(EEZ_All[,c("TERRITORY1", "ISO_TER1")]),
                           by.x = "eez_territory",
                           by.y = "ISO_TER1",
                           all.x = TRUE)
   Country_Mapping <- unique(st_drop_geometry(Countries[,c("NAME_EN", "Polygon_ID")]))


      

   Fiji = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select * 
                                                                         from shorelines_annual
                                                                         where eez_territory = 'FJI'")

plot(Fiji[Fiji$certainty == 'good',2])
plot(Fiji[(Fiji$certainty == 'good') & (Fiji$year == 2023),2])
plot(Fiji[(Fiji$year == 2023),2])

Fiji <- st_transform(Fiji, st_crs(EEZ_All))

Closest_Addresses <- st_intersection(Fiji, st_make_valid(EEZ_All))



French_Polynesia_Maybe = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select * 
                                                                                        from shorelines_annual
                                                                                        where eez_territory = 'PYF'")

French_Polynesia_Maybe <- st_transform(French_Polynesia_Maybe, st_crs(EEZ_All))

Closest_Addresses <- st_intersection(st_bbox(French_Polynesia_Maybe), st_make_valid(EEZ_All))







plot(Closest_Addresses[Closest_Addresses$certainty == 'good',2])

test <- st_make_valid(EEZ)



Country_Poly <- as.polygons(X, round=FALSE, na.all = TRUE, na.rm=FALSE)
Country_sf <- st_as_sf(as.polygons(Country_Poly))

X <- st_rotate(st_intersection(Country_sf, st_make_valid(EEZ)))
                                    

                                    
                                       
                                       
                                       
                                       rm(list=c("X"))
                                       return(Z[!is.na(Z$value),])
                                    },
                                warning = function(w) {}, 
                                error   = function(e) {NULL}, 
                                finally = {})})
      Colour_Breadth <- do.call(rbind, All_Data)
      Colour_Breadth
      
      
      Colour_Frequency <- with(Colour_Breadth,
                             aggregate(list(count = count),
                                       list(value = value,
                                            Colour  = Colour),
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

save(Range_Red, file = 'Data_Intermediate/Range_Red.rda')
save(Range_Green, file = 'Data_Intermediate/Range_Green.rda')
save(Range_Blue, file = 'Data_Intermediate/Range_Blue.rda')

save(DecileStats_Red, file = 'Data_Intermediate/DecileStats_Red.rda')
save(DecileStats_Green, file = 'Data_Intermediate/DecileStats_Green.rda')
save(DecileStats_Blue, file = 'Data_Intermediate/DecileStats_Blue.rda')






   ##
   ##    Put them on the same CRS
   ##
      crs(Sentinel_blue) <- crs(ESA)




   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##



matrix(data = values(r), nrow = 18, ncol = 36, byrow = TRUE)



r <- raster(ncols=36, nrows=18)
values(r) <- rnorm(ncell(r)) *3
breaks <- -2:2 * 3
rc <- cut(r, breaks=breaks)


