##
##    Programme: Organise_Input_Data.r
##
##    Objective:  EEZ shapes sourced from https://www.marineregions.org/downloads.php
##
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
                                                ifelse(str_detect(GEONAME,"Northern Mariana Islands"), "Northern Mariana Islands", 
                                                ifelse(str_detect(GEONAME,"French Exclusive Economic Zone (New Caledonia)"), "New Caledonia", 
                                                ifelse(str_detect(GEONAME,"American Samoa"), "American Samoa", 
                                                ifelse(str_detect(GEONAME,"Republic of Mauritius"), "Mauritius", 
                                                ifelse(str_detect(GEONAME,"Tokelau"), "Tokelau", 
                                                ifelse(str_detect(GEONAME,"United States Exclusive Economic Zone"), "United States of America", 
                                                ifelse(str_detect(GEONAME,"Pitcairn"), "Pitcairn Islands", TERRITORY1)))))))))),
                                 sum, 
                                 na.rm = TRUE))         

         Land_Size <- with(Countries,
                       aggregate(list(Land_Area = st_area(Countries)/(1000*1000)),
                                 list(Country = ifelse(str_detect(FORMAL_EN,"Federated States of Micronesia"), "Micronesia", 
                                                ifelse(str_detect(FORMAL_EN,"United States"), "United States of America",
                                                ifelse(str_detect(FORMAL_EN,"Sint Maarten"), "Sint-Maarten", NAME_EN)))),
                               sum, 
                               na.rm = TRUE))        

         Land_to_EEZ <- merge(Land_Size,
                              EEZ_Size,
                              by = "Country",
                              all = TRUE)
         Land_to_EEZ$Ratio <- as.numeric(Land_to_EEZ$Land_Area / Land_to_EEZ$EEZ_Area)
         Land_to_EEZ$Pacific <- ifelse(Land_to_EEZ$Country %in% c("American Samoa","French Polynesia","Guam","Papua New Guinea","Fiji","Nauru","Northern Mariana Islands",
                                                                  "Palau","Marshall Islands","Micronesia","Wallis and Futuna","Kiribati","New Caledonia","Pitcairn Islands",
                                                                  "Tonga","Samoa","Vanuatu","Wallis and Futuna","Solomon Islands","Cook Islands","Niue","Tuvalu","Tokelau"), "PICT", "Non PICT")

         Plot_Me <- Land_to_EEZ[!is.na(Land_to_EEZ$Ratio),]
         
         ggplot(Plot_Me[Plot_Me$Ratio < 10,], 
                aes(y = Pacific,
                    x = Ratio)) +
                 geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01, min_height = 0)
            
            

   ##
   ## Save files
   ##
      save(EEZ,      file = 'Data_Spatial/EEZ_subset.rda')
      save(Countries,file = 'Data_Spatial/Countries.rda')

X <- unique(EEZ$TERRITORY1)
X[order(X)]


X <- unique(Countries$NAME_EN)
X[order(X)]




Countries[Countries$NAME_EN == "Solomon Islands",]
Land_Size[Land_Size$Country == "Solomon Islands",]


          Land_Size <- with(Countries,
                        aggregate(list(Land_Area = st_area(Countries)/(1000*1000)),
                                  list(Country = NAME_EN),
                                sum, 
                                na.rm = TRUE))     


Test <- Countries[Countries$NAME_EN == "Solomon Islands",]

Test$Tickles <- ifelse(str_detect(Test$FORMAL_EN,"Federated States of Micronesia"), "Micronesia", 
                ifelse(str_detect(Test$FORMAL_EN,"United States"), "United States of America",
                ifelse(str_detect(Test$FORMAL_EN,"Sint Maarten"), "Sint-Maarten", Test$NAME_EN)))
            
                                                
                                                