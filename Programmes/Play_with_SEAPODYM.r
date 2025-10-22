##
##    Programme:  Play_with_SEAPODYM.r
##
##    Objective:  SEAPODYM is going to be the main data source for the Aquatic Resources component of the 
##                environmental asset account work. Lets have a look at the data source.
##
##                SEAPODYM's documentation is here: Ocean_Accounts/Documentation/Seapodym_user_manual.pdf
##
##                Skipjack data is here: https://osf.io/hgfjq/files/mnpxe
##                Bigeye data is here:   https://osf.io/qa8w4/files/cbgn6
##                Yellowfin data is here:https://osf.io/qa8w4/files/27vsx
##                Albacore data is here: https://spccloud-my.sharepoint.com/personal/innas_spc_int/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Finnas%5Fspc%5Fint%2FDocuments%2FSEAPODYM%2Frun%2FALB&ga=1&LOF=1
##
##                Also, in order to read the files, you need to download and install the library(dym) from here: https://osf.io/hgfjq/files/5kqyf
##
##    Author:     James Hogan, Senior Marine Resource Economist, 17 October 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      source("R/functions.r")
   ##
   ##    Lets have a look at one of SEAPODYM's files
   ##
      ##
      ## Skipjack
      ##      
            Skipjack <- read.var.dym("Data_Raw/Skipjack_SEAPODYM/output_F0/skj_adult.dym")
            summary(Skipjack[["x"]])
            summary(Skipjack[["y"]])
            summary(Skipjack[["t"]])

         ##
         ##    Item var looks like the ticket. I suspect this is tonnes of fish by time, lat and long... 
         ##
         ##       Also, its upside down :) terra has a function called flip, which is the ticket
         ##
            Spin_Me <- as.array(Skipjack[["var"]])
            
            Spun    <- array(data = NA, 
                              dim = c(120,260, 756))

            for(x in 1:120)
            {
               for(y in 1:260)
               {
                  for(z in 1:756)
                  {
                     Spun[x,y,z] <- Spin_Me[z,y,x]
                  }
               }
            }  
            
            SEAPODYM_Skipjack_Adults <- flip(rast(Spun))
            SEAPODYM_Skipjack_Adults[SEAPODYM_Skipjack_Adults == 0] <- NA
            names(SEAPODYM_Skipjack_Adults) <-Skipjack[["t"]]
            
            SEAPODYM_Land <- rast(Skipjack[["landmask"]])
            
            ext(SEAPODYM_Skipjack_Adults)<- c(30.5, 289.5, -58.5, 60.5)
            ext(SEAPODYM_Land)           <- c(30.5, 289.5, -58.5, 60.5)
            
            SEAPODYM_Skipjack_Adults[SEAPODYM_Skipjack_Adults == 0] <- NA
            SEAPODYM_Land[SEAPODYM_Land != 0] <- NA
            
            plot(SEAPODYM_Skipjack_Adults, 10)
            plot(SEAPODYM_Land, add=TRUE)

   ##
   ## Bigeye
   ##
      Bigeye   <- read.var.dym("Data_Raw/Bigeye_SEAPODYM/output/output_F0/bet_adult.dym")
      str(Bigeye)
      summary(Bigeye[["x"]])
      summary(Bigeye[["y"]])
      summary(Bigeye[["t"]])

      ##
      ##    Item var looks like the ticket. I suspect this is tonnes of fish by time, lat and long... 
      ##
      ##       Also, its upside down :) terra has a function called flip, which is the ticket
      ##
         Spin_Me <- as.array(Bigeye[["var"]])
         
         Spun    <- array(data = NA, 
                           dim = c(60,101, 372))

         for(x in 1:60)
         {
            for(y in 1:101)
            {
               for(z in 1:372)
               {
                  Spun[x,y,z] <- Spin_Me[z,y,x]
               }
            }
         }  
         
         SEAPODYM_Bigeye_Adults <- flip(rast(Spun))
         SEAPODYM_Bigeye_Adults[SEAPODYM_Bigeye_Adults == 0] <- NA
         names(SEAPODYM_Bigeye_Adults) <-Bigeye[["t"]]
         
         ext(SEAPODYM_Bigeye_Adults)<- c(89.5, 289.5, -53.5, 64.5)
         
         SEAPODYM_Bigeye_Adults[SEAPODYM_Bigeye_Adults == 0] <- NA
         
         plot(SEAPODYM_Bigeye_Adults, 10)

   ##
   ## Yellowfin
   ##
      Yellowfin   <- read.var.dym("Data_Raw/Yellowfin_SEAPODYM/output/output_F0/yft_adult.dym")
      str(Yellowfin)
      summary(Yellowfin[["x"]])
      summary(Yellowfin[["y"]])
      summary(Yellowfin[["t"]])

      ##
      ##    Item var looks like the ticket. I suspect this is tonnes of fish by time, lat and long... 
      ##
      ##       Also, its upside down :) terra has a function called flip, which is the ticket
      ##
         Spin_Me <- as.array(Yellowfin[["var"]])
         
         Spun    <- array(data = NA, 
                           dim = c(60,101, 372))

         for(x in 1:60)
         {
            for(y in 1:101)
            {
               for(z in 1:372)
               {
                  Spun[x,y,z] <- Spin_Me[z,y,x]
               }
            }
         }  
         
         SEAPODYM_Yellowfin_Adults <- flip(rast(Spun))
         SEAPODYM_Yellowfin_Adults[SEAPODYM_Yellowfin_Adults == 0] <- NA
         names(SEAPODYM_Yellowfin_Adults) <-Yellowfin[["t"]]
         
         ext(SEAPODYM_Yellowfin_Adults)<- c(89.5, 289.5, -53.5, 64.5)
         
         SEAPODYM_Yellowfin_Adults[SEAPODYM_Yellowfin_Adults == 0] <- NA
         
         plot(SEAPODYM_Yellowfin_Adults, 10)
         

   ##
   ## Albacore
   ##
      Albacore   <- read.var.dym("Data_Raw/Albacore_SEAPODYM/output/alb_adult.dym")
      str(Albacore)
      summary(Albacore[["x"]])
      summary(Albacore[["y"]])
      summary(Albacore[["t"]])

      ##
      ##    Item var looks like the ticket. I suspect this is tonnes of fish by time, lat and long... 
      ##
      ##       Also, its upside down :) terra has a function called flip, which is the ticket
      ##
         Spin_Me <- as.array(Albacore[["var"]])
         
         Spun    <- array(data = NA, 
                           dim = c(32,80, 384))

         for(x in 1:32)
         {
            for(y in 1:80)
            {
               for(z in 1:384)
               {
                  Spun[x,y,z] <- Spin_Me[z,y,x]
               }
            }
         }  
         
         SEAPODYM_Albacore_Adults <- flip(rast(Spun))
         SEAPODYM_Albacore_Adults[SEAPODYM_Albacore_Adults == 0] <- NA
         names(SEAPODYM_Albacore_Adults) <-Albacore[["t"]]
         
         ext(SEAPODYM_Albacore_Adults)<- c(131.5, 289.5, -58.5, 3.5)
         
         SEAPODYM_Albacore_Adults[SEAPODYM_Albacore_Adults == 0] <- NA
         
         plot(SEAPODYM_Albacore_Adults, 10)

   ##
   ## Save files our produce some final output of something
   ##
      writeRaster(SEAPODYM_Skipjack_Adults,  file = 'Data_Spatial/SEAPODYM_Skipjack_Adults.tif',  gdal=c("COMPRESS=DEFLATE"),overwrite = TRUE)
      writeRaster(SEAPODYM_Bigeye_Adults,    file = 'Data_Spatial/SEAPODYM_Bigeye_Adults.tif',    gdal=c("COMPRESS=DEFLATE"),overwrite = TRUE)
      writeRaster(SEAPODYM_Yellowfin_Adults, file = 'Data_Spatial/SEAPODYM_Yellowfin_Adults.tif', gdal=c("COMPRESS=DEFLATE"),overwrite = TRUE)
      writeRaster(SEAPODYM_Albacore_Adults,  file = 'Data_Spatial/SEAPODYM_Albacore_Adults.tif',  gdal=c("COMPRESS=DEFLATE"),overwrite = TRUE)
      writeRaster(SEAPODYM_Land,             file = 'Data_Spatial/SEAPODYM_Land.tif',             gdal=c("COMPRESS=DEFLATE"),overwrite = TRUE)
##
##    And we're done
##



rm(list=ls(all=TRUE))
x <- rast('Data_Spatial/SEAPODYM_Skipjack_Adults.tif')
y <- rast('Data_Spatial/SEAPODYM_Land.tif')
plot(x, "1989-07-15" )
plot(y, add=TRUE)
