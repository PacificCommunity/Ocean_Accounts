##
##    Programme:  Play_with_SEAPODYM.r
##
##    Objective:  SEAPODYM is going to be the main data source for the Aquatic Resources component of the 
##                environmental asset account work. Lets have a look at the data source.
##
##                SEAPODYM's documentation is here: Ocean_Accounts/Documentation/Seapodym_user_manual.pdf
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

   ##
   ##    Lets have a look at one of SEAPODYM's files
   ##
      
      Lookie <- read.var.dym("Data_Raw/skipjack_jra55np_1x30d_ref_F0/output_F0/skj_adult.dym")
      str(Lookie)
      summary(Lookie[["x"]])
      summary(Lookie[["y"]])
      summary(Lookie[["t"]])

   ##
   ##    Item var looks like the ticket. I suspect this is tonnes of fish by time, lat and long... 
   ##
   ##       Also, its upside down :) terra has a function called flip, which is the ticket
   ##
      Spin_Me <- as.array(Lookie[["var"]])
      
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
      names(SEAPODYM_Skipjack_Adults) <-Lookie[["t"]]
      
      SEAPODYM_Land <- rast(Lookie[["landmask"]])
      
      ext(SEAPODYM_Skipjack_Adults)<- c(30.5, 289.5, -58.5, 60.5)
      ext(SEAPODYM_Land)           <- c(30.5, 289.5, -58.5, 60.5)
      
      SEAPODYM_Skipjack_Adults[SEAPODYM_Skipjack_Adults == 0] <- NA
      SEAPODYM_Land[SEAPODYM_Land != 0] <- NA
      
      plot(SEAPODYM_Skipjack_Adults, 10)
      plot(SEAPODYM_Land, add=TRUE)
      

   ##
   ## Save files our produce some final output of something
   ##
      writeRaster(SEAPODYM_Skipjack_Adults, file = 'Data_Spatial/SEAPODYM_Skipjack_Adults.tif',gdal=c("COMPRESS=DEFLATE"),overwrite = TRUE)
      writeRaster(SEAPODYM_Land,            file = 'Data_Spatial/SEAPODYM_Land.tif',           gdal=c("COMPRESS=DEFLATE"),overwrite = TRUE)
##
##    And we're done
##



rm(list=ls(all=TRUE))
x <- rast('Data_Spatial/SEAPODYM_Skipjack_Adults.tif')
y <- rast('Data_Spatial/SEAPODYM_Land.tif')
plot(x, "1989-07-15" )
plot(y, add=TRUE)
