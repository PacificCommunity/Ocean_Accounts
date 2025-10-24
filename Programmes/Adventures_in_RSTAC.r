##
##    Programme:  Adventures_in_RSTAC.r
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
      load('Data_Spatial/EEZ.rda')
      load('Data_Spatial/Target_Countries.rda')
      load('Data_Spatial/Countries.rda')
      load('Data_Spatial/BBX.rda')      

   ##
   ##    Open a link to the catalog and collection 
   ##

      s_obj <- stac("https://stac.digitalearthpacific.org")

      
      Search <- stac_search(q = s_obj,
                            collections= "dep_s2_geomad",
                            limit = 999)
      Search    
      
      Cook_Islands <- st_as_sf(Countries[4,])
      plot(st_geometry(Cook_Islands))
      Cook_Islands_BBox <- st_bbox(Cook_Islands)
     
      stac_query <- stac_search(q = s_obj,
                                collections= "dep_s2_geomad",
                                bbox = Cook_Islands_BBox)      
      stac_query
      
      
      
      
      
      
      
      
      
      
      


      Search <- stac_search(s_obj,
                            collections= "dep_s2_geomad", 
                            bbox= c(131.1202,2.999413,134.7139,8.064127),
                            limit = 100)

      it_obj <- get_request(Search)
      items_matched(it_obj)

      collections_query <- collections(s_obj)
      available_collections <- get_request(collections_query)


   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
