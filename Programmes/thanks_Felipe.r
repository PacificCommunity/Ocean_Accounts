

##
##    Clear the memory
##
rm(list=ls(all=TRUE))
library(rstac)
library(sf)
library(gpkg)
setwd("c:\\Git_Projects\\Ocean_Accounts")

##
##    Read in the DEP shorelines project (1.97gig): https://s3.us-west-2.amazonaws.com/dep-public-data/dep_ls_coastlines/dep_ls_coastlines_0-7-0-55.gpkg
##
g <- geopackage("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg")
Shorelines <- gpkg_table(g, "shorelines_annual")

##
##    Extract the Palau, Cook Islands, Fiji and New Caledonian coastlines
##
Target_Countries = st_read("Data_Spatial/dep_ls_coastlines_0-7-0-55.gpkg",query="select * 
                                                                                  from shorelines_annual
                                                                                  where eez_territory in ('FJI')")
Target_Countries <- st_transform(Target_Countries, crs = "epsg:4326")

Target_Countries <- Target_Countries[(Target_Countries$year == max(Target_Countries$year)) & (Target_Countries$certainty == "good"),]
                                           
                                           
X <- st_as_sf(st_union(Target_Countries))
X <- st_shift_longitude(st_transform(X, st_crs(4326)))

X$Country = "Fiji"
plot(X)


fiji_convex  <- st_shift_longitude(st_convex_hull(X))
fiji_concave <- st_concave_hull(X, ratio = .01)

plot(fiji_convex[,1])
plot(fiji_concave[,1])


# Creating a STAC obj
s_obj <- rstac::stac("https://stac.digitalearthpacific.org")


##
##    Convex
##

# Creating the search requisition
search <- rstac::stac_search(
    q = s_obj,
    limit = 9999
) |> rstac::ext_filter(
    collection == "dep_s2_geomad" && s_intersects(geometry, {{fiji_convex}})
)

# Requesting items
items <- rstac::post_request(search)
items

##
##    Concave
##
search <- rstac::stac_search(
    q = s_obj,
    limit = 9999
) |> rstac::ext_filter(
    collection == "dep_s2_geomad" && s_intersects(geometry, {{fiji_concave}})
)
# Requesting items
items <- rstac::post_request(search)
items






