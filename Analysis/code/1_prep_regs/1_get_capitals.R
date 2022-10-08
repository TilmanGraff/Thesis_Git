require(rgdal)
require(geosphere)
require(raster)
require(rgeos)
require(gepaf)
require(sf)


#################
# prepare capitals
#################

capitals <- read.csv("./Analysis/input/country-capitals.csv")
capitals <- capitals[capitals$ContinentName == "Africa",]

capitals$x <- as.numeric(paste(capitals$CapitalLongitude))
capitals$y <- as.numeric(paste(capitals$CapitalLatitude))

capitals_sp = SpatialPoints(cbind(capitals$x, capitals$y))

crs(capitals_sp) = "+proj=longlat +datum=WGS84 +no_defs"


#################
# onto grid cells
#################

grid = readOGR("Analysis/input/grid_shapefile/grid.shp")
capdf = grid@data
capdf$iscapital = 0
distances = gDistance(grid, capitals_sp, byid = T)

for(i in 1:nrow(distances)){
  cand_cell = which.min(distances[i,])
  if(distances[i,cand_cell] < 2){ # the issue here is that sometimes the exact capital location falls slightly outside of a grid cell, so instead we assign the nearest grid cell to the capital point, but only if the distance is less than 2 degrees. This just tosses out one errant observation of St. Helena, etc...
    capdf[cand_cell,"iscapital"] = 1
  }

}

#################
# Export
#################

write.csv(capdf[,c("ID", "iscapital")], file="./Analysis/temp/ID_capitals.csv", row.names = FALSE)
