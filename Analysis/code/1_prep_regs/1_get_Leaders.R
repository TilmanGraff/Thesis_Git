require(rgdal)
require(geosphere)
require(raster)
require(rgeos)
require(gepaf)
require(pdftools)


##############
# import
##############

leaders = read.csv("./Analysis/temp/leaders_raw.csv")
leaders = leaders[!is.na(leaders$x),]
grid = readOGR("./Analysis/input/grid_shapefile/grid.shp")
leadersdf = grid@data

leaders_sp = SpatialPointsDataFrame(cbind(leaders$x, leaders$y), leaders)
crs(leaders_sp) = "+proj=longlat +datum=WGS84 +no_defs"


# Merge onto opt_loc gridcells

merger = over(leaders_sp[,"years_in_power"], grid, byid = T, returnList = T)
leadersdf$years_in_power = 0

for(i in 1:length(merger)){
  gridid = as.numeric(unlist(merger[i])[1])
    if(!is.na(gridid)){
      leadersdf[leadersdf$ID==gridid, "years_in_power"] = leadersdf[leadersdf$ID==gridid, "years_in_power"] + (leaders[i,"until"] - leaders[i,"from"])
    }
}


##############
# export
##############

write.csv(leadersdf[,c("ID", "years_in_power")], file="./Analysis/temp/leaders.csv", row.names = FALSE, col.names = FALSE)
