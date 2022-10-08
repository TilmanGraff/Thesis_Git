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

leaders_sp = SpatialPoints(cbind(leaders$x, leaders$y))
crs(leaders_sp) = "+proj=longlat +datum=WGS84 +no_defs"


# Merge onto opt_loc gridcells

merger = over(leaders_sp, grid)
leadersdf$years_in_power = 0

for(i in 1:nrow(merger)){
    if(!is.na(merger[i,"ID"])){
      leadersdf[merger[i,"ID"], "years_in_power"] = leadersdf[merger[i,"ID"], "years_in_power"] + (leaders[i,"until"] - leaders[i,"from"])
    }
}


##############
# export
##############

write.csv(leadersdf[,c("ID", "years_in_power")], file="./Analysis/temp/leaders.csv", row.names = FALSE, col.names = FALSE)
