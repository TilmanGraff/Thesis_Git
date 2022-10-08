require(rgdal)
require(geosphere)
require(raster)
require(rgeos)
require(gepaf)
require(sf)

###############
# Import grid
###############

polygon_dataframe = readOGR("Analysis/input/grid_shapefile/grid.shp")


###############
# Create higher level cluster
###############

cell_size <- 3 # this sets the cell size in degrees

df <- makegrid(polygon_dataframe, cellsize = cell_size)

df$bl_x <- df$x1 - cell_size / 2
df$tl_x <- df$x1 - cell_size / 2
df$br_x <- df$x1 + cell_size / 2
df$tr_x <- df$x1 + cell_size / 2

df$bl_y <- df$x2 - cell_size / 2
df$tl_y <- df$x2 + cell_size / 2
df$br_y <- df$x2 - cell_size / 2
df$tr_y <- df$x2 + cell_size / 2

clusters = SpatialPolygons(lapply(1:nrow(df), function(x) Polygons(list(Polygon( cbind(t(df[x, c("bl_x", "tl_x", "tr_x", "br_x")]), t(df[x, c("bl_y", "tl_y", "tr_y", "br_y")])) )), paste0(x))))

crs(clusters) = "+proj=longlat +datum=WGS84 +no_defs"

###############
# Overlay grid and cluster
###############


pts <- gCentroid(polygon_dataframe, byid = T)
overlap <- as.data.frame(over(pts, clusters))

clusterdf = polygon_dataframe@data
clusterdf$cluster <- overlap[,1]

###############
# Export
###############

write.csv(clusterdf[,c("ID", "cluster")], file="./Analysis/temp/clusters.csv", row.names = FALSE)
