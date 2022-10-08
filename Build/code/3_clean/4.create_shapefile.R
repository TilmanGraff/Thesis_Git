df <- read.csv("./Build/temp/centroids.csv")

# back out corner locations from centroid
df$bl_x <- df$x - 0.25
df$tl_x <- df$x - 0.25
df$br_x <- df$x + 0.25
df$tr_x <- df$x + 0.25

df$bl_y <- df$y - 0.25
df$tl_y <- df$y + 0.25
df$br_y <- df$y - 0.25
df$tr_y <- df$y + 0.25


polygon_file = SpatialPolygons(lapply(1:nrow(df), function(x) Polygons(list(Polygon( cbind(t(df[x, c("bl_x", "tl_x", "tr_x", "br_x")]), t(df[x, c("bl_y", "tl_y", "tr_y", "br_y")])) )), paste0(x))))

polygon_dataframe = SpatialPolygonsDataFrame(polygon_file, df)[,c("ID", "x", "y")]
proj4string(polygon_dataframe) = "+proj=longlat +ellps=WGS84 +datum=WGS84"

writeOGR(polygon_dataframe, "Analysis/input/grid_shapefile/grid.shp", driver="ESRI Shapefile", layer = "polygon_dataframe")
