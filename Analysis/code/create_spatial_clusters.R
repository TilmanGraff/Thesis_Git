library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rgeos", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library(classInt)
library(sp)
library("spatialEco", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")


df <- opt_loc[!is.na(opt_loc$zeta),]
row.names(df) <- 1:nrow(df)


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

polygon_dataframe = SpatialPolygonsDataFrame(polygon_file, df)

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

pts <- SpatialPoints(opt_loc[,c("x", "y")])
overlap <- as.data.frame(over(pts, clusters))

opt_loc$cluster <- overlap[,1]

write.csv(opt_loc, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv", row.names = FALSE)
