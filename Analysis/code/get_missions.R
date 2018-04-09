
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rgeos", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("Imap", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gepaf", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("vegan", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

missions <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Missions_Map_Nunn/Missions.shp")

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

for(i in 1:length(polygon_dataframe)){
    merger <- (point.in.polygon(missions@coords[,1], missions@coords[,2], polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,1], polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,2]))

    subset <- missions@data[merger,]

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "missions"] <- sum(merger !=0, na.rm=T)
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "missions_cath"] <- sum(grepl("Catholic", subset[,"LAYER"]))
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "missions_prot"] <- sum(grepl("Protestant", subset[,"LAYER"]))


}

write.csv(opt_loc[,c("ID", "missions", "missions_cath", "missions_prot")], file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_missions.csv", row.names = FALSE)
