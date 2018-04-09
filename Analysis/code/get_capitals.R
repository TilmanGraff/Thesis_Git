
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rgeos", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("Imap", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gepaf", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("vegan", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("scales", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("countrycode", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

#################
# prepare latlong
#################

capitals <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/country-capitals.csv")
capitals <- capitals[capitals$ContinentName == "Africa",]

capitals$x <- as.numeric(paste(capitals$CapitalLongitude))
capitals$y <- as.numeric(paste(capitals$CapitalLatitude))


#################
# onto grid cells
#################


opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")

# #df <- opt_loc[!is.na(opt_loc$zeta),]
# row.names(df) <- 1:nrow(df)
# # back out corner locations from centroid
# df$bl_x <- df$x - 0.25
# df$tl_x <- df$x - 0.25
# df$br_x <- df$x + 0.25
# df$tr_x <- df$x + 0.25
# df$bl_y <- df$y - 0.25
# df$tl_y <- df$y + 0.25
# df$br_y <- df$y - 0.25
# df$tr_y <- df$y + 0.25
#
# polygon_file = SpatialPolygons(lapply(1:nrow(df), function(x) Polygons(list(Polygon( cbind(t(df[x, c("bl_x", "tl_x", "tr_x", "br_x")]), t(df[x, c("bl_y", "tl_y", "tr_y", "br_y")])) )), paste0(x))))
# polygon_dataframe = SpatialPolygonsDataFrame(polygon_file, df)
#
# for(i in 1:length(polygon_dataframe)){
#     merger <- (point.in.polygon(capitals$x, capitals$y, polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,1], polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,2]))
#
#     if(sum(merger.A, na.rm=T)>0){
#       opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "capital"] <- 1
#     } else{
#       opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "capital"] <- 0
#     }
#
# }

opt_loc$wbcode <- countrycode(opt_loc$country, origin = "country.name", destination="wb")
df <- opt_loc[!is.na(opt_loc$wbcode),]

df$capital <- NA

for(i in 1:nrow(capitals)){

  country <- countrycode(capitals$CountryCode[i], origin = "iso2c", destination = "wb")
  if(!is.na(country)){

    subset <- df[df$wbcode==country,]
    nearest_ID <- NA
    min_dist <- 1000000000
if(nrow(subset)>0){
    for(j in 1:nrow(subset)){
      if(!is.na(subset[j,c("x")]) & !is.na(subset[j,c("y")])){
      dist <- gdist(capitals[i,c("x")], capitals[i,c("y")], subset[j,c("x")], subset[j,c("y")])
      if(dist < min_dist){
        min_dist <- dist
        nearest_ID <- subset[j,"ID"]
      }
    }
  }
    df[df$ID==nearest_ID,"capital"] <- 1
}
}
}

df[is.na(df$capital), "capital"] <- 0

df <- merge(opt_loc, df, by="ID", all.x=T)
df[is.na(df$capital), "capital"] <- 0


write.csv(df[,c("ID", "capital")], file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_capitals.csv", row.names = FALSE)
