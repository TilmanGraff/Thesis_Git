
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rgeos", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

rails <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/Built/Railroads.TAB")

south_african_rails <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/South_africa.shp")

south_african_rails@data <- rails@data[1:length(south_african_rails),] # this creates an empty databox for south africa (of data which I do not have), in order to merge seemlessly
south_african_rails@data[1] <- NA
south_african_rails@data[2] <- NA
south_african_rails@data[3] <- NA
south_african_rails@data[4] <- NA
south_african_rails@data[5] <- NA
south_african_rails@data[6] <- NA
south_african_rails@data[7] <- NA

south_african_rails <- spTransform(south_african_rails, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) # gives common CRS to merge seemlessly
rails <- spTransform(rails, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) # gives common CRS to merge seemlessly

all_rails <- rbind(rails, south_african_rails) # merges


opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")

opt_loc$dist2rail <- NA
opt_loc$id_closest_rail <- NA


for(i in 1:nrow(opt_loc)){ # over all centroids (10,000)

  loc <- c(opt_loc$x[i], opt_loc$y[i])
  #mindist <- 10^9
  #whichline <- NA

  #for(j in 1:length(all_rails)){ # over all rails (240)

  #  dist <- dist2Line(loc, all_rails@lines[[j]]@Lines[[1]]@coords)[1]

  #  if(mindist > dist){ # iteratively updates to find smallest distance
  #    mindist <- dist
  #    whichline <- j # saves slot number of closest rail
  #  }

  dist <- dist2Line(loc, all_rails)
  opt_loc$dist2rail[i] <- dist[1] / 1000
  opt_loc$id_closest_rail[i] <- dist[4]

  print(i)
}

write.csv(opt_loc, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv", row.names = FALSE)



## get length measure




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

opt_loc$RailKM <- NA

for(i in 1:nrow(polygon_dataframe)){
  single_cell <- polygon_dataframe[which(polygon_dataframe@data$ID == polygon_dataframe@data$ID[i]),]
  if(gIntersects(single_cell, all_rails)){
    intersect <- gIntersection(single_cell, all_rails)
    if(!("coords" %in% slotNames(intersect))){
    if("lineobj" %in% slotNames(intersect)){
      opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "RailKM"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
      print(c(i, "with multiple"))
    } else{
      opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "RailKM"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
      print(i)
    }

  }
  } else{
    opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "RailKM"] <- 0
}
}

write.csv(opt_loc, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv", row.names = FALSE)


####################################
# Placebo Lines
####################################
