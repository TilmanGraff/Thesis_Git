
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rgeos", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("Imap", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gepaf", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("vegan", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

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
# Differential for rail purpose
####################################

rails <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/Built/Railroads.TAB")
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


opt_loc$RailKM_military <- 0
opt_loc$RailKM_mining <- 0

for(i in 1:nrow(polygon_dataframe)){
  if(opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "RailKM"] > 0 & !is.na(opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "RailKM"])){ # only does it for those that have any rails (computation times!)
    single_cell <- polygon_dataframe[which(polygon_dataframe@data$ID == polygon_dataframe@data$ID[i]),]
    if(gIntersects(single_cell, all_rails[all_rails@data$military==1,])){ # intersects only with subset of military rails
      intersect <- gIntersection(single_cell, all_rails[all_rails@data$military==1,])
        if(!("coords" %in% slotNames(intersect))){
          if("lineobj" %in% slotNames(intersect)){
            opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "RailKM_military"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
            print(c(i, "with multiple"))
          } else{
            opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "RailKM_military"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
            print(i)
          }

        }
      }
      if(gIntersects(single_cell, all_rails[all_rails@data$mining==1,])){ # intersects only with subset of mining rails
        intersect <- gIntersection(single_cell, all_rails[all_rails@data$mining==1,])
          if(!("coords" %in% slotNames(intersect))){
            if("lineobj" %in% slotNames(intersect)){
              opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "RailKM_mining"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
              print(c(i, "with multiple"))
            } else{
              opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "RailKM_mining"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
              print(i)
            }

          }
        }

    }
  }

write.csv(opt_loc, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv", row.names = FALSE)

####################################
# Placebo Lines
####################################

# this code somehow vanished...
# its the same as above really...


####################################
# EMST Lines
####################################

nodes <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/EMST_Lines/nodes.TAB")
#emst <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/EMST_Lines/EMST.TAB")
opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")

# this is all the cities that are mentioned on the south africa map plus pretoria (capital back then)
cape <- c(18.598227, -34.107790)
beaufort_west <- c(22.561545, -32.305843)
bleomfontain <- c(26.087945, -29.105537)
port_elizabeth <- c(25.670227, -33.844601)
johannesburg <- c(28.024229, -26.173970)
pretoria <- c(28.138890, -25.792591)
eendekuil <- c(18.886597, -32.688726)
port_nolloth <- c(16.875627, -29.224785)
de_aar <- c(23.974669, -30.668652)
george <- c(22.500869, -33.997303)
port_alfred <- c(26.867809, -33.624552)
east_london <- c(27.890928, -33.040990)
kimberley <- c(24.733504, -28.766301)
vryburg <- c(24.755151, -26.932349)

sa_nodes <- rbind(cape, beaufort_west, bleomfontain, port_elizabeth, johannesburg, pretoria, eendekuil, port_nolloth, de_aar, george, port_alfred, east_london, kimberley, vryburg)

dta <- as.data.frame(rep(1, nrow(sa_nodes)))
colnames(dta) <- "node"

sa_pts <- SpatialPointsDataFrame(SpatialPoints(sa_nodes, proj4string = CRS(proj4string(nodes))), data=dta)

all_nodes <- rbind(nodes, sa_pts)

dist_matrix <- matrix(0, nrow=length(all_nodes), ncol=length(all_nodes))

for(i in 1:length(all_nodes)){
  for(j in i:length(all_nodes)){

    dist_matrix[i,j] <- gdist(as.numeric(all_nodes@coords[i,1]), as.numeric(all_nodes@coords[i,2]), as.numeric(all_nodes@coords[j,1]), as.numeric(all_nodes@coords[j,2]), units="km")

    dist_matrix[j,i] <- dist_matrix[i,j]

  }
}

connections <- as.data.frame(cbind(2:length(all_nodes), spantree(dist_matrix)$kid))
row.names(connections) <- 1:nrow(connections)

my_emst <- (SpatialLinesDataFrame(SpatialLines(lapply(1:nrow(connections), function(x) Lines(list(Line(rbind(cbind(as.numeric(all_nodes@coords[connections[x,1],1]), as.numeric(all_nodes@coords[connections[x,1],2])), cbind(as.numeric(all_nodes@coords[connections[x,2],1]), as.numeric(all_nodes@coords[connections[x,2],2]))))), paste(x)))), connections))

opt_loc$dist2emst <- NA

for(i in 1:nrow(opt_loc)){ # over all centroids (10,000)

  loc <- c(opt_loc$x[i], opt_loc$y[i])

  dist <- dist2Line(loc, my_emst)
  opt_loc$dist2emst[i] <- dist[1] / 1000

  print(i)
}


# indicate EMST nodes

df_coords <- rbind(nodes@coords, sa_nodes)
row.names(df_coords) <- 1:nrow(df_coords)
opt_loc$isnode <- 0

for(i in 1:nrow(polygon_dataframe)){
  single_cell <- polygon_dataframe[which(polygon_dataframe@data$ID == polygon_dataframe@data$ID[i]),]
  if(gIntersects(single_cell, all_nodes)){
    opt_loc[i, "isnode"] <- 1
  }
}


# Get EmstKM

rails <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/Built/Railroads.TAB")
opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")


df <- opt_loc[!is.na(opt_loc$zeta),]
row.names(df) <- 1:nrow(df)

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


opt_loc$emstKM <- NA

for(i in 1:nrow(polygon_dataframe)){
  single_cell <- polygon_dataframe[which(polygon_dataframe@data$ID == polygon_dataframe@data$ID[i]),]
  if(gIntersects(single_cell, my_emst)){
    intersect <- gIntersection(single_cell, my_emst)
    if(!("coords" %in% slotNames(intersect))){
    if("lineobj" %in% slotNames(intersect)){
      opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "emstKM"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
      print(c(i, "with multiple"))
    } else{
      opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "emstKM"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
      print(i)
    }

  }
  } else{
    opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "emstKM"] <- 0
}
}

write.csv(opt_loc, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv", row.names = FALSE)
