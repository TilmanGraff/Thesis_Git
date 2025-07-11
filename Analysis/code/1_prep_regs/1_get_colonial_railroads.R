
require(rgdal)
require(geosphere)
require(raster)
require(rgeos)
require(gepaf)
require(sf)


###############
# Create all_rails dataset
###############

rails <- readOGR("./Analysis/input/Railroads/Built/Railroads.TAB")

south_african_rails <- readOGR("./Analysis/input/Railroads/South_africa.shp")

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
south_african_rails$country = "South Africa"

all_rails <- rbind(rails, south_african_rails) # merges

####################################
# Create all_placebo dataset
####################################

placebo <- readOGR("./Analysis/input/Railroads/Placebo/placebo_1916_1922.TAB")
south_african_placebo <- readOGR("./Analysis/input/Railroads/South_Africa_placebo.shp")
south_african_placebo@data <- as.data.frame(placebo@data[1:length(south_african_placebo),]) # this creates an empty databox for south africa (of data which I do not have), in order to merge seemlessly
colnames(south_african_placebo@data) <- "placebo_year"
south_african_placebo@data[1] <- NA
south_african_placebo <- spTransform(south_african_placebo, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) # gives common CRS to merge seemlessly
placebo <- spTransform(placebo, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) # gives common CRS to merge seemlessly

all_placebo <- rbind(placebo, south_african_placebo) # merges


###############
# Import grid
###############

polygon_dataframe = readOGR("Analysis/input/grid_shapefile/grid.shp")
opt_loc = read.csv("Analysis/input/opt_loc.csv")

raildf = polygon_dataframe@data

raildf$RailKM <- 0
raildf$PlaceboKM <- 0
raildf$RailKM_military <- 0
raildf$RailKM_mining <- 0
raildf$dist2rail <- NA
raildf$dist2placebo <- NA


runs <- data.frame("names" = c("mob", "imm"), "paths" = c("2024-02-18_040636_mobile_fp_final", "2024-02-20_140808_imm_fp_final"))


###############
# Rail edge connections
###############

print("Begin edge connections with Rails...")

for(railtype in c("rails", "placebo")){
  
  y = get(paste0("all_", railtype))
  x = gIntersects(polygon_dataframe, y, byid = T)
  
  cells_affected = as.numeric(which(colSums(x) > 0))
  IDs_affected = polygon_dataframe[cells_affected,]$ID
  countries_affected = unique(opt_loc[opt_loc$ID %in% IDs_affected,"country"])

  for(c in countries_affected){
    rosetta <- read.csv(paste0("./Build/temp/rosettastones/rosetta_", c, ".csv"))
    I_stat = read.csv(paste0("./Build/temp/I/I_", c, ".csv"), header = T)
    ID_c = IDs_affected[opt_loc[opt_loc$ID %in% IDs_affected,"country"] == c]
    ID_r = rosetta[rosetta$ID %in% ID_c,]$rownumber
    for(j in 1:length(ID_r)){
      raildf[raildf$ID == ID_c[j], paste0(railtype, "con_I_stat")] = sum(I_stat[ID_r[j],ID_r])
    
      
      for(runidx in 1:nrow(runs)){
        run = runs[runidx, "names"]
        path = runs[runidx, "paths"]
        
        for(type in c("", "_10p")){
          I_opt = read.csv(paste0("./Build/output/", path, "/Optimised_Networks/", c, type, ".csv"), header = F)
          raildf[raildf$ID == ID_c[j], paste0(railtype, "con_I_", run, type)] = sum(I_opt[ID_r[j],ID_r])
        }
        
      }
    }
    print(paste0(c))
}
}
###############
# Distance to Rail Lines
###############

print("Begin Distance to Rails...")

st_rails = st_distance(st_as_sf(gCentroid(polygon_dataframe, byid = T)), st_as_sf(all_rails))
st_placebo = st_distance(st_as_sf(gCentroid(polygon_dataframe, byid = T)), st_as_sf(all_placebo))

for(i in 1:nrow(raildf)){

  raildf$dist2rail[i] <- min(st_rails[i,]) / 1000
  raildf$dist2placebo[i] <- min(st_placebo[i,]) / 1000


}


# for(i in 1:nrow(raildf)){ # over all centroids (10,000)
#
#   loc <- c(raildf$x[i], raildf$y[i])
#
#   # rails
#   dist <- dist2Line(loc, all_rails)
#   raildf$dist2rail[i] <- dist[1] / 1000
#   raildf$id_closest_rail[i] <- dist[4]
#
#   # placebos
#   dist <- dist2Line(loc, all_placebo)
#   raildf$dist2placebo[i] <- dist[1] / 1000
#
#   print(i)
# }


###############
# Intersection with Rail Lines
###############

print("Begin Intersection with Rails...")


for(i in 1:nrow(polygon_dataframe)){
  single_cell <- polygon_dataframe[which(polygon_dataframe@data$ID == polygon_dataframe@data$ID[i]),]
  if(gIntersects(single_cell, all_rails)){
    intersect <- gIntersection(single_cell, all_rails)
    if(!("coords" %in% slotNames(intersect))){
    if("lineobj" %in% slotNames(intersect)){
      raildf[raildf$ID == polygon_dataframe@data$ID[i], "RailKM"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
      #print(c(i, "with multiple"))
    } else{
      raildf[raildf$ID == polygon_dataframe@data$ID[i], "RailKM"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
      #print(i)
    }

  }

  #### add the purpose as well
  #### MILITARY
  if(gIntersects(single_cell, all_rails[all_rails@data$military==1,])){
    intersect <- gIntersection(single_cell, all_rails[all_rails@data$military==1,])
    if(!("coords" %in% slotNames(intersect))){
      if("lineobj" %in% slotNames(intersect)){
        raildf[raildf$ID == polygon_dataframe@data$ID[i], "RailKM_military"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
        #print(c(i, "with multiple"))
      } else{
        raildf[raildf$ID == polygon_dataframe@data$ID[i], "RailKM_military"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
        #print(i)
      }

    }
  }

  #### MINING
  if(gIntersects(single_cell, all_rails[all_rails@data$mining==1,])){
    intersect <- gIntersection(single_cell, all_rails[all_rails@data$mining==1,])
    if(!("coords" %in% slotNames(intersect))){
      if("lineobj" %in% slotNames(intersect)){
        raildf[raildf$ID == polygon_dataframe@data$ID[i], "RailKM_mining"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
        #print(c(i, "with multiple"))
      } else{
        raildf[raildf$ID == polygon_dataframe@data$ID[i], "RailKM_mining"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
        #print(i)
      }

    }
  }

  } else{
    raildf[raildf$ID == polygon_dataframe@data$ID[i], "RailKM"] <- 0
}

  #### Lastly, do the same with PLACEBO lines
  if(gIntersects(single_cell, all_placebo)){
      intersect <- gIntersection(single_cell, all_placebo)
      if(!("coords" %in% slotNames(intersect))){
      if("lineobj" %in% slotNames(intersect)){
        raildf[raildf$ID == polygon_dataframe@data$ID[i], "PlaceboKM"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
        #print(c(i, "with multiple"))
      } else{
        raildf[raildf$ID == polygon_dataframe@data$ID[i], "PlaceboKM"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
        #print(i)
      }

    }
  } else{
      raildf[raildf$ID == polygon_dataframe@data$ID[i], "PlaceboKM"] <- 0
  }

}



###############
# Export
###############


write.csv(raildf, file="./Analysis/temp/raildf_new.csv", row.names = FALSE)
#writeOGR(all_rails, "./Analysis/temp/allrails.shp", driver="ESRI Shapefile", layer = "all_rails")
#writeOGR(all_placebo, "./Analysis/temp/allplacebo.shp", driver="ESRI Shapefile", layer = "all_placebo")




#
#
#
# # Draw railroads
# library(scales)
#
#  png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/other_maps/Railroads.png", sep=""), width=6, height=6, units = 'in', res = 300)
#
# print(plot(polygon_dataframe, lwd=0.5, border=alpha("black", 0.5)))
# print(plot(all_rails, lwd=1.5, col="red", add=T))
# dev.off()
#




# ####################################
# # EMST Lines
# ####################################
#
# nodes <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/EMST_Lines/nodes.TAB")
# #emst <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/EMST_Lines/EMST.TAB")
# opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")
#
# # this is all the cities that are mentioned on the south africa map plus pretoria (capital back then)
# cape <- c(18.598227, -34.107790)
# beaufort_west <- c(22.561545, -32.305843)
# bleomfontain <- c(26.087945, -29.105537)
# port_elizabeth <- c(25.670227, -33.844601)
# johannesburg <- c(28.024229, -26.173970)
# pretoria <- c(28.138890, -25.792591)
# eendekuil <- c(18.886597, -32.688726)
# port_nolloth <- c(16.875627, -29.224785)
# de_aar <- c(23.974669, -30.668652)
# george <- c(22.500869, -33.997303)
# port_alfred <- c(26.867809, -33.624552)
# east_london <- c(27.890928, -33.040990)
# kimberley <- c(24.733504, -28.766301)
# vryburg <- c(24.755151, -26.932349)
#
# sa_nodes <- rbind(cape, beaufort_west, bleomfontain, port_elizabeth, johannesburg, pretoria, eendekuil, port_nolloth, de_aar, george, port_alfred, east_london, kimberley, vryburg)
# dta <- as.data.frame(rep(1, nrow(sa_nodes)))
# colnames(dta) <- "node"
# sa_pts <- SpatialPointsDataFrame(SpatialPoints(sa_nodes, proj4string = CRS(proj4string(nodes))), data=dta)
# all_nodes <- rbind(nodes, sa_pts)
#
# ###
# # A) all nodes EMST
# ###
#
# dist_matrix <- matrix(0, nrow=length(all_nodes), ncol=length(all_nodes))
#
# for(i in 1:length(all_nodes)){
#   for(j in i:length(all_nodes)){
#
#     dist_matrix[i,j] <- gdist(as.numeric(all_nodes@coords[i,1]), as.numeric(all_nodes@coords[i,2]), as.numeric(all_nodes@coords[j,1]), as.numeric(all_nodes@coords[j,2]), units="km")
#
#     dist_matrix[j,i] <- dist_matrix[i,j]
#
#   }
# }
#
# connections <- as.data.frame(cbind(2:length(all_nodes), spantree(dist_matrix)$kid))
# row.names(connections) <- 1:nrow(connections)
#
# my_emst <- (SpatialLinesDataFrame(SpatialLines(lapply(1:nrow(connections), function(x) Lines(list(Line(rbind(cbind(as.numeric(all_nodes@coords[connections[x,1],1]), as.numeric(all_nodes@coords[connections[x,1],2])), cbind(as.numeric(all_nodes@coords[connections[x,2],1]), as.numeric(all_nodes@coords[connections[x,2],2]))))), paste(x))), proj4string = CRS(proj4string(nodes))), connections))
#
# ###
# # B) Subregions
# ###
# #
# # subregions <- unionSpatialPolygons(polygon_dataframe, IDs=polygon_dataframe@data$subregion)
# #
# # for(region in 1:length(subregions)){
# #
# #   nodes_subset <- gIntersection(all_nodes, subregions[region])
# #
# #   dist_matrix <- matrix(0, nrow=length(nodes_subset), ncol=length(nodes_subset))
# #   for(i in 1:length(nodes_subset)){
# #     for(j in i:length(nodes_subset)){
# #       dist_matrix[i,j] <- gdist(as.numeric(nodes_subset@coords[i,1]), as.numeric(nodes_subset@coords[i,2]), as.numeric(nodes_subset@coords[j,1]), as.numeric(nodes_subset@coords[j,2]), units="km")
# #       dist_matrix[j,i] <- dist_matrix[i,j]
# #     }
# #   }
# #   connections <- as.data.frame(cbind(2:length(nodes_subset), spantree(dist_matrix)$kid))
# #   row.names(connections) <- 1:nrow(connections)
# #
# #    assign(paste("sub_emst", region, sep="_"), (SpatialLinesDataFrame(SpatialLines(lapply(1:nrow(connections), function(x) Lines(list(Line(rbind(cbind(as.numeric(nodes_subset@coords[connections[x,1],1]), as.numeric(nodes_subset@coords[connections[x,1],2])), cbind(as.numeric(nodes_subset@coords[connections[x,2],1]), as.numeric(nodes_subset@coords[connections[x,2],2]))))), paste(x))), proj4string = CRS(proj4string(nodes))), connections)))
# #
# # }
# #
#
# ###
# # dist2emst
# ###
#
# opt_loc$dist2emst <- NA
#
# for(i in 1:nrow(opt_loc)){ # over all centroids (10,000)
#
#   loc <- c(opt_loc$x[i], opt_loc$y[i])
#
#   dist <- dist2Line(loc, my_emst)
#   opt_loc$dist2emst[i] <- dist[1] / 1000
#
#   print(i)
# }
#
#
# ###
# # indicate EMST nodes
# ###
#
# df_coords <- rbind(nodes@coords, sa_nodes)
# row.names(df_coords) <- 1:nrow(df_coords)
# opt_loc$isnode <- 0
#
# for(i in 1:nrow(polygon_dataframe)){
#   single_cell <- polygon_dataframe[which(polygon_dataframe@data$ID == polygon_dataframe@data$ID[i]),]
#   if(gIntersects(single_cell, all_nodes)){
#     opt_loc[i, "isnode"] <- 1
#   }
# }
#
#
# # Get EmstKM
#
# rails <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/Built/Railroads.TAB")
# opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")
#
#
# df <- opt_loc[!is.na(opt_loc$zeta),]
# row.names(df) <- 1:nrow(df)
#
# df$bl_x <- df$x - 0.25
# df$tl_x <- df$x - 0.25
# df$br_x <- df$x + 0.25
# df$tr_x <- df$x + 0.25
#
# df$bl_y <- df$y - 0.25
# df$tl_y <- df$y + 0.25
# df$br_y <- df$y - 0.25
# df$tr_y <- df$y + 0.25
#
#
# polygon_file = SpatialPolygons(lapply(1:nrow(df), function(x) Polygons(list(Polygon( cbind(t(df[x, c("bl_x", "tl_x", "tr_x", "br_x")]), t(df[x, c("bl_y", "tl_y", "tr_y", "br_y")])) )), paste0(x))))
#
# polygon_dataframe = SpatialPolygonsDataFrame(polygon_file, df)
#
#
# opt_loc$emstKM <- NA
#
# for(i in 1:nrow(polygon_dataframe)){
#   single_cell <- polygon_dataframe[which(polygon_dataframe@data$ID == polygon_dataframe@data$ID[i]),]
#   if(gIntersects(single_cell, my_emst)){
#     intersect <- gIntersection(single_cell, my_emst)
#     if(!("coords" %in% slotNames(intersect))){
#     if("lineobj" %in% slotNames(intersect)){
#       opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "emstKM"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
#       print(c(i, "with multiple"))
#     } else{
#       opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "emstKM"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
#       print(i)
#     }
#
#   }
#   } else{
#     opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "emstKM"] <- 0
# }
# }
#
# write.csv(opt_loc, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv", row.names = FALSE)
#
# #####
# # Differential lines by EMST
# #####
#
# buffer_emst <- spTransform(buffer(spTransform(my_emst, CRS("+proj=robin +datum=WGS84")), width=40000), CRS("+proj=longlat +datum=WGS84")) # this is a double projection: I first project it from longlat to metric coordiantes ("robin"), apply the buffer of 40KM in each direction, and then reproject it to longlat for it to be adjusted to the rest of the space
#
# rails_in_buffer <- gIntersection(buffer_emst, all_rails)
#
# opt_loc$bufferKM <- NA
#
# for(i in 1:nrow(polygon_dataframe)){
#   single_cell <- polygon_dataframe[which(polygon_dataframe@data$ID == polygon_dataframe@data$ID[i]),]
#   if(gIntersects(single_cell, rails_in_buffer)){
#     intersect <- gIntersection(single_cell, rails_in_buffer)
#     if(!("coords" %in% slotNames(intersect))){
#     if("lineobj" %in% slotNames(intersect)){
#       opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "bufferKM"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
#       print(c(i, "with multiple"))
#     } else{
#       opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "bufferKM"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
#       print(i)
#     }
#
#   }
#   } else{
#     opt_loc[opt_loc$ID == polygon_dataframe@data$ID[i], "bufferKM"] <- 0
# }
# }
#
# write.csv(opt_loc[,c("ID", "bufferKM")], file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_bufferKM.csv", row.names = FALSE)


#####
# Railroad Maps
#####
#
# africa <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/Territory/Africa.TAB")
#
# png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/other_maps/all_rails.png", sep=""), width=6, height=6, units = 'in', res=300 )
# plot(africa)
# plot(all_rails, col="red", add=T)
# plot(all_placebo, col="blue", add=T, lty=2)
# dev.off()
#
# png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/other_maps/emst.png", sep=""), width=6, height=6, units = 'in', res=300 )
# plot(africa)
# plot(my_emst, col="orange", add=T)
# plot(buffer_emst, col=alpha("orange", 0.1), add=T, border=alpha("grey", 0.3))
# plot(all_nodes, add=T, col=alpha("red", 0.5), pch=20, cex=.5)
# dev.off()
