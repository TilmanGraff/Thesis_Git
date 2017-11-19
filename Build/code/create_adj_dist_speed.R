
# This file transforms given centroid geometries into adj, dist, and infrastr matrices

library("osrm", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

centroids <- read.csv("/Build/temp/country_centroids/Nigeria.csv")
n <- nrow(centroids)

centroids$rownumber <- c(1:n) # this is to create a later rosetta stone

adj <- matrix(0, nrow = n, ncol = n)
dist <- matrix(0, nrow = n, ncol = n)
speed <- matrix(0, nrow = n, ncol = n)

#for(i in 1:nrow(centroids)){
  for(i in 1:30){

  neighbour_ids <- centroids[(centroids$top-centroids$top[i])^2+(centroids$left-centroids$left[i])^2<=0.25^2+0.25^2,"ID"]
  df <- centroids[centroids$ID %in% neighbour_ids, c("ID", "X", "Y") ]
  colnames(df) <- c("id", "lon", "lat")

    for(a in neighbour_ids[! neighbour_ids %in% centroids[i, "ID"]]){           # scrapes OSRM for routes to all neighbours excluding itself
      route <- osrmRoute(src=df[df$id==centroids$ID[i],], dst=df[df$id==a,], sp=TRUE, overview = F)
      j <- centroids[centroids$ID == a,"rownumber"]

      dist[i, j] <- route[2]
      speed[i, j] <- route[2] / (route[1]/60)
      adj[i, j] <- 1

      }

  }

write.csv(dist, file="Build/temp/dist/Nigeria.csv")
write.csv(speed, file="/Build/temp/speed/Nigeria.csv")
write.csv(adj, file="/Build/temp/adj/Nigeria.csv")
