# df <- data.frame(id =NA, lon=NA, lat=NA)
#
# df[1,] <- c(1, -1.256301, 51.746715)
# df[2,] <- c(2, -1.263206, 51.762509)
#
# route <- osrmRoute(src=df[1,], dst=df[2,], sp=TRUE)
#

# fromJSON("http://router.project-osrm.org/route/v1/driving/51.746715,-1.256301;51.762509,-1.263206?overview=false&annotations=speed")

1+1





plain_centroids <- read_csv("~/Desktop/plain_centroids.csv")

centroids <- as.data.frame(plain_centroids[plain_centroids$X<7 & plain_centroids$Y<8,])
n <- nrow(centroids)
centroids$rownumber <- c(1:n)

adj <- matrix(0, nrow = n, ncol = n)
dist <- matrix(0, nrow = n, ncol = n)
speed <- matrix(0, nrow = n, ncol = n)


plot(centroids$X, centroids$Y, cex=.2)

# df$avg_speed_to1 <- NA
# df$dist_to1 <- NA
# df$time_to_1 <- NA

for(i in 1:nrow(centroids)){

  ids <- centroids[(centroids$top-centroids$top[i])^2+(centroids$left-centroids$left[i])^2<=0.25^2,"ID"]

  df <- centroids[centroids$ID %in% ids, c("ID", "X", "Y") ]
  colnames(df) <- c("id", "lon", "lat")

  for(a in ids){
    route <- osrmRoute(src=df[df$id==centroids$ID[i],], dst=df[df$id==a,], sp=TRUE, overview = "simplified")

    coords <- route@lines[[1]]@Lines[[1]]@coords
    #points(coords[,1], coords[,2], type="l")
  }
    # df[(df$lon-df$lon[i])^2+(df$lat-df$lat[i])^2<=0.25^2 & df$id != df$id[i],"id"]
    # df$dist_to1[i] <- route[2]
    # df$avg_speed_to1[i] <- route[2] / (route[1]/60)
    # df$time_to_1[i] <- route[1]
  }


  # route <- osrmRoute(src=df[1,], dst=df[i,], sp=TRUE, overview = FALSE)
  #
  # df[(df$lon-df$lon[i])^2+(df$lat-df$lat[i])^2<=0.25^2 & df$id != df$id[i],"id"]
  # df$dist_to1[i] <- route[2]
  # df$avg_speed_to1[i] <- route[2] / (route[1]/60)
  # df$time_to_1[i] <- route[1]
  # coords <- route@lines[[1]]@Lines[[1]]@coords
  # points(coords[,1], coords[,2], type="l")
}
