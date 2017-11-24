
# This file transforms given centroid geometries into adj, dist, and infrastr matrices

library("osrm", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

# Import clean global centroids file
centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv")
colnames(centroids)[1] <- "ID"

# Restrict file sample to Africa
centroids <- centroids[centroids$region == 2,]
centroids <- centroids[centroids$country == "Benin",]


# Gather country names
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

###############

# For every country, calculate matrices
for(country in country_names){

case_centroids <- centroids[centroids$country == country,]

n <- nrow(case_centroids)

# this is to create a rosetta stone which translates the (globally unique) cell ID into the rownumber of the square matrices needed for Matlab
case_centroids$rownumber <- c(1:n)


case_centroids$productivity <- case_centroids$lights / case_centroids$pop


adj <- matrix(0, nrow = n, ncol = n)
dist <- matrix(0, nrow = n, ncol = n)
speed <- matrix(0, nrow = n, ncol = n)


for(i in 1:n){


# identifies the nine centroids within the respective circle to find itself plus eight nearest neighbors
  neighbour_ids <- case_centroids[(case_centroids$y-case_centroids$y[i])^2+(case_centroids$x-case_centroids$x[i])^2<=0.5^2+0.5^2,"ID"]
  df <- case_centroids[case_centroids$ID %in% neighbour_ids, c("ID", "x", "y") ]
  colnames(df) <- c("id", "lon", "lat")

# scrapes OSRM for routes to all neighbours excluding itself
    for(a in neighbour_ids[! neighbour_ids %in% case_centroids[i, "ID"]]){
      route <- osrmRoute(src=df[df$id==case_centroids$ID[i],], dst=df[df$id==a,], sp=TRUE, overview = F)
      j <- case_centroids[case_centroids$ID == a,"rownumber"]

    # creates matrices for every country
      dist[i, j] <- route[2]
      speed[i, j] <- route[2] / (route[1]/60)
      adj[i, j] <- 1

      }

  }


write.csv(dist, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/dist/dist_", country, ".csv", sep=""))
write.csv(speed, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/speed/speed_", country, ".csv", sep=""))
write.csv(adj, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/adj/adj_", country, ".csv", sep=""))
write.csv(case_centroids[,c("ID", "rownumber", "productivity", "rugg", "pop")], file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/characteristics/characteristics_", country, ".csv", sep=""))
}
