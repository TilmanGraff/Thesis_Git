
##############
# This file makes adj dist and speed matrices symmetric. In doing so, it captures two distinct notions of assymmetry produced by OSRM: a) For dist and speed, OSRM oftentimes spits out different routes and hence different distances and speeds depending on whether you go X->Y or Y->X. This is corrected by simply taking the average of the two directions. b) Is more treacherous: sometimes OSRM would refuse to give a route in one direction (producing NAs) while supplying a route in the opposite direction. For this, I created a loop which in this case assigns each directions the information of the one direction with information. Obviously there could be reasons why one direction is unattainable, but I don't believe this should play a role in my analysis. Also, it utterly destroys the Matlab code.

# Since I overwrite files, I should only need to do this once (or each time after running create_adj_dist_speed). When I ran it on Dec 14, I corrected cases in Algeria (6 times), Cameroon (1 time), Central African Republic (1 time), and Tunisia (2 times).

# Import clean global centroids file, just for clean representation of country names
centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv")

# Restrict file sample if needed
centroids <- centroids[centroids$region == 2,]

centroids$rownumber <- as.numeric(paste(centroids$rownumber))

# Gather country names
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

for (country in country_names){

  case_centroids <- centroids[centroids$country == country,]
  n <- nrow(case_centroids)

  speed <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/speed/speed_", country, ".csv", sep=""))

  dist <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/dist/dist_", country, ".csv", sep=""))

  adj <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/adj/adj_", country, ".csv", sep=""))

  #this deals with b)
  for(i in 1:n){
    for(j in i:n){

      if(adj[i,j] != adj[j,i]){ # this check if one direction is supplied while the other one is not

        print(country)

        adj[i,j] <- 1 # if one direction is connected, the other is said to be conencted as well.
        adj[j,i] <- 1

        dist[i,j] <- max(dist[i,j], dist[j,i]) # in this case, one direction would have dist = 0, so I give both directions the larger of the two
        dist[j,i] <- dist[i,j]

        speed[i,j] <- max(speed[i,j], speed[j,i]) # same with speed
        speed[j,i] <- speed[i,j]


      }
    }
  }


  # this then deals with a) -- the much more benign problem.
  speed <- (speed + t(speed)) / 2
  dist <- (dist + t(dist)) / 2


  write.csv(dist, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/dist/dist_", country, ".csv", sep=""), row.names = FALSE)
  write.csv(speed, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/speed/speed_", country, ".csv", sep=""), row.names = FALSE)
  write.csv(adj, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/adj/adj_", country, ".csv", sep=""), row.names = FALSE)




}
