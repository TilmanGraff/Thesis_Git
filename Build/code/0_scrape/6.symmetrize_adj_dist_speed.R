
##############
# This file makes adj dist and speed matrices symmetric. In doing so, it captures two distinct notions of assymmetry produced by OSRM: a) For dist and speed, OSRM oftentimes spits out different routes and hence different distances and speeds depending on whether you go X->Y or Y->X. This is corrected by simply taking the average of the two directions. b) Is more treacherous: sometimes OSRM would refuse to give a route in one direction (producing NAs) while supplying a route in the opposite direction. For this, I created a loop which in this case assigns each directions the information of the one direction with information. Obviously there could be reasons why one direction is unattainable, but I don't believe this should play a role in my analysis. Also, it utterly destroys the Matlab code.

# Since I overwrite files, I should only need to do this once (or each time after running create_adj_dist_speed). When I ran it on Dec 14, I corrected cases in Algeria (6 times), Cameroon (1 time), Central African Republic (1 time), and Tunisia (2 times).

# Import clean global centroids file, just for clean representation of country names
centroids <- read.csv("./Build/temp/centroids.csv")


# Gather country names
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

for (country in country_names){

  if(file.exists(paste("./Build/temp/raw_from_OSRM/adj/adj_", country, ".csv", sep=""))){


  case_centroids <- centroids[centroids$country == country,]
  n <- nrow(case_centroids)

  speed <- read.csv(paste("./Build/temp/raw_from_OSRM/speed/speed_", country, ".csv", sep=""))

  dist <- read.csv(paste("./Build/temp/raw_from_OSRM/dist/dist_", country, ".csv", sep=""))

  adj <- read.csv(paste("./Build/temp/raw_from_OSRM/adj/adj_", country, ".csv", sep=""))



  # this then deals with a) -- the much more benign problem.
  speed <- (speed + t(speed))
  dist <- (dist + t(dist))
  adj <- (adj + t(adj))


  write.csv(dist, file=paste("./Build/temp/dist/dist_", country, ".csv", sep=""), row.names = FALSE)
  write.csv(speed, file=paste("./Build/temp/speed/speed_", country, ".csv", sep=""), row.names = FALSE)
  write.csv(adj, file=paste("./Build/temp/adj/adj_", country, ".csv", sep=""), row.names = FALSE)


}

}
