
# Import clean global centroids file, just for clean representation of country names
centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv")

# Restrict file sample if needed
centroids <- centroids[centroids$region == 2,]


# Gather country names
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

for (country in country_names){

  case_centroids <- centroids[centroids$country == country,]
  n <- nrow(case_centroids)

  speed <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/speed/speed_", country, ".csv", sep=""))

  dist <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/dist/dist_", country, ".csv", sep=""))

  chars <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/characteristics/characteristics_", country, ".csv", sep=""))

  # Existing infrastructure as a function of the speed matrix
  I <- speed

  # Infrastructure building cost as a function of geography
  delta_I_unscaled <- matrix(0, nrow = n, ncol = n)
  delta_I <- matrix(0, nrow = n, ncol = n)

  for(i in 1:n){
    for(j in i:n){
      if(dist[i,j] != 0 & !is.na(dist[i,j])){
        delta_I_unscaled[i,j] <- exp(- 0.11 * as.numeric(dist[i,j] > 50) + 0.12 * log(0.5*(chars$rugg[i] + chars$rugg[j])) + log(dist[i,j])) # can I find more things in Collier's paper on this? Altitude differences?
        delta_I_unscaled[j,i] <- delta_I_unscaled[i,j]
      }
    }
  }

  scaling_parameter <- 1/(sum(delta_I_unscaled * I)) # this sum has to be equal to K (=1) via the network-building constraint

  delta_I <- delta_I_unscaled * scaling_parameter


  # Trade Cost Constant as a function of distance
  delta_0_tau <- 1.2 # this comes from Table A.3 in FS2017, and has to vary according to the model parameters I end up using. (1.2 is for gamma = 0.5*beta and fixed labour, but will amend)

  delta_tau <- delta_0_tau * dist


  write.csv(delta_tau, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/delta_tau/delta_tau_", country, ".csv", sep=""), row.names = FALSE)

  write.csv(delta_I, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/delta_I/delta_I_", country, ".csv", sep=""), row.names = FALSE)

  write.csv(I, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/I/I_", country, ".csv", sep=""), row.names = FALSE)

}
