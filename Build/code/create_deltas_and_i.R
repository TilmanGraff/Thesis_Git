
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

  if(0 %in% case_centroids$rugg){ # this eliminates zero ruggedness which causes problems with delta_I later on. In essence, I just spot these instances and replace them with the mean ruggedness of the entire country
    case_centroids[case_centroids$rugg == 0, "rugg"] <- mean(case_centroids$rugg)
    print(country)
  }

  speed <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/speed/speed_", country, ".csv", sep=""))

  dist <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/dist/dist_", country, ".csv", sep=""))

########
  # Existing infrastructure as a function of the speed matrix
########

  I <- speed

########
  # Infrastructure building cost as a function of geography
########

  delta_I_unscaled <- matrix(0, nrow = n, ncol = n)
  delta_I <- matrix(0, nrow = n, ncol = n)

  for(i in 1:n){
    for(j in i:n){
      if(dist[i,j] != 0 & !is.na(dist[i,j])){
        delta_I_unscaled[i,j] <- exp(- 0.11 * as.numeric(dist[i,j] > 50) + 0.12 * log(0.5*(case_centroids[case_centroids$rownumber==i, "rugg"] + case_centroids[case_centroids$rownumber==j, "rugg"])) + log(dist[i,j])) # can I find more things in Collier's paper on this? UPDATE; Why should these matter? Collier have population density, but that's a selection issue, not a geographic determinant.
        delta_I_unscaled[j,i] <- delta_I_unscaled[i,j]
      }
    }
  }

  scaling_parameter <- 1/(sum(delta_I_unscaled * I)) # this sum has to be equal to K (=1) via the network-building constraint

  delta_I <- delta_I_unscaled * scaling_parameter

########
  # Trade Cost Constant as a function of distance
########

  # OLD VERSION - built on FS2017
  # delta_0_tau <- 1.2 # this comes from Table A.3 in FS2017, and has to vary according to the model parameters I end up using. (1.2 is for gamma = 0.5*beta and fixed labour, but will amend)
  # delta_tau <- delta_0_tau * dist

  # NEW VERSION -- built on Donaldson / Atkin 2015
  delta_0_tau <- (0.0374 + 0.0558) / 2 # this is the mean of columns (3) and (6) on page 44 of their paper
  delta_tau <- delta_0_tau * log(dist)
  delta_tau[delta_tau < 0] <- 0



  write.csv(delta_tau, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/delta_tau/delta_tau_", country, ".csv", sep=""), row.names = FALSE)

  write.csv(delta_I, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/delta_I/delta_I_", country, ".csv", sep=""), row.names = FALSE)

  write.csv(I, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/I/I_", country, ".csv", sep=""), row.names = FALSE)

}
