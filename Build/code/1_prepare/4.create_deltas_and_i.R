# Parameterises deltas and I matrices from the OSM raw data

# Import clean global centroids file, just for clean representation of country names
# centroids <- read.csv("./Build/temp/centroids.csv")

# Restrict file sample if needed
# centroids <- centroids[centroids$region == 2,]
#centroids$rownumber <- as.numeric(paste(centroids$rownumber))

# Gather country names
# country_table <- as.data.frame(table(centroids$country))
# country_names <- paste(country_table[country_table$Freq != 0,"Var1"])
countries = read.csv("./Build/temp/country_names.csv")$x
#countries = c("Germany", "Japan", "China", "United-States")

for (country in countries){

  if(file.exists(paste("./Build/temp/adj/adj_", country, ".csv", sep=""))){

  case_centroids <- read.csv(paste("./Build/temp/borderregions/", country, "_borderregion.csv", sep=""))
  n <- nrow(case_centroids)

  if(0 %in% case_centroids$rugg){ # this eliminates zero ruggedness which causes problems with delta_I later on. In essence, I just spot these instances and replace them with the mean ruggedness of the entire country
    case_centroids[case_centroids$rugg == 0, "rugg"] <- mean(case_centroids$rugg)
    print(country)
  }

  adj <- read.csv(paste("./Build/temp/adj/adj_", country, ".csv", sep=""))
  speed <- read.csv(paste("./Build/temp/speed/speed_", country, ".csv", sep=""))
  dist <- read.csv(paste("./Build/temp/dist/dist_", country, ".csv", sep=""))
  abr <- read.csv(paste("./Build/temp/abr/abr_", country, ".csv", sep=""))

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
        delta_I_unscaled[i,j] <- exp(- 0.11 * as.numeric(dist[i,j] > 50) + 0.12 * log(0.5*(case_centroids[i, "rugg"] + case_centroids[j, "rugg"])) + log(dist[i,j])) # can I find more things in Collier's paper on this? UPDATE; Why should these matter? Collier have population density, but that's a selection issue, not a geographic determinant.
        delta_I_unscaled[j,i] <- delta_I_unscaled[i,j]
      }
    }
  }

  scaling_parameter <- 1/(sum(delta_I_unscaled * I)) # this sum has to be equal to K (=1) via the network-building constraint. I only rescale this respective to the homeland! NO, with the new toolbox, I can rescale this to the entire thing!

  delta_I <- delta_I_unscaled * scaling_parameter

########
  # Trade Cost Constant as a function of distance
########

  # OLD VERSION - built on FS2017
  # delta_0_tau <- 1.2 # this comes from Table A.3 in FS2017, and has to vary according to the model parameters I end up using. (1.2 is for gamma = 0.5*beta and fixed labour, but will amend)
  # delta_tau <- delta_0_tau * dist

  # NEW VERSION -- built on Donaldson / Atkin 2015
  delta_0_tau <- (0.0374/0.43 + 0.0558/1.03) / 2 # this is the mean of columns (3) and (6) on page 44 of their paper, scaled by mean base prices for each country, so as to make these ad-valorem
  delta_tau <-  delta_0_tau * log(dist / 1.609)
  delta_tau[delta_tau < 0] <- 0

  # NEW VERSION WITH FIXEDPOINT
  delta_0_tau_fp <- (0.0374/(0.43 * 0.13385) + 0.0558/(1.03 * 0.983)) / 2 # this is the mean of columns (3) and (6) on page 44 of their paper, scaled by mean base prices for each country, so as to make these ad-valorem
  delta_tau_fp <-  delta_0_tau_fp * log(dist / 1.609)
  delta_tau_fp[delta_tau_fp < 0] <- 0

  delta_0_tau_withcomp <- (0.0248/0.43 + 0.0254/1.03) / 2 # this is the mean of columns (2) and (5) on page 44 of their paper, scaled by mean base prices for each country, so as to make these ad-valorem
  delta_tau_withcomp <- delta_0_tau_withcomp * log(dist / 1.609)
  delta_tau_withcomp[delta_tau_withcomp < 0] <- 0

  delta_0_tau_withcomp_fp <- (0.0248/(0.43*0.25541) + 0.0254/(1.03*4.1418)) / 2 # this is the mean of columns (2) and (5) on page 44 of their paper, scaled by mean base prices for each country, so as to make these ad-valorem
  delta_tau_withcomp_fp <- delta_0_tau_withcomp_fp * log(dist / 1.609)
  delta_tau_withcomp_fp[delta_tau_withcomp_fp < 0] <- 0


  write.csv(delta_tau, file=paste("./Build/temp/delta_tau/delta_tau_", country, ".csv", sep=""), row.names = FALSE)
  write.csv(delta_tau_withcomp, file=paste("./Build/temp/delta_tau/delta_tau_withcomp_", country, ".csv", sep=""), row.names = FALSE)
  write.csv(delta_tau_withcomp_fp, file=paste("./Build/temp/delta_tau/delta_tau_withcomp_fp_", country, ".csv", sep=""), row.names = FALSE)
  write.csv(delta_tau_fp, file=paste("./Build/temp/delta_tau/delta_tau_fp_", country, ".csv", sep=""), row.names = FALSE)

  
  write.csv(delta_I, file=paste("./Build/temp/delta_I/delta_I_", country, ".csv", sep=""), row.names = FALSE)
  write.csv(I, file=paste("./Build/temp/I/I_", country, ".csv", sep=""), row.names = FALSE)
  

}
}
