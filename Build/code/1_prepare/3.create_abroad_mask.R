# for every country, I create a mask-matrix that is 1 for all abroad connections. Hence the idea is that I * abroad gives I_abroad, and I * (adj-abroad) gives I_home

countries = read.csv("./Build/temp/country_names.csv")

for(country in countries$x){

  if(file.exists(paste("./Build/temp/adj/adj_", country, ".csv", sep=""))){

  borderregion = read.csv(paste0("./Build/temp/borderregions/", country, "_borderregion.csv"))
  adj <- read.csv(paste("./Build/temp/adj/adj_", country, ".csv", sep=""))

  abr = matrix(0, nrow = nrow(borderregion), ncol = nrow(borderregion))

  for(i in 1:nrow(borderregion)){
    for(j in which(adj[i,] ==1)){

      if(borderregion[i,"abroad"]==1 | borderregion[j,"abroad"]==1){

        abr[i,j] = 1

      }

    }
  }

  write.csv(abr, file=paste("./Build/temp/abr/abr_", country, ".csv", sep=""), row.names = FALSE)
}

}
