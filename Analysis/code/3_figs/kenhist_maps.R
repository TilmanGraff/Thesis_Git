require("osrm")
require("Imap")
require("gepaf")
require("rgeos")
require("rgdal")
require("scales")
require("viridis")
require("classInt")
require("mapview")


polys = readOGR("/Users/tilmangraff/Dropbox (Harvard University)/spin-inputs/input/World_Countries/TM_WORLD_BORDERS-0.3.shp")
ken = polys[polys$NAME == "Kenya",]

coords = read.csv("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/centroids.csv")


kennetwork = function(I){
I_opt_scaled <- 11 * ((I - 3)^0.8 / max((I)^0.8))

outcomes$pop_scaled <- 5 * ((outcomes$L_statmob)^0.4 / max((outcomes$L_statmob))^0.4)

# plot(coords$centroidx, coords$centroidy, main = year, bty = "n", pch = 19, axes = F, ylab = "", xlab = "", asp = 1, type = "n") # if you want to plot it


plot(ken, col = alpha("lightskyblue1", 0.3), border = NA,  main = year, bty = "n")
points(coords$centroidx, coords$centroidy, pch = 21, bg = "dodgerblue4", cex = outcomes$pop_scaled)

for(i in 1:nrow(outcomes)){
  for(j in 1:nrow(outcomes)){
    if(I_opt[i,j] != 0){
      points(coords[c(i,j),"centroidx"], coords[c(i,j),"centroidy"], type = "l", lwd = I_opt_scaled[i,j], col = alpha("dodgerblue", .8))
    }
  }
}
}

for(year in c(1963,1990, 2007)){

outcomes = read.csv(paste0("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_outcomes/outcomes_", year, ".csv"))
I_opt = read.csv(paste0("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_outcomes/I_opt_", year, ".csv"), header = F)
I_stat = read.csv(paste0("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_data/I_", year, ".csv"), header = T)


pdf(file = paste0("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_outcomes/map_", year, "_opt.pdf"), width = 11, height = 11)
kennetwork(I_opt)
dev.off()

pdf(file = paste0("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_outcomes/map_", year, "_stat.pdf"), width = 11, height = 11)
kennetwork(I_stat)
dev.off()

}

