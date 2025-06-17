require(readstata13)
require(sp)
require(rgdal)
require(dismo)
require(rgeos)

x = readOGR("/Users/tilmangraff/Downloads/dataverse_files/JedwabStoreygardReplication/data/inputs/roads_CS_polyline.shp")
ken = x[x$COUNTRY == "Kenya",]
# opt_loc = read.dta13("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Analysis/input/maingrid.dta")
# grid = readOGR("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Analysis/input/grid_shapefile/grid.shp")
# grid@data = merge(grid@data, opt_loc, by.x="ID", by.y = "id")

timecost = read.csv("/Users/tilmangraff/Downloads/dataverse_files/JedwabStoreygardReplication/data/inputs/timecost.csv")

centroids = read.csv("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/centroids.csv")

centsp = SpatialPointsDataFrame(coords = centroids[,2:3], data = centroids, proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs"))

#kengrid = grid[grid$country == "Kenya",]
africa = readOGR("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/African Borders/AfricanBorders.shp")

kenbord = africa[africa$NAME == "Kenya",]
newgrid = raster::intersect(voronoi(centsp, ext = bbox(kenbord)), kenbord)

clipken = raster::intersect(newgrid, ken)

############

gdp = read.csv("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/wb_gdp.csv")
pop = read.csv("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/wb_pop.csv")

ken_gdp2010 = gdp[gdp$label == 2010,2]



delta_I_unscaled <- matrix(0, nrow = 41, ncol = 41)
delta_i <- matrix(0, nrow = 41, ncol = 41)

for(i in 1:41){
  for(j in i:41){
    if(dist[i,j] != 0 & !is.na(dist[i,j])){
      delta_I_unscaled[i,j] <- exp(- 0.11 * as.numeric(dist[i,j] > 50) + 0.12 + log(dist[i,j])) 
      delta_I_unscaled[j,i] <- delta_I_unscaled[i,j]
    }
  }
}

############

dist = gDistance(centsp, byid = T)
adj = 1*(gTouches(newgrid, newgrid, byid = T))

############

over_sp = gIntersects(clipken, newgrid, byid = T)

for(year in 1960:2012){
  if(paste0("R", year) %in% names(x)){
    
    for(i in 1:nrow(over_sp)){
      
      roadtypes = x[as.vector(over_sp[i,]),]@data[,paste0("R", year)]
      roadspeeds = vector()
      for(r in 1:length(roadtypes)){
        roadspeeds = c(roadspeeds, 120/timecost[timecost$roadclass == roadtypes[r],"base"])
      }
      
      newgrid[i,"i"] = mean(roadspeeds, na.rm = T)
    }
    
    I = matrix(0, nrow = nrow(newgrid), ncol = nrow(newgrid))
    
    
    for(i in 1:nrow(newgrid)){
      for(j in as.numeric(which(adj[i,] == 1))){
        I[i,j] = mean(newgrid[c(i,j),]$i)
      }
    }
    
    
    prod = matrix(0, nrow = 41, ncol = 6)
    
    curr_other = 1
    for(i in 1:nrow(prod)){
      
      if(rank(-centroids$pop1962)[i] < 6){
        thisone = curr_other
        curr_other = curr_other + 1
      }else{
        thisone = 6
      }
      
      prod[i,thisone] = 1
      
    }
    
    prod = prod * (sum(grid[grid$country == "Kenya",]$lights * 900) / sum(pop[pop$label==year,2])) * gdp[gdp$label==year,2] / ken_gdp2010
    
    centroids[,paste0("p", year)] = (centroids$pop1962 / pop[pop$label==1962,2]) * pop[pop$label==year,2] 
    
    write.csv(prod, paste0("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_data/prod_", year, ".csv"), row.names = F)
    write.csv(I, paste0("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_data/I_", year, ".csv"), row.names = F)
    
    
    
    scaling_parameter <- 1/(sum(delta_I_unscaled * I))
    
    
    delta_i <- delta_I_unscaled * scaling_parameter
    write.csv(delta_i, paste0("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_data/delta_i_", year, ".csv"), row.names = F)
    
    
    
  }
}


delta_tau = adj * ((0.0248/(0.43*0.25541) + 0.0254/(1.03*4.1418)) / 2) * log(dist * 55 / 1.609) 
delta_tau[delta_tau < 0] <- 0
diag(delta_tau) <- 0


write.csv(adj, "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/adj.csv", row.names = F)

write.csv(delta_tau, "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/delta_tau.csv", row.names = F)

write.csv(centroids, "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_data/allpop.csv", row.names = F)





