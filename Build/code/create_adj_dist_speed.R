
# This file transforms given centroid geometries into adj, dist, and infrastr matrices

library("osrm", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("Imap", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")


# Import clean global centroids file
centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv")
colnames(centroids)[1] <- "ID"

# Restrict file sample to Africa
centroids <- centroids[centroids$region == 2,]
#centroids <- centroids[centroids$country == "Lesotho",]

# Gather country names
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

plot(centroids$x, centroids$y) # if you want to plot it

###############

# For every country, calculate matrices
for(country in country_names){
case_centroids <- centroids[centroids$country == country,]

png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Road_Networks/network_", country, ".png", sep=""))
plot(case_centroids$x, case_centroids$y, main=country) # if you want to plot it

n <- nrow(case_centroids)

# this is to create a rosetta stone which translates the (globally unique) cell ID into the rownumber of the square matrices needed for Matlab
case_centroids$rownumber <- c(1:n)


case_centroids$productivity <- case_centroids$lights / case_centroids$pop


adj <- matrix(0, nrow = n, ncol = n)
dist <- matrix(0, nrow = n, ncol = n)
speed <- matrix(0, nrow = n, ncol = n)
walked <- matrix(0, nrow = n, ncol = n)

minutes_per_kilometer <- 15

for(i in 1:n){


# identifies the nine centroids within the respective circle to find itself plus eight nearest neighbors
  neighbour_ids <- case_centroids[(case_centroids$y-case_centroids$y[i])^2+(case_centroids$x-case_centroids$x[i])^2<=0.5^2+0.5^2,"ID"]
  df <- case_centroids[case_centroids$ID %in% neighbour_ids, c("ID", "x", "y") ]
  colnames(df) <- c("id", "lon", "lat")

# scrapes OSRM for routes to all neighbours excluding itself
    for(a in neighbour_ids[! neighbour_ids %in% case_centroids[i, "ID"]]){
      route <- osrmRoute(src=df[df$id==case_centroids$ID[i],], dst=df[df$id==a,], sp=TRUE, overview = "simplified")
      j <- case_centroids[case_centroids$ID == a,"rownumber"]

          if(!is.null(route)){
            coords <- route@lines[[1]]@Lines[[1]]@coords
            # creates the walked distance to the start of the route and from the end of it
            walked[i,j] <- gdist(df[df$id==case_centroids$ID[i],"lon"], df[df$id==case_centroids$ID[i],"lat"], coords[1,"lon"], coords[1,"lat"], units = "km")  + gdist(df[df$id==a,"lon"], df[df$id==a,"lat"], coords[nrow(coords),"lon"], coords[nrow(coords),"lat"], units = "km")

            if(route$duration + walked[i,j]*minutes_per_kilometer < minutes_per_kilometer*(gdist(df[df$id==case_centroids$ID[i],"lon"], df[df$id==case_centroids$ID[i],"lat"], df[df$id==a,"lon"], df[df$id==a,"lat"], units = "km"))){ # if walking entire way is longer than proposed route

            # creates matrices for every country
            dist[i, j] <- route$distance + walked[i,j]
            speed[i, j] <- dist[i,j] / ((route$duration + walked[i,j]*minutes_per_kilometer) / 60)
            adj[i, j] <- 1

            points(c(df[df$id==case_centroids$ID[i],"lon"], coords[,1], df[df$id==a,"lon"]), c(df[df$id==case_centroids$ID[i],"lat"], coords[,2], df[df$id==a,"lat"]), type='l') # if you want to plot it

          } else{ # if not, you just walk the entire direct line
              dist[i, j] <- gdist(df[df$id==case_centroids$ID[i],"lon"], df[df$id==case_centroids$ID[i],"lat"], df[df$id==a,"lon"], df[df$id==a,"lat"], units = "km")
              speed[i, j] <- 60/minutes_per_kilometer
              adj[i, j] <- 1

              points(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==a,"lon"]), c(df[df$id==case_centroids$ID[i],"lat"],  df[df$id==a,"lat"]), type='l', col="red") # if you want to plot it

              }
            } else{
            dist[i, j] <- NA
            speed[i, j] <- NA
            adj[i, j] <- NA
            points(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==a,"lon"]), c(df[df$id==case_centroids$ID[i],"lat"],  df[df$id==a,"lat"]), type='l', col="blue") # if you want to plot it
          }
    }

  }


write.csv(dist, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/dist/dist_", country, ".csv", sep=""), row.names = FALSE)
write.csv(speed, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/speed/speed_", country, ".csv", sep=""), row.names = FALSE)
write.csv(adj, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/adj/adj_", country, ".csv", sep=""), row.names = FALSE)
write.csv(case_centroids[,c("ID", "rownumber", "productivity", "rugg", "pop")], file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/characteristics/characteristics_", country, ".csv", sep=""), row.names = FALSE)
dev.off()

country

}
