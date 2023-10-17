
# This file transforms given centroid geometries into adj, dist, and infrastr matrices
# It does so by scraping OSM for Moore neighborhood connections between centroid coordinates

require("osrm")
require("Imap")
require("gepaf")
require("rgeos")
require("rgdal")
require("scales")
require("sf")
require("geosphere")


centroids <- read.csv("./Build/temp/centroids.csv")

# Gather country names
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

polys = readOGR("/Users/tilmangraff/Dropbox (Harvard University)/spin-inputs/input/World_Countries/TM_WORLD_BORDERS-0.3.shp")
polys$NAME = gsub(" ", "-", polys$NAME)

###############

extra_countries = c("Germany", "Japan", "China", "United-States")


# For every country, calculate matrices
for(country in c(country_names, extra_countries)){

  if(!(paste("dist_", country, ".csv", sep="") %in% list.files("./Build/temp/raw_from_OSRM/dist/"))){

    case_centroids <- read.csv(paste0("./Build/temp/borderregions/", country, "_borderregion.csv"))
    case_poly = polys[polys$NAME == country,]

    pdf(file=paste("./Build/output/Road_Networks/network_", country, ".pdf", sep=""), width = 8, height = 8)

    plot(case_centroids$x, case_centroids$y, main=country, bty = "n", pch = ifelse(case_centroids$country == country, 19, 1), axes=F, ylab="", xlab="", asp=1, col="grey") # if you want to plot it
    plot(case_poly, add=T, col = alpha("dodgerblue", .2), border = NA)

    n <- nrow(case_centroids)

    # this is to create a rosetta stone which translates the (globally unique) cell ID into the rownumber of the square matrices needed for Matlab
    case_centroids$rownumber <- c(1:n)

    adj <- matrix(0, nrow = n, ncol = n)
    dist <- matrix(0, nrow = n, ncol = n)
    speed <- matrix(0, nrow = n, ncol = n)
    walked <- matrix(0, nrow = n, ncol = n)

    minutes_per_kilometer <- 15

    for(i in 1:n){


      # identifies the nine centroids within the respective circle to find itself plus eight nearest neighbors
      if(country %in% c("United-States", "China")){
      neighbour_ids <- case_centroids[(case_centroids$y-case_centroids$y[i])^2+(case_centroids$x-case_centroids$x[i])^2<=2,"ID"]
      } else{
      neighbour_ids <- case_centroids[(case_centroids$y-case_centroids$y[i])^2+(case_centroids$x-case_centroids$x[i])^2<=0.5^2+0.5^2,"ID"]
      }



      df <- case_centroids[case_centroids$ID %in% neighbour_ids, c("ID", "x", "y") ]
      colnames(df) <- c("id", "lon", "lat")

      # scrapes OSRM for routes to all neighbours excluding itself
      for(a in neighbour_ids[! neighbour_ids %in% case_centroids[i, "ID"]]){

        j <- case_centroids[case_centroids$ID == a,"rownumber"]
        if(adj[j,i] == 0){

          route = NULL
          attemptcounter = 1
          while(is.null(route) & attemptcounter <= 50){ # I try until I have a non-null route, or 100 times, whichever comes faster

            # This is where the action happens, I scrape the route from OSRM
            message = try(route <- osrmRoute(src=df[df$id==case_centroids$ID[i],c("lon", "lat")], dst=df[df$id==a,c("lon", "lat")], overview = "simplified"), silent = T)


            attemptcounter = attemptcounter+1
            if(attemptcounter > 2){ # now, if I've already tried twice, I begin to sleep for a second to let the server catch its breath
              Sys.sleep(1)
              print(attemptcounter)
              if(attemptcounter == 3 | attemptcounter %% 5 == 0){ # I also check once what the reason for the error was, it it's "NoRoute", I gather this here and immediately jump to the next id. This is to circumvent super-long annoying loops in which I retry 100 times in vain because there is no route. There is still a rare problem that occurs when the server is down first, so attemptcounter goes beyond 3 and then when it comes back it turns out there is no route. Then attemptcounter is already past the moment where it should check what the problem is. Not sure how often this would happen, but worth a think. Maybe do something like if(attemptcounter %% 10 == 0) or something?
                message = try(route <- osrmRoute(src=df[df$id==case_centroids$ID[i],c("lon", "lat")], dst=df[df$id==a,c("lon", "lat")], overview = "simplified"), silent = T) 
                #message = capture.output(route <- osrmRoute(src=df[df$id==case_centroids$ID[i],c("lon", "lat")], dst=df[df$id==a,c("lon", "lat")], overview = "simplified"))
                #captures the server's error message
                if(grepl("Impossible route", message[1])){ # if NoRoute, sett counter to 101 to end loop
                  attemptcounter = 1001 # break the while loop
                }
              }
            }
          }


          if(!is.null(route)){
            #coords <- route@lines[[1]]@Lines[[1]]@coords
            coords = st_coordinates(route)[,c("X", "Y")]
            # creates the walked distance to the start of the route and from the end of it
            walked[i,j] <- distm(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==case_centroids$ID[i],"lat"]), c(coords[1,"X"], coords[1,"Y"])) / 1000  + distm(c(df[df$id==a,"lon"], df[df$id==a,"lat"]), c(coords[nrow(coords),"X"], coords[nrow(coords),"Y"])) / 1000

            across_border = case_centroids[case_centroids$ID==df[df$id==case_centroids$ID[i],"id"],"country"] != country | case_centroids[case_centroids$ID==df[df$id==a,"id"],"country"] != country

            if(route$duration + walked[i,j]*minutes_per_kilometer < minutes_per_kilometer*(distm(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==case_centroids$ID[i],"lat"]), c(df[df$id==a,"lon"], df[df$id==a,"lat"]))) / 1000){ # if walking entire way is longer than proposed route

              # creates matrices for every country
              dist[i, j] <- route$distance + walked[i,j]
              speed[i, j] <- dist[i,j] / ((route$duration + walked[i,j]*minutes_per_kilometer) / 60)
              adj[i, j] <- 1

              points(c(df[df$id==case_centroids$ID[i],"lon"], coords[,1], df[df$id==a,"lon"]), c(df[df$id==case_centroids$ID[i],"lat"], coords[,2], df[df$id==a,"lat"]), type='l', col = ifelse(across_border, alpha("grey", .5), "dodgerblue3"), lwd = ifelse(across_border, 1.2, 2)) # if you want to plot it

            } else{ # if not, you just walk the entire direct line
              dist[i, j] <- distm(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==case_centroids$ID[i],"lat"]), c(df[df$id==a,"lon"], df[df$id==a,"lat"])) / 1000
              speed[i, j] <- 60/minutes_per_kilometer
              adj[i, j] <- 1

              points(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==a,"lon"]), c(df[df$id==case_centroids$ID[i],"lat"],  df[df$id==a,"lat"]), type='l', col = ifelse(across_border, alpha("grey", .2), alpha("dodgerblue1", .4)), lwd = 1) # if you want to plot it

            }
          } else{
            dist[i, j] <- 0
            speed[i, j] <- 0
            adj[i, j] <- 0
            #points(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==a,"lon"]), c(df[df$id==case_centroids$ID[i],"lat"],  df[df$id==a,"lat"]), type='l', col="blue") # if you want to plot it
          }
        }
      }

      print(paste0(round(i/n*100, 2), "% of ", country, " completed"))

    }


    write.csv(dist, file=paste("./Build/temp/raw_from_OSRM/dist/dist_", country, ".csv", sep=""), row.names = FALSE)
    write.csv(speed, file=paste("./Build/temp/raw_from_OSRM/speed/speed_", country, ".csv", sep=""), row.names = FALSE)
    write.csv(adj, file=paste("./Build/temp/raw_from_OSRM/adj/adj_", country, ".csv", sep=""), row.names = FALSE)
    write.csv(case_centroids[,c("ID", "rownumber")], file=paste("./Build/temp/rosettastones/rosetta_", country, ".csv", sep=""), row.names = FALSE)
    dev.off()
  }
}
