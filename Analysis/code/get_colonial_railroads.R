
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

rails <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/Built/Railroads.TAB")

south_african_rails <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/South_africa.shp")

south_african_rails@data <- rails@data[1:length(south_african_rails),] # this creates an empty databox for south africa (of data which I do not have), in order to merge seemlessly
south_african_rails@data[1] <- NA
south_african_rails@data[2] <- NA
south_african_rails@data[3] <- NA
south_african_rails@data[4] <- NA
south_african_rails@data[5] <- NA
south_african_rails@data[6] <- NA
south_african_rails@data[7] <- NA

south_african_rails <- spTransform(south_african_rails, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) # gives common CRS to merge seemlessly
rails <- spTransform(rails, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) # gives common CRS to merge seemlessly

all_rails <- rbind(rails, south_african_rails) # merges


opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")

opt_loc$dist2rail <- NA
opt_loc$id_closest_rail <- NA


for(i in 1:nrow(opt_loc)){ # over all centroids (10,000)

  loc <- c(opt_loc$x[i], opt_loc$y[i])
  mindist <- 10^9
  whichline <- NA

  for(j in 1:length(all_rails)){ # over all rails (240)

    dist <- dist2Line(loc, all_rails@lines[[j]]@Lines[[1]]@coords)[1]

    if(mindist > dist){ # iteratively updates to find smallest distance
      mindist <- dist
      whichline <- j # saves slot number of closest rail
    }

  }

  opt_loc$dist2rail[i] <- mindist / 1000
  opt_loc$id_closest_rail[i] <- whichline

  print(i)
}

write.csv(opt_loc, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist", row.names = FALSE)
