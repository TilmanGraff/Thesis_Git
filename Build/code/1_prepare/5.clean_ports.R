# import and clean ports coordinates dataset

require("rgdal")
require("geosphere")
require("raster")
require("rgeos")
require("Imap")
require("gepaf")
require("vegan")
require("scales")


# Import
ports = data.frame(readxl::read_excel("/Users/tilmangraff/Dropbox (Harvard University)/spin-inputs/input/ports/african_ports.xlsx", col_names=F))

colnames(ports) = "coords.raw"

# From raw text coordinates to numeric decimal coordinates

split = strsplit(ports$coords.raw, ";")

for(i in 1:nrow(ports)){

  ports[i,"lat.raw"] = split[[i]][1]
  ports[i,"lon.raw"] = split[[i]][2]


}

ports$lat.degrees = as.numeric(gsub("(.*)Lat ", "", (gsub("°(.*)", "", ports$lat.raw))))
ports$lon.degrees = as.numeric(gsub("(.*)Long ", "", (gsub("°(.*)", "", ports$lon.raw))))

ports$lat.minutes = as.numeric(gsub("(.*)°.", "", (gsub("\\'(.*)", "", ports$lat.raw))))
ports$lon.minutes = as.numeric(gsub("(.*)°.", "", (gsub("\\'(.*)", "", ports$lon.raw))))

ports$lat.degrees = ifelse(grepl("' N", ports$coords.raw), 1, -1) * ports$lat.degrees
ports$lon.degrees = ifelse(grepl("' E", ports$coords.raw), 1, -1) * ports$lon.degrees

ports$lat = ports$lat.degrees + ifelse(grepl("' N", ports$coords.raw), 1, -1) * ports$lat.minutes / 60
ports$lon = ports$lon.degrees + ifelse(grepl("' E", ports$coords.raw), 1, -1) * ports$lon.minutes / 60

ports = ports[1:90,]


######
# Spatial Merge onto grid cell level

df <- read.csv("./Build/temp/centroids.csv")
df$isport = 0

row.names(df) <- 1:nrow(df)
# back out corner locations from centroid
df$bl_x <- df$x - 0.25
df$tl_x <- df$x - 0.25
df$br_x <- df$x + 0.25
df$tr_x <- df$x + 0.25
df$bl_y <- df$y - 0.25
df$tl_y <- df$y + 0.25
df$br_y <- df$y - 0.25
df$tr_y <- df$y + 0.25

polygon_file = SpatialPolygons(lapply(1:nrow(df), function(x) Polygons(list(Polygon( cbind(t(df[x, c("bl_x", "tl_x", "tr_x", "br_x")]), t(df[x, c("bl_y", "tl_y", "tr_y", "br_y")])) )), paste0(x))))
polygon_dataframe = SpatialPolygonsDataFrame(polygon_file, df)

# compute pairwise distance between all points and all centroids
dists = gDistance(SpatialPoints(ports[,c("lon", "lat")]), polygon_dataframe, byid = T)


for(i in 1:nrow(ports)){ # assign each port to its closest grid cell

  whichid = which(dists[,i] == min(dists[,i]))
  df[whichid,"isport"] = 1

}

write.csv(df[,c("ID", "isport")], "./Build/temp/ports.csv")

# Plot ports on map
polys = readOGR("./Build/output/African Borders/AfricanBorders.shp")

pdf(file="./Build/output/other_maps/ports.pdf", width = 11, height = 11)

plot(polys, col = alpha("lightskyblue1", 1), border = "white")
points(ports$lon, ports$lat, pch=19, col=alpha("navy", .6))

dev.off()
