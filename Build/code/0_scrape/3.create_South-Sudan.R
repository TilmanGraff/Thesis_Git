##
# This file denotes centroids that fall over the territory of South Sudan as belonging to this new country. Basically, it just edits the country-name tag of these entries. The rest more or less goes automatically.


require("rgdal")
require("polyclip")

# Import borders from a UN source
ssudan <- readOGR("./Build/input/South-Sudan/ssd_admbnda_adm0_200k_ssnbs_20160114.shp")

centroids <- read.csv("./Build/temp/centroids_noSSudan.csv")

levels(centroids$country) <- c(levels(centroids$country), "South-Sudan") # this creates a new level for countries, as they are not saved as strings in order to save space

centroids[point.in.polygon(centroids$x, centroids$y, ssudan@polygons[[1]]@Polygons[[1]]@coords[,1], ssudan@polygons[[1]]@Polygons[[1]]@coords[,2]) == 1 & centroids$country == "Sudan", "country"] <- "South-Sudan" # this basically checks whether a point's coordinates fall over the polygon of the borders of South Sudan. If they do, I adjust the country name to "South-Sudan". I have to do two minor corrections: a) one centroid is initially coded as DRC, which I believe is a noise issue of the UN border data. I keep this centroid as belonging to DRC.

centroids[centroids$country=="Sudan" & centroids$y < 8.5, "country"] <- "South-Sudan" # two centroids are just along the border of the South, but not recognised by the imprecise UN border data. When plotting the new countries, these two points dangle weirdly below everything else. They obviously belong to South Sudan but are not captured by the previous procedure. I hence manually code them as belonging to the South.

centroids[centroids$country=="South-Sudan", "un_code"] <- 728


write.csv(format(centroids, scientific=F), "./Build/temp/centroids.csv", row.names = FALSE) # I export back to centroids. IMPORTANT: If you were to run this script now, nothing would happen as all is rightly coded already.
