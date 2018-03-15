
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rgeos", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("Imap", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gepaf", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("vegan", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("readstata13", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library(RColorBrewer)
library(classInt)



###############
# ON GRID LEVEL
###############


epr_poly <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/GeoEPR-2014/GeoEPR-2014.shp")
epr <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/GeoEPR-2014/EPR-2014.csv")
opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")

for(i in 1:(length(epr_poly))){
  merger <- (point.in.polygon(opt_loc$x, opt_loc$y, epr_poly@polygons[[i]]@Polygons[[1]]@coords[,1], epr_poly@polygons[[i]]@Polygons[[1]]@coords[,2]))

  opt_loc[merger==1,"gwgroupid"] <- epr_poly@data[i,"gwgroupid"]
}

opt_loc <- merge(opt_loc, epr[epr$to==2013,c("gwgroupid", "status", "group")], by="gwgroupid", all.x=T)

write.csv(opt_loc[,c("ID", "status", "group")], file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_epr.csv", row.names = FALSE)

###############
# ON ETHNICITY LEVEL
###############

# opt_ethn <- as.data.frame(unique(opt_loc[!is.na(opt_loc$gwgroupid), "gwgroupid"]))
# colnames(opt_ethn) <- "gwgroupid"
#
# for(i in 1:nrow(opt_ethn)){
#   subset <- opt_loc[opt_loc$gwgroupid==opt_ethn$gwgroupid[i],]
#   subset <- subset[!is.na(subset$rownumber),]
#
#   opt_ethn[i, "group"] <- subset[1, "group"]
#   opt_ethn[i, "country"] <- subset[1, "country"]
#   opt_ethn[i, "pop"] <- sum(subset$pop, na.rm=T)
#   opt_ethn[i, "rugg"] <- mean(subset$rugg, na.rm=T)
#   opt_ethn[i, "malaria"] <- mean(subset$malaria, na.rm=T)
#   opt_ethn[i, "lights"] <- mean(subset$lights, na.rm=T)
#   opt_ethn[i, "growingdays"] <- mean(subset$growingdays, na.rm=T)
#   opt_ethn[i, "temp"] <- mean(subset$temp, na.rm=T)
#   opt_ethn[i, "altitude"] <- mean(subset$altitude, na.rm=T)
#   opt_ethn[i, "urban"] <- mean(subset$urban, na.rm=T)
#   opt_ethn[i, "landsuit"] <- mean(subset$landsuit, na.rm=T)
#   opt_ethn[i, "precip"] <- mean(subset$precip, na.rm=T)
#   opt_ethn[i, "x"] <- mean(subset$x, na.rm=T)
#   opt_ethn[i, "y"] <- mean(subset$y, na.rm=T)
#   opt_ethn[i, "first.x"] <- subset[1, "x"]
#   opt_ethn[i, "first.y"] <- subset[1, "y"]
#   opt_ethn[i, "RailKM"] <- sum(subset$RailKM, na.rm=T)
#
#   if(opt_ethn[i, "pop"] >0 ){
#     opt_ethn[i, "zeta"] <- (sum(subset$pop * subset$util_opt)  / sum(subset$pop * subset$util_stat))
#   } else{
#     opt_ethn[i, "zeta"] <- NA
#   }
#   opt_ethn[i, "strict_mean_zeta"] <- mean(subset$zeta, na.rm=T)
#   opt_ethn[i, "status"] <- subset[1, "status"]
#
# }
#
# write.csv(opt_ethn, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_ethn.csv", row.names = FALSE)

###########
# Make heatmap
###########

# Collapse grids

mappable_dataset <- as.data.frame(unique(opt_loc$gwgroupid))
colnames(mappable_dataset) <- c("gwgroupid")
mappable_dataset$zeta <- NA
mappable_dataset <- mappable_dataset[!is.na(mappable_dataset[,1]),]

for(i in mappable_dataset$gwgroupid){
  subset <- opt_loc[opt_loc$gwgroupid==i,]
  subset <- subset[!is.na(subset$rownumber),]

  mappable_dataset[mappable_dataset$gwgroupid==i,"zeta"] <- (sum(subset$pop * subset$util_opt)  / sum(subset$pop * subset$util_stat))

}


# breaks_qt <- classIntervals(mappable_dataset$zeta, n = 8, style = "quantile")
# br <- breaks_qt$brks
# offs <- 0.0000001
# br[1] <- br[1] - offs
# br[length(br)] <- br[length(br)] + offs
mappable_dataset$zeta_bracket <- cut(mappable_dataset$zeta, br)

epr_merged <- merge(epr_poly, mappable_dataset, by="gwgroupid")
epr_merged <- epr_merged[!is.na(epr_merged$zeta),]
my.palette <- brewer.pal(n = 9, name = "OrRd") # for an orange palette

## THE EDGES SOMEHOW ARE WEIRD SO I TAKE THOSE FROM THE ACLED DATASET (RUN BOTH OF THEM FOLLOWING EACH OTHER IN RSTUDIO)
# edges <- epr_merged@bbox
# scale.parameter = 1.1
# xshift = 0
# yshift = 0
#
# edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,
#     ]) + xshift
# edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2,
#     ]) + yshift
#
# max_extend <- max(abs(edges[1,2]-edges[1,1]), abs(edges[2,2]-edges[2,1]))
#
# edges[1,1] <- mean(edges[1,]) - 0.5*max_extend
# edges[1,2] <- mean(edges[1,]) + 0.5*max_extend
#
# edges[2,1] <- mean(edges[2,]) - 0.5*max_extend
# edges[2,2] <- mean(edges[2,]) + 0.5*max_extend


png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/other_maps/epr_zeta.png", sep=""), width=6, height=6, units = 'in', res = 200)
print(spplot(epr_merged, "zeta_bracket", col="transparent", col.regions=my.palette, cuts=8, xlim=edges[1,], ylim=edges[2,]))

dev.off()
