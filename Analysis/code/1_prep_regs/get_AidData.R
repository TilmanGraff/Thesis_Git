require(rgdal)
require(geosphere)
require(raster)
require(rgeos)
require(gepaf)
require(sf)

##############################
# ENTER RAW DATA
##############################

# WORLDBANK
###########
aid_loc <- read.csv("./Analysis/input/AidData_WorldBank/data/locations_perturbed.csv")
aid_loc <- aid_loc[grepl("Africa", aid_loc$gazetteer_adm_name),]

aid_proj <- read.csv("./Analysis/input/AidData_WorldBank/data/projects.csv")
aid_proj <- aid_proj[aid_proj$is_geocoded==1,]

aid <- merge(aid_loc, aid_proj, by="project_id")


for(i in unique(aid$project_id)){ # this is to uniformly streamline total project disbursements over all locations which are part of the project. (assuming uniform allocation...)

  total_comm <- (aid[aid$project_id==i,"total_commitments"])[1]
  total_dis <- (aid[aid$project_id==i,"total_disbursements"])[1]

  aid[aid$project_id==i,"share_commitments"] <- total_comm / length(aid[aid$project_id==i,"total_commitments"])
  aid[aid$project_id==i,"share_disbursements"] <- total_dis / length(aid[aid$project_id==i,"total_disbursements"])

}

aid <- aid[aid$recipients_iso3 != "Unspecified" & aid$recipients != "Namibia",]
aid <- aid[aid$precision_code < 4,]
#aid <- aid[aid$share_disbursements < quantile(aid$share_disbursements, .99, na.rm=T),]
sp_aid = SpatialPointsDataFrame(aid[,c("longitude", "latitude")], aid, proj4string = crs(grid))



# CHINA
###########
aid_loc_China <- read.csv("./Analysis/input/AidData_China/locations_perturbed.csv", stringsAsFactors=FALSE)
aid_proj_China <- read.csv("./Analysis/input/AidData_China/projects.csv", sep=";", stringsAsFactors=FALSE)
colnames(aid_proj_China)[1] <- "ID"

aid_China <- merge(aid_loc_China, aid_proj_China[,c("ID", "usd_current", "description")], by.x="project_id", by.y="ID")

aid_China$usd_current <- as.numeric(gsub("\\,", ".", aid_China$usd_current))
aid_China$latitude <- as.numeric(gsub("\\,", ".", aid_China$latitude))
aid_China$longitude <- as.numeric(gsub("\\,", ".", aid_China$longitude))


for(i in unique(aid_China$project_id)){ # this is to uniformly streamline total project disbursements over all locations which are part of the project. (assuming uniform allocation...)

  total_comm <- (aid_China[aid_China$project_id==i,"usd_current"])[1]

  aid_China[aid_China$project_id==i,"share_usd"] <- total_comm / length(aid_China[aid_China$project_id==i,"usd_current"])

}

aid_China <- aid_China[!grepl("Africa, regional", aid_China$recipient_condensed),]
#aid_China <- aid_China[!grepl("independent political entity", aid_China$location_type),]
aid_China <- aid_China[aid_China$precision_code < 4,]
#aid_China <- aid_China[aid_China$share_usd < quantile(aid_China$share_usd, 0.99, na.rm=T),]

aid_China$year_completion <- as.numeric(format(as.Date(as.character(aid_China$end_actual), format="%d. %b %y"), "%Y"))

##############################
# MERGE ONTO GRID CELLS
##############################

grid = readOGR("./Analysis/input/grid_shapefile/grid.shp")
the_merge = over(grid, sp_aid["project_id"], byid = T, returnList = T, FN = NULL)
outputfile = data.frame("ID" = grid$ID)

for(i in 1:length(grid)){
  gridid = grid[i,]$ID
  rel_projectcodes = as.vector(unlist(the_merge[paste(i)]))
  rel_projects = aid[aid$project_id %in% rel_projectcodes,]

  outputfile[outputfile$ID == gridid, "wb_dis"] = sum(rel_projects$share_disbursements, na.rm=T) / 1000000
  outputfile[outputfile$ID == gridid, "wb_dis_compl"] <- sum(rel_projects[rel_projects$status == "Completion","share_disbursements"], na.rm=T) / 1000000
  outputfile[outputfile$ID == gridid, "wb_dis_old"] <- sum(rel_projects[rel_projects$transactions_end_year < 2002, "share_disbursements"], na.rm=T) / 1000000

  outputfile[outputfile$ID == gridid, "wb_commitments"] <- sum(rel_projects$share_commitments, na.rm=T) / 1000000

  outputfile[outputfile$ID == gridid, "wb_dis_transp"] <- sum(rel_projects[grepl("Transport and storage", rel_projects$ad_sector_names),"share_disbursements"], na.rm=T) / 1000000
  outputfile[outputfile$ID == gridid, "wb_dis_transp_compl"] <- sum(rel_projects[grepl("Transport and storage", rel_projects$ad_sector_names) & rel_projects$status == "Completion","share_disbursements"], na.rm=T) / 1000000
  outputfile[outputfile$ID == gridid, "wb_dis_transp_old"] <- sum(rel_projects[grepl("Transport and storage", rel_projects$ad_sector_names) & rel_projects$transactions_end_year < 2002, "share_disbursements"], na.rm=T) / 1000000

  outputfile[outputfile$ID == gridid, "wb_num"] <- nrow(rel_projects)
  outputfile[outputfile$ID == gridid, "wb_num_compl"] <- nrow(rel_projects[rel_projects$status == "Completion",])
  outputfile[outputfile$ID == gridid, "wb_num_old"] <- nrow(rel_projects[rel_projects$transactions_end_year < 2002,])


  outputfile[outputfile$ID == gridid, "wb_num_transp"] <- nrow(rel_projects[grepl("Transport and storage", rel_projects$ad_sector_names),])
  outputfile[outputfile$ID == gridid, "wb_num_transp_compl"] <- nrow(rel_projects[grepl("Transport and storage", rel_projects$ad_sector_names) & rel_projects$status == "Completion",])
  outputfile[outputfile$ID == gridid, "wb_num_transp_old"] <- nrow(rel_projects[grepl("Transport and storage", rel_projects$ad_sector_names) & rel_projects$transactions_end_year < 2002,])

}

### I stopped this here last time
xxx


for(i in 1:length(grid)){
    merger_WB <- (point.in.polygon(aid$longitude, aid$latitude, polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,1], polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,2]))

    subset_aid_WB <- aid[merger_WB==1,]

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_dis"] <- sum(subset_aid_WB$share_disbursements, na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_dis_compl"] <- sum(subset_aid_WB[subset_aid_WB$status == "Completion","share_disbursements"], na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_dis_old"] <- sum(subset_aid_WB[subset_aid_WB$transactions_end_year < 2002, "share_disbursements"], na.rm=T) / 1000000

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_commitments"] <- sum(subset_aid_WB$share_commitments, na.rm=T) / 1000000

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_dis_transp"] <- sum(subset_aid_WB[grepl("Transport and storage", subset_aid_WB$ad_sector_names),"share_disbursements"], na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_dis_transp_compl"] <- sum(subset_aid_WB[grepl("Transport and storage", subset_aid_WB$ad_sector_names) & subset_aid_WB$status == "Completion","share_disbursements"], na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_dis_transp_old"] <- sum(subset_aid_WB[grepl("Transport and storage", subset_aid_WB$ad_sector_names) & subset_aid_WB$transactions_end_year < 2002, "share_disbursements"], na.rm=T) / 1000000

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_num"] <- nrow(subset_aid_WB)
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_num_compl"] <- nrow(subset_aid_WB[subset_aid_WB$status == "Completion",])
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_num_old"] <- nrow(subset_aid_WB[subset_aid_WB$transactions_end_year < 2002,])


    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_num_transp"] <- nrow(subset_aid_WB[grepl("Transport and storage", subset_aid_WB$ad_sector_names),])
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_num_transp_compl"] <- nrow(subset_aid_WB[grepl("Transport and storage", subset_aid_WB$ad_sector_names) & subset_aid_WB$status == "Completion",])
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "wb_num_transp_old"] <- nrow(subset_aid_WB[grepl("Transport and storage", subset_aid_WB$ad_sector_names) & subset_aid_WB$transactions_end_year < 2002,])

    ########
    # China
    ########

    merger_China <- (point.in.polygon(aid_China$longitude, aid_China$latitude, polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,1], polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,2]))

    subset_aid_China <- aid_China[merger_China==1,]

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_dis"] <- sum(subset_aid_China$share_usd, na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_dis_compl"] <- sum(subset_aid_China[subset_aid_China$status == "Completion", "share_usd"], na.rm=T) / 1000000

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_dis_transp"] <- sum(subset_aid_China[grepl("Transport", subset_aid_China$crs_sector_name), "share_usd"], na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_dis_transp_compl"] <- sum(subset_aid_China[grepl("Transport", subset_aid_China$crs_sector_name) & subset_aid_China$status == "Completion", "share_usd"], na.rm=T) / 1000000

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_num"] <- nrow(subset_aid_China)
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_num_compl"] <- nrow(subset_aid_China[subset_aid_China$status == "Completion",])

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_num_transp"] <- nrow(subset_aid_China[grepl("Transport", subset_aid_China$crs_sector_name),])
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_num_transp_compl"] <- nrow(subset_aid_China[grepl("Transport", subset_aid_China$crs_sector_name) & subset_aid_China$status == "Completion",])

}

keepvars <- c("ID", colnames(opt_loc)[grepl("wb", colnames(opt_loc)) | grepl("china", colnames(opt_loc))])

write.csv(opt_loc[,keepvars], file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/grid_ids_aid.csv", row.names = FALSE)


#####
# Aid Maps
#####
#
# world <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/World_Countries/TM_WORLD_BORDERS-0.3.shp") # reads in the global country shapefile
#
# africa <- world[world@data$REGION==2,]
#
#
# ssudan <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/South-Sudan/ssd_admbnda_adm0_200k_ssnbs_20160114.shp")
#
# ssudan@data <- africa@data[1,]
# ssudan@data[1,] <- NA
# ssudan@data$NAME <- "South-Sudan"
#
#
# africa <- rbind(africa, ssudan)
#
# # Worldbank
# aid$cex_size <- log((aid$share_disbursements / max(aid$share_disbursements, na.rm=T)) + 1.01) * 15
#
# cex <- vector()
# label <- vector()
# for(i in c(10, 100, 200)){
#   cex <- c(cex, log((i*10^6 / max(aid$share_disbursements, na.rm=T)) + 1.01) * 15)
#   label <- c(label, i)
#
# }
# png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/other_maps/aid_wb.png", sep=""), width=6, height=6, units = 'in', res=300 )
# plot(africa)
# points(aid$longitude, aid$latitude, col=alpha("dodgerblue3", 0.3), cex=aid$cex_size, pch=20)
# legend("bottomleft", legend=rev(label), pt.cex=rev(cex), inset = 0.2, pch=20, col=alpha("dodgerblue3", 0.1), bty='n', cex=0.7)
# dev.off()
#
# # China
# aid_China$cex_size <- log((aid_China$share_usd / max(aid_China$share_usd, na.rm=T)) + 1.01) * 10
#
# #for legend
# cex <- vector()
# label <- vector()
# for(i in c(100, 400, 1000)){
#   cex <- c(cex, log((i*10^6 / max(aid_China$share_usd, na.rm=T)) + 1.01) * 10)
#   label <- c(label, i)
#
# }
#
# png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/other_maps/aid_China.png", sep=""), width=6, height=6, units = 'in', res=300 )
# plot(africa)
# points(aid_China$longitude, aid_China$latitude, col=alpha("firebrick3", 0.3), cex=aid_China$cex_size, pch=20)
# legend("bottomleft", legend=rev(label), pt.cex=rev(cex), inset = 0.2, pch=20, col=alpha("firebrick3", 0.1), bty='n', cex=0.7)
# dev.off()
