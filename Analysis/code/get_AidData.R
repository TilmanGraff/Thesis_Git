
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rgeos", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("Imap", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gepaf", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("vegan", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

aid_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/AidData_WorldBank/data/locations.csv")
aid_loc <- aid_loc[grepl("Africa", aid_loc$gazetteer_adm_name),]


aid_proj <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/AidData_WorldBank/data/projects.csv")
aid_proj <- aid_proj[aid_proj$is_geocoded==1,]

aid <- merge(aid_loc, aid_proj, by="project_id")

for(i in unique(aid$project_id)){ # this is to uniformly streamline total project disbursements over all locations which are part of the project. (assuming uniform allocation...)

  total_comm <- (aid[aid$project_id==i,"total_commitments"])[1]
  total_dis <- (aid[aid$project_id==i,"total_disbursements"])[1]

  aid[aid$project_id==i,"share_commitments"] <- total_comm / length(aid[aid$project_id==i,"total_commitments"])
  aid[aid$project_id==i,"share_disbursements"] <- total_dis / length(aid[aid$project_id==i,"total_disbursements"])

}

aid <- aid[!grepl("Africa", aid$recipients),]
aid <- aid[!grepl("independent political entity", aid$location_type_name),]

# China

aid_loc_China <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/AidData_China/locations.csv", sep=";", stringsAsFactors=FALSE)
aid_proj_China <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/AidData_China/projects.csv", sep=";", stringsAsFactors=FALSE)
colnames(aid_proj_China)[1] <- "ID"

aid_China <- merge(aid_loc_China, aid_proj_China[,c("ID", "usd_current", "description")], by.x="project_id", by.y="ID")

aid_China$usd_current <- as.numeric(aid_China$usd_current)
aid_China$latitude <- as.numeric(gsub("\\,", ".", aid_China$latitude))
aid_China$longitude <- as.numeric(gsub("\\,", ".", aid_China$longitude))


for(i in unique(aid_China$project_id)){ # this is to uniformly streamline total project disbursements over all locations which are part of the project. (assuming uniform allocation...)

  total_comm <- (aid_China[aid_China$project_id==i,"usd_current"])[1]

  aid_China[aid_China$project_id==i,"share_usd"] <- total_comm / length(aid_China[aid_China$project_id==i,"usd_current"])

}

aid_China <- aid_China[!grepl("Africa", aid_China$recipient_oecd_name),]
aid_China <- aid_China[!grepl("independent political entity", aid_China$location_type),]
aid_China$year_completion <- as.numeric(format(as.Date(as.character(aid_China$end_actual), format="%d. %b %y"), "%Y"))

# Merge onto opt_loc gridcells

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")

df <- opt_loc[!is.na(opt_loc$zeta),]
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

for(i in 1:length(polygon_dataframe)){
    merger_WB <- (point.in.polygon(aid$longitude, aid$latitude, polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,1], polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,2]))

    merger_China <- (point.in.polygon(aid_China$longitude, aid_China$latitude, polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,1], polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,2]))

    subset_aid_WB <- aid[merger_WB==1,]
    subset_aid_China <- aid_China[merger_China==1,]

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "worldbank_disbursements"] <- sum(subset_aid_WB$share_disbursements, na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "worldbank_commitments"] <- sum(subset_aid_WB$share_commitments, na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "worldbank_transport_disbursements"] <- sum(subset_aid_WB[grepl("Transport and storage", subset_aid_WB$ad_sector_names),"share_disbursements"], na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "worldbank_transport_disbursements_completed"] <- sum(subset_aid_WB[grepl("Transport and storage", subset_aid_WB$ad_sector_names) & subset_aid_WB$status == "Completion","share_disbursements"], na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "number_wb_projects"] <- nrow(subset_aid_WB)

    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_aid"] <- sum(subset_aid_China$share_usd, na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_transport_aid"] <- sum(subset_aid_China[grepl("Transport", subset_aid_China$crs_sector_name), "share_usd"], na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "china_transport_aid_completed"] <- sum(subset_aid_China[grepl("Transport", subset_aid_China$crs_sector_name) & !is.na(subset_aid_China$year_completion), "share_usd"], na.rm=T) / 1000000
    opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "number_china_projects"] <- nrow(subset_aid_China)


}

write.csv(opt_loc[,c("ID", "worldbank_commitments", "worldbank_disbursements", "worldbank_transport_disbursements", "worldbank_transport_disbursements_completed", "number_wb_projects", "number_china_projects")], file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/grid_ids_aid.csv", row.names = FALSE)
