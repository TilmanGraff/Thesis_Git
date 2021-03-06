# Aggregates findings from optimisation into the opt_loc dataset. Most importantly, defines lambda.

#setwd("/Users/tilmangraff/Documents/GitHub/Thesis_Git")


centroids <- read.csv("./Build/temp/centroids.csv")

# Gather country names
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

countryfiles <- NA

for (country in country_names){

  if(file.exists(paste("./Build/output/Network_outcomes/", country, "_outcomes.csv", sep = ""))){

    countryfile <- read.csv(paste("./Build/output/Network_outcomes/", country, "_outcomes.csv", sep = ""))

    # quich excursion to also capture the I_change:
    # I_stat
    I_initial <- read.csv(paste("./Build/temp/I/I_", country, ".csv", sep = ""))
    # I_opt
    I_opt <- read.csv(paste("./Build/output/Optimised_Networks/", country, ".csv", sep = ""),  header=FALSE)
    # create difference
    countryfile$I_change <- rowSums(I_opt - I_initial) / 2

    countryfile$country <- country

    if(length(countryfiles)==1){
      countryfiles <- countryfile
    } else{
      countryfiles <- rbind(countryfiles, countryfile)
    }

  }

}

# to make it unique, just toss out all abroad cells
countryfiles = countryfiles[countryfiles$abroad == 0,]

# merge in other data
opt_loc <- merge(centroids, countryfiles[,!names(countryfiles) %in% c("x","y","pop","abroad")], by=c("ID", "country"), all.x=T)

# for the first time, create the zeta variable
opt_loc$zeta <- opt_loc$util_opt / opt_loc$util_stat



write.csv(opt_loc, file="./Analysis/input/opt_loc.csv", row.names = FALSE)
