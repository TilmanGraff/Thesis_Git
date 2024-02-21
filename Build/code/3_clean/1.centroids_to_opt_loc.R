require(igraph)
require(dplyr)
# Aggregates findings from optimisation into the opt_loc dataset. Most importantly, defines lambda.

# setwd("/Users/tilmangraff/Documents/GitHub/Thesis_Git")

centroids <- read.csv("./Build/temp/centroids.csv")

# Gather country names
countries <- read.csv("./Build/temp/country_names.csv")

countryfiles <- NA


gamma <- 0.946
beta <- 1.2446 * gamma
sigma <- 5
# gamma = 0.5*beta

runs <- data.frame("names" = c("base_old", "mob", "imm"), "paths" = c("2023-07-17_111951_base", "2024-02-18_040636_mobile_fp_final", "2024-02-20_140808_imm_fp_final"))

for (country in countries$x) {
  if (file.exists(paste0("./Build/output/", runs[runs$names == "mob", "paths"], "/Network_outcomes/", country, "_outcomes.csv"))) {
    countryfile <- read.csv(paste0("./Build/output/", runs[runs$names == "mob", "paths"], "/Network_outcomes/", country, "_outcomes.csv"))
    countryfile$country <- country

    delta_tau <- read.csv(paste("./Build/temp/delta_tau/delta_tau_", country, ".csv", sep=""))

    #quich excursion to also capture the I_change:
    #I_stat
    I_initial <- read.csv(paste("./Build/temp/I/I_", country, ".csv", sep = ""))
    adj <- read.csv(paste("./Build/temp/adj/adj_", country, ".csv", sep = ""))


    g_initial_m <- as.matrix(I_initial)^(-gamma)
    g_initial_m[adj == 0] <- 0
    g_initial <- graph_from_adjacency_matrix(g_initial_m, weighted = T)
    d_initial <- as.matrix(distances(g_initial))^(-sigma)
    diag(d_initial) <- 0



    for (runidx in 1:nrow(runs)) {
      run <- runs$names[runidx]
      path <- runs$paths[runidx]

      if (file.exists(paste0("./Build/output/", path, "/Network_outcomes/", country, "_outcomes.csv"))) {
        

        for(type in c("", "_10p")){
          if(file.exists(paste("./Build/output/", path, "/Optimised_Networks/", country, type, ".csv", sep = ""))){
            tcountryfile <- read.csv(paste("./Build/output/", path, "/Network_outcomes/", country, "_outcomes.csv", sep = ""))
            I_opt <- read.csv(paste("./Build/output/", path, "/Optimised_Networks/", country, type, ".csv", sep = ""), header = FALSE)

            g_opt_m <- as.matrix(I_opt)^(-gamma)
            g_opt_m[adj == 0] <- 0
            g_opt <- graph_from_adjacency_matrix(g_opt_m, weighted = T)
            d_opt <- as.matrix(distances(g_opt))^(-sigma)
            diag(d_opt) <- 0

          
            if(grepl("mob", run)){
              tcountryfile[, paste0("amenity_", run, type)] <- tcountryfile$amenities
              tcountryfile[, paste0("pop_opt_", run, type)] <- tcountryfile[,paste0("pop_opt", type)]
              tcountryfile[, paste0("MA_initial_", run, type)] <- d_initial %*% as.vector(tcountryfile$pop_stat)
              tcountryfile[, paste0("MA_opt_", run, type)] <- d_opt %*% as.vector(tcountryfile[,paste0("pop_opt", type)])
            }else{
              tcountryfile[, paste0("amenity_", run, type)] <- NA
              tcountryfile[, paste0("pop_opt_", run, type)] <- NA
              tcountryfile[, paste0("MA_initial_", run, type)] <- d_initial %*% as.vector(tcountryfile$pop)
              tcountryfile[, paste0("MA_opt_", run, type)] <- d_opt %*% as.vector(tcountryfile$pop)
            }

      
            tcountryfile[, paste0("dMA_", run, type)] <- tcountryfile[, paste0("MA_opt_", run, type)] - tcountryfile[, paste0("MA_initial_", run, type)]
            tcountryfile[, paste0("fMA_", run, type)] <- tcountryfile[, paste0("MA_opt_", run, type)] / tcountryfile[, paste0("MA_initial_", run, type)]

            tcountryfile[, paste0("I_change_", run, type)] <- rowSums(I_opt - I_initial) / 2
            # tcountryfile[, paste0("zeta_", run, type)] <- tcountryfile[,paste0("util_opt", type)] / tcountryfile$util_stat
            # tcountryfile[tcountryfile$pop == 0, paste0("zeta_", run)] <- 1

            tcountryfile[, paste0("util_opt_", run, type)] <- tcountryfile[,paste0("util_opt", type)]
            tcountryfile[, paste0("util_stat_", run, type)] <- tcountryfile[,paste0("util_stat")]

            tcountryfile[, paste0("c_opt_", run, type)] <- tcountryfile[,paste0("c_opt", type)]
            tcountryfile[, paste0("c_stat_", run, type)] <- tcountryfile$c_stat


            countryfile <- merge(countryfile, tcountryfile[, c("ID", paste0("I_change_", run, type), paste0("MA_initial_", run, type), paste0("MA_opt_", run, type), paste0("dMA_", run, type), paste0("fMA_", run, type), paste0("util_opt_", run, type), paste0("util_stat_", run, type), paste0("amenity_", run, type), paste0("pop_opt_", run, type), paste0("c_opt_", run, type), paste0("c_stat_", run, type))], by = "ID")
            }
        }
      } else{
        countryfile[, c(paste0("I_change_", run), paste0("MA_initial_", run), paste0("MA_opt_", run), paste0("dMA_", run), paste0("fMA_", run), paste0("zeta_", run), paste0("util_opt_", run), paste0("util_stat_", run))] = NA
      }
    }

    if (length(countryfiles) == 1) {
      countryfiles <- countryfile
    } else {
      #countryfiles <- rbind(countryfiles, countryfile, fill = T)
      countryfiles <- bind_rows(countryfiles, countryfile)
    }
  }

  print(country)
}



# to make it unique, just toss out all abroad cells
countryfiles <- countryfiles[countryfiles$abroad == 0, ]

# merge in other data
opt_loc <- merge(centroids[, !names(centroids) %in% "pop"], countryfiles[, !names(countryfiles) %in% c("x", "y", "abroad")], by = c("ID", "country"), all.x = T, all.y = T)

opt_loc$zeta <- opt_loc$zeta_base # just for convenience, create the main zeta

write.csv(opt_loc, file = "./Analysis/input/opt_loc.csv", row.names = FALSE)
