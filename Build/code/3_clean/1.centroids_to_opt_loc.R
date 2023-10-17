require(igraph)

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

runs <- data.frame("names" = c("base", "10perc", "base_old"), "paths" = c("2023-10-12_151412_newbase", "2023-10-12_160452_new10perc", "2023-07-17_111951_base"))

for (country in countries$x) {
  if (file.exists(paste0("./Build/output/", runs[runs$names == "base", "paths"], "/Network_outcomes/", country, "_outcomes.csv"))) {
    countryfile <- read.csv(paste0("./Build/output/", runs[runs$names == "base", "paths"], "/Network_outcomes/", country, "_outcomes.csv"))
    countryfile$country <- country

    # delta_tau <- read.csv(paste("./Build/temp/delta_tau/delta_tau_", country, ".csv", sep=""))

    # quich excursion to also capture the I_change:
    # I_stat
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
        tcountryfile <- read.csv(paste("./Build/output/", path, "/Network_outcomes/", country, "_outcomes.csv", sep = ""))

        I_opt <- read.csv(paste("./Build/output/", path, "/Optimised_Networks/", country, ".csv", sep = ""), header = FALSE)

        g_opt_m <- as.matrix(I_opt)^(-gamma)
        g_opt_m[adj == 0] <- 0
        g_opt <- graph_from_adjacency_matrix(g_opt_m, weighted = T)
        d_opt <- as.matrix(distances(g_opt))^(-sigma)
        diag(d_opt) <- 0

        tcountryfile[, paste0("MA_initial_", run)] <- d_initial %*% as.vector(tcountryfile$pop)
        tcountryfile[, paste0("MA_opt_", run)] <- d_opt %*% as.vector(tcountryfile$pop)

        tcountryfile[, paste0("dMA_", run)] <- tcountryfile[, paste0("MA_opt_", run)] - tcountryfile[, paste0("MA_initial_", run)]
        tcountryfile[, paste0("fMA_", run)] <- tcountryfile[, paste0("MA_opt_", run)] / tcountryfile[, paste0("MA_initial_", run)]

        tcountryfile[, paste0("I_change_", run)] <- rowSums(I_opt - I_initial) / 2
        tcountryfile[, paste0("zeta_", run)] <- tcountryfile$util_opt / tcountryfile$util_stat
        tcountryfile[tcountryfile$pop == 0, paste0("zeta_", run)] <- 1

        tcountryfile[, paste0("util_opt_", run)] <- tcountryfile$util_opt
        tcountryfile[, paste0("util_stat_", run)] <- tcountryfile$util_stat

        countryfile <- merge(countryfile, tcountryfile[, c("ID", paste0("I_change_", run), paste0("MA_initial_", run), paste0("MA_opt_", run), paste0("dMA_", run), paste0("fMA_", run), paste0("zeta_", run), paste0("util_opt_", run), paste0("util_stat_", run))], by = "ID")
      } else{
        countryfile[, c(paste0("I_change_", run), paste0("MA_initial_", run), paste0("MA_opt_", run), paste0("dMA_", run), paste0("fMA_", run), paste0("zeta_", run), paste0("util_opt_", run), paste0("util_stat_", run))] = NA
      }
    }

    if (length(countryfiles) == 1) {
      countryfiles <- countryfile
    } else {
      countryfiles <- rbind(countryfiles, countryfile)
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
