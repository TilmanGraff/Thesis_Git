require(igraph)
require(dplyr)
# Aggregates findings from optimisation into the opt_loc dataset. Most importantly, defines lambda.

setwd("/Users/tilmangraff/Documents/GitHub/Thesis_Git")


MA = function(tau, pop){
  d_stat = matrix(nrow = nrow(tau), ncol = nrow(tau))
  for(i in 1:nrow(tau)){
    for(j in which(adj[1,] == 1)){
      indices = (0:5) * nrow(tau)  + j 
      vals = tau[i,c(indices)]
      vals[vals == Inf] = NA
      d_stat[i,j] = mean(as.numeric(vals), na.rm = T)
    }
  }
  d_stat = as.matrix(1+tau[,1:nrow(tau)])
  d_stat[adj == 0] = 0
  d_stat[d_stat == Inf] = 0
  diag(d_stat) = 1
  d_stat[is.na(d_stat)] = 0
  
  g_stat = graph_from_adjacency_matrix(d_stat, weighted = T)
  dists = as.matrix(distances(g_stat))
  diag(dists) = 1
  return(dists^(-sigma) %*% pop)
}




centroids <- read.csv("./Build/temp/centroids.csv")

# Gather country names
countries <- read.csv("./Build/temp/country_names.csv")

countryfiles <- NA


gamma <- 0.946
beta <- 1.2446 * gamma
sigma <- 5
# gamma = 0.5*beta

runs <- data.frame("names" = c("base_old", "mob", "imm", "mob_nocomp", "imm_nocomp"), "paths" = c("2023-07-17_111951_base", "2024-02-18_040636_mobile_fp_final", "2024-02-20_140808_imm_fp_final", "2024-02-21_212312_mobile_nocomp", "2024-02-21_140858_imm_nocomp"))

for (country in countries$x) {
  if (file.exists(paste0("./Build/output/", runs[runs$names == "mob", "paths"], "/Network_outcomes/", country, "_outcomes.csv"))) {
    countryfile <- read.csv(paste0("./Build/output/", runs[runs$names == "mob", "paths"], "/Network_outcomes/", country, "_outcomes.csv"))
    countryfile$country <- country

    delta_tau <- read.csv(paste("./Build/temp/delta_tau/delta_tau_", country, ".csv", sep=""))
    delta_I <- read.csv(paste("./Build/temp/delta_I/delta_I_", country, ".csv", sep=""))

    #quich excursion to also capture the I_change:
    #I_stat
    I_initial <- read.csv(paste("./Build/temp/I/I_", country, ".csv", sep = ""))
    adj <- read.csv(paste("./Build/temp/adj/adj_", country, ".csv", sep = ""))


    g_initial_m <- 1 + as.matrix(I_initial)^(-gamma)
    g_initial_m[adj == 0] <- 0
    g_initial <- graph_from_adjacency_matrix(g_initial_m, weighted = T)
    d_initial <- as.matrix(distances(g_initial))^(-sigma)
    diag(d_initial) <- 1

    prod <- read.csv(paste0("./Build/temp/productivities/productivities_", country, ".csv"))


    for (runidx in 1:nrow(runs)) {
      run <- runs$names[runidx]
      path <- runs$paths[runidx]

      if (file.exists(paste0("./Build/output/", path, "/Network_outcomes/", country, "_outcomes.csv"))) {
        

        for(type in c("", "_10p")){
          if(file.exists(paste("./Build/output/", path, "/Optimised_Networks/", country, type, ".csv", sep = ""))){
            tcountryfile <- read.csv(paste("./Build/output/", path, "/Network_outcomes/", country, "_outcomes.csv", sep = ""))
            tcountryfile$pop_stat = read.csv(paste0("./Build/temp/borderregions/", country, "_borderregion", ".csv"))$pop
            I_opt <- read.csv(paste("./Build/output/", path, "/Optimised_Networks/", country, type, ".csv", sep = ""), header = FALSE)

            g_opt_m <- 1 + as.matrix(I_opt)^(-gamma)
            g_opt_m[adj == 0] <- 0
            g_opt <- graph_from_adjacency_matrix(g_opt_m, weighted = T)
            d_opt <- as.matrix(distances(g_opt))^(-sigma)
            diag(d_opt) <- 1

          
            if(grepl("mob", run)){
              tcountryfile[, paste0("amenity_", run, type)] <- tcountryfile$amenities
              tcountryfile[, paste0("pop_opt_", run, type)] <- tcountryfile[,paste0("pop_opt", type)]
              tcountryfile[, paste0("MA_initial_", run, type)] <- d_initial %*% as.vector(tcountryfile$pop_stat * rowSums(prod))
              tcountryfile[, paste0("MA_opt_", run, type)] <- d_opt %*% as.vector(tcountryfile[,paste0("pop_opt", type)] * rowSums(prod))
            }else{
              tcountryfile[, paste0("amenity_", run, type)] <- NA
              tcountryfile[, paste0("pop_opt_", run, type)] <- NA
              tcountryfile[, paste0("MA_initial_", run, type)] <- d_initial %*% as.vector(tcountryfile$pop_stat * rowSums(prod))
              tcountryfile[, paste0("MA_opt_", run, type)] <- d_opt %*% as.vector(tcountryfile$pop_stat * rowSums(prod))
            }


            # tau_opt <- read.csv(paste("./Build/output/", path, "/taus/tau_opt_", country, type, ".csv", sep = ""), header = FALSE)
            # tau_stat <- read.csv(paste("./Build/output/", path, "/taus/tau_stat_", country, type, ".csv", sep = ""), header = FALSE)

            # tcountryfile[, paste0("dMA_", run, type)] <- MA(tau_opt, tcountryfile$pop_stat) / MA(tau_stat, tcountryfile$pop_stat)
            tcountryfile[, paste0("fMA_", run, type)] <- tcountryfile[, paste0("MA_opt_", run, type)] / tcountryfile[, paste0("MA_initial_", run, type)]


      
           
            tcountryfile[, paste0("I_change_", run, type)] <- rowSums(I_opt - I_initial) / 2
            tcountryfile[, paste0("I_stat_", run, type)] <- rowSums(I_initial) / 2
            
            tcountryfile[, paste0("I_fraction_", run, type)] <- rowSums(I_opt) / rowSums(I_initial)
            tcountryfile[, paste0("Is_stat_", run, type)] <- rowSums(I_initial * delta_I) / 2
            tcountryfile[, paste0("Is_change_", run, type)] <- rowSums((I_opt - I_initial)*delta_I) / 2

            # tcountryfile[, paste0("zeta_", run, type)] <- tcountryfile[,paste0("util_opt", type)] / tcountryfile$util_stat
            # tcountryfile[tcountryfile$pop == 0, paste0("zeta_", run)] <- 1

            tcountryfile[, paste0("util_opt_", run, type)] <- tcountryfile[,paste0("util_opt", type)]
            tcountryfile[, paste0("util_stat_", run, type)] <- tcountryfile[,paste0("util_stat")]

            tcountryfile[, paste0("c_opt_", run, type)] <- tcountryfile[,paste0("c_opt", type)]
            tcountryfile[, paste0("c_stat_", run, type)] <- tcountryfile$c_stat


            countryfile <- merge(countryfile, tcountryfile[, c("ID", paste0("I_change_", run, type),  paste0("I_stat_", run, type), paste0("Is_change_", run, type), paste0("Is_stat_", run, type), paste0("I_fraction_", run, type), paste0("MA_initial_", run, type), paste0("MA_opt_", run, type), paste0("fMA_", run, type), paste0("util_opt_", run, type), paste0("util_stat_", run, type), paste0("amenity_", run, type), paste0("pop_opt_", run, type), paste0("c_opt_", run, type), paste0("c_stat_", run, type))], by = "ID")
            }
        }
      } else{
        countryfile[, c(paste0("I_change_", run),  paste0("I_fraction_", run, type), paste0("Is_change_", run), paste0("Is_stat_", run, type), paste0("I_stat_", run, type), paste0("MA_initial_", run), paste0("MA_opt_", run), paste0("fMA_", run), paste0("zeta_", run), paste0("util_opt_", run), paste0("util_stat_", run))] = NA
      }
    }



    for(i in 1:nrow(tcountryfile)){
      
      nghbs = which(adj[i,] == 1)
      for(type in c("", "_10p")){
        for (runidx in 1:nrow(runs)) {
          run <- runs$names[runidx]
          countryfile[i, paste0("I_change_nghbs_", run, type)] = mean(countryfile[nghbs, paste0("I_change_", run, type)], na.rm = T)
        }
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
