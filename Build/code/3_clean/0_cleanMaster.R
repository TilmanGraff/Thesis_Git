# Master file batching all files in the clean folder.

# Order in which it should work

# combine geography and optimisation to create opt_loc
source("./Build/code/3_clean/1.centroids_to_opt_loc.R")

# make the nice graphs happen
source("./Build/code/3_clean/2.graph_even_nicer_networks.R")

# create the numconnections variable
source("./Build/code/3_clean/3.create_numconnections.R")
