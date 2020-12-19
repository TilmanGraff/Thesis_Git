# Master file batching all files in the scrape folder.

# Order in which it should work

# 1. tidy_up_centroids_raw.R
# 2. get_Henderson_lightdata.R
# 3. create_South-Sudan.R
# 4. create_adj_dist_speed.R
# 5. symmetrize_adj_dist_speed.R

# initial housekeeping
source("./Build/code/0_scrape/1.tidy_up_centroids_raw.R")
source("./Build/code/0_scrape/2.get_Henderson_lightdata.R")
source("./Build/code/0_scrape/3.create_South-Sudan.R")

# identify which borderregions a country belongs to, also creates the "is.abroad" identifier
source("./Build/code/0_scrape/4.create_borderregions.R")

# major script scraping OSRM
# watch out before running!!!
#source("./Build/code/0_scrape/5.create_adj_dist_speed.R")
#source("./Build/code/0_scrape/5a.symmetrize_adj_dist_speed.R") # run this always after running the previous, but only once each time
