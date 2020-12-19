# Master file batching all files in the prepare folder.

# Order in which it should work

# 1. clean_csv_files.R
# 2. create_country_names.R
# 3. create_deltas_and_i.R
# 4. define_productivity_and_rownames.R


# do some minor housekeeping
source("./Build/code/1_prepare/1.clean_csv_files.R")
source("./Build/code/1_prepare/2.create_country_names.R")
source("./Build/code/1_prepare/3.create_abroad_mask.R")

# major script: create the delta and infrastructure matrices
source("./Build/code/1_prepare/4.create_deltas_and_i.R")

# get the ports data in Order
source("./Build/code/1_prepare/5.clean_ports.R")

# create the productivity matrices
source("./Build/code/1_prepare/6.define_productivity_and_rownames.R")
