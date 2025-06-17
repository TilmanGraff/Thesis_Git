##########
# Cross country welfare losses barcharts
##########


# Aggregating national means for welfare_gains
###############

library("reshape2")
library("stargazer")
require("RColorBrewer")
require("viridis")

setwd("/Users/tilmangraff/Documents/GitHub/Thesis_Git")

outpath <- paste0("./Analysis/output/descriptives/revision/")
dir.create(outpath)

opt_loc_nat = read.csv("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Analysis/input/country_means.csv")

opt_loc_nat = opt_loc_nat[!is.na(opt_loc_nat$Lambda_mob_barchart),]

for (version in c("imm", "mob")) {
  
  # immobilepath = runs[runs$names == run & runs$version == "immobile", "paths"]
  # mobilepath = runs[runs$names == run & runs$version == "immobile", "paths"]


  

  # opt_loc <- read.csv("./Analysis/input/opt_loc.csv")
  # opt_loc_nat <- data.frame()

  # opt_loc$zeta <- opt_loc[, paste0("Lambda_", run)]

  # i <- 1
  # country_table <- as.data.frame(table(opt_loc$country))
  # country_names <- paste(country_table[country_table$Freq != 0, "Var1"])

  # for (country in country_names) {
  #   df <- opt_loc[opt_loc$country == country, ]

  #   if (!is.na(df[1, "zeta"]) & nrow(df) > 2) {
  #     opt_loc_nat[i, "country"] <- country
  #     opt_loc_nat[i, paste0("Lambda_imm", version, "_barchart")] <- ((sum(df$pop * df[,paste0("util_opt_", run)]) / sum(df$pop * df[,paste0("util_stat_", run)])) - 1) * 100


  #     i <- i + 1
  #   }
  # }

  # Colors for plot
  # my.palette <- brewer.pal(n = 9, name = "OrRd")
  if(version == "imm"){
  my.palette <- rocket(21)
  }else{
    my.palette <- viridis(21)
  }

  opt_loc_nat$col <- my.palette[as.numeric(cut(opt_loc_nat[,paste0("Lambda_", version, "_barchart")], breaks = 20))]

  for(i in 1:nrow(opt_loc_nat)){
    opt_loc_nat[i,"max_val"] = max(c(opt_loc_nat[i, paste0("Lambda_", version, "_barchart")], opt_loc_nat[i, paste0("Lambda_", version, "_10p_barchart")]))
    opt_loc_nat[i,"sort_val"] = opt_loc_nat[i, paste0("Lambda_", version, "_barchart")]
  }
  
  
  # opt_loc_africa = opt_loc[!is.na(opt_loc$region),]
  # total <- ((sum(opt_loc_africa$pop * opt_loc_africa$util_opt, na.rm = T) / sum(opt_loc_africa$pop * opt_loc_africa$util_stat, na.rm = T)) - 1) * 100
  # opt_loc_nat[nrow(opt_loc_nat) + 1, "country"] <- "Africa"
  # opt_loc_nat[opt_loc_nat$country == "Africa", paste0("Lambda_imm", version, "_barchart")] <- total
  opt_loc_nat[opt_loc_nat$country == "Africa", "col"] <- "#574444"
  opt_loc_nat[opt_loc_nat$country %in% c("United-States", "Japan", "China", "Germany"), "col"] <- "grey"


  # Plot
  ###############

  
  pdf(paste0(outpath, "country_barchart_", version, ".pdf"), width = 8, height = 8)
  par(mar = c(5, 1, 1, 1))
  plot(0, 0, type = "n", xlab = "Hypothetical welfare gain", ylab = "", bty = "n", axes = F, xlim = c(0,11.0), ylim = c(1, nrow(opt_loc_nat)), )

  for (i in 1:nrow(opt_loc_nat)) {
    base_point = opt_loc_nat[order(opt_loc_nat$sort_val), paste0("Lambda_", version, "_barchart")][i]
    p10_point = opt_loc_nat[order(opt_loc_nat$sort_val), paste0("Lambda_", version, "_10p_barchart")][i]
    textpoint = max(base_point, p10_point)
    points(c(0, base_point), c(i, i), type = "l", lend = "butt", lwd = 8, col = opt_loc_nat[order(opt_loc_nat$sort_val), "col"][i])
    points(p10_point, i, pch = 4, col = "grey50")
    points(c(0, p10_point), c(i,i), pch = 4, col = "grey50", type = "l")

    if (opt_loc_nat[order(opt_loc_nat$sort_val), "country"][i] != "Africa") {
      text(textpoint, i, opt_loc_nat[order(opt_loc_nat$sort_val), "country"][i], pos = 4, cex = .8)
    } else {
      text(textpoint, i, labels = expression(bold("Africa")), pos = 4, cex = .8)
    }
  }

  abline(v = 0)
  axis(1, at = c(0:10), labels = paste0(c(0:10), "%"))
  legend("bottomright", bty = "n", pch = c(15, 4), col = c("#574444", "grey50"), legend = c("Optimal reallocation", "Optimal 10% expansion"))
  dev.off()
}


# delete later
# plot(opt_loc$Lambda_base_old_nocomp, opt_loc$Lambda_base, ylim = c(0.5, 2.5), xlim = c(0.5, 2.5), col = alpha("#a148b3", .3), cex = log(opt_loc$pop / 50000), xlab = "Old Welfare Improvement Measure", ylab = "New Welfare Improvement Measure")
# abline(a = 0, b = 1, lty = 2)

# plot(opt_loc$I_change_base_old_nocomp, opt_loc$I_change_base, col = alpha("dodgerblue3", .3), cex = log(opt_loc$pop / 50000), xlab = "Old roads change", ylab = "New roads change")
# abline(a = 0, b = 1, lty = 2)
