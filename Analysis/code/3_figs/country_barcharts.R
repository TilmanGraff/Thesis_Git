##########
# Cross country welfare losses barcharts
##########


# Aggregating national means for welfare_gains
###############

library("reshape2")
library("stargazer")
require("RColorBrewer")
require("viridis")

opt_loc <- read.csv("./Analysis/input/opt_loc.csv")
opt_loc_nat <- data.frame()
i = 1
country_table <- as.data.frame(table(opt_loc$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

for(country in country_names){
  df <- opt_loc[opt_loc$country==country,]

  if(!is.na(df[1, "zeta"]) & nrow(df)>2){

    opt_loc_nat[i, "country"] <- country
    opt_loc_nat[i, "welfare_gains"] <- ((sum(df$pop * df$util_opt)  / sum(df$pop * df$util_stat)) - 1) * 100


    i = i+1
  }
}

# Colors for plot
#my.palette <- brewer.pal(n = 9, name = "OrRd")
my.palette = rocket(21)

opt_loc_nat$col = my.palette[as.numeric(cut(opt_loc_nat$welfare_gains,breaks = 20))]


total = ((sum(opt_loc$pop*opt_loc$util_opt, na.rm=T) / sum(opt_loc$pop*opt_loc$util_stat, na.rm=T))-1) * 100
opt_loc_nat[nrow(opt_loc_nat)+1,"country"] = "Africa"
opt_loc_nat[opt_loc_nat$country=="Africa","welfare_gains"] = total
opt_loc_nat[opt_loc_nat$country=="Africa","col"] = "grey"


# Plot
###############

pdf("./Analysis/output/descriptives/country_barchart.pdf", width=8,height=8)
par(mar=c(5,1,1,1))
plot(0,0, type="n", xlab="Hypothetical welfare gain", ylab="", bty="n", axes=F, xlim=c(0, max(opt_loc_nat$welfare_gains)*1.12), ylim=c(1,nrow(opt_loc_nat)))

for(i in 1:nrow(opt_loc_nat)){

  points(c(0, opt_loc_nat[order(opt_loc_nat$welfare_gains), "welfare_gains"][i]), c(i,i), type="l", lend="butt", lwd=8, col=opt_loc_nat[order(opt_loc_nat$welfare_gains), "col"][i])

  if(opt_loc_nat[order(opt_loc_nat$welfare_gains), "country"][i] != "Africa"){

    text(opt_loc_nat[order(opt_loc_nat$welfare_gains), "welfare_gains"][i], i, opt_loc_nat[order(opt_loc_nat$welfare_gains), "country"][i], pos=4, cex=.8)
  } else{
    text(opt_loc_nat[order(opt_loc_nat$welfare_gains), "welfare_gains"][i], i, labels=expression(bold("Africa")), pos=4, cex=.8)
  }
}

abline(v=0)
axis(1, at=c(0:4), labels=paste0(c(0:4), "%"))

dev.off()
