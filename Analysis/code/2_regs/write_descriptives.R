
###############
# This file writes tables of (so far merely) descriptive statistics of zeta

opt_loc <- read.csv("./Analysis/input/opt_loc.csv")

###
# Total Welfare Gains
###

total <- (sum(opt_loc$pop*opt_loc$util_opt, na.rm=T) / sum(opt_loc$pop*opt_loc$util_stat, na.rm=T) -1) * 100
print(paste("total welfare gains across the entire continent are", total, "%"))


###
# MAKE HISTORGRAM
###

png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/descriptives/histogram.png", sep=""))
hist(opt_loc$zeta, breaks=40, col="salmon", main="", xlab="")
par(new=T)
curve(dnorm(x, mean=mean(opt_loc$zeta), sd=sd(opt_loc$zeta)), xaxt='n', yaxt='n', xlim=c(min(opt_loc$zeta), max(opt_loc$zeta)), ylab = '', xlab='')
dev.off()
