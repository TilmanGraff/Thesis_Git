
###############
# This file writes tables of (so far merely) descriptive statistics of zeta

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")

#####
# 1) Rank all sample countries by zeta

# library("xtable", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# national_zeta <- data.frame()
# i = 1
#
#
#
# country_table <- as.data.frame(table(opt_loc$country))
# country_names <- paste(country_table[country_table$Freq != 0,"Var1"])
#
#
# for(country in country_names){
#   df <- opt_loc[opt_loc$country==country,]
#
#   if(!is.na(df[1, "zeta"])){
#
#     national_zeta[i, "country"] <- country
#     national_zeta[i, "zeta"] <- ((sum(df$pop * df$util_opt)  / sum(df$pop * df$util_stat)) - 1) * 100
#
#
#     i = i+1
#   }
# }
#
# national_zeta <- national_zeta[order(-national_zeta$zeta),]
#
# print(xtable(national_zeta), include.rownames = FALSE) # gives you latex table

###
# Total Welfare Gains
###

total <- sum(opt_loc$pop*opt_loc$util_opt, na.rm=T) / sum(opt_loc$pop*opt_loc$util_stat, na.rm=T)
print(paste("total welfare gains across the entire continent are", total))


###
# MAKE HISTORGRAM
###

png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/descriptives/histogram.png", sep=""))
hist(opt_loc$zeta, breaks=40, col="salmon", main="", xlab="")
par(new=T)
curve(dnorm(x, mean=mean(opt_loc$zeta), sd=sd(opt_loc$zeta)), xaxt='n', yaxt='n', xlim=c(min(opt_loc$zeta), max(opt_loc$zeta)), ylab = '', xlab='')
dev.off()
