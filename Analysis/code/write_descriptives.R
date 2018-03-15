
###############
# This file writes tables of (so far merely) descriptive statistics of zeta

#####
# 1) Rank all sample countries by zeta

library("xtable", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
national_zeta <- data.frame()
i = 1


opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")

country_table <- as.data.frame(table(opt_loc$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])


for(country in country_names){
  df <- opt_loc[opt_loc$country==country,]

  if(!is.na(df[1, "zeta"])){

    national_zeta[i, "country"] <- country
    national_zeta[i, "zeta"] <- ((sum(df$pop * df$util_opt)  / sum(df$pop * df$util_stat)) - 1) * 100


    i = i+1
  }
}

national_zeta <- national_zeta[order(-national_zeta$zeta),]

print(xtable(national_zeta), include.rownames = FALSE) # gives you latex table


###
# MAKE HISTORGRAM
###

png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/descriptives/histogram.png", sep=""))
hist(opt_loc$zeta, breaks=50, col="salmon", main="", xlab="")
curve(dnorm(x, mean=mean(opt_loc$zeta), sd=sd(opt_loc$zeta))*380, add=T)
dev.off()
