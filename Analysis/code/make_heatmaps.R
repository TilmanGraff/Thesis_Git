
opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")

country_table <- as.data.frame(table(opt_loc$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

file.remove(list.files(path="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/zeta_heatmaps/", full.names = T))


for(country in country_names){
  df <- opt_loc[opt_loc$country==country,]

if(!is.na(df[1, "zeta"])){
  colfunc <- colorRampPalette(c("red", "green"))
  df$Col <- colfunc(10)[as.numeric(cut(df$zeta,breaks = 10))]

  png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/zeta_heatmaps/", country, "_zeta.png", sep=""))
  par(pty="s")
  plot(x=df$x, y=df$y, col=df$Col, pch=19, cex=4.5, asp=1, main=country, xlab="", ylab="")
  dev.off()

    
}
}


library("xtable", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
national_zeta <- data.frame()
i = 1

for(country in country_names){
  df <- opt_loc[opt_loc$country==country,]

  if(!is.na(df[1, "zeta"])){

    national_zeta[i, "country"] <- country
    national_zeta[i, "zeta"] <- ((sum(df$pop * df$util_opt)  / sum(df$pop * df$util_stat)) - 1) * 100


    i = i+1
  }
}

national_zeta <- national_zeta[order(-national_zeta$zeta),]

print(xtable(national_zeta), include.rownames = FALSE)
