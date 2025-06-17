df = readstata13::read.dta13("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Analysis/temp/railgradient.dta")

dists = unique(df$dist)

for(railtype in c("rail", "placebo")){

df1 = df[df$outcome  != "i_change_nghbs" & df$railtype == railtype,]

coldf = data.frame("y" = c("i_stat", "i_opt", "fma", "Lambda"), "col" = c( "hotpink3","coral3", "dodgerblue2", "navyblue"))

  col1 = "hotpink3"
  col2 = "navyblue"
  col3 = "pink2"
  col4 = "royalblue1"


pdf(paste0("/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/figs/gradient_", railtype, ".pdf"), width = 8, height = 5)
par(mar = c(4,5,1,1))
plot(dists, dists, type = "n", ylim = c(-0.6, 0.6), bty = "n", xlab = ifelse(railtype == "rail", "Distance to colonial railroad (in KM)", "Distance to colonial placebo railroad (in KM)"), ylab = "Coefficient size", axes = F)
axis(1, at = seq(10, 100, 10))
axis(2)

for(y in c("i_stat", "Lambda")){
  
  if(y == "i_stat"){
    col = col1
  }else{
    col = col2
  }
  ydf = df1[df1$outcome == y,]
  points(ydf$dist, ydf$b, type = "l", col = col, lwd = 2)
  polygon(x = c(ydf$dist, rev(ydf$dist)), y = c(ydf$b + 1.58*ydf$se, rev(ydf$b - 1.58*ydf$se)), 
          border = F, col = alpha(col, .4))
  
}

abline(a = 0, b= 0, lty = 2)
legend("bottomright", bty = "n", col = c(col1, col2), legend = c("Current road infrastructure (z-scored)", "Infrastructure discrimination index (z-scored)"), pch = 19)
dev.off()
}


dfreal = df[df$outcome  != "i_change_nghbs" & df$railtype == "rail",]
dfplac = df[df$outcome  != "i_change_nghbs" & df$railtype == "placebo",]


pdf(paste0("/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/figs/gradient_all.pdf"), width = 9, height = 5)
par(mar = c(4,5,1,1))
plot(dists, dists, type = "n", ylim = c(-0.6, 0.6), bty = "n", xlab = "Distance to colonial railroad / placebo road (in KM)", ylab = "Coefficient size", axes = F)
axis(1, at = seq(10, 100, 10))
axis(2)

for(y in c("i_stat", "Lambda")){
  
  if(y == "i_stat"){
    col_real = col1
    col_plac = col3
  }else{
    col_real = col2
    col_plac = col4
  }
  
  ydf_plac = dfplac[dfplac$outcome == y,]
  points(dists, ydf_plac$b, pch = 19, cex = .5, col = col_plac)
  
  for(di in dists){
    b = ydf_plac[ydf_plac$dist == di, "b"]
    se = ydf_plac[ydf_plac$dist == di, "se"]
    points(c(di, di), c(b+1.96*se, b-1.96*se), type = "l", col = col_plac, lwd = 2)
  }
  
  
  ydf = dfreal[dfreal$outcome == y,]
  points(ydf$dist, ydf$b, type = "l", col = col_real, lwd = 2)
  polygon(x = c(ydf$dist, rev(ydf$dist)), y = c(ydf$b + 1.58*ydf$se, rev(ydf$b - 1.58*ydf$se)), 
          border = F, col = alpha(col_real, .4))
  
  
 
  
}

abline(a = 0, b= 0, lty = 2)
legend("bottomright", bty = "n", col = c(col1, col2, col3, col4), legend = c("Current road infrastructure (z-scored), actual rail", "Infrastructure discrimination index (z-scored), actual rail", "Current road infrastructure (z-scored), placebo rail", "Infrastructure discrimination index (z-scored), placebo rail"), pch = c(19, 19, NA, NA), lty = c(NA, NA, 1, 1))
dev.off()

