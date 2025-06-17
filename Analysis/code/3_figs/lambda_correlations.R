###############
# Basic correlations
###############

require(binsreg)
require(scales)

df = readstata13::read.dta13("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Analysis/input/maingrid.dta")


pdf("/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/figs/2024-02-21-figs/lambda_corrs_trash.pdf")

pdf("/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/figs/2024-02-21-figs/lambda_corrs.pdf", width = 8, height = 10)
par(mfrow = c(4,3), mar = c(4,5,1,1))

types = c("imm", "imm_10p", "mob", "mob_10p")

set.seed(1234)
sample = sample(1:nrow(df), 2000)

for(type in types){
  yvars = c(paste0("fma_", type, "_forR") , paste0("i_stat_", type, "_forR"), paste0("i_change_", type, "_forR"))
  if(type == "imm"){
    xlab = "Lambda, immobile"
  }
  if(type == "imm_10p"){
    xlab = "Lambda, immobile, 10% expansion"
  }
  if(type == "mob"){
    xlab = "Lambda, mobile"
  }
  if(type == "mob_10p"){
    xlab = "Lambda, mobile, 10% expansion"
  }
  if(grepl("mob", type)){
    lambda_var = paste0("Lambda_L_", type)
  }else{
    lambda_var = paste0("Lambda_", type)
  }
  i = 1
  for(yvar in yvars){
    
    yvar = gsub("_forR", "", yvar)
    
    col = c("mediumseagreen", "plum2", "mediumturquoise")[i]
    i = i+1
    
    if(grepl("fma", yvar)){
      ylims = c(-2, 4)
      ylab = "Market Access Gain"
    }
    if(grepl("i_stat", yvar)){
      ylims = c(-0.03, 0.04)
      ylab = "Ex-ante Road Density"
    }
    if(grepl("i_change", yvar)){
      ylims = c(-0.07, 0.07)
      ylab = "Road Gain"
    }
    
    if(grepl("10p", type)){
      ylab = paste0(ylab, " 10% expansion")
    }
    
    dev.set(dev.prev())
    x = binsreg( df[,yvar], df[,lambda_var])
    
    dev.set(dev.next())
    plot(x$data.plot$`Group Full Sample`$data.dots$x, x$data.plot$`Group Full Sample`$data.dots$fit,
         bty = "n", col = alpha(col, 0.9), pch = 19, cex = 1.2,
         ylab = ylab, xlab = xlab)
    legend("bottomright", paste0("corr: ", round(cor(df[,gsub("_forR", "", lambda_var)], df[,gsub("_forR", "", yvar)], use = "complete.obs"), 3)), bty = "n")
   
    
    
  }
}


dev.off()
dev.off()
par(mfrow = c(1,1))
file.remove("/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/figs/2024-02-21-figs/lambda_corrs_trash.pdf")


sample = sample(1:nrow(df), 5000)


pdf("/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/figs/2024-02-21-figs/lambda_corrs_10p_imm.pdf", width = 7, height = 7)
plot(df[sample,"Lambda_imm"], df[sample,"Lambda_imm_10p"], col = alpha("coral2", .3), pch = 19, cex = .8, bty = "n", 
     ylab = "Lambda: optimal expansion", xlab = "Lambda: optimal realloaction", ylim = c(0.45, 3.0) , xlim = c(0.45, 3.0))
abline(a = 0, b = 1, lty = 2)
legend("bottomright", paste0("corr = ", round(cor(df$Lambda_imm, df$Lambda_imm_10p, use = "complete.obs"), 3)), bty = "n")
dev.off()
pdf("/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/figs/2024-02-21-figs/lambda_corrs_10p_mob.pdf", width = 7, height = 7)
plot(df[sample,"Lambda_L_mob"], df[sample,"Lambda_L_mob_10p"], col = alpha("dodgerblue2", .2), pch = 19, cex = .8, bty = "n", 
     ylab = "Lambda: optimal expansion", xlab = "Lambda: optimal realloaction", xlim = c(0.5, 1.6), ylim = c(0.5, 1.6))
abline(a = 0, b = 1, lty = 2)
legend("bottomright", paste0("corr = ", round(cor(df$Lambda_mob, df$Lambda_mob_10p, use = "complete.obs"), 3)), bty = "n")
dev.off()

