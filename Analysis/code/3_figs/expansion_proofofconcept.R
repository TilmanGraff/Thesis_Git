x = c(1, 2, 2.5)
y = c(1,2,2)

pop = c(2, 10, 4)
cols = c("mediumpurple1", "darksalmon", "mediumpurple1")

for(i in 1:2){
  
  pdf(paste0("/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/figs/2024-02-21-figs/proofofconcept_", i, ".pdf"), width = 6, height =3)
  
  par(mar = c(0,0,0,0))
  
  plot(x,y, bty = "n", axes = F, ylab = "", xlab = "", cex = pop + 5, ylim = c(0.9, 2.3), col = cols, pch = 19, type = "n", xlim = c(0.9, 2.9))
  
  points(x[1:2], y[1:2], type = "l", lwd = 10, col = "lightsteelblue3")
  
  
  
  if(i == 2){
    points(x[2:3], y[2:3], type = "l", lwd = 3, col = "lightsteelblue3")
    
    
  }
  
  points(x,y, cex = pop + 5, ylim = c(1, 2.1), col = cols, pch = 19)
  
  
  if(i == 2){
    text(x,y, labels = c("-1%", "+1%", "+2%"), cex = 1.5)
  }else{
    text(x,y, labels = c("A", "B", "C"), cex = 1.5)
  }
  
  dev.off()
  
}