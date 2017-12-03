
centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv")

alpha <- 0.7

centroids <- centroids[!is.na(centroids$lights),]
centroids$productivity <- centroids$lights / (centroids$pop^alpha)

centroids[centroids$pop==0, "productivity"] <- 0


write.csv(format(centroids, scientific=F), "/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv", row.names = FALSE)
