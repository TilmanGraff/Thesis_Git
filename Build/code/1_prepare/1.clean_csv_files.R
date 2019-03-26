# Minor file overwriting NA-speeds with 0-speeds

files <- list.files(path="./Build/temp/speed/", full.names=T)


for (i in 1:length(files)){

  df <- read.csv(files[i])
  df[is.na(df)] <- 0

  write.csv(df, file=files[i], row.names = FALSE)


}
