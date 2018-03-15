library(pdftools)

text <- pdf_text("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Leaders.pdf")
text2 <- strsplit(text, "\n")

texts <- c(gsub("\\s{2,}", ",", text2[[1]]), gsub("\\s{2,}", ",", text2[[2]]))

df <- data.frame(lapply(read.delim(textConnection(texts), sep=",", header=F), as.character), stringsAsFactors=FALSE)

leaders <- df[-1,-1]
colnames(leaders) <- leaders[1,]
leaders <- leaders[-1,]

leaders <- leaders[!grepl("office", leaders$`Entered office`),]
leaders <- leaders[!grepl("Table", leaders$Country),]


leaders$Country <- gsub(" ", "-", leaders$Country)
leaders$Country <- gsub("'", "", leaders$Country)

leaders[grepl("Ivoire", leaders$Country), "Country"] <- "Cote-dIvoire"
leaders[grepl("Democratic-Republic-of-Congo", leaders$Country), "Country"] <- "Democratic-Republic-of-the-Congo"
leaders[grepl("Republic-of-the-Congo", leaders$Country), "Country"] <- "Congo"
leaders[grepl("Egypt,-Arab-Rep", leaders$Country), "Country"] <- "Egypt"
leaders[grepl("Tanzania", leaders$Country), "Country"] <- "United-Republic-of-Tanzania"

leaders$Ethnicity <- toupper(leaders$Ethnicity)

leaders[grepl("TIGRINYA", leaders$Ethnicity), "Ethnicity"] <- "TIGRINYA"
leaders[grepl("KROU", leaders$Ethnicity), "Ethnicity"] <- "KRU"
leaders[grepl("FULA", leaders$Ethnicity), "Ethnicity"] <- "FULA"
leaders[grepl("KABRE", leaders$Ethnicity), "Ethnicity"] <- "KABRE"
leaders[grepl("DIOULA", leaders$Ethnicity), "Ethnicity"] <- "DIULA"
leaders[grepl("BETI", leaders$Ethnicity), "Ethnicity"] <- "BETE"
leaders[grepl("ASANTE", leaders$Ethnicity), "Ethnicity"] <- "ASHANTI"
leaders[grepl("LHOMWE", leaders$Ethnicity), "Ethnicity"] <- "LOMWE"
leaders[grepl("XHOSA", leaders$Ethnicity), "Ethnicity"] <- "XOSA"

leaders$until <- leaders$`Left office`
leaders[grepl("ongoing", leaders$`Left office`), "until"] <- "01.01.2014"
leaders$from <- as.numeric(format(as.Date(as.character(leaders$`Entered office`), format="%d.%m.%Y"), "%Y"))
leaders$until <- as.numeric(format(as.Date(as.character(leaders$until), format="%d.%m.%Y"), "%Y"))

leaders$in_power <- leaders$until-leaders$from

years_in_power <- aggregate(leaders$in_power, by=list(leaders$Ethnicity, leaders$Country), FUN="sum")

colnames(years_in_power) <- c("ethn_NAME", "country", "years_in_power")

write.csv(years_in_power, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/leaders.csv", row.names = FALSE)
