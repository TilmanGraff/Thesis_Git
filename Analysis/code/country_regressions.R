###
# This file conducts regressions at the country level.

############

# First, aggregating national means for welfare_gains

library("reshape", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("stargazer", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")
opt_loc_nat <- data.frame()
i = 1
country_table <- as.data.frame(table(opt_loc$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

for(country in country_names){
  df <- opt_loc[opt_loc$country==country,]

  if(!is.na(df[1, "zeta"])){

    opt_loc_nat[i, "country"] <- country
    opt_loc_nat[i, "welfare_gains"] <- ((sum(df$pop * df$util_opt)  / sum(df$pop * df$util_stat)) - 1) * 100


    i = i+1
  }
}


###################


# ASSOCIATIONS WITH WORLD DEVELOPMENT INDICATORS
#####




wdi <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/WDI_Worldbank.csv")
colnames(wdi) <- c("country", "code", "series", "series.code", "value")

wdi <- wdi[, c("country", "series", "value")]
wdi_wide <- reshape(wdi, idvar = "country", timevar = "series", direction = "wide")


# Some data-mining

wdi_wide$country <- gsub(" ", "-", wdi_wide$country)
wdi_wide$country <- gsub("'", "", wdi_wide$country)
wdi_wide$country <- gsub("Cabo-Verde", "Cape-Verde", wdi_wide$country)
wdi_wide$country <- gsub("Congo,-Rep.", "Congo", wdi_wide$country)
wdi_wide$country <- gsub("Congo,-Dem.-Rep.", "Democratic-Republic-of-the-Congo", wdi_wide$country)
wdi_wide$country <- gsub("Egypt,-Arab-Rep.", "Egypt", wdi_wide$country)
wdi_wide$country <- gsub("Gambia,-The", "Gambia", wdi_wide$country)
wdi_wide$country <- gsub("Tanzania", "United-Republic-of-Tanzania", wdi_wide$country)



opt_loc_nat <- merge(opt_loc_nat, wdi_wide, by="country")

colnames(opt_loc_nat) <- c("country", "welfare_gains", "corruption", "property_rights", "legal_rights", "poverty1", "poverty2", "poverty3", "povertyratio", "ease_business", "life_expectancy", "gdp10", "x")

opt_loc_nat <- opt_loc_nat[,c("country", "welfare_gains", "corruption", "property_rights", "legal_rights", "ease_business", "life_expectancy", "gdp10")]


for(var in c("corruption", "property_rights", "legal_rights", "ease_business", "life_expectancy", "gdp10")){

  opt_loc_nat[,var] <- as.numeric(paste(opt_loc_nat[,var]))

}

## Add in Fractal-Measure from Alesina_ArtificialStates_2011

opt_loc_nat[opt_loc_nat$country=="Algeria", "fract"] <- 0.013163
opt_loc_nat[opt_loc_nat$country=="Angola", "fract"] <- 0.0165523
opt_loc_nat[opt_loc_nat$country=="Benin", "fract"] <- 0.0365344
opt_loc_nat[opt_loc_nat$country=="Botswana", "fract"] <- 0.0294423
opt_loc_nat[opt_loc_nat$country=="Burkina-Faso", "fract"] <- 0.036303
opt_loc_nat[opt_loc_nat$country=="Burundi", "fract"] <- 0.0467118
opt_loc_nat[opt_loc_nat$country=="Cameroon", "fract"] <- 0.0351837
opt_loc_nat[opt_loc_nat$country=="Central-African-Republic", "fract"] <- 0.0418999
opt_loc_nat[opt_loc_nat$country=="Chad", "fract"] <- 0.0175157
opt_loc_nat[opt_loc_nat$country=="Congo", "fract"] <- 0.0261842
opt_loc_nat[opt_loc_nat$country=="Democratic-Republic-of-the-Congo", "fract"] <- 0.0241071
opt_loc_nat[opt_loc_nat$country=="Djibuti", "fract"] <- 0.0313146
opt_loc_nat[opt_loc_nat$country=="Egypt", "fract"] <- 0.0105541
opt_loc_nat[opt_loc_nat$country=="Equatorial-Guinea", "fract"] <- 0.0073728
opt_loc_nat[opt_loc_nat$country=="Eritrea", "fract"] <- 0.0079781
opt_loc_nat[opt_loc_nat$country=="Ethiopia", "fract"] <- 0.0192632
opt_loc_nat[opt_loc_nat$country=="Lesotho", "fract"] <- 0.0400953
opt_loc_nat[opt_loc_nat$country=="Liberia", "fract"] <- 0.0459384
opt_loc_nat[opt_loc_nat$country=="Libya", "fract"] <- 0.0029258
opt_loc_nat[opt_loc_nat$country=="Malawi", "fract"] <- 0.0412763
opt_loc_nat[opt_loc_nat$country=="Mali", "fract"] <- 0.025853
opt_loc_nat[opt_loc_nat$country=="Mauritania", "fract"] <- 0.015962
opt_loc_nat[opt_loc_nat$country=="Morocco", "fract"] <- 0.0056441
opt_loc_nat[opt_loc_nat$country=="Namibia", "fract"] <- 0.0237068
opt_loc_nat[opt_loc_nat$country=="Niger", "fract"] <- 0.0176729
opt_loc_nat[opt_loc_nat$country=="Nigeria", "fract"] <- 0.0316343
opt_loc_nat[opt_loc_nat$country=="Rwanda", "fract"] <- 0.0720322
opt_loc_nat[opt_loc_nat$country=="Senegal", "fract"] <- 0.0464541
opt_loc_nat[opt_loc_nat$country=="Sierra-Leone", "fract"] <- 0.0550279
opt_loc_nat[opt_loc_nat$country=="Somalia", "fract"] <- 0.0029158
opt_loc_nat[opt_loc_nat$country=="South-Africa", "fract"] <- 0.0254923
opt_loc_nat[opt_loc_nat$country=="Gabon", "fract"] <- 0.0297724
opt_loc_nat[opt_loc_nat$country=="Ghana", "fract"] <- 0.0428391
opt_loc_nat[opt_loc_nat$country=="Guinea", "fract"] <- 0.0459671
opt_loc_nat[opt_loc_nat$country=="Cote-dIvoire", "fract"] <- 0.0516242
opt_loc_nat[opt_loc_nat$country=="Kenya", "fract"] <- 0.0133011
opt_loc_nat[opt_loc_nat$country=="Sudan", "fract"] <- 0.0241656
opt_loc_nat[opt_loc_nat$country=="Swaziland", "fract"] <- 0.0263498
opt_loc_nat[opt_loc_nat$country=="United-Republic-of-Tanzania", "fract"] <- 0.0250633
opt_loc_nat[opt_loc_nat$country=="Togo", "fract"] <- 0.0383742
opt_loc_nat[opt_loc_nat$country=="Tunisia", "fract"] <- 0.0223485
opt_loc_nat[opt_loc_nat$country=="Uganda", "fract"] <- 0.0236287
opt_loc_nat[opt_loc_nat$country=="Zambia", "fract"] <- 0.0193908
opt_loc_nat[opt_loc_nat$country=="Zimbabwe", "fract"] <- 0.0276735

# Add in Independence Years

opt_loc_nat[opt_loc_nat$country=="Algeria", "indep_year"] <- 1962
opt_loc_nat[opt_loc_nat$country=="Angola", "indep_year"] <- 1975
opt_loc_nat[opt_loc_nat$country=="Benin", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Botswana", "indep_year"] <- 1966
opt_loc_nat[opt_loc_nat$country=="Burkina-Faso", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Burundi", "indep_year"] <- 1962
opt_loc_nat[opt_loc_nat$country=="Cameroon", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Central-African-Republic", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Chad", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Congo", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Democratic-Republic-of-the-Congo", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Djibuti", "indep_year"] <- 1977
opt_loc_nat[opt_loc_nat$country=="Egypt", "indep_year"] <- 1922
opt_loc_nat[opt_loc_nat$country=="Equatorial-Guinea", "indep_year"] <- 1968
opt_loc_nat[opt_loc_nat$country=="Eritrea", "indep_year"] <- 1993
opt_loc_nat[opt_loc_nat$country=="Ethiopia", "indep_year"] <- NA
opt_loc_nat[opt_loc_nat$country=="Lesotho", "indep_year"] <- 1966
opt_loc_nat[opt_loc_nat$country=="Liberia", "indep_year"] <- 1847
opt_loc_nat[opt_loc_nat$country=="Libya", "indep_year"] <- 1951
opt_loc_nat[opt_loc_nat$country=="Malawi", "indep_year"] <- 1964
opt_loc_nat[opt_loc_nat$country=="Mali", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Mauritania", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Morocco", "indep_year"] <- 1956
opt_loc_nat[opt_loc_nat$country=="Namibia", "indep_year"] <- 1990
opt_loc_nat[opt_loc_nat$country=="Niger", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Nigeria", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Rwanda", "indep_year"] <- 1962
opt_loc_nat[opt_loc_nat$country=="Senegal", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Sierra-Leone", "indep_year"] <- 1961
opt_loc_nat[opt_loc_nat$country=="Somalia", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="South-Africa", "indep_year"] <- 1931
opt_loc_nat[opt_loc_nat$country=="Gabon", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Ghana", "indep_year"] <- 1957
opt_loc_nat[opt_loc_nat$country=="Guinea", "indep_year"] <- 1958
opt_loc_nat[opt_loc_nat$country=="Cote-dIvoire", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Kenya", "indep_year"] <- 1963
opt_loc_nat[opt_loc_nat$country=="Sudan", "indep_year"] <- 1956
opt_loc_nat[opt_loc_nat$country=="Swaziland", "indep_year"] <- 1968
opt_loc_nat[opt_loc_nat$country=="United-Republic-of-Tanzania", "indep_year"] <- 1964
opt_loc_nat[opt_loc_nat$country=="Togo", "indep_year"] <- 1960
opt_loc_nat[opt_loc_nat$country=="Tunisia", "indep_year"] <- 1956
opt_loc_nat[opt_loc_nat$country=="Uganda", "indep_year"] <- 1962
opt_loc_nat[opt_loc_nat$country=="Zambia", "indep_year"] <- 1964
opt_loc_nat[opt_loc_nat$country=="Zimbabwe", "indep_year"] <- 1980

# plot(log(gdp10)~welfare_gains, data=opt_loc_nat)
stargazer(lm(corruption~welfare_gains, data=opt_loc_nat), type="text")
stargazer(lm(log(gdp10)~welfare_gains, data=opt_loc_nat), type="text")
stargazer(lm(property_rights~welfare_gains, data=opt_loc_nat), type="text")


cor.test(opt_loc_nat$welfare_gains, log(opt_loc_nat$gdp10))
cor.test(opt_loc_nat$welfare_gains, opt_loc_nat$property_rights)
cor.test(opt_loc_nat$welfare_gains, opt_loc_nat$corruption)
