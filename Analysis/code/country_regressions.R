###
# This file conducts regressions at the country level.

############

# First, aggregating national means for zeta

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
    opt_loc_nat[i, "zeta"] <- ((sum(df$pop * df$util_opt)  / sum(df$pop * df$util_stat)) - 1) * 100


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

colnames(opt_loc_nat) <- c("country", "zeta", "corruption", "property_rights", "legal_rights", "poverty1", "poverty2", "poverty3", "povertyratio", "ease_business", "life_expectancy", "gdp10", "x")

opt_loc_nat <- opt_loc_nat[,c("country", "zeta", "corruption", "property_rights", "legal_rights", "ease_business", "life_expectancy", "gdp10")]


for(var in c("corruption", "property_rights", "legal_rights", "ease_business", "life_expectancy", "gdp10")){

  opt_loc_nat[,var] <- as.numeric(paste(opt_loc_nat[,var]))

}

# plot(log(gdp10)~zeta, data=opt_loc_nat)
stargazer(lm(corruption~zeta, data=opt_loc_nat), type="text")
stargazer(lm(log(gdp10)~zeta, data=opt_loc_nat), type="text")
stargazer(lm(property_rights~zeta, data=opt_loc_nat), type="text")


cor.test(opt_loc_nat$zeta, log(opt_loc_nat$gdp10))
cor.test(opt_loc_nat$zeta, opt_loc_nat$property_rights)
cor.test(opt_loc_nat$zeta, opt_loc_nat$corruption)
