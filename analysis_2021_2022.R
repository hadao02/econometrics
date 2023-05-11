library(stats)
# install.packages('stats')
library(dplyr)
library(ggplot2)
library(tidyr)
# install.packages("gdata")
library(gdata)
# install.packages("readxl")
library(readxl)

# install.packages("purrr")
library(purrr)

# read data
df_2021 <- read_excel("D:/Personal project/IMDB/econometrics/index2021_data.xls")
df_2021 <- subset(df_2021, select = -c(WEBNAME))
print(str(df_2021))
df_2022 <- read_excel("D:/Personal project/IMDB/econometrics/index2022_data.xls")
print(str(df_2022))

colnames(df_2021) <- c("CountryID", "CountryName", "Region", "WorldRank", "RegionRank", "Score", 
                  "PropertyRights", "JudicialEffectiveness", "GovernmentIntegrity", "TaxBurden", "GovSpending", 
                  "FiscalHealth", "BusinessFreedom", "LaborFreedom", "MonetaryFreedom", "TradeFreedom", 
                  "InvestmentFreedom", "FinancialFreedom", "TariffRate", "IncomeTaxRate", "CorporateTaxRate", 
                  "TaxBurdenRateOfGDP", "GovExpenditureRateOfGDP", "PopulationMillions", 
                  "GDPBillionsPPP", "GDPGrowthRate", "5YearGDPGrowthRate", "GDPperCapitaPPP", "UnemploymentRate", 
                  "InflationRate", "FDIInflowMillions", "PublicDebtRateOfGDP")

colnames(df_2022) <- c("CountryID", "CountryName", "Region", "WorldRank", "RegionRank", "Score", 
                       "PropertyRights", "JudicialEffectiveness", "GovernmentIntegrity", "TaxBurden", "GovSpending", 
                       "FiscalHealth", "BusinessFreedom", "LaborFreedom", "MonetaryFreedom", "TradeFreedom", 
                       "InvestmentFreedom", "FinancialFreedom", "TariffRate", "IncomeTaxRate", "CorporateTaxRate", 
                       "TaxBurdenRateOfGDP", "GovExpenditureRateOfGDP", "PopulationMillions", 
                       "GDPBillionsPPP", "GDPGrowthRate", "5YearGDPGrowthRate", "GDPperCapitaPPP", "UnemploymentRate", 
                       "InflationRate", "FDIInflowMillions", "PublicDebtRateOfGDP")

print(str(df_2021))
print(str(df_2022))

colnames(df_2021)
colnames(df_2022)

df_2021[, -c(2,3)] <- sapply(df_2021[, -c(2,3)], as.numeric)
print(str(df_2021))
df_2022[, -c(2,3)] <- sapply(df_2022[, -c(2,3)], as.numeric)
print(str(df_2022))


df_2021$year <- 2021
df_2022$year <- 2022

# Stack the data frames on top of each other
merged_df <- rbind(df_2021, df_2022)

# Sort by CountryName
merged_df <- merged_df[order(merged_df$CountryName), ]
colnames(merged_df)

print(str(merged_df))


RANKS <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
           'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
           'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
           'InvestmentFreedom', 'FinancialFreedom')

# earlier list plus 2019 score column
RANKS_PLUS_TOTAL <- c('Score', RANKS)

# columns with other statistics for each country calculated as a percentage
PERCENTAGE_STATS <- c('TariffRate', 'IncomeTaxRate', 'CorporateTaxRate', 
                      'TaxBurdenRateOfGDP', 'GovExpenditureRateOfGDP', 'GDPGrowthRate', 
                      '5YearGDPGrowthRate', 'UnemploymentRate', 'InflationRate', 
                      'PublicDebtRateOfGDP', 'GDPperCapitaPPP')
merged_df <- drop_na(merged_df)

classifier <- function(item) {
  if (is.na(item) || !is.numeric(item)) {
    return('gray')
  } else if (item > 80) {
    return('darkgreen')
  } else if (item > 70) {
    return('limegreen')
  } else if (item > 60) {
    return('yellow')
  } else if (item > 50) {
    return('orange')
  } else {
    return('red')
  }
}

# function for making boxplots
boxPlots <- function(merged_df) {
  boxplot(merged_df, col = sapply(apply(merged_df, 2, median), classifier), main = "World Scores by Category", xlab = "", ylab = "Score", las = 2)
}

# calls function
boxPlots(merged_df[RANKS_PLUS_TOTAL])

### scatter plot
scatterPlot <- function(X, Y, predictions=NULL) {
  p <- ggplot() +
    geom_point(aes(x=X, y=Y)) +
    ggtitle(paste(colnames(X), "vs", colnames(Y))) +
    xlab(colnames(X)) +
    ylab(colnames(Y))
  
  if(!is.null(predictions)) {
    p <- p + geom_line(aes(x=X, y=predictions), color="black")
  }
  
  print(p)
}

X_total <- merged_df[["Score"]]
Y <- merged_df[["GDPperCapitaPPP"]]

scatterPlot(X_total, Y)


### linear regression
Y <- sqrt(merged_df[["GDPperCapitaPPP"]])
names(Y) <- "sqrt of GDP per Capita"

# fit <- lm(Y ~ WorldRank + score_2019 + PropertyRights + JudicialEffectiveness + GovernmentIntegrity
#           + TaxBurden + GovSpending + FiscalHealth + BusinessFreedom + LaborFreedom
#           + MonetaryFreedom + TradeFreedom + InvestmentFreedom + FinancialFreedom, data = df)
# 
# summary(fit)

merged_df$Intercept <- 1

# specify the model formula
formula <- as.formula(paste("Y ~", paste(c("Intercept", RANKS), collapse = " + "), "- 1"))

# fit the linear model
model <- lm(formula, data = merged_df)
summary(model)

