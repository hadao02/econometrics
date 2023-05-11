library(stats)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gdata)
library(readxl)
library(purrr)

library(tidyverse)
# install.packages('tidyverse')
# install.packages('estimatr')
# install.packages('texreg')
# install.packages('sandwich')
# install.packages('haven')
# install.packages('magrittr')

library(estimatr)
library(texreg)
library(sandwich)
library(haven)
library(magrittr) # needs to be run every time you start R and want to use %>%
######################## read data ######################## 

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

######################## clearn ######################## 

### change to numeric

df_2021[, -c(2,3)] <- sapply(df_2021[, -c(2,3)], as.numeric)
print(str(df_2021))
df_2022[, -c(2,3)] <- sapply(df_2022[, -c(2,3)], as.numeric)
print(str(df_2022))

######################## merge ######################## 

df_2021$year <- 2021
df_2022$year <- 2022

# Stack the data frames on top of each other
merged_df <- rbind(df_2021, df_2022)

# Sort by CountryName
merged_df <- merged_df[order(merged_df$CountryName), ]
colnames(merged_df)

print(str(merged_df))

######################## pre-processing ######################## 

RANKS <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
           'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
           'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
           'InvestmentFreedom', 'FinancialFreedom')

# earlier list plus score column
RANKS_PLUS_TOTAL <- c('Score', RANKS)

# columns with other statistics for each country calculated as a percentage
PERCENTAGE_STATS <- c('TariffRate', 'IncomeTaxRate', 'CorporateTaxRate', 
                      'TaxBurdenRateOfGDP', 'GovExpenditureRateOfGDP', 'GDPGrowthRate', 
                      '5YearGDPGrowthRate', 'UnemploymentRate', 'InflationRate', 
                      'PublicDebtRateOfGDP', 'GDPperCapitaPPP')

RANKS_AND_RATE <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
                    'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
                    'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
                    'InvestmentFreedom', 'FinancialFreedom', 'TariffRate', 
                    'IncomeTaxRate', 'CorporateTaxRate', 
                    'TaxBurdenRateOfGDP', 'GovExpenditureRateOfGDP', 'GDPGrowthRate', 
                    '5YearGDPGrowthRate', 'UnemploymentRate', 'InflationRate', 
                    'PublicDebtRateOfGDP', 'GDPperCapitaPPP')


### drop na

merged_df <- drop_na(merged_df)

######################## box plot ######################## 

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


######################## scatter plot ######################## 

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

######################## correlation matrix ######################## 

# install.packages('GGally')
library(GGally)
# install.packages("reshape2")
library(reshape2)

correlationMatrix <- function(merged_df) {
  # Compute correlation matrix
  corr <- cor(merged_df)
  
  # Melt
  melted_corr <- reshape2::melt(corr)
  
  # Plot 
  ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggtitle("Correlation Matrix")
}

# Call the function with the desired columns
correlationMatrix(merged_df[, c(RANKS_AND_RATE, "Score")])


######################## linear regression ######################## 

Y <- sqrt(merged_df[["GDPperCapitaPPP"]])
names(Y) <- "sqrt of GDP per Capita"

merged_df$Intercept <- 1

# specify the model formula
formula <- as.formula(paste("Y ~", paste(c("Intercept", RANKS), collapse = " + "), "- 1"))

# fit the linear model
model <- lm_robust(formula, data = merged_df)
summary(model)

######################## chi-squared ######################## 

tbl <- table(merged_df$Region)

# Perform the chi-squared test
chisq.test(tbl)

######################## different region ######################## 

model_formula <- Y ~ Region

# Fit the model
model <- lm(model_formula, data = merged_df)

# View the model summary
summary(model)


######################## 2sls ######################## 
## GovSpending as an endogenous regressor and PopulationMillions as an instrumental variable for GovSpending
library(AER)

formula <- as.formula("Y ~ GovSpending + PropertyRights + JudicialEffectiveness 
                      + GovernmentIntegrity + TaxBurden + FiscalHealth + BusinessFreedom 
                      + LaborFreedom + MonetaryFreedom + TradeFreedom + InvestmentFreedom 
                      + FinancialFreedom | PopulationMillions + PropertyRights 
                      + JudicialEffectiveness + GovernmentIntegrity + TaxBurden 
                      + FiscalHealth + BusinessFreedom + LaborFreedom + MonetaryFreedom 
                      + TradeFreedom + InvestmentFreedom + FinancialFreedom")
# Fit the 2SLS model
ivreg_fit <- ivreg(formula, data = merged_df)

summary(ivreg_fit)