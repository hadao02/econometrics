library(stats)
library(dplyr)
library(ggplot2)
library(tidyr)

# read data
df <- read.csv("D:/Personal project/IMDB/econometrics/World Happiness Report.csv")

# check columns' names
colnames(df)

# change colnames
colnames(df)[7] <- "score_2019"
print(str(df))

### other

RANKS <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
           'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
           'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
           'InvestmentFreedom', 'FinancialFreedom')

# earlier list plus 2019 score column
RANKS_PLUS_TOTAL <- c('score_2019', RANKS)

# columns with other statistics for each country calculated as a percentage
PERCENTAGE_STATS <- c('TariffRate', 'IncomeTaxRate', 'CorporateTaxRate', 
                      'TaxBurdenRateOfGDP', 'GovExpenditureRateOfGDP', 'GDPGrowthRate', 
                      '5YearGDPGrowthRate', 'UnemploymentRate', 'InflationRate', 
                      'PublicDebtRateOfGDP', 'GDPperCapitaPPP')


classifier <- function(item) {
  if (item > 80) {
    return("darkgreen")
  } else if (item > 70) {
    return('limegreen')
  } else if (item > 60) {
    return("yellow")
  } else if (item > 50) {
    return('orange')
  } else {
    return('red')
  }
}

# function for making boxplots
boxPlots <- function(df) {
  boxplot(df, col = sapply(apply(df, 2, median), classifier), main = "World Scores by Category", xlab = "Categories", ylab = "Score", las = 2)
}

# calls function
boxPlots(df[RANKS_PLUS_TOTAL])

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

X_total <- df[["score_2019"]]
Y <- df[["GDPperCapitaPPP"]]

scatterPlot(X_total, Y)


### linear regression
Y <- sqrt(df[["GDPperCapitaPPP"]])
names(Y) <- "sqrt of GDP per Capita"

fit <- lm(Y ~ WorldRank + score_2019 + PropertyRights + JudicialEffectiveness + GovernmentIntegrity
          + TaxBurden + GovSpending + FiscalHealth + BusinessFreedom + LaborFreedom
          + MonetaryFreedom + TradeFreedom + InvestmentFreedom + FinancialFreedom, data = df)

summary(fit)

df$Intercept <- 1

# specify the model formula
formula <- as.formula(paste("Y ~", paste(c("Intercept", RANKS), collapse = " + "), "- 1"))

# fit the linear model
model <- lm(formula, data = df)
summary(model)

