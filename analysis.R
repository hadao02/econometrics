library(stats)
library(dplyr)
library(ggplot2)
library(tidyr)

# read data
df <- read.csv("D:/Personal project/IMDB/econometrics/economic_freedom_index2019_data.csv")

# check columns' names
# colnames(df)

# remove null data
df <- df[complete.cases(df), ]

df <- drop_na(df)

# clean data
columnsToChange <- c('FDI.Inflow..Millions.', 'GDP.per.Capita..PPP.', 'GDP..Billions..PPP.', 'Unemployment....', 'Population..Millions.')
for (column in columnsToChange) {
  data <- df[[column]]
  edited <- c()
  for (row in data) {
    noComma <- gsub(',', '', row)
    noDollar <- gsub('\\$', '', noComma)
    edited <- c(edited, as.numeric(noDollar))
  }
  df[[column]] <- edited
}

print(str(df))

# convert to numeric

df[, -c(2:4, 25)] <- sapply(df[, -c(2:4, 25)], as.numeric)
print(str(df))

# # check indices of columns
# indices <- seq_along(df)
# names <- colnames(df)
# 
# # combine the indices and names into a data frame
# result <- data.frame(index = indices, name = names)
# result

# rename
colnames(df) <- c("CountryID", "CountryName", "WEBNAME", "Region", "WorldRank", "RegionRank", "2019Score", 
                  "PropertyRights", "JudicialEffectiveness", "GovernmentIntegrity", "TaxBurden", "GovSpending", 
                  "FiscalHealth", "BusinessFreedom", "LaborFreedom", "MonetaryFreedom", "TradeFreedom", 
                  "InvestmentFreedom", "FinancialFreedom", "TariffRate", "IncomeTaxRate", "CorporateTaxRate", 
                  "TaxBurdenRateOfGDP", "GovExpenditureRateOfGDP", "Country", "PopulationMillions", 
                  "GDPBillionsPPP", "GDPGrowthRate", "5YearGDPGrowthRate", "GDPperCapitaPPP", "UnemploymentRate", 
                  "InflationRate", "FDIInflowMillions", "PublicDebtRateOfGDP")


# set up ranks component

RANKS <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
           'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
           'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
           'InvestmentFreedom', 'FinancialFreedom')

# earlier list plus 2019 score column
RANKS_PLUS_TOTAL <- c('2019Score', RANKS)

# columns with other statistics for each country calculated as a percentage
PERCENTAGE_STATS <- c('TariffRate', 'IncomeTaxRate', 'CorporateTaxRate', 
                      'TaxBurdenRateOfGDP', 'GovExpenditureRateOfGDP', 'GDPGrowthRate', 
                      '5YearGDPGrowthRate', 'UnemploymentRate', 'InflationRate', 'PublicDebtRateOfGDP', 'GDPperCapitaPPP')

df <- drop_na(df)
print(str(df))

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

### scater plot

df <- df[order(df$"2019Score"),]

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

X_total <- df[["2019Score"]]
Y <- df[["GDPperCapitaPPP"]]

scatterPlot(X_total, Y)

### squared
Y <- sqrt(df[["GDPperCapitaPPP"]])
names(Y) <- "sqrt of GDP per Capita"
X_total <- df[["2019Score"]]

scatterPlot(X_total, Y)

### OLS

model <- lm(Y ~ X_total)
summary(model)

### correlation

Y <- sqrt(df[["GDPperCapitaPPP"]])
names(Y) <- "sqrt of GDP per Capita"
X <- df[RANKS]
cor(Y, X)

library(corrplot)
M <- cor(Y, X)
corrplot(M)


# add a column of ones to represent the intercept term
df$Intercept <- 1

# specify the model formula
formula <- as.formula(paste("Y ~", paste(c("Intercept", PERCENTAGE_STATS), collapse = " + "), "- 1"))

# fit the linear model
model <- lm(formula, data = df)
