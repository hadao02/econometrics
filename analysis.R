library(stats)
library(dplyr)
library(ggplot2)

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
colnames(df) <- c("CountryID", "Country Name", "WEBNAME", "Region", "World Rank", "Region Rank", "2019 Score", "Property Rights", "Judicial Effectiveness", "Government Integrity", "Tax Burden", "Gov't Spending", "Fiscal Health", "Business Freedom", "Labor Freedom", "Monetary Freedom", "Trade Freedom", "Investment Freedom", "Financial Freedom", "Tariff Rate (%)", "Income Tax Rate (%)", "Corporate Tax Rate (%)", "Tax Burden % of GDP", "Gov't Expenditure % of GDP", "Country", "Population (Millions)", "GDP (Billions, PPP)", "GDP Growth Rate (%)", "5 Year GDP Growth Rate (%)", "GDP per Capita (PPP)", "Unemployment (%)", "Inflation (%)", "FDI Inflow (Millions)", "Public Debt (% of GDP)")


# set up ranks component

RANKS <- c('Property Rights', 'Judicial Effectiveness', 'Government Integrity', 'Tax Burden', 
           'Gov Spending', 'Fiscal Health', 'Business Freedom', 
           'Labor Freedom', 'Monetary Freedom', 'Trade Freedom', 
           'Investment Freedom', 'Financial Freedom')

# earlier list plus 2019 score column
RANKS_PLUS_TOTAL <- c('2019 Score', RANKS)

# columns with other statistics for each country calculated as a percentage
PERCENTAGE_STATS <- c('Tariff Rate (%)', 'Income Tax Rate (%)', 'Corporate Tax Rate (%)', 
                      'Tax Burden % of GDP', 'Gov Expenditure % of GDP', 'GDP Growth Rate (%)', '5 Year GDP Growth Rate (%)',
                      'Unemployment (%)', 'Inflation (%)', 'Public Debt (% of GDP)', 'GDP per Capita (PPP)')

df <- drop_na(df)
print(str(df))

### scater plot

df <- df[order(df$"2019 Score"),]

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

X_total <- df[["2019 Score"]]
Y <- df[["GDP per Capita (PPP)"]]

scatterPlot(X_total, Y)

### squared
Y <- sqrt(df[["GDP per Capita (PPP)"]])
names(Y) <- "sqrt of GDP per Capita"
X_total <- df[["2019 Score"]]

scatterPlot(X_total, Y)

### OLS

model <- lm(Y ~ X_total)
summary(model)

### correlation

X_factors <- df['Property Rights', 'Judicial Effectiveness', 'Government Integrity', 'Tax Burden', 
                'Gov Spending', 'Fiscal Health', 'Business Freedom', 
                'Labor Freedom', 'Monetary Freedom', 'Trade Freedom', 
                'Investment Freedom', 'Financial Freedom']
Y <- df[["GDP per Capita (PPP)"]]
cor(X_factors, Y)
X_const <- cbind(1, X_factors)

model <- lm(Y ~ X_const)
summary(model)
