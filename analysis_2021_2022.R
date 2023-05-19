library(stats)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gdata)
library(readxl)
library(purrr)
library(tidyverse)
library(estimatr)
library(texreg)
library(sandwich)
library(haven)
library(magrittr)

######################## read data ######################## 
df_2017<-read_excel("D:/Personal project/IMDB/econometrics/index2017_data.xls")
print(str(df_2017))

df_2018<-read_excel("D:/Personal project/IMDB/econometrics/index2018_data.xls")
print(str(df_2018))

df_2019<-read_excel("D:/Personal project/IMDB/econometrics/index2019_data.xls")
print(str(df_2019))

df_2020<-read_excel("D:/Personal project/IMDB/econometrics/index2020_data.xls")
print(str(df_2020))

df_2021 <- read_excel("D:/Personal project/IMDB/econometrics/index2021_data.xls")
df_2021 <- subset(df_2021, select = -c(WEBNAME))
print(str(df_2021))

df_2022 <- read_excel("D:/Personal project/IMDB/econometrics/index2022_data.xls")
print(str(df_2022))

colnames(df_2017) <- c("CountryID", "CountryName", "Region", "WorldRank", "RegionRank", "Score", 
                       "PropertyRights", "JudicialEffectiveness", "GovernmentIntegrity", "TaxBurden", "GovSpending", 
                       "FiscalHealth", "BusinessFreedom", "LaborFreedom", "MonetaryFreedom", "TradeFreedom", 
                       "InvestmentFreedom", "FinancialFreedom", "TariffRate", "IncomeTaxRate", "CorporateTaxRate", 
                       "TaxBurdenRateOfGDP", "GovExpenditureRateOfGDP", "PopulationMillions", 
                       "GDPBillionsPPP", "GDPGrowthRate", "5YearGDPGrowthRate", "GDPperCapitaPPP", "UnemploymentRate", 
                       "InflationRate", "FDIInflowMillions", "PublicDebtRateOfGDP")
colnames(df_2018) <- c("CountryID", "CountryName", "Region", "WorldRank", "RegionRank", "Score", 
                       "PropertyRights", "JudicialEffectiveness", "GovernmentIntegrity", "TaxBurden", "GovSpending", 
                       "FiscalHealth", "BusinessFreedom", "LaborFreedom", "MonetaryFreedom", "TradeFreedom", 
                       "InvestmentFreedom", "FinancialFreedom", "TariffRate", "IncomeTaxRate", "CorporateTaxRate", 
                       "TaxBurdenRateOfGDP", "GovExpenditureRateOfGDP", "PopulationMillions", 
                       "GDPBillionsPPP", "GDPGrowthRate", "5YearGDPGrowthRate", "GDPperCapitaPPP", "UnemploymentRate", 
                       "InflationRate", "FDIInflowMillions", "PublicDebtRateOfGDP")
colnames(df_2019) <- c("CountryID", "CountryName", "Region", "WorldRank", "RegionRank", "Score", 
                       "PropertyRights", "JudicialEffectiveness", "GovernmentIntegrity", "TaxBurden", "GovSpending", 
                       "FiscalHealth", "BusinessFreedom", "LaborFreedom", "MonetaryFreedom", "TradeFreedom", 
                       "InvestmentFreedom", "FinancialFreedom", "TariffRate", "IncomeTaxRate", "CorporateTaxRate", 
                       "TaxBurdenRateOfGDP", "GovExpenditureRateOfGDP", "PopulationMillions", 
                       "GDPBillionsPPP", "GDPGrowthRate", "5YearGDPGrowthRate", "GDPperCapitaPPP", "UnemploymentRate", 
                       "InflationRate", "FDIInflowMillions", "PublicDebtRateOfGDP")
colnames(df_2020) <- c("CountryID", "CountryName", "Region", "WorldRank", "RegionRank", "Score", 
                       "PropertyRights", "JudicialEffectiveness", "GovernmentIntegrity", "TaxBurden", "GovSpending", 
                       "FiscalHealth", "BusinessFreedom", "LaborFreedom", "MonetaryFreedom", "TradeFreedom", 
                       "InvestmentFreedom", "FinancialFreedom", "TariffRate", "IncomeTaxRate", "CorporateTaxRate", 
                       "TaxBurdenRateOfGDP", "GovExpenditureRateOfGDP", "PopulationMillions", 
                       "GDPBillionsPPP", "GDPGrowthRate", "5YearGDPGrowthRate", "GDPperCapitaPPP", "UnemploymentRate", 
                       "InflationRate", "FDIInflowMillions", "PublicDebtRateOfGDP")
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


print(str(df_2017))
print(str(df_2018))
print(str(df_2019))
print(str(df_2020))
print(str(df_2021))
print(str(df_2022))

colnames(df_2017)
colnames(df_2018)
colnames(df_2019)
colnames(df_2020)
colnames(df_2021)
colnames(df_2022)

######################## clearn ######################## 

### change to numeric
df_2017[, -c(2,3)] <- sapply(df_2017[, -c(2,3)], as.numeric)
print(str(df_2017))
df_2018[, -c(2,3)] <- sapply(df_2018[, -c(2,3)], as.numeric)
print(str(df_2018))
df_2019[, -c(2,3)] <- sapply(df_2019[, -c(2,3)], as.numeric)
print(str(df_2019))
df_2020[, -c(2,3)] <- sapply(df_2020[, -c(2,3)], as.numeric)
print(str(df_2020))
df_2021[, -c(2,3)] <- sapply(df_2021[, -c(2,3)], as.numeric)
print(str(df_2021))
df_2022[, -c(2,3)] <- sapply(df_2022[, -c(2,3)], as.numeric)
print(str(df_2022))



######################## merge ######################## 

df_2017$year <- 2017
df_2018$year <- 2018
df_2019$year <- 2019
df_2020$year <- 2020
df_2021$year <- 2021
df_2022$year <- 2022

# Stack the data frames on top of each other
merged_df <- rbind(df_2017, df_2018, df_2019, df_2020, df_2021, df_2022)

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
                      'TaxBurdenRateOfGDP', 'GovExpenditureRateOfGDP', 'GDPGrowthRate', 'UnemploymentRate', 'InflationRate')
# doesnt count 'PublicDebtRateOfGDP'

RANKS_AND_RATE_original <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
                    'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
                    'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
                    'InvestmentFreedom', 'FinancialFreedom', 'TariffRate', 
                    'IncomeTaxRate', 'CorporateTaxRate',  'UnemploymentRate', 'InflationRate')

RANKS_AND_RATE <- c('PropertyRights', 'GovernmentIntegrity', 'TaxBurden', 
                    'BusinessFreedom', 
                    'FinancialFreedom', 
                    'IncomeTaxRate', 'CorporateTaxRate', 'InflationRate')


### drop na

merged_df <- drop_na(merged_df)
print(is.na(merged_df))

write.csv(merged_df, "D:/Personal project/IMDB/econometrics/index_merged_data.csv", row.names=FALSE)


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

######################## box plot ######################## 

classifier <- function(item) {
  if (is.na(item) || !is.numeric(item)) {
    return('gray')
  } else if (item < 0) {
    return('darkgreen')
  } else if (item > 30) {
    return('limegreen')
  } else if (item > 20) {
    return('yellow')
  } else if (item > 10) {
    return('orange')
  } else {
    return('red')
  }
}

# function for making boxplots
boxPlots <- function(merged_df) {
  boxplot(merged_df, col = sapply(apply(merged_df, 2, median), classifier), main = "World Scores by Category", xlab = "", ylab = "Score", las = 2, ylim=c(-30,110))
}

par(mar=c(13, 4, 4, 2))

# calls function
boxPlots(merged_df[PERCENTAGE_STATS])

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

X_total <- merged_df[["TaxBurden"]]
Y <- merged_df[["GDPGrowthRate"]]

scatterPlot(X_total, Y)

######################## correlation matrix ######################## 
library(GGally)
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
correlationMatrix(merged_df[, c(RANKS_AND_RATE_original, "GDPGrowthRate")])


######################## linear regression w/ stepwise selection ######################## 

Y <- merged_df[["GDPGrowthRate"]]
# Y <- merged_df[["GDPGrowthRate"]]
names(Y) <- "GDPGrowthRate"

merged_df$Intercept <- 1

# specify the model formula
formula <- as.formula(paste("Y ~", paste(c("Intercept", RANKS_AND_RATE_original), collapse = " + "), "- 1"))

# fit the full model
full_model <- lm(formula, data = merged_df)

# Perform stepwise selection
step_model <- step(full_model, direction = "both")
summary(step_model)


######################## fixed effects model ######################## 
library(plm)
Y <- merged_df[["GDPGrowthRate"]]
# Y <- merged_df[["GDPGrowthRate"]]
names(Y) <- "sqrt of GDPGrowthRate"

merged_df$Intercept <- 1

formula <- as.formula(paste("Y ~", paste(c("Intercept", RANKS_AND_RATE_original), collapse = " + "), "+ factor(CountryName) - 1"))

# Run a fixed effects regression with country fixed effects
# model_fixed <- lm(formula, data = merged_df)
# model_fixed <- plm(formula + factor(CountryName), data = merged_df)


# Run a fixed effects regression with multiple independent variables using plm()
model <- plm(formula, data = merged_df, index = c("CountryName", "year"), model = "within")
summary(model)

'
The result shows that you have fitted a fixed effects regression model with 17 independent variables and one dependent variable (Y) using plm() in R. 
You have used the within estimator, which means that you have removed the individual-specific effects (CountryName) by subtracting the group-level average over time. 
This way, you can control for the unobserved heterogeneity that is constant over time and may be correlated with the independent variables. 
A fixed effects model makes sense here better than a normal linear regression model because you have panel data, 
which means that you have repeated observations for the same subjects (countries) over time. 
A normal linear regression model would not account for the individual-specific effects and may produce biased estimates due to omitted variable bias12

The result also shows the estimated coefficients, standard errors, t-values, and p-values for each independent variable. 
You can use these to test your hypotheses and interpret the effects of each variable on Y. 
For example, you can see that FinancialFreedom has a negative and significant effect on Y at 5% level, 
which means that higher financial freedom is associated with lower Y, holding other variables constant. 
You can also see the R-squared and adjusted R-squared values, which measure how well the model fits the data. 
The R-squared value is 0.047345, which means that the model explains about 4.7% of the variation in Y. 
The adjusted R-squared value is -0.25382, which means that the model performs worse than a simple mean model 
after adjusting for the number of variables. You can also see the F-statistic and its p-value, 
which test whether the model is overall significant or not. The F-statistic is 1.8125 and its p-value is 0.023389, 
which means that you can reject the null hypothesis that all the coefficients are zero at 5% level.
'

# ######################## spurious regression w/ time series ######################## 
# 
# RANKS_AND_RATE_original <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
#                              'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
#                              'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
#                              'InvestmentFreedom', 'FinancialFreedom', 'TariffRate', 
#                              'IncomeTaxRate', 'CorporateTaxRate',  'UnemploymentRate', 'InflationRate')
# 
# library(zoo)
# # merged_df <- merged_df[1:length(ts), ]
# 
# 
# # merged_df <- merged_df[complete.cases(merged_df), ]
# Y <- merged_df[["GDPGrowthRate"]]
# # Y <- merged_df[["GDPGrowthRate"]]
# names(Y) <- "GDPGrowthRate"
# ts <- ts(Y, start = c(2017, 1), end = c(2022, 368), frequency = 1)
# # ts <- ts(cbind(Y, InflationRate, UnemploymentRate), start = c(2021, 1), end = c(2022, 368), frequency = 1)
# 
# 
# merged_df$InflationRate[is.na(merged_df$InflationRate)] <- mean(merged_df$InflationRate, na.rm = TRUE)
# 
# # Impute missing values using linear interpolation
# merged_df$InflationRate <- na.approx(merged_df$InflationRate)
# 
# model <- lm(ts ~ InflationRate + UnemploymentRate, data = merged_df)
# summary(model)
# 
# 
# 
# sum(is.na(merged_df$GDPGrowthRate))
# 
# print(str(merged_df$year))
# Y <- merged_df[c("year", "GDPGrowthRate")]
# # summary(merged_df[["GDPGrowthRate"]])
# 
# 
# # Y <- merged_df[["GDPGrowthRate"]]
# # names(Y) <- "GDPGrowthRate"
# Y <- merged_df[c("year", "GDPGrowthRate")]
# RANKS_AND_RATE_original <- merged_df[c("year", 'PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
#                                        'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
#                                        'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
#                                        'InvestmentFreedom', 'FinancialFreedom', 'TariffRate', 
#                                        'IncomeTaxRate', 'CorporateTaxRate',  'UnemploymentRate', 'InflationRate')]
# time_series <- merge(Y, RANKS_AND_RATE_original, by = "year")
# ts <- ts(time_series, start = c(2017, 1), end = c(2022, 1024), frequency = 1)
# 
# model <- lm(GDPGrowthRate ~ ., data = ts) 
# summary(model)
# 
# 
# 
# for (var in time_series) {
# 
#   # Create a time series object with the variable
#   ts <- ts(df[[var]], start = c(2021, 1), end = c(2022, 368), frequency = 1)
#   assign(var, ts) }
# 
# sapply(merged_df[RANKS_AND_RATE_original], length)
# # merged_df[RANKS_AND_RATE_original] <- na.omit(merged_df[RANKS_AND_RATE_original])
# 
# 
# length(merged_df)
# length(merged_df$UnemploymentRate)
# length(ts)
# 
# 
# sum(is.na(merged_df$InflationRate))
# sum(is.na(merged_df$UnemploymentRate))
# sum(is.na(ts))
# 
# formula <- as.formula(paste("ts ~", paste(c("Intercept", RANKS_AND_RATE_original), collapse = " + "), "- 1"))
# 
# full_model <- lm(formula, data = merged_df)
# summary(full_model)
# 
# ######################## multicollinear ######################## 
# install.packages("car")
# library(car)
# 
# Y <- merged_df[c("year", "GDPGrowthRate")]
# RANKS_AND_RATE_original <- merged_df[c("year", 'PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
#                                        'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
#                                        'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
#                                        'InvestmentFreedom', 'FinancialFreedom', 'TariffRate', 
#                                        'IncomeTaxRate', 'CorporateTaxRate',  'UnemploymentRate', 'InflationRate')]
# 
# # fit linear model
# model <- lm(GDPGrowthRate ~ ., data = ts)
# 
# # check model summary to see which predictor variables have been dropped
# summary(model)
# 
# # remove dropped predictor variables from data frame
# ts_no_alias <- ts[, !names(coef(model))[is.na(coef(model))]]
# 
# # calculate VIF for each predictor variable
# vif_values <- vif(lm(GDPGrowthRate ~ ., data = ts_no_alias))
# 
# # remove highly correlated variables (VIF > 5)
# ts_no_multicollinearity <- ts_no_alias[, !names(vif_values[vif_values > 5])]
# 
# # fit linear model using remaining predictor variables
# model_no_multicollinearity <- lm(GDPGrowthRate ~ ., data = ts_no_multicollinearity)
# summary(model_no_multicollinearity)
# ###########
# time_series <- merge(Y, RANKS_AND_RATE_original, by = "year")
# ts <- ts(time_series, start = c(2017, 1), end = c(2022, 1024), frequency = 1)
# 
# # calculate VIF for each predictor variable
# vif_values <- vif(lm(GDPGrowthRate ~ ., data = ts))
# 
# # remove highly correlated variables (VIF > 5)
# ts_no_multicollinearity <- ts[, !names(vif_values[vif_values > 5])]
# 
# # fit linear model using remaining predictor variables
# model_no_multicollinearity <- lm(GDPGrowthRate ~ ., data = ts_no_multicollinearity)
# summary(model_no_multicollinearity)

######################## linear regression w/ cross validation ######################## 

Y <- sqrt(merged_df[["GDPGrowthRate"]])
# Y <- merged_df[["GDPGrowthRate"]]
names(Y) <- "sqrt of GDPGrowthRate"

merged_df$Intercept <- 1

# specify the model formula
formula <- as.formula(paste("Y ~", paste(c("Intercept", RANKS_AND_RATE_original), collapse = " + "), "- 1"))

# fit the linear model
# Fit the full model
full_model <- lm(formula, data = merged_df)
# Set the number of folds
k <- 10

# Create a vector to store the cross-validated errors
cv_errors <- rep(0, k)

# Set a random seed for reproducibility
set.seed(123)

# Perform k-fold cross-validation
folds <- sample(rep(1:k, length.out = nrow(merged_df)))
for (i in 1:k) {
  # Split the data into training and test sets
  test <- which(folds == i)
  train <- which(folds != i)
  train_data <- merged_df[train, ]
  test_data <- merged_df[test, ]
  
  # Fit the full model on the training set
  full_model <- lm(formula, data = train_data)
  
  # Perform stepwise selection on the training set
  step_model <- step(full_model, direction = "both")
  
  print(summary(step_model))
  
  # Compute the test error
  preds <- predict(step_model, newdata = test_data)
  cv_errors[i] <- mean((preds - test_data$Y)^2)
}

# Compute the average cross-validated error
mean(cv_errors)


######################## based on region ########################


Y <- merged_df[["GDPGrowthRate"]]
names(Y) <- "GDPGrowthRate"

merged_df$Y <- Y

# define the model formula
formula <- as.formula(paste("Y ~", paste(RANKS_AND_RATE_original, collapse = " + ")))

# get the unique regions
regions <- unique(merged_df$Region)

# loop over the regions
for (region in regions) {
  # subset the data for the current region
  region_data <- merged_df[merged_df$Region == region, ]
  
  # fit the linear model to the subsetted data
  model <- lm(formula, data = region_data)
  step_model <- step(model, direction = "both", trace = FALSE)
  
  # print the model coefficients
  cat("\nRegion:", region, "\n")
  print(summary(step_model))
}
  # print the length of each variable in the region_data data frame
  # cat("Region:", region, "\n")
  # for (var in all.vars(formula)) {
  #   cat(var, ":", length(region_data[[var]]), "\n")
  # }


######################## chi-squared ######################## 

tbl <- table(merged_df$Region)

# Perform the chi-squared test
chisq.test(tbl)

## X-squared = 39.959, df = 4, p-value = 4.414e-08
## the distribution of observations across different regions in the merged_df data frame is not random and that there are significant differences in the frequencies of observations between different regions

######################## different region ######################## 

model_formula <- Y ~ Region

# Fit the model
model <- lm(model_formula, data = merged_df)

# View the model summary
summary(model)


######################## 2sls ######################## 
## GovSpending as an endogenous regressor and PopulationMillions as an instrumental variable for GovSpending
library(AER)
  
  # Create the formula
formula <- as.formula("Y ~ GovSpending + PopulationMillions + PropertyRights + JudicialEffectiveness + TaxBurden + FiscalHealth + BusinessFreedom +
                      LaborFreedom + MonetaryFreedom + TradeFreedom + InvestmentFreedom +
                      FinancialFreedom | GovernmentIntegrity + PopulationMillions + PropertyRights +
                      JudicialEffectiveness + TaxBurden +
                      FiscalHealth + BusinessFreedom + LaborFreedom + MonetaryFreedom +
                      TradeFreedom + InvestmentFreedom + FinancialFreedom")

# Fit the 2SLS model
ivreg_fit <- ivreg(formula, data = merged_df)

summary(ivreg_fit)

install.packages('lmtest')
library(AER)

library(lmtest)

cor.test(merged_df$PopulationMillions, merged_df$GovSpending)

# Perform the Sargan-Hansen test
ivreg_fit <- ivreg(formula, data = merged_df)
sargan(ivreg_fit)

######################## ANOVA ######################## 

for (response_variable in PERCENTAGE_STATS) {
  # Specify the model formula
  model_formula <- as.formula(paste(response_variable, "~ Region"))
  
  # Fit the model
  model <- aov(model_formula, data = merged_df)
  
  # View the ANOVA table
  cat("\nANOVA for", response_variable, "\n")
  print(summary(model))
  
}

######################## Krustal-Wallis ######################## 

kruskal.test(Y ~ Region, data = merged_df)

# data:  Y by Region
# Kruskal-Wallis chi-squared = 184.15, df = 4, p-value < 2.2e-16
# The p-value is less than 2.2e-16, which is very close to zero. This indicates that there is a statistically significant difference in the medians of Y (square root of GDP per capita) between the different regions.

