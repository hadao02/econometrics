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
library(corrplot)
library(ggcorrplot)
library(tseries)
library(plm)
library(lmtest)

df <- read.csv("D:/Personal project/IMDB/econometrics/index_merged_data.csv")
print(str(df))

if (!is.numeric(df$GDPGrowthRate)) {
  df$GDPGrowthRate <- as.numeric(df$GDPGrowthRate)
}

######################## box plot ######################## 
RANKS <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
           'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
           'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
           'InvestmentFreedom', 'FinancialFreedom')

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
boxPlots <- function(df) {
  boxplot(df, col = sapply(apply(df, 2, median), classifier), main = "World Scores by Category", xlab = "", ylab = "Score", las = 2)
}

# calls function
boxPlots(df[RANKS])


######################## box plot ######################## 

PERCENTAGE_STATS <- c('TariffRate', 'IncomeTaxRate', 'CorporateTaxRate',
                      'TaxBurdenRateOfGDP', 'GovExpenditureRateOfGDP', 'GDPGrowthRate', 'UnemploymentRate', 'InflationRate')


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
boxPlots <- function(df) {
  boxplot(df, col = sapply(apply(df, 2, median), classifier), main = "World Scores by Category", xlab = "", ylab = "Score", las = 2, ylim=c(-30,110))
}

par(mar=c(13, 4, 4, 2))

# calls function
boxPlots(df[PERCENTAGE_STATS])

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

X_total <- df[["TaxBurden"]]
Y <- df[["GDPGrowthRate"]]

scatterPlot(X_total, Y)


######################## correlation matrix ######################## 
library(GGally)
library(reshape2)

RANKS_AND_RATE_original <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
                             'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
                             'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
                             'InvestmentFreedom', 'FinancialFreedom', 'TariffRate', 
                             'IncomeTaxRate', 'CorporateTaxRate',  'UnemploymentRate', 'InflationRate')

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
correlationMatrix(df[, c(RANKS_AND_RATE_original, "GDPGrowthRate")])

######################## check correlation ######################## 

Independent_Variables <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
                           'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
                           'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
                           'InvestmentFreedom', 'FinancialFreedom', 'TariffRate', 
                           'IncomeTaxRate', 'CorporateTaxRate',  'UnemploymentRate', 'InflationRate')

######################## chi-squared ######################## 

tbl <- table(df$Region)

# Perform the chi-squared test
chisq.test(tbl)

######################## different region ######################## 

model_formula <- Y ~ Region

# Fit the model
model <- lm(model_formula, data = df)

# View the model summary
summary(model)

######################## step model ######################## 

Y <- df[["GDPGrowthRate"]]
names(Y) <- "GDPGrowthRate"

df$Intercept <- 1

formula_step <- as.formula(paste("Y ~", paste(c("Intercept", Independent_Variables), collapse = " + "), "- 1"))

full_model <- lm(formula_step, data = df)

# Perform stepwise selection
step_model <- step(full_model, direction = "both")
summary(step_model)

######################## spurious regression ######################## 

# Select independent variables
independent_variables <- select(df, -c(Score, RegionRank, WorldRank, GDPGrowthRate, year, CountryName, Region, CountryID, X5YearGDPGrowthRate))

# Perform ADF test on each independent variable
adf_results <- sapply(independent_variables, function(x) adf.test(x)$p.value)

# Choose n independent variables with lowest p-values
n <- 24
best_variables <- names(sort(adf_results))[1:n]

# Run spurious regression
spu_model <- lm(GDPGrowthRate ~ ., data = df[,c("GDPGrowthRate", best_variables)])
summary(spu_model)

######################## fixed effects model ######################## 

Y <- df[["GDPGrowthRate"]]
names(Y) <- "GDPGrowthRate"

reg = lm(Y ~ UnemploymentRate, data=df)
summary(reg)

# dummy variable
dummyvar = lm(Y ~ UnemploymentRate + factor(year) + factor(CountryID), data=df)
summary(dummyvar)


formula_fix <- as.formula(paste("Y ~ factor(Region) +", paste(c("Intercept", Independent_Variables), collapse = " + "), "+ factor(CountryName) - 1"))

# using index = c("CountryName", "year") 
# and model = "within" in the plm function is a reasonable way to specify 
# a fixed effects panel regression model. This will estimate a fixed effect 
# for each country and allow you to control for unobserved time-invariant heterogeneity 
# across countries.

# Run a fixed effects regression with multiple independent variables using plm()
model_fix <- plm(formula_fix, data = df, index = c("CountryName"), model = "within")
summary(model_fix)


Independent_Variables <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
                           'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
                           'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
                           'InvestmentFreedom', 'FinancialFreedom', 'TariffRate', 
                           'IncomeTaxRate', 'CorporateTaxRate',  'UnemploymentRate', 'InflationRate')




# Perform Breusch-Godfrey test for serial correlation
pbgtest(model_fix)
# >> p-value of 0.09518, indicating that there is no evidence of serial correlation in the errors

# Perform Pesaran's test for cross-sectional dependence
pcdtest(model_fix)
# >> a very low p-value (< 2.2e-16), indicating that there is strong evidence of cross-sectional dependence in the data

# Perform Breusch-Pagan test for heteroskedasticity
bptest(model_fix)
# >> a very low p-value (1.251e-05), indicating that there is strong evidence of heteroskedasticity in the errors
# variance of the errors is not constant across observations

# Compute panel-corrected standard errors
coeftest(model_fix, vcov = function(x) vcovHC(x, method = "arellano", type = "HC1", cluster = "group"))

significant_va <- c("JudicialEffectiveness", "GovernmentIntegrity", "FiscalHealth", "BusinessFreedom", 
                    "LaborFreedom", "MonetaryFreedom", "TradeFreedom", "TariffRate", "InflationRate")

formula_coef <- as.formula(paste("Y ~", paste(c("Intercept", significant_va), collapse = " + "), "- 1"))

coef_model <- lm(formula_coef, data = df)
summary(coef_model)

# Estimate FGLS model
# model_fgls <- pvcm(formula, data = df, index = c("CountryName", "year"), effect = "individual", model = "within")
# summary(model_fgls)
# >> Error in FUN(X[[i]], ...) : insufficient number of observations

# Estimate GMM model
# library(pgmm)
# model_gmm <- pgmm(formula, data = df, index = c("CountryName", "year"), effect = "twoways", model = "onestep")
# summary(model_gmm)
# >> cannot run

######################## based on region ########################

F <- c('PropertyRights', 'JudicialEffectiveness', 'GovernmentIntegrity', 'TaxBurden', 
       'GovSpending', 'FiscalHealth', 'BusinessFreedom', 
       'LaborFreedom', 'MonetaryFreedom', 'TradeFreedom', 
       'InvestmentFreedom', 'FinancialFreedom', 'TariffRate', 'IncomeTaxRate', 'CorporateTaxRate',
       'TaxBurdenRateOfGDP', 'GovExpenditureRateOfGDP', 'UnemploymentRate', 'InflationRate')
df$Y <- Y

# define the model formula
formula <- as.formula(paste("Y ~", paste(Independent_Variables, collapse = " + ")))

# get the unique regions
regions <- unique(df$Region)

# loop over the regions
for (region in regions) {
  # subset the data for the current region
  region_data <- df[df$Region == region, ]
  
  # fit the linear model to the subsetted data
  model <- lm(formula, data = region_data)
  step_model <- step(model, direction = "both", trace = FALSE)
  
  # print the model coefficients
  cat("\nRegion:", region, "\n")
  print(summary(step_model))
}

