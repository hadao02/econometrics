library(stats)
library(dplyr)
library(ggplot2)
library(tidyr)

# read data
df <- read.csv("D:/Personal project/IMDB/econometrics/World Happiness Report.csv")

# check columns' names
colnames(df)


colnames(df)[7] <- "score_2019"
print(str(df))

Y <- sqrt(df[["GDPperCapitaPPP"]])
names(Y) <- "sqrt of GDP per Capita"

fit <- lm(Y ~ WorldRank + score_2019 + PropertyRights, data = data)

# Print the summary of the model
summary(fit)