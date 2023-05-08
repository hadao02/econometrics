library(stats)
library(dplyr)

data <- read.csv("D:/Personal project/IMDB/econometrics/economic_freedom_index2019_data.csv")

colnames(data)

numeric_data <- select_if(data, is.numeric)
heatmap(is.na(numeric_data), cbar = FALSE)

"D:\Personal project\IMDB\econometrics\economic_freedom_index2019_data.csv"
# plot(Dat$TV, Dat$sales,pch=4,cex=2)

