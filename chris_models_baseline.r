install.packages("Metrics")
library(Metrics)

dataset <- read.csv('./data/complete_dataset.csv')
summary(dataset)
colnames(dataset)

dataset$mean = mean(dataset$DEP_DELAY_NEW)

rmse(dataset$DEP_DELAY_NEW, dataset$mean)