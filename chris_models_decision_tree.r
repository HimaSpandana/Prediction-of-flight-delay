install.packages("Metrics")
install.packages("caTools")
library(Metrics)
library(rpart)
library(rattle)
library(caret)
library(caTools)
library(dplyr)

dataset <- read.csv('./data/debugging_dataset.csv')
dataset <- within(dataset, rm(DEP_DELAY, DEP_DEL15, NAS_DELAY))
split = sample.split(dataset$DEP_DELAY_NEW, SplitRatio = 0.8)

dataset <- subset(dataset, split == TRUE)
new_data <- subset(dataset, split == FALSE)


lm1 <- rpart(DEP_DELAY_NEW ~ AIRLINE_NAME + AIRPORT_SIZE + DAY_NAME + CRS_DEP_TIME + AIRPORT_SIZE + ARR_DELAY_NEW, data=dataset, method = 'anova')
fancyRpartPlot(lm1)

printcp(lm1)
plotcp(lm1)
summary(lm1)

new_data %>% filter(is.na(DEP_DELAY_NEW))

predictions <- predict(lm1, newdata = new_data)

mean((predictions - new_data$DEP_DELAY_NEW)^2)
