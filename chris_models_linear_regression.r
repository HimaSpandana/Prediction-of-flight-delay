install.packages("Metrics")
library(Metrics)

library(dplyr)
dataset <- read.csv('./data/complete_dataset.csv')

cor(dataset$CRS_DEP_TIME, dataset$DEP_TIME)
cor(dataset$ARR_TIME, dataset$ARR_DELAY_NEW)
colnames(dataset)
model1 = lm(DEP_DELAY_NEW ~ . - AIRPORT_NAME - DEST_CITY_NAME - ORIGIN_AIRPORT_ID - ORIGIN_CITY_NAME - ORIGIN_STATE_ABR - DEST - DEST_CITY_NAME - DEST_STATE_ABR - AIRPORT_NAME - LATE_AIRCRAFT_DELAY - SECURITY_DELAY - NAS_DELAY - WEATHER_DELAY - CARRIER_DELAY - DISTANCE - ORIGIN - OP_CARRIER - MONTH - DAY_OF_WEEK - DAY_OF_MONTH, dataset)
summary(model1)
  pred <- predict(model1, newdata=dataset)

rmse(pred, dataset$DEP_DELAY_NEW)
