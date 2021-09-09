install.packages("GGally", repos = "http://cran.us.r-project.org")

library(purrr)
library(tidyr)
library(ggplot2)
library(GGally)
library(dplyr)

#import data, change and then save data
dataset <- read.csv('./data/debugging_dataset.csv')

class(dataset)
dim(dataset)
str(dataset)
names(dataset)

###general summary/big 5 
summary(dataset)
mean(dataset$DEP_DELAY)
median(dataset$DEP_DELAY)

#####Do traveling day/month have something to do with delay?
#most common delay day
dataset %>%
  group_by(DAY_OF_WEEK) %>%
  summarise(mean_arr_delay = mean(ARR_DELAY, na.rm = T),
            mean_dep_delay = mean(DEP_DELAY, na.rm=T))

##plot to view 
ggplot(dataset, aes(x=factor(DAY_OF_WEEK), y=DEP_DELAY)) + geom_boxplot()
ggplot(dataset, aes(x=factor(MONTH), y=DEP_DELAY)) + geom_boxplot()

##plots/graphs to find the distribution of dept/arrival delay:
ggplot(dataset, mapping=aes(x=DEP_DELAY)) + geom_histogram(bins = 150)
ggplot(dataset, mapping=aes(x=ARR_DELAY)) + geom_histogram(bins = 150)
##conclusion: dept delay has more extreme values 

#quick historgram of all variables that is numeric
dataset %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(binwidth = 100)

#finding the correlations among the varaibles
##first do a correlation matrix
ggcorr(dataset)

#then calculate the cor value of each pair that seems to be highly correlated

cor(dataset$DAY_OF_WEEK, dataset$DEP_DELAY)
cor(dataset$MONTH, dataset$WEATHER_DELAY)
cor(dataset$DEP_DELAY, dataset$ARR_DELAY)
cor(dataset$DISTANCE, dataset$DEP_DELAY)
cor(dataset$ARR_DELAY, dataset$DISTANCE)
cor(dataset$DEP_DELAY, dataset$SECURITY_DELAY)
cor(dataset$DEP_DELAY, dataset$NAS_DELAY)
##conclusion - the highest correlation is bt dept and arr delay, and there is a slight correlation
##between deptdelay and distance, but not so much between arrdelay and distance
##indication -> make up lost time on air?

##strong cor between late aircraft delay and arr/dept delat (around .68 and .66)
cor(dataset$DEP_DELAY, dataset$LATE_AIRCRAFT_DELAY)
cor(dataset$ARR_DELAY, dataset$LATE_AIRCRAFT_DELAY)

##slight cor bt weather delay (both around .258)
cor(dataset$DEP_DELAY, dataset$WEATHER_DELAY)
cor(dataset$ARR_DELAY, dataset$WEATHER_DELAY)

##strong cor bt carrier delay (.66 and .64)
cor(dataset$DEP_DELAY, dataset$CARRIER_DELAY)
cor(dataset$ARR_DELAY, dataset$CARRIER_DELAY)

##what can these correlations tell us about the nature of air travel/traffic



