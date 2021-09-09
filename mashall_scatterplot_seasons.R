library(tidyverse)


data <- read.csv("./data/debugging_dataset.csv")

str(data)
summary(data)


# number of flights per carrier
data %>% 
  group_by(OP_CARRIER) %>% 
  count()

# statistics per carrier
data %>% 
  group_by(OP_CARRIER) %>%
  summarise(mean_arr_delay = mean(ARR_DELAY, na.rm=T),
            median_dist = median(DISTANCE, na.rm=T), 
            n_flights = n())

# statistics on the whole set
data %>% 
  summarise(mean_arr_delay = mean(ARR_DELAY, na.rm=T),
            median_dist = median(DISTANCE, na.rm=T), 
            n_flights = n())

# Average departure delay and the maximum departure delay
# for each origin airport
data %>%
  group_by(ORIGIN) %>%
  summarise(mean(DEP_DELAY_NEW, na.rm=T), max(DEP_DELAY_NEW, na.rm=T))

# statistics per MONTH
data %>% 
  group_by(MONTH) %>%
  summarise(median_dist = median(DISTANCE, na.rm=T), 
            mean_dep_delay = mean(DEP_DELAY_NEW, na.rm=T),
            mean_dep_delay15 = mean(DEP_DEL15, na.rm=T),
            mean_dep_delaynew = mean(DEP_DELAY_NEW, na.rm=T),
            n_flights = n())

# statistics per day of month
data %>% 
  group_by(DAY_OF_MONTH) %>%
  summarise(median_dist = median(DISTANCE, na.rm=T), 
            mean_dep_delay = mean(DEP_DELAY_NEW, na.rm=T),
            mean_dep_delay15 = mean(DEP_DEL15, na.rm=T),
            mean_dep_delaynew = mean(DEP_DELAY_NEW, na.rm=T),
            n_flights = n())

# statistics per day of week
data %>% 
  group_by(DAY_OF_WEEK) %>%
  summarise(median_dist = median(DISTANCE, na.rm=T), 
            mean_dep_delay = mean(DEP_DELAY_NEW, na.rm=T),
            mean_dep_delay15 = mean(DEP_DEL15, na.rm=T),
            mean_dep_delaynew = mean(DEP_DELAY_NEW, na.rm=T),
            n_flights = n())


## ggplot2
# histogram

mean(data$DEP_DELAY, na.rm=T)

ggplot(data = data, mapping=aes(x=DEP_DELAY_NEW)) +
  geom_histogram(bins = 50, color='red', fill='royalblue') + theme_bw()

# Boxplot
ggplot(data, aes(x=factor(MONTH), y=DEP_DELAY_NEW)) + geom_boxplot()


# For every day of every month, determine the average delay for each day of the week
data %>% 
  sample_n(5000) %>%
  ggplot(aes(x=DISTANCE, y=DEP_DELAY_NEW, color=DAY_OF_WEEK)) + geom_point() +
  facet_grid(DAY_OF_WEEK~MONTH)


# Scatter plot
data %>% 
  sample_n(5000) %>%
  ggplot(aes(x=DISTANCE, y=DEP_DELAY_NEW, color=DAY_OF_MONTH)) + geom_point() +
  facet_grid(DAY_OF_MONTH~MONTH)

mutate(data, season= ifelse(MONTH %in% 3:5, "SPRING", 
                            ifelse (MONTH %in% 6:8, "SUMMER",
                                    ifelse (MONTH %in% 9:12, "FALL",
                                            ifelse( MONTH %in% 1:3, "WINTER")))))
       
head(data)

# plot correlation matrix 
# cormat <- cor(data)
