install.packages("dplyr")
library("dplyr")

library(ggplot2)
library(scales)

dataset <- read.csv('./data/complete_dataset.csv')

delay_times <- dataset %>%
  group_by(AIRLINE_NAME) %>%
  summarise(AVG_DELAY_TIME = mean(DEP_DELAY_NEW)) %>%
  arrange(desc(AVG_DELAY_TIME))

ggplot(delay_times, aes(AIRLINE_NAME, AVG_DELAY_TIME)) +
  geom_col(fill="steelblue") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="2019 Delay times of 10 largest airlines", x="Airline", y="Average Delay Minutes")