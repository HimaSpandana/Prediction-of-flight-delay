install.packages("dplyr")
library("dplyr")

library(ggplot2)
library(scales)

dataset <- read.csv('./data/debugging_dataset.csv')

delay_types <- dataset %>%
  group_by(MONTH) %>%
  summarise(
    WEATHER_DELAY = sum(WEATHER_DELAY, na.rm = T),
    CARRIER_DELAY = sum(CARRIER_DELAY, na.rm = T),
    NAS_DELAY = sum(NAS_DELAY, na.rm = T),
    SECURITY_DELAY = sum(SECURITY_DELAY, na.rm = T),
    LATE_AIRCRAFT_DELAY = sum(LATE_AIRCRAFT_DELAY, na.rm = T)
  )

df <- data.frame("DELAY_TYPE"=character(0), "DEP_DELAY_MINUTES"=character(0), "MONTH"= character(0))

df <- rbind(df, data.frame(DELAY_TYPE="WEATHER", DEP_DELAY_MINUTES=delay_types$WEATHER_DELAY, MONTH=delay_types$MONTH))
df <- rbind(df, data.frame(DELAY_TYPE="CARRIER", DEP_DELAY_MINUTES=delay_types$CARRIER_DELAY, MONTH=delay_types$MONTH))
df <- rbind(df, data.frame(DELAY_TYPE="NAS", DEP_DELAY_MINUTES=delay_types$NAS_DELAY, MONTH=delay_types$MONTH))
df <- rbind(df, data.frame(DELAY_TYPE="SECURITY", DEP_DELAY_MINUTES=delay_types$SECURITY_DELAY, MONTH=delay_types$MONTH))
df <- rbind(df, data.frame(DELAY_TYPE="LATE_AIRCRAFT", DEP_DELAY_MINUTES=delay_types$LATE_AIRCRAFT_DELAY, MONTH=delay_types$MONTH))

ggplot(data=df, aes(x=factor(MONTH), y=DEP_DELAY_MINUTES, group=DELAY_TYPE)) +
  geom_line(aes(color=DELAY_TYPE))+
  geom_point(aes(color=DELAY_TYPE)) +
  scale_y_continuous(name="Total Delay Minutes", labels = comma) +
  labs(title="2019 Causes of Flight Delay", x="Month") +
  theme_dark()
