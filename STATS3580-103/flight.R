library(nycflights13)
library(dplyr)
library(ggplot2)

delayedflights <- filter(flights, dep_delay > 100)
delayedflights <- na.omit(delayedflights)
validflights <- na.omit(flights)
percent <- (nrow(delayedflights)/nrow(validflights))*100

mean(na.omit(flights$dep_delay > 100))*100


ggplot(data = validflights, aes(x=dep_delay)) +
  geom_density(fill = "green") + 
  theme_bw() +
  xlim(-25, 200) +
  labs(x = "Departure delay in minutes")
