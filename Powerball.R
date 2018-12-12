library(dplyr)
library(ggplot2)
library(DT)
library(moderndive)

drawings <- read.csv("NCELPowerball.csv",  header = TRUE)
max(drawings$Number.1, na.rm = TRUE)
max(drawings$Number.2, na.rm = TRUE)
max(drawings$Number.3, na.rm = TRUE)
max(drawings$Number.4, na.rm = TRUE)
max(drawings$Number.5, na.rm = TRUE)


min(drawings$Number.1, na.rm = TRUE)
min(drawings$Number.2, na.rm = TRUE)
min(drawings$Number.3, na.rm = TRUE)
min(drawings$Number.4, na.rm = TRUE)
min(drawings$Number.5, na.rm = TRUE)

hist(drawings$Number.1)
abline(v = mean(drawings$Number.1),
       col = "royalblue")
mean(drawings$Number.1)
median(drawings$Number.1)

hist(drawings$Number.2)
abline(v = mean(drawings$Number.2),
       col = "royalblue")
mean(drawings$Number.2)
median(drawings$Number.2)

hist(drawings$Number.3)
abline(v = mean(drawings$Number.3),
       col = "royalblue")
mean(drawings$Number.3)
median(drawings$Number.3)

hist(drawings$Number.4)
abline(v = mean(drawings$Number.4),
       col = "royalblue")
mean(drawings$Number.4)
median(drawings$Number.4)

hist(drawings$Number.5)
abline(v = mean(drawings$Number.5),
       col = "royalblue")
mean(drawings$Number.5)
median(drawings$Number.5)

hist(drawings$Powerball)
abline(v = mean(drawings$Powerball),
       col = "royalblue")
mean(drawings$Powerball)
median(drawings$Powerball)

model <- lm(Powerball ~ Number.1 + Number.2 + Number.3 + Number.4 + Number.5, data = drawings)
get_regression_table(model)
