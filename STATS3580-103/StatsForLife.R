# This is me 8/22/19

ages <- c(20, 18, 19, 19, 21, 21)
sum(ages)/6
mean(ages)

yg <- c("SO", "SO", "FR", "JR", "JR", "JR")
class(yg)

loc <- c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)

DF <- data.frame(ages, loc, yg)
rm(ages, loc, yg)

#import dplyr package
library(dplyr)

# pipe filter
DF %>%
  filter(ages < 20)

#no pipe filter
filter(DF, ages < 20)


filter(DF, loc)
filter(DF, !loc)