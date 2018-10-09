library(ggplot2)
library(dplyr)
library(PASWR2)

TITANIC3 %>%
  ggplot(aes(x=pclass,y=prop.table(survived)) +
  geom_col() +
  facet_grid(.~sex)