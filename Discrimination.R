library(readxl)
library(dplyr)
library(ggplot2)
DF <- read_excel("../Data/TMP.xlsx")
head(DF)

DF$Age_Cohort <- gsub(42898, "6-12", DF$Age_Cohort)
DF$Age_Cohort <- gsub("0 - 5", "0-5", DF$Age_Cohort)
DF$Age_Cohort <- factor(DF$Age_Cohort, levels = c("0-5","6-12","13-17","18-21","22-50","51 +"))
table(DF$Age_Cohort)

DT::datatable(DF)

DF %>% 
  group_by(Gender) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n(), SD = sd(Expenditures))


