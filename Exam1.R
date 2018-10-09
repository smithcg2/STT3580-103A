library(ggplot2)
library(dplyr)
set.seed(113)
X <- rnorm(1000, 10, 3) + runif(1000, 7, 13)
DF <- data.frame(x = X)

DF %>%
  ggplot(aes(x = x)) +
  geom_density()

DF %>%
  summary()

---

library(dplyr)
library(resampledata)
TBL <- Verizon %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(Time), n())
TBL
obs_diff <- TBL$Mean[1] - TBL$Mean[2]
obs_diff
Times <- Verizon$Time
set.seed(413)
sims <- 10^4 - 1
ans <- numeric(sims)
for(i in 1:sims){
  index <- sample(1664 + 23, 23)
  ans[i] <- mean(Times[index]) - mean(Times[index])
}
pvalue <- (sum(ans >= obs_diff) + 1)/(sims + 1)
pvalue