
library(infer)
library(resampledata)
library(tidyverse)
library(MASS)
#fullterm data is TXBirths where the age is 25-29 gestation is greater than 38 but less or equal to than 42 and no multiples.
fullterm <- TXBirths2004 %>% 
  filter(MothersAge == "25-29", Gestation >= 38, 
         Gestation <= 42, Multiple == "No")

## ------------------------------------------------------------------------

B <- 10^4
bt <- numeric(B)
for (i in 1:B) {
  bss <- sample(fullterm$Weight, size = sum(!is.na(fullterm$Weight)), replace = TRUE)
  bt[i] <- ((mean(bss)-mean(fullterm$Weight))/(sd(bss)/sqrt(sum(!is.na(fullterm$Weight)))))
}

Q <- quantile(bt, probs = c(0.05, 0.95))
Q

BTCI <- c(mean(fullterm$Weight) - Q[2] * sd(fullterm$Weight)/sqrt(length(fullterm$Weight)),mean(fullterm$Weight) - Q[1] * sd(fullterm$Weight)/sqrt(length(fullterm$Weight)))


smokers <- fullterm %>%
  filter(Smoker == "Yes")

nonsmokers <- fullterm %>%
  filter(Smoker == "No")


B <- 10^4
bt1 <- numeric(B)
for (i in 1:B) {
  bss1 <- sample(nonsmokers$Weight, size = sum(!is.na(nonsmokers$Weight)), replace = TRUE)
  bss2 <- sample(smokers$Weight, size = sum(!is.na(smokers$Weight)), replace = TRUE)
  bt1[i] <- (((mean(bss1) - mean(bss2))-(mean(smokers$Weight)) - mean(nonsmokers$Weight))/sqrt(((sd(bss1)^2)/length(!is.na(smokers$Weight)))+((sd(bss2)^2)/length(!is.na(nonsmokers$Weight)))))
}

perc <- .93 #must be between 1 and 0, 1 = 100%, .9 = 90% ...
l <- (1-(perc))/2
u <- 1-(1-(perc))/2
Q <- quantile(bt1, probs = c(l, u))
Q

t <- qt(u, 22.821)
t

t.test(Weight~Smoker, data = fullterm, conf = perc)


BTCI <- c(mean(fullterm$Weight) - Q[2] * sd(fullterm$Weight)/sqrt(length(fullterm$Weight)),mean(fullterm$Weight) - Q[1] * sd(fullterm$Weight)/sqrt(length(fullterm$Weight)))
BTCI


ft <- fullterm %>%
  group_by(Smoker) %>%
  summarise(Mean = mean(Weight), SD = sd(Weight), n = n())
ft

weightSM <- subset(fullterm, select = Weight, Smoker == "Yes", drop = TRUE)
weightSM

weightNSM <- fullterm %>%
  filter(Smoker == "No")
weightNSM <- weightNSM$Weight
weightNSM

B1 <- 10^4
bt2 <- numeric(B1)

for(i in 1:B1) {
  bss3 <- sample(weightNSM,size = length(weightNSM), replace = TRUE)
  bss4 <- sample(weightSM,size = length(weightSM), replace = TRUE)
  bt2[i] <- (((mean(bss3) - mean(bss4))-(mean(weightSM)) - mean(weightNSM))/sqrt(((sd(bss3)^2)/length(!is.na(weightSM)))+((sd(bss4)^2)/length(!is.na(weightNSM)))))
}


wnsm <- fullterm %>%
  filter(Smoker == "No") %>%
  dplyr::select(Weight) %>%
  pull()


wsm <- fullterm %>%
  filter(Smoker == "Yes") %>%
  dplyr::select(Weight) %>%
  pull()

xbarwnsm <- mean(wnsm)
xbarwsm <- mean(wsm)
theta <- xbarwnsm - xbarwsm

B <- 10^4
bt3 <- numeric(B)
for (i in 1:B){
  bss5 <- sample(wnsm, size = length(wnsm), replace = TRUE)
  bss6 <- sample(wsm, size = length(wsm), replace = TRUE)
  bt3[i] <- ((mean(bss5) - mean(bss6)) - theta)/(sqrt(var(bss5)/length(bss5)+var(bss6)/length(bss6)))
}

Q <- quantile(bt3, probs = c(0.025, .975))

CI <- c(theta - Q[2]*(sqrt(var(wnsm)/length(wnsm)+var(wsm)/length(wsm))),theta - Q[1]*(sqrt(var(wnsm)/length(wnsm)+var(wsm)/length(wsm))))
CI

CI <- theta - Q[2:1]*(sqrt(var(wnsm)/length(wnsm)+var(wsm)/length(wsm)))
CI

#permutation test and pvalue with for loop = test hypothesis that mean weight for non smokers - mean weight for smokers = 0 vs alt that they are not equal to 0