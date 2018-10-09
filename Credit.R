library(ISLR)
library(dplyr)
library(ggplot2)

str(Credit)

Credit %>%
  group_by(Gender) %>%
  summarize(mean(Balance))

plot(Credit$Gender, Credit$Balance)

Credit %>%
  ggplot(aes(x = Gender, y = Balance)) +
  geom_boxplot()

mod <- lm(Balance ~ Gender, Credit)
mod
summary(mod)

X <- model.matrix(mod)
head(X)

contrasts(Credit$Gender)

##Same thing as lm()
y <- Credit$Balance
betahat <- solve(t(X) %*% X)%*%t(X)%*%y
betahat

Credit %>%
  ggplot(aes(x = Rating, y = Balance, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

Credit %>%
  ggplot(aes(x = Rating, y = Balance, color = Gender)) +
  facet_grid(~Gender) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)







mod2 <- lm(Balance ~ Age + Income + Rating + Limit, Credit)
mod2
summary(mod2)

Credit %>%
  group_by(Age) %>%
  summarize(mean(Balance))

plot(Credit$Age, Credit$Balance)

Credit %>%
  ggplot(aes(x = Age, y = Balance)) +
  geom_boxplot()

X1 <- model.matrix(mod)
head(X1)

##Same thing as lm()
y <- Credit$Balance
betahat <- solve(t(X1) %*% X1)%*%t(X1)%*%y
betahat

par(mfrow=c(2,2))
plot(mod2)
par(mfrow = c(1,1))