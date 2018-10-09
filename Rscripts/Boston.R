library(MASS)
library(dplyr)
library(ggplot2)
#?Boston

##With ggplot2
Boston %>%
  ggplot(aes(x=rm, y=medv)) +
  geom_point(color = "darkgreen") +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)

##without ggplot2 (base r)
plot(Boston$rm, Boston$medv)
abline(lm(medv ~ rm, Boston))

##correlation coefficent
cor(Boston$rm, Boston$medv)

##linear model, line of best fit
mod <- lm(medv ~ rm, data = Boston)

##details of the linear model
summary(mod)

##
1 - pnorm(115, 100, 15)

##
pnorm(120, 100, 15) - pnorm(90, 100, 15)

##t-distribution area under curve
pt(-13.08, 504) * 2

(1 - pt(21.72, 504)) * 2

##H\/0: Beta\/0 = 0
##H\/a: Beta\/0 != 0

##linear model, line of best fit
mod2 <- lm(medv ~ rm + lstat, data = Boston)

##details of the linear model
summary(mod2)

y <- Boston$medv
X <- model.matrix(mod2)
head(X)
solve(t(X) %*% X) %*% t(X) %*% y

