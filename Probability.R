ages <- c(21, 20, 19, 22, 21, 19, 20, 19, 22 ,26, 19, 15)

hist(ages)
mean(ages)
sd(ages)
boxplot(ages)

mean(ages) + c(-1, 1)*qt(.975, 11)*sd(ages)/sqrt(12)


##Birthday Probability Problem
students <- 61
days <- 365
m <- 1:students
p <- numeric(students)
for(i in m) {
  q = prod(days:(days - i + 1)/days)
  p[i] = 1 - q
}
plot(p)

## Probability Problem
possibilities <- 100
samples <- 365
m <- 1:possibilities
p <- numeric(possibilities)
for(i in m) {
  q = prod(samples:(samples - i + 1)/samples)
  p[i] = 1 - q
}
plot(p)


##Children Probability Problem
x <- c(1, 2, 3)
Px <- c(.53, ((.47)*(.53)), ((.47)^3+(.47)^2*(.53)))
EX <- sum(x*Px)
EX

x1 <- c(0:3)
Px1 <- c(.53, (.47*.53), ((.47)^2*(.53)), ((.47)^3))
EX1 <- sum(x1*Px1)
EX1

x3 <- c(0,13,26,41)
Px3 <- c(.5, 13/52, 12/52,1/52)
EX3 <- sum(x3*Px3)
EX3
VX3 <- sum((x3-EX3)^2*EX3)
SX3 <- sqrt(VX3)
SX3

#Insurance Problem
x4 <- c(110, -1890, -8890)
Px4 <- c((1-(1/2083)-(1/495)), (1/495), (1/2083))
EX4 <- sum(x4*Px4)
EX4
VX4 <- sum((x4-EX4)^2*Px4)
SX4 <- sqrt(VX4)
SX4

prob <- function(x, Px){
  EX <- sum(x*Px)
  VX <- sum((x-EX)^2*Px)
  SX <- sqrt(VX)
  c(EX, SX)
}

prob(c(110, -1890, -8890), c((1-(1/2083)-(1/495)), (1/495), (1/2083)))

1-pnorm(2000,1920, sqrt(6)*22)

pnorm(290, 265, 15) - pnorm(275, 266, 15)

pnorm(267, 265, 15/sqrt(90))

# Potato Chips
# W ~ N(2.41, 0.07)
pnorm(24, 24.1, 0.07)

(1-pnorm(24, 24.1, 0.07))^3

pnorm(24, 24.1, 0.07/sqrt(3))
pnorm(24, 24.1, 0.07/sqrt(24))

# IQ Problem
1-pnorm(140, 150, 8)

1 - pnorm(10, 20, sqrt(8^2 + 10^2))

1 - pnorm(140, 130, 10/sqrt(3))

