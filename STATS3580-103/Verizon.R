library(ggplot2)
url <- "https://raw.githubusercontent.com/alanarnholt/STT3850/gh-pages/DataCSV/Verizon.csv"
Verizon <- read.csv(url)
str(Verizon)
ILEC <- filter(Verizon, Verizon$Group == "ILEC")
CLEC <- filter(Verizon, Verizon$Group == "CLEC")
hist(ILEC$Time)
hist(CLEC$Time)
ggplot(data = Verizon, aes(x = Time,
                           y = ..density..)) +
  geom_histogram(color = "black", fill = "navy") +
  facet_grid(.~Group) +
  theme_bw()

ggplot(data = Verizon, aes(x = Time,
                           geom_density(fill = "green", bw = 5))) +
  geom_histogram(color = "black", fill = "navy") +
  facet_grid(.~Group) +
  theme_bw()

Verizon %>%
  group_by(Group) %>%
  summarize(m = n(), mean = mean(Time), SD = sd(Time))

Times <- Verizon$Time
sims <- 10^4 - 1
ans <- numeric(sims)
for (i in 1:sims) {
  index <- sample(1687, 23, FALSE)
  ans[i] <- mean(Times[index]) - mean(Times[-index])
}

OBS <- tapply(Verizon$Time, Verizon$Group, mean)
OBS
OBS_DIFF <- OBS[1] - OBS[2]
OBS_DIFF

pval <- (sum(ans >= OBS_DIFF) + 1)/(sims + 1)
pval


median_ans <- numeric(sims)
for (i in 1:sims) {
  index <- sample(1687, 23, FALSE)
  median_ans[i] <- median(Times[index]) - median(Times[-index])
}

OBS <- tapply(Verizon$Time, Verizon$Group, median)
OBS
OBS_DIFF <- OBS[1] - OBS[2]
OBS_DIFF

pval <- (sum(median_ans >= OBS_DIFF) + 1)/(sims + 1)
pval

trimmed_ans <- numeric(sims)
trimamnt <- 0.3
for (i in 1:sims) {
  index <- sample(1687, 23, FALSE)
  trimmed_ans[i] <- mean(Times[index], trim=trimamnt) - mean(Times[-index], trim=trimamnt)
}

OBS <- tapply(Verizon$Time, Verizon$Group, mean, trim = trimamnt)
OBS
OBS_DIFF <- OBS[1] - OBS[2]
OBS_DIFF

pval <- (sum(trimmed_ans >= OBS_DIFF) + 1)/(sims + 1)
pval
