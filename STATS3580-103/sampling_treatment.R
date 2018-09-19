library(dplyr)

## K11777 test dataframe and plot
group <- c(rep("Treatment", 5), rep("Control", 5))
worms <- c(1, 2, 2, 10, 7, 7, 17, 16, 10, 10)
schis <- data_frame(group, worms)
rm(group, worms)


ggplot(data = schis, aes(x = group, y = worms, color = group)) +
  geom_point(position = "jitter") + 
  theme_bw() +
  labs(x = "", y = "Number of schistosomes", title = "K11777 results", subtitle = "Some reference goes here") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 1))




## run test in while loop n times calculate diff of averages.
values <- c(17, 16, 10, 10, 10, 7, 7, 2, 2, 1)
results <- c()
i <- 1
sims <- 20000
##set.seed(sample(1:100, 1))
set.seed(3)
while(i<sims)
{
  rand <- sample(values)
  control <- rand[1:5]
  treatment <- rand[6:10]
  results[i] <- (mean(control) - mean(treatment))
  i <- sum(i, 1)
}
##results[results > 7.0]
pvalue <- (sum(results >= 7.6) + 1) / sims
pvalue

