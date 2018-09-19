library(dplyr)

cell <- data_frame(name = "test", age = 1, x = 0, y = 0)
trail[[cell$age]] <- c(cell$x, cell$y)
##while(cell$age < 10)
{
  cell$age <- sum(cell$age, 1)
  cell$x <- sum(cell$x, runif(1.0, -1.0, 1.0))
  cell$y <- sum(cell$y, runif(1.0, -1.0, 1.0))
  trail[[cell$age]] <- c(cell$x, cell$y)
}
ggplot(data = trail)
cell$age
