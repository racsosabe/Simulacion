install.packages("EstimationTools")
library("EstimationTools")

x <- rnorm(50, mean = 10, sd = 8)

fit1 <- maxlogL(x, dist = 'dnorm', start = c(10, 8))
