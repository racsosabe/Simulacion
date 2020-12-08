
getValues <- function(lambda = 1, times = 1000){
	values <- runif(times)
	return(-log(values) / lambda)
}

pdf("InversaContinua.pdf")

lamb = 3

values <- getValues(lamb)

hist(values)
