
getApproximation <- function(k = 100, N = 10000){
	values <- floor(N * runif(k)) + 1
	a <- N * exp(values / N)
	return(a)
}

pdf("Problema2.pdf")

k = 100

xx <- getApproximation()
toplot <- cumsum(xx) / k
plot(1 : k, toplot, type = "l")
print(toplot[k])
