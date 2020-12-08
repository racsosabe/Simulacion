
getPvalue <- function(size, times){
	k <- 5
	p <- 0.2 * rep(1, k)
	t <- 12.8
	T <- rep(0, times)
	Y <- matrix(rep(0, times * size), nrow = times, ncol = size, byrow = TRUE)
	N <- matrix(rep(0, times * k), nrow = times, ncol = k, byrow = TRUE)

	for(j in 1 : times){
		Y[j, ] <- floor(k * runif(size)) + 1
		for(x in Y[j, ]){
			N[j, x] <- N[j, x] + 1
		}
		carry <- (N[j, ] - size * p)
		T[j] <- sum(carry * carry / (size * p))
	}
	Tp <- (T > t)
	pvalue <- cumsum(Tp) / (1 : times)
	return(pvalue)
}

pdf("Estadistico.pdf")

ans <- getPvalue(50, 20000)
print(1.0 - pchisq(12.8, df = 4))
plot(1 : length(ans), ans, type = "l")
print(ans[length(ans)])
