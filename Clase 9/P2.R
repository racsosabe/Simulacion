
getTvalue <- function(frec, p, n) {
	return(sum((frec - n * p) * (frec - n * p) / (n * p)))
}

getPvalue <- function(size, times, t){
	k <- 6
	p <- (1 / 6) * rep(1, k)
	T <- rep(0, times)
	Y <- matrix(rep(0, times * size), nrow = times, ncol = size, byrow = TRUE)
	N <- matrix(rep(0, times * k), nrow = times, ncol = k, byrow = TRUE)

	for(j in 1 : times){
		Y[j, ] <- floor(k * runif(size)) + 1
		for(x in Y[j, ]){
			N[j, x] <- N[j, x] + 1
		}
		T[j] <- getTvalue(N[j, ], p, size)
	}
	Tp <- (T > t)
	pvalue <- cumsum(Tp) / (1 : times)
	return(pvalue)
}

pdf("P2.pdf")

t <- getTvalue(c(158, 172, 164, 181, 160, 165), (1 / 6) * rep(1, 6), 1000)
ans <- getPvalue(1000, 20000, t)

plot(1 : length(ans), ans, type = "l")
print(1.0 - pchisq(t, df = 5))
print(ans[length(ans)])
