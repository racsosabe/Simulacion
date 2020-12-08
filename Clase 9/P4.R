
getTvalue <- function(frec, p, n) {
	X <- frec - n * p
	return(sum((X * X) / (n * p)))
}

getProbs <- function(a, b, i){
	h <- (b - a) / i
	prob <- rep(1 / i, i)
	return(prob)
}

getFrecs <- function(X, a, b, i){
	h <- (b - a) / i
	frec <- rep(0, i)
	for(x in X){
		id <- floor((x - a) / h) + 1
		frec[id] <- frec[id] +  1
	}
	return(frec)
}

getPvalue <- function(size, times, t, p, k){
	T <- rep(0, times)
	Y <- matrix(rep(0, times * size), nrow = times, ncol = size, byrow = TRUE)
	N <- matrix(rep(0, times * k), nrow = times, ncol = k, byrow = TRUE)

	for(j in 1 : times){
		Y[j, ] <- floor(k * runif(size)) + 1
		for(x in Y[j, ]){
			N[j, x] <- N[j, x] + 1
		}
		carry <- (N[j, ] - size * p)
		T[j] <- sum((carry * carry) / (size * p))
	}
	Tp <- (T > t)
	pvalue <- cumsum(Tp) / (1 : times)
	return(pvalue)
}

pdf("P4.pdf")

X <- sort(c(164, 142, 110, 153, 103, 52, 174, 88, 178, 184, 58, 62, 132, 128))
k <- 5 # Particionamos en 5 intervalos el rango (50, 200)
p <- getProbs(50, 200, k)
frec <- getFrecs(X, 50, 200, k)
n <- 14
t <- getTvalue(frec, p, n)
ans <- getPvalue(n, 9000, t, p, k)

plot(1 : length(ans), ans, type = "l")
print(1.0 - pchisq(t, df = k - 1)) # Valor estimado correcto
print(ans[length(ans)]) # Valor estimado simulado
