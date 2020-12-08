install.packages("EstimationTools")
library("EstimationTools")

getEstimator <- function(values){
	result <- maxlogL(values, dist = 'dbinom', start = c(size = 10, prob = 0.8))
	#summary(result)
	return(result$fit$par)
}

getTvalue <- function(frec, p, n) {
	return(sum((frec - n * p) * (frec - n * p) / (n * p)))
}

generateValues <- function(times, p){ # Transformada inversa + Biseccion para generar "times" valores en O(times * log(k))
	ans <- NULL
	F <- cumsum(p)
	for(i in 1 : times){
		U <- runif(1)
		lo <- 1
		hi <- length(F)
		while(lo < hi){
			mi <- lo + (hi - lo) %/% 2
			if(F[mi] < U){
				lo <- mi + 1
			} else {
				hi <- mi
			}
		}
		ans <- c(ans, lo - 1)
	}
	return(ans)
}

getPvalue <- function(size, times, t, p){
	k <- length(p)
	T <- rep(0, times)
	Y <- matrix(rep(0, times * size), nrow = times, ncol = size, byrow = TRUE)
	N <- matrix(rep(0, times * k), nrow = times, ncol = k, byrow = TRUE)
	for(j in 1 : times){
		Y[j, ] <- generateValues(size, p) # Genero en base a la probabilidad p
		for(a in Y[j, ]){
			N[j, a + 1] <- N[j, a + 1] + 1
		}
		estimators <- getEstimator(Y[j, ])
		s <- round(estimators[1])
		prob <- estimators[2]
		newp <- dbinom(0 : s, size = s, prob = prob)
		carry <- (N[j, ] - size * newp)
		T[j] <- sum(carry * carry / (size * newp))
	}
	Tp <- (T >= t)
	pvalue <- cumsum(Tp) / (1 : times)
	return(pvalue)
}

pdf("9abondad.pdf") 

# Binomial n = 10, p = ?

n <- 50

x <- c(rep(1, 12), rep(2, 5), rep(3, 19), rep(4, 7), rep(5, 7))

estimators <- getEstimator(x)
size <- round(estimators[1])
prob <- estimators[2]

p <- dbinom(0 : size, size = size, prob = prob)

frec <- rep(0, size + 1)

for(a in x){
	frec[a + 1] <- frec[a + 1] + 1
}

t <- getTvalue(frec, p, n)
print(t)
ans <- getPvalue(n, 10000, t, p)

plot(1 : length(ans), ans, type = "l")
print(ans[length(ans)])
