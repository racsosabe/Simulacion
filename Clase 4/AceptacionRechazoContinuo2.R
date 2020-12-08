
# k = 2.5
# lambda1 = 2
# lambda2 = lambda1 / k = 4 / 5
# Generalización para k > 1:
# xmax = (k - 1) / (lambda1 - lambda1 / k)
# c = f(1.25) / g(1.25)

f <- function(x, k, lambda){ # Gamma density function
	return(lambda * exp(-lambda * x) * (lambda * x)^(k - 1) / gamma(k))
}

g <- function(x, lambda){ # Exponential density function
	return(lambda * exp(-lambda * x))
}

getExponential <- function(lambda, times = 1){
	return(-log(runif(times)) / lambda)
}

getVariable <- function(c, f, g, k, lambda1, lambda2, times){
	# k, lamdba1 <- Gamma
	# lambda2 <- Exponencial
	ans <- NULL
	tries <- NULL
	for(i in 1 : times){
		Y <- getExponential(lambda2)
		u <- runif(1)
		steps <- 1
		while(u * c * g(Y, lambda2) > f(Y, k, lambda1)){
			Y <- getExponential(lambda2)
			u <- runif(1)
			steps <- steps + 1
		}
		ans <- c(ans, Y)
		tries <- c(tries, steps)
	}
	return(c(ans,tries))
}

pdf("AceptacionRechazoContinuo2.pdf")

n <- 10000

c = f(1.25, 2.5, 2) / g(1.25, 0.8)

results <- getVariable(c, f, g, 2.5, 2, 0.8, n)
xx <- results[1:n]
xx2 <- results[(n+1):(2*n)]

print(c)

hist(xx, prob = TRUE) # Usando frecuencia / total
lines(density(xx), col = "blue", lwd = 3)

plot(cumsum(xx2) / 1 : n, type = "l")
abline(h = c)


