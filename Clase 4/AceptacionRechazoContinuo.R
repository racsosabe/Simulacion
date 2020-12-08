
# Alpha = 3
# Beta = 1
# f(x) = (3 + 1 - 1)! * x^(3 - 1) * (1 - x)^(1 - 1) / (3 - 1) ! / (1 - 1)!
# f(x) = 3 * x^2
# c = 3

f <- function(x){
	return(3 * x ^ 2)
}

g <- function(x){
	return(1)
}

getVariable <- function(c, f, g, times){
	ans <- NULL
	tries <- NULL
	for(i in 1 : times){
		Y <- runif(1)
		u <- runif(1)
		steps <- 1
		while(u * c * g(Y) > f(Y)){
			Y <- runif(1)
			u <- runif(1)
			steps <- steps + 1
		}
		ans <- c(ans, Y)
		tries <- c(tries, steps)
	}
	return(c(ans,tries))
}

pdf("AceptacionRechazoContinuo.pdf")

n <- 10000

results <- getVariable(3, f, g, n)
xx <- results[1:n]
xx2 <- results[(n+1):(2*n)]

hist(xx, prob = TRUE) # Usando frecuencia / total
lines(density(xx), col = "blue", lwd = 3)

plot(cumsum(xx2) / 1 : n, type = "l")
abline(h = 3)


