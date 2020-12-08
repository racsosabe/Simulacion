f <- function(x) {
	return((1- x^2)^1.5)
}

approximateIntegral <- function(k, f) {
	ans <- NULL
	for(i in 1 : k) {
		ans <- c(ans, f(runif(1)))
	}
	return(sum(ans) / k)
}

testIntegral <- function(times, k, f) {
	ans <- NULL
	for(i in 1 : times) {
		ans <- c(ans, approximateIntegral(times, f))
	}
	return(ans)
}

len <- 100
ans <- testIntegral(len, 10000, f)

xx <- 1 : len
yy <- cumsum(ans) / xx

print(sum(ans) / len)

plot(xx, yy, type = "l")
abline(h = 0.589048622548086) # EXACT INTEGRAL IS APPROXIMATELY 0.589048622548086
