f <- function(x) {
	return(exp(16 * x ^ 2 - 12 * x + 2))
}

approximateIntegral <- function(k, f) {
	ans <- NULL
	for(i in 1 : k) {
		ans <- c(ans, 4 * f(runif(1)))
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
ans <- testIntegral(len, 1000, f)

xx <- 1 : len
yy <- cumsum(ans) / xx

print(sum(ans) / len)

plot(xx, yy, type = "l")
abline(h = 93.1628) # EXACT INTEGRAL IS APPROXIMATELY 93.1628
