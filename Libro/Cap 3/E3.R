f <- function(x) {
	return(exp(exp(x)))
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
ans <- testIntegral(len, 1000, f)

xx <- 1 : len
yy <- cumsum(ans) / xx

print(sum(ans) / len)

plot(xx, yy, type = "l")
abline(h = 6.31656) # EXACT INTEGRAL IS APPROXIMATELY 6.31656
