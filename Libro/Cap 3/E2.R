generateSequence <- function(x0, a, c, m, n) {
	ans <- NULL
	for(i in 1 : n) {
		xn = (a * x0 + c) %% m
		ans <- c(ans, xn)
		x0 = xn
	}
	return(ans)
}

print(generateSequence(3, 5, 7, 200, 10))
