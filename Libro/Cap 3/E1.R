generateSequence <- function(x0, a, c, m, n) {
	ans <- NULL
	for(i in 1 : n) {
		xn = (a * x0 + c) %% m
		ans <- c(ans, xn)
		x0 = xn
	}
	return(ans)
}

print(generateSequence(5, 3, 0, 150, 10))
