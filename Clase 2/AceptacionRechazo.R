
getUniform <- function(L, R, times = 1) {
	return(floor((R - L + 1) * runif(times)) + L)
}

getVariable <- function(p, q, x, times) {
	ans <- NULL
	C <- max(p[q > 0] / q[q > 0])
	print(C)
	for(i in 1 : times) {
		Y <- getUniform(1, length(x))
		U <- runif(1)
		while(U * q[Y] * C > p[Y]){
			Y <- getUniform(1, length(x))
			U <- runif(1)
		}
		ans <- c(ans, x[Y])
	}
	return(ans)
}

pdf('AceptacionRechazo.pdf')

xx <- getVariable(c(0.11, 0.12, 0.09, 0.08, 0.12, 0.10, 0.09, 0.09, 0.10, 0.10), c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 100000)

print(table(xx))
barplot(table(xx))
