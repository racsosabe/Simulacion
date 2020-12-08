
# Ejercicio 4f - Ross
# P[X = j] = 0.05 si j = 1, 2, 3, 4, 5 o 0.15 para j = 6, 7, 8, 9, 10

getUniform <- function(L, R, times = 1) {
	return(floor((R - L + 1) * runif(times)) + L)
}

getComposed <- function(alpha = 0.5, times) {
	ans <- NULL
	for(i in 1 : times) {
		U = runif(1)
		if(U < alpha) {
			ans <- c(ans, getUniform(1, 10))
		} else {
			ans <- c(ans, getUniform(6, 10))
		}
	}
	return(ans)
}

pdf('Composicion.pdf')

values <- getComposed(0.5, 100000)

barplot(table(values))
