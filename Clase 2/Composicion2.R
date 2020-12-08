
# Ejercicio Propuesto 14 Cap 4  - Ross
# P[X = j] = 0.11 si j = 5, 7, 9, 11, 13 o 0.09 para j = 6, 8, 10, 12, 14
# Elegimos alpha = 0.09 / 0.1 = 0.9 para poder eliminar la segunda probabilidad
# De esa forma, la restante tendrá una distribución:
# 0.02 si j = 5, 7, 9, 11, 13 o 0 para j = 6, 8, 10, 12, 14
# Notemos que esto es una uniforme multiplicada por 0.1 (1 - alpha) sobre
# los 5 valores impares de j, que son 2 * k - 1 para k = 3, 4, 5, 6, 7
#
# Nuestros datos serán generados de la forma
#
# alpha = 0.9
# X1 = Uniforme discreta en [5, 14], P(j) = 0.1
# X2 = Uniforme discreta en [3, 7] pero tomando como x_i = 2 * i - 1, P(j) = 0.2

getUniform <- function(L, R, times = 1) {
	return(floor((R - L + 1) * runif(times)) + L)
}

getComposed <- function(alpha = 0.5, times) {
	ans <- NULL
	for(i in 1 : times) {
		U = runif(1)
		if(U < alpha) {
			ans <- c(ans, getUniform(5, 14))
		} else {
			ans <- c(ans, 2 * getUniform(3, 7) - 1)
		}
	}
	return(ans)
}

pdf('Composicion2.pdf')

values <- getComposed(0.9, 100000)

barplot(table(values))
