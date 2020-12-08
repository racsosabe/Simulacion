
getUniform <- function(L, R, times) {
	return(floor((R - L + 1) * runif(times)) + L)
}

distribution <- getUniform(1, 10, 100)

pdf('Uniforme.pdf')

barplot(table(distribution))
