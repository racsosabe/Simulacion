
f <- function(x){
	return(0.25 + 2 * x^3 + 5 * x^4 / 4)
}

# g = 1, así que f(x) / g(x) = f(x)

getVariableAR <- function(c, times){
	ans <- NULL
	for(i in 1 : times){
		U <- runif(1)
		X <- runif(1)
		while(U * c >= f(X)){
			U <- runif(1)
			X <- runif(1)
		}
		ans <- c(ans, X)
	}
	return(ans)
}

pdf("Pregunta2-AR.pdf")

c = f(1)
n <- 200000

xx <- getVariableAR(c, n)

hist(xx, main = "Funcion de densidad usando Aceptacion - Rechazo", prob = TRUE)
