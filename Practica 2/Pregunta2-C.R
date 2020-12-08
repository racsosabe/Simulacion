
getInversed <- function(k, times = 1){
	return(runif(times)^(1.0 / k))
}

getVariable <- function(times){
	ans <- NULL
	for(i in 1 : times){
		U <- runif(1)
		if(4 * U < 1){
			ans <- c(ans, runif(1))
		} else if(4 * U <= 3){
			ans <- c(ans, getInversed(4))
		} else{
			ans <- c(ans, getInversed(4))
		}
	}
	return(ans)
}

pdf("Pregunta2-C.pdf")

n <- 10000

xx <- getVariable(n)

hist(xx, main = "Funcion de densidad usando Composicion", prob = TRUE)
