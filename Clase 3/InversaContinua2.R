
getValues <- function(times){
	u <- runif(times)
	return((- 1 + sqrt(1 + 8 * u)) / 2)
}

getValuesAlternateWithComposition <- function(times, alpha = 0.5){
	ans <- NULL
	for(i in 1 : times){
		u = runif(1)
		if(u < alpha){
			ans <- c(ans, sqrt(runif(1)))
		} else {
			ans <- c(ans, runif(1))
		}
	}
	return(ans)
}

pdf("InversaContinua2.pdf")

xx <- getValues(1000)
yy <- getValuesAlternateWithComposition(1000)

hist(xx, main = "Metodo de Transformada Inversa a lo bruto")
hist(yy, main = "Metodo de Transformada Inversa con Composicion")
