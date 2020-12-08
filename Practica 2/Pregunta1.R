
getExponential <- function(lambda, times = 1){
	return(-log(runif(times)) / lambda)
}

getVariable <- function(times){
	ans <- NULL
	for(i in 1 : times){
		U = runif(1)
		if(3 * U < 1){ # U < 1/3
			ans <- c(ans, getExponential(2))
		} else {	
			ans <- c(ans, runif(1))
		}
	}
	return(ans)
}

pdf("Pregunta1.pdf")

n <- 100

x <- getVariable(n)

qplot(x)

