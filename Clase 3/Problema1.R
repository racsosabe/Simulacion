
getComposed <- function(times){
	ans <- NULL
	for(i in 1 : times) {
		a = runif(1)
		if(a < 0.5) {
			u = runif(1)
			ans <- c(ans, floor(log(u) / log(1 - 1/2)) + 1)
		} else { 
			u = runif(1)
			ans <- c(ans, floor(log(u) / log(1 - 1/3)) + 1)
		}
	}
	return(ans)
}

pdf("Problema1.pdf")

n <- 10000

xx <- getComposed(n)

barplot(table(xx) / n)
