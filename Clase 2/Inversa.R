
getInverse <- function(x, p, val) {
	lo <- 1
	hi <- length(p)
	while(lo < hi){
		mi <- lo + (hi - lo) %/% 2
		if(val >= p[mi]) lo = mi+1
		else hi = mi
	}
	return(x[lo])
}

getValues <- function(x, p, times) {
	p <- cumsum(p)
	ans <- NULL
	for(i in 1 : times) {
		ans <- c(ans, getInverse(x, p, runif(1)))
	}
	return(ans)
}

distribution <- getValues(c(1, 2, 3, 4), c(0.3, 0.2, 0.35, 0.15), 100000)

pdf('Inversa.pdf')
barplot(table(distribution))

