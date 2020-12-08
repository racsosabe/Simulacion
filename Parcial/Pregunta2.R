
f <- function(z1, z2){
	if(z1 <= z2){
		return(exp(-(1 / z1 - 1 + 1 / z2 - 1)) / z1 / z1 / z2 / z2)
	} else {
		return(0)
	}
}


getIntegral <- function(times){
	ans <- NULL
	for(i in 1 : times){
		z1 <- runif(1)
		z2 <- runif(1)
		ans <- c(ans, f(z1, z2))
	}
	return(ans)
}

pdf("Pregunta2.pdf")

times <- 100000

ans <- getIntegral(times)

print(sum(ans) / times)

plot(1 : times, cumsum(ans) / 1 : times, type = "l")

