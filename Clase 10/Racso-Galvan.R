
# rm(list = ls()) # Usar solo si es que está en una IDE

f <- function(x){ # Must be distribution function. Unif(50, 200)
	return(punif(x, 50, 200))
}

getDvalue <- function(n, y, F){ # Gets d value for comparison.
	values <- max(1 / n - F[1], F[1])
	for(i in 2 : n){
		values <- max(values, max(i / n - F[i], F[i] - (i - 1) / n))
	}
	return(values)
}

simulateOnce <- function(n, D){ # Simulates once the maximum difference
	U <- sort(runif(n))
	for(i in 1 : n){ # It suffices to have one that beats D for maximum to beat D. This can reduce the execution time in a small constant.
		if(i / n - U[i] >= D) return(1)
		if(U[i] - (i - 1) / n >= D) return(1)
	}
	return(0)
}

compute <- function(n, D, times){ # Simulates "times" times the comparison
	ans <- NULL
	for(i in 1 : times){
		ans <- c(ans, simulateOnce(n, D))
	}
	return(ans)
}

pdf("Racso-Galvan.pdf")

n <- 14
times <- 50000
y <- sort(c(164, 142, 110, 153, 103, 52, 174, 88, 178, 184, 58, 62, 132, 128))
F <- f(y)
d <- getDvalue(n, y, F)
ans <- compute(n, d, times)

plot(1 : times, cumsum(ans) / 1 : times, type = "l")
pvalue <- sum(ans) / times

plot(ecdf(y), col = "red", xlab = "X", ylab = "Y")
par(new = TRUE)
plot(F, type = "l", col = "blue", xlab = "X", ylab = "Y")
cat(sprintf("Pvalue = %.8f\n", c(pvalue)))
if(pvalue >= 0.60){
	cat(sprintf("Dado que %.8f >= 0.30, aceptaremos la hipótesis por ser lo suficientemente alto el p-value\n", c(pvalue)))
} else {
	cat(sprintf("Dado que %.8f < 0.30, rechazaremos la hipótesis por ser muy bajo el p-value\n", c(pvalue)))
}
