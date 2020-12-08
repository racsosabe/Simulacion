
f <- function(x){ # Must be distribution function
	return(1 - exp(- x / 100))
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
	for(i in 1 : n){ # It suffices to have one that beats D for maximum to beat D. This can reduce the execution time in a small constant
		if(i / n - U[i] >= D){
			return(1)
		}
		if(U[i] - (i - 1) / n >= D){
			return(1)
		}
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

pdf("Kolmogorov.pdf")

n <- 10
times <- 10000
y <- c(66, 72, 81, 94, 112, 116, 124, 140, 145, 155)
F <- f(y)
d <- getDvalue(n, y, F)
ans <- compute(n, d, times)

plot(1 : times, cumsum(ans) / 1 : times, type = "l")
print(sum(ans) / times)

plot(ecdf(y), col = "red", xlab = "X", ylab = "Y")
par(new = TRUE)
plot(F, type = "l", col = "blue", xlab = "X", ylab = "Y")
