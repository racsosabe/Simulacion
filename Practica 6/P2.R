
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

pdf("P2.pdf")

n <- 14
times <- 10000
y <- sort(c(164, 142, 110, 153, 103, 52, 174, 88, 178, 184, 58, 62, 132, 128))
F <- punif(y, 50, 200)
d <- getDvalue(n, y, F)
ans <- compute(n, d, times)
print(d)

plot(1 : times, cumsum(ans) / 1 : times, type = "l")
print(sum(ans) / times) # P-value aproximado

plot(ecdf(y), col = "red", xlab = "X", ylab = "Y")
par(new = TRUE)
plot(F, type = "l", col = "blue", xlab = "X", ylab = "Y")

