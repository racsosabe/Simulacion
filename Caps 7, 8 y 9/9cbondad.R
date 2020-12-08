install.packages("EstimationTools")
library("EstimationTools")

getEstimator <- function(values){
	result <- maxlogL(values, dist = 'dexp', start = c(rate = 1))
	#summary(result)
	return(result$fit$par)
}

getDvalue <- function(n, y, F){ # Gets d value for comparison.
	values <- max(1 / n - F[1], F[1])
	for(i in 2 : n){
		values <- max(values, max(i / n - F[i], F[i] - (i - 1) / n))
	}
	return(values)
}

simulateOnce <- function(n, D, rate){ # Simulates once the maximum difference
	simulated <- sort(rexp(n, rate = rate))
	pos <- 1
	for(i in 1 : n){
		cur_difference <- i / n - pexp(simulated[i], rate = rate)
		if(cur_difference >= D){
			return(1)
		}
		cur_difference <- pexp(simulated[i], rate = rate) - (i - 1) / n
		if(cur_difference >= D){
			return(1)
		}
	}
	return(0)
}

compute <- function(n, D, times, rate){ # Simulates "times" times the comparison
	ans <- NULL
	for(i in 1 : times){
		ans <- c(ans, simulateOnce(n, D, rate))
	}
	return(ans)
}

pdf("9cbondad.pdf")

n <- 10
times <- 1000
y <- c(66, 72, 81, 94, 112, 116, 124, 140, 145, 155)
estimator <- getEstimator(y)
rate <- estimator[1]
print(rate)
F <- pexp(y, rate = rate)
d <- getDvalue(n, y, F)
print(d)
ans <- compute(n, d, times, y, rate)

plot(1 : times, cumsum(ans) / 1 : times, type = "l")
print(sum(ans) / times)

plot(ecdf(y), col = "red", xlab = "X", ylab = "Y")
par(new = TRUE)
plot(F, type = "l", col = "blue", xlab = "X", ylab = "Y")
