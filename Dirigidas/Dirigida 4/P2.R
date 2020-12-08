
f <- function(times = 1){
	ans <- NULL
	for(i in 1 : times){
		limit <- 17 * 60 * 60
		lambda <- 2 / 3600
		t <- -log(runif(1)) / lambda
		while(t <= limit){
			ans <- c(ans, t)
			t <- t - log(runif(1)) / lambda
		}
	}
	return(ans[length(ans)])
}

getMeanEstimator <- function(f, d, initial = 30){
	# Just give the function f to generate the values
	# and the accepted tolerance d.
	# Initialized with initial (default: 30) values
	# Returns the k generated means
	X <- c(f())
	Xm <- c(X[1])
	S2 <- c(0)
	j <- 1
	while(j < initial){
		X <- c(X, f())
		Xm <- c(Xm, Xm[j] + (X[j + 1] - Xm[j]) / (j + 1))
		S2 <- c(S2, (1 - 1 / j) * S2[j] + (j + 1) * (Xm[j + 1] - Xm[j]) ^ 2)
		j <- j + 1
	}
	cat(sprintf("Initialized with %d values\n", c(j)))
	while(sqrt(S2[j]) >= d * sqrt(j)){
		X <- c(X, f())
		Xm <- c(Xm, Xm[j] + (X[j + 1] - Xm[j]) / (j + 1))
		S2 <- c(S2, (1 - 1 / j) * S2[j] + (j + 1) * (Xm[j + 1] - Xm[j]) ^ 2)
		j <- j + 1
	}
	return(Xm)
}

getVarianceEstimator <- function(f, d, initial = 30){
	# Just give the function f to generate the values
	# and the accepted tolerance d.
	# Initialized with initial (default: 30) values
	# Returns the k generated means
	X <- c(f())
	Xm <- c(X[1])
	Vars <- c(Xm[1] * (1 - Xm[1]))
	j <- 1
	while(j < initial){
		X <- c(X, f())
		Xm <- c(Xm, Xm[j] + (X[j + 1] - Xm[j]) / (j + 1))
		Vars <- c(Vars, Xm[j + 1] * (1 - Xm[j + 1]))
		j <- j + 1
	}
	while(sqrt(Vars[j]) >= d * sqrt(j)){
		X <- c(X, f())
		Xm <- c(Xm, Xm[j] + (X[j + 1] - Xm[j]) / (j + 1))
		Vars <- c(Vars, Xm[j + 1] * (1 - Xm[j + 1]))
		j <- j + 1
	}
	return(Xm)
}

pdf("P2.pdf")

means <- getMeanEstimator(f, 15 / 1.96)

m <- means[length(means)]
hours <- m %/% 3600
minutes <- (m %% 3600) %/% 60
seconds <- (m %% 60) %/% 1

cat(sprintf("Promedio de ultimo cliente en llegar: %02d:%02d:%02d\n", hours, minutes, seconds))
plot(1 : length(means), means, type = "l")
