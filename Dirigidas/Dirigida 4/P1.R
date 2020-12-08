
f <- function(times = 1){
	return(exp(runif(times)))
}

g <- function(times = 1){
	U = runif(1)
	if(U <= 0.4){
		return(1)
	}
	else{
		return(0)
	}
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
	cat(sprintf("Initialized with %d values\n", c(j)))
	while(sqrt(Vars[j]) >= d * sqrt(j)){
		X <- c(X, f())
		Xm <- c(Xm, Xm[j] + (X[j + 1] - Xm[j]) / (j + 1))
		Vars <- c(Vars, Xm[j + 1] * (1 - Xm[j + 1]))
		j <- j + 1
	}
	return(Xm)
}

pdf("P1.pdf")

means <- getMeanEstimator(f, 0.01)
means2 <- getVarianceEstimator(g, 0.01)

print(means[length(means)])
print(means2[length(means2)])
plot(1 : length(means), means, type = "l")
plot(1 : length(means2), means2, type = "l")

