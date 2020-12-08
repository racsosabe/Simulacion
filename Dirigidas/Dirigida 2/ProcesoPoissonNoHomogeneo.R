
getPoissonNonHomogeneous <- function(lambda, ts, ls){
	k <- length(ts)
	pos <- 1
	ans <- NULL
	time <- 0
	X = -log(runif(1)) / ls[1]
	while(pos <= k){
		while(time + X <= ts[pos]){
			time <- time + X
			U <- runif(1)
			if(U * ls[pos] <= lambda(time)){
				ans <- c(ans, time)
			}
		}
		if(pos + 1 <= k){
			X <- (X - ts[pos] + time) * ls[pos] / l[pos+1]
		}
		pos <- pos + 1
	}
	return(ans)
}

lambda <- function(t){
	if(0 < t && t < 5){
		return(t / 5)
	} else if () {
		return(1 + 5 * (t - 5))
	} else {
		return(0)
	}
}
