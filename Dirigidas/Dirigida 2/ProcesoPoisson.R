

getPoisson <- function(lambda, times = 1){
	return(-log(runif(times)) / lambda)
}

getPoissonProcess <- function(T, lambda){
	time <- 0
	events <- NULL
	while(time <= T){
		new_event <- time + getPoisson(lambda)
		if(new_event <= T){
			events <- c(events, new_event)
		}
		time <- new_event
	}
	return(events)
}

getPoissonProcessExperiment <- function(T, lambda, times){
	successful <- NULL
	for(i in 1 : times){
		events <- getPoissonProcess(T, lambda)
		successful <- c(successful, length(events))
	}
	return(successful)
}

pdf("ProcesoPoisson.pdf")

n <- 200000

xx <- getPoissonProcessExperiment(4, 0.3, n)

NoEvents = xx == 0

print(NoEvents)

print(cumsum(NoEvents))

plot(1 : n, cumsum(NoEvents) / 1 : n, type = "l")

print(sum(NoEvents) / n)
print(dpois(0, 1.2))
