
getExponential <- function(lambda, times = 1){
	return(-log(runif(times)) / lambda)
}

simulateServer <- function(lambdaT, lambdaY, T){
	t <- 0 # Tiempo
	A <- NULL # Secuencia de llegadas hasta t
	D <- NULL # Secuencia de salidas hasta t
	n <- 0 # Clientes en el sistema
	tA <- getExponential(lambdaT)
	tD <- Inf
	terminate = FALSE
	while(!terminate){
		if(tA <= tD && tA <= T){
			t <- tA
			n <- n + 1
			tA <- t + getExponential(lambdaT)
			if(n == 1){
				tD <- t + getExponential(lambdaY)
			}
			A <- c(A, t)
		} else if(tD < tA && tD <= T){
			t <- tD
			n <- n - 1
			if(n == 0){
				tD <- Inf
			} else { 
				tD <- t + getExponential(lambdaY)
			}
			D <- c(D, t)
		} else if(min(tA, tD) > T && n > 0){
			t <- tD
			n <- n - 1
			if(n > 0){
				tD <- t + getExponential(lambdaY)
			}
			D <- c(D, t)
		} else if(min(tA, tD) > T && n == 0) {
			Tp <- max(t - T, 0)
			terminate <- TRUE
		}
	}
	DA = 0
	if(length(A) > 0){
		DA = D - A
	}
	return(c(Tp, DA))
}

plotExperiments <- function(totalDA, totalTp){
	total_clients <- length(totalDA)
	par(mfrow = c(1, 2))
	plot(1 : total_clients, cumsum(totalDA) / 1 : total_clients, type = "l", col = "red")
	total_tests <- length(totalTp)
	plot(1 : total_tests, cumsum(totalTp) / 1 : total_tests, type = "l", col = "green")
}

getExperiments <- function(lambdaT, lambdaY, T, times = 1){
	totalDA <- NULL
	totalTp <- NULL
	for(i in 1 : times){
		act <- simulateServer(lambdaT, lambdaY, T)
		totalDA <- c(totalDA, act[2 : length(act)])
		totalTp <- c(totalTp, act[1])
	}
	if(length(totalDA) == 0){ # Si no hay clientes no se puede obtener informacion de nada
		print("Error, no clients appeared")
		return(c(Inf, Inf))
	}
	plotExperiments(totalDA, totalTp)
	return(c(sum(totalDA) / length(totalDA), sum(totalTp) / times))
}

pdf("SLE1Servidor.pdf")

print(getExperiments(0.5, 2, 1000, 20))
