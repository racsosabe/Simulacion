
getExponential <- function(lambda, times = 1){
	return(-log(runif(times)) / lambda)
}

simulateServer <- function(lambdaT, lambdaY, T){
	# lambdaT: lambda de la la siguiente llegada desde el tiempo actual
	# lambdaY: lambda de la duración del tiempo de servicio
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
	return(c(sum(D), sum(D - A)))
}

getVariableControl <- function(k, lambdaT, lambdaY, T){
	# k: al menos k simulaciones para empezar a analizar la convergencia
	# lambdaT, lambdaY, T argumentos de la simulación del servidor
	mu_y = (1 / lambdaY) * (lambdaT * T)
	n <- 0
	y <- NULL
	x <- NULL
	while(n < k){ # Generar k valores iniciales de X y Y
		act <- simulateServer(lambdaT, lambdaY, T)
		x <- c(x, act[1])
		y <- c(y, act[2])
		n <- n + 1
	}
	xmean <- cumsum(x) / 1 : n # Medias de cada prefijo de valores
	ymean <- cumsum(y) / 1 : n # Medias de cada prefijo de valores
	cp <- -cov(x, y) / var(y) # C* según el método

	xy_mean_control <- c(xmean[n] + cp * (ymean[n] - mu_y)) # Puntos de control de la media

	xy_var_control <- c((1 / n) * var(x) * (1 - cor(x, y)^2)) # Puntos de control de la varianza

	while(sqrt(xy_var_control[n - k + 1] / n) >= 0.001){
		act <- simulateServer(lambdaT, lambdaY, T)
		x <- c(x, act[1])
		y <- c(y, act[2])
		n <- n + 1
		xmean <- c(xmean, mean(x))
		ymean <- c(ymean, mean(y))
		cp <- -cov(x, y) / var(y)
		xy_mean_control <- c(xy_mean_control, xmean[n] + cp * (ymean[n] - mu_y))
		xy_var_control <- c(xy_var_control, (1 / n) * var(x) * (1 - cor(x, y)^2))
	}
	return(xy_mean_control)
}

pdf("VariableControl.pdf")

ans <- getVariableControl(3, 0.5, 2, 5)

print(length(ans))
print(ans[length(ans)])
plot(1 : length(ans), ans, type = "l")
