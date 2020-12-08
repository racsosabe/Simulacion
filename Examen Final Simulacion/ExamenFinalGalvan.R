# Problema 1
# Nos encontramos en la estimación del intervalo de una media poblacional
# Deseamos que la estimación final de la media tenga un error absoluto máximo de 0.5 con 99 % de confianza.
# Entonces, tendremos que alpha = 0.01, por lo cual z_{0.005} \approx 2.33
# Finalmente, nuestro intervalo será de:
# [media - z_{0.005} * stdev / sqrt(n), media + z_{0.005} * stdev / sqrt(n)]
# Y por definición, podemos aproximar los valores con:
# media = media de los datos observados
# stdev = sqrt(varianza de los datos observados)
# Luego de calcular los valores estimados, obtendremos
# media = 122.85
# stdev = sqrt(282.2395) = 16.79998
# Si usamos como una aproximación dichos valores para intentar hallar cuál es eln
# con el que se llegará a obtener un intervalo de longitud <= 1 ([-0.5, 0.5])
# tendríamos que
# 
# 2 * z_{0.005} * stdev / sqrt(n) \approx 1
# 2 * 2.33 * 16.79998 / sqrt(n) \approx 1
# sqrt(n) \approx 2 * 2.33 * 16.79998 = 78.2879068
# n \approx 6129 (redondeado para arriba siempre)
# Por lo que estimaríamos que hacen falta 6109 datos más para poder cumplir con lo que se desea.
# A pesar de lo anterior, estaríamos considerando que el sistema es estable, porlo
# que un aproximación usando solo 20 datos sería insuficiente para deducir el n
# adecuado sin el conocimiento de la distribución de los mismos. Nótese que a simple vista
# la longitud del intervalo decrece relativamente rápido, así que podría tomar la cantidad anterior como una estimación bastante vaga.

x <- c(102,112,131,107,114,95,133,145,139,117,93,111,124,122,136,141,119, 122, 151,143)
n <- length(x)

medias <- c(x[1])
varianzas <- c(0)
L <- c(0)
R <- c(10000)
for(i in 2 : n){
	medias <- c(medias, medias[i - 1] + (x[i] - medias[i - 1]) / i)
	varianzas[i] <- (1 - 1 / (i - 1)) * varianzas[i - 1] + i * (medias[i] - medias[i - 1]) * (medias[i] - medias[i - 1])
	L <- c(L, medias[i] - 2.33 * sqrt(varianzas[i]) / i)
	R <- c(R, medias[i] + 2.33 * sqrt(varianzas[i]) / i)
}

print(medias[n])
print(sqrt(varianzas[n]))
print(R - L)
plot(1 : n, medias, type = "l", main = "Medias")
plot(1 : n, varianzas, type = "l", main = "Varianzas")
plot(2 : n, R[2:n] - L[2:n], type = "l", main = "Longitud del intervalo de confianza")

# Problema 2
# M = min(n : U_{1} <= U_{2} <= ... <= U_{n - 1} > U_{n})
# 2.1 Notemos que si M > n, entonces se debe cumplir estrictamente que:
# U_{1} <= U_{2} <= ... <= U_{n}.
# Luego de esto, los elementos son libres en tomar el valor que sea y cada uno de esos eventos elementales aporta a la probabilidad, cuyo total será 1.
# Por lo anterior, lo único que nos queda es verificar la probabilidad de que se dé la situación inicial.
# Notemos que existen n! posibles permutaciones de n elementos, por lo que el tener
# los n elementos ordenados no descendentemente tendrá una probabilidad de:
# 1 / n!, así que:
# P(M > n) = 1 / n! para todo n >= 0.
# 2.2 Si usamos la identidad E[M] = \sum\limits_{n = 0}^{\infty}P(M > n), tendremos
# E[M] = \sum\limits_{n = 0}^{\infty} = 1 / n!, pero esta sumatoria infinita da
# como resultado el número e (se puede demostrar usando teorema de Taylor).
# Finalmente E[M] = e
# 2.3 Para poder estimar e usando 1000 términos lo que haremos es lo siguiente:
# 
# Generaremos 1000 números aleatorios partiendo de una distribución uniforme Unif(0, 1) y hallaremos los m_{i} para cada experimento
# Luego de lo anterior, fijaremos cada valor posible de n hasta que encontremos el primer n tal que
# Q(m_{i} > n) = 0, pues en este momento, para todo y > n, la cantidad será 0 siempre.
# Entonces sumaremos a la respuesta el valor Q(m_{i} > n) / 1000.
# Con esto obtendremos una estimación de e.
# Nótese que Q(m_{i} > n) = Cantidad de m_{i} > n.
# 2.4 Podemos estimar la varianza usando los métodos convencionales de cálculo de varianza para una secuencia de valores
# Y estimaremos el intervalo de confianza en base a los 1000 datos.
# alpha = 0.05 ---> z_{0.025} \approx 1.96
# Nuestro intervalo estimado tendrá la forma:
# [media - z_{0.025} * stdev / sqrt(n), media + z_{0.025} * stdev / sqrt(n)]
# [media - 1.96 * stdev / sqrt(1000), media + 1.96 * stdev / sqrt(1000)]
# 
# 
# 
# 

simulateFirst <- function(){
	U <- runif(2)
	n <- 2
	while(U[n - 1] <= U[n]){
		U <- c(U, runif(1))
		n <- n + 1
	}
	return(n)
}

simulateForE <- function(times = 1000){
	values <- NULL
	for(i in 1 : times){
		values <- c(values, simulateFirst())
	}
	return(values)
}

estimateEandVariance <- function(times = 1000){
	values <- simulateForE(times)
	ans <- 0
	m <- 0
	valid <- TRUE
	while(valid){
		cnt <- 0
		for(x in values){
			if(x > m){
				cnt <- cnt + 1
			}
		}
		ans <- ans + cnt / times
		if(cnt == 0){
			valid <- FALSE
		}
		m <- m + 1
	}
	medias <- c(values[1])
	varianzas <- c(0)
	L <- c(0)
	R <- c(10000)
	for(i in 2 : times){
		medias <- c(medias, medias[i - 1] + (values[i] - medias[i - 1]) / i)
		varianzas[i] <- (1 - 1 / (i - 1)) * varianzas[i - 1] + i * (medias[i] - medias[i - 1]) * (medias[i] - medias[i - 1])
		L <- c(L, medias[i] - 1.96 * sqrt(varianzas[i]) / i)
		R <- c(R, medias[i] + 1.96 * sqrt(varianzas[i]) / i)
	}
	plot(1 : times, medias, type = "l", main = "Medias")
	plot(1 : times, varianzas, type = "l", main = "Varianzas")
	plot(2 : times, R[2 : times] - L[2 : times], type = "l", main = "Longitud del intervalo de confianza")
	return(c(ans, L[times], R[times]))
}

print(estimateEandVariance())

# Problema 3
# 3.1 Podemos simplemente plantear una simulación directa del proceso propuesto
# Y calcularemos la media de los valores generados.
# Usaremos el método de la transformada inversa para generar los valores discretos
# Obtuvimos, usando 400 simulaciones, una media de 271.9775000
# 3.2 Podemos extender la implementación de 3.1 para que podamos obtener la frecuencia
# de cada valor, así obtenemos las siguientes medias usando 400 valores:
# N_{1} = 6.8325 
# N_{2} = 13.3125 
# N_{3} = 20.1050 
# N_{4} = 26.3775 
# N_{5} = 33.3725
# 3.3 Al usar simulación para obtener los valores de Y y L con los mismos L_{i} generados
# podemos notar que la correlación tiende a ser negativa, pues en las gráficas
# es evidente que cuando Y sube, el L baja y viceversa.
# 3.4 Usaremos a Y como variable control pero considerando una estimación de su media
# puesto que no es muy fácil calcularla de manera analítica.
# Al usar 400 simulaciones, aproximaremos la media de Y como 366.1450000.
# Así que usaremos la variable:
# L + c * (Y - 366.1450000).
# Consideramos que deseamos un error máximo de 0.1 y para lograrlo se usaron aproximadamente
# 115 - 120 iteraciones del procedimiento.
# 3.5 Obtenemos el T-value usando la expresión usual: 12.925
# Luego de ello, simulamos los valores para estimar el p-value.
# Luego de 1000 simulaciones obtenemos un p-value de: 0.596
# Por lo tanto, la Hipótesis nula es Aceptada debido al alto valor del p-value.

generateRandomPermutation <- function(n = 5){ # Tested
	ans <- 1 : n
	for(i in 2 : n){
		p <- floor(i * runif(1)) + 1
		if(p != i){
			temp <- ans[i]
			ans[i] <- ans[p]
			ans[p] <- temp
		}
	}
	return(ans)
}

generateNxtValue <- function(times = 1){ # Tested
	ans <- NULL
	for(i in 1 : times){
		p <- c(1 : 5) * rep(1 / 15, 5)
		p <- cumsum(p)
		U <- runif(1)
		for(i in 1 : 5){
			if(p[i] >= U){
				ans <- c(ans, i)
				break
			}
		}
	}
	return(ans)
}

generateOneL <- function(times = 100){
	perm <- generateRandomPermutation(5)
	L <- NULL
	N <- rep(0, 5)
	for(i in 1 : times){
		nxtval <- generateNxtValue()
		N[nxtval] <- N[nxtval] + 1
		pos <- 1
		while(perm[pos] != nxtval){
			pos <- pos + 1
		}
		L <- c(L, pos)
		if(pos != 1){
			if(pos == 5){
				perm <- c(nxtval, perm[1 : pos - 1])
			} else {
				perm <- c(nxtval, perm[1 : pos - 1], perm[pos + 1 : 5])
			}
		}
	}
	return(c(sum(L), N))
}

getExpectations <- function(times = 400){
	Ls <- NULL
	Ns <- matrix(rep(0, 5 * times), nrow = times, ncol = 5, byrow = TRUE)
	Ys <- NULL
	CorrLY <- NULL
	for(i in 1 : times){
		cur <- generateOneL()
		L <- cur[1]
		N <- cur[2 : 6]
		Ls <- c(Ls, L)
		Ns[i, ] <- N
		Ys <- c(Ys, sum(c(1 : 5) * N))
		CorrLY <- c(CorrLY, cor(Ls, Ys))
	}
	plot(1 : times, cumsum(Ls) / 1 : times, type = "l", main = "Media de L")
	plot(1 : times, cumsum(Ys) / 1 : times, type = "l", main = "Media de Y")
	plot(1 : times, CorrLY, type = "l", main = "Correlacion de Y y L")
	plot(1 : times, cumsum(Ns[, 1]) / 1 : times, type = "l", main = "Media de N_{1}")
	plot(1 : times, cumsum(Ns[, 2]) / 1 : times, type = "l", main = "Media de N_{2}")
	plot(1 : times, cumsum(Ns[, 3]) / 1 : times, type = "l", main = "Media de N_{3}")
	plot(1 : times, cumsum(Ns[, 4]) / 1 : times, type = "l", main = "Media de N_{4}")
	plot(1 : times, cumsum(Ns[, 5]) / 1 : times, type = "l", main = "Media de N_{5}")
	print(c(sum(Ls) / times, sum(Ys) / times, CorrLY[times]))
	print(c(sum(Ns[, 1]) / times, sum(Ns[, 2]) / times, sum(Ns[, 3]) / times, sum(Ns[, 4]) / times, sum(Ns[, 5]) / times))
}

useVariableControl <- function(k, d){
	X <- NULL
	Y <- NULL
	media_x <- NULL
	media_y <- NULL
	n <- 0
	mu_y <- 366.1450000
	while(n < k){ # Generate first k values
		cur <- generateOneL()
		L <- cur[1]
		N <- cur[2 : 6]
		curY <- sum(c(1 : 5) * N)
		Y <- c(Y, curY)
		X <- c(X, L)
		media_x <- c(media_x, mean(X))
		media_y <- c(media_y, mean(Y))
		n <- n + 1
	}
	cp <- -cov(X, Y) / var(Y)
	media_control <- c(media_x[n] + cp * media_y[n])
	varianza_control <- c(1 / n * var(X) * (1 - cor(X, Y)^2))
	while(sqrt(varianza_control[n - k + 1]) >= d * sqrt(n)){
		cur <- generateOneL()
		L <- cur[1]
		N <- cur[2 : 6]
		curY <- sum(c(1 : 5) * N)
		Y <- c(Y, curY)
		X <- c(X, L)
		n <- n + 1
		media_x <- c(media_x, mean(X))
		media_y <- c(media_y, mean(Y))
		cp <- -cov(X, Y) / var(Y)
		media_control <- c(media_control, media_x[n] + cp * media_y[n])	
		varianza_control <- c(varianza_control, 1 / n * var(X) * (1 - cor(X, Y)^2))
	}
	print(n)
	print(n - k + 1)
	print(length(media_x))
	print(length(media_y))
	print(length(media_control))
	print(length(varianza_control))
	plot(1 : n, media_x, type = "l", main = "Media de L")
	plot(1 : n, media_y, type = "l", main = "Media de Y")
	plot(1 : (n - k + 1), media_control, type = "l", main = "Media Control")
	plot(1 : (n - k + 1), varianza_control, type = "l", main = "Varianza Control")
}

getTvalue <- function(frec, p, n){
	return(sum((frec - n * p) * (frec - n * p) / (n * p)))
}

getPvalue <- function(times, p, k, t){
	frec <- matrix(rep(0, k * times), nrow = times, ncol = k, byrow = TRUE)
	Y <- matrix(rep(0, k * times), nrow = times, ncol = k, byrow = TRUE)
	T <- NULL
	for(i in 1 : times){
		Y[i, ] <- generateNxtValue(k)
		for(x in Y[i, ]){
			frec[i, x] <- frec[i, x] + 1
		}
		T <- c(T, getTvalue(frec[i, ], p, k))
	}
	T <- T >= t
	return(T)
}

checkHzero <- function(times){
	values <- c(2,1,4,3,1,5,2,1,2,3)
	n <- length(values)
	frec <- rep(0, 5)
	for(x in values){
		frec[x] <- frec[x] + 1
	}
	p <- c(1 : 5) * rep(1 / 15, 5)
	t <- getTvalue(frec, p, n)
	print(t)
	ans <- getPvalue(times, p, n, t)
	plot(1 : times, cumsum(ans) / 1 : times, type = "l", main = "Oscilacion del p-value")
	print(sum(ans) / times)
}

getExpectations() # 3.1, 3.2, 3.3
useVariableControl(10, 0.1)
checkHzero(1000) # 3.5
