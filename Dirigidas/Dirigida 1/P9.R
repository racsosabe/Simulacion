g <- function(u, v){ # Funcion definida
	return(exp(-( u / (1-u) + v / (1-v) ) ) / (1-u)^2 / (1-v)^2)
}

test <- function(times, n) {
	values <- NULL # Arreglo de valores
	for(i in 1:times){
		u <- runif(n, 0, 1) # Genero n valores uniformes en 0,1
		v <- runif(n, 0, 1) # Genero n valores uniformes en 0,1
		I = sum(g(u[v <= u], v[v <= u])) / n # Obtengo la suma acumulada de los que consideramos
		values <- c(values, I) # Agrego la respuesta al arreglo
	}
	return(values)
}

vals = test(100, 10000) # 100 tests con 10000 valores

# Para graficar los 100 tests y sus promedios acumulados

xx = 1 : 100
yy = cumsum(vals) / xx

plot(xx, yy, type = "l")
abline(h = 0.5)
