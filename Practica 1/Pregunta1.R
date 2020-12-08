

getUniform <- function(L, R, times = 1){ # Uniforme en [L, R]
	return(floor((R - L + 1) * runif(times)) + L)
}

getDiceSum <- function(){
	return(getUniform(1, 6) + getUniform(1, 6))
}

getSimulation <- function(){
	throws <- 0
	used <- 0
	visited <- rep(0, 12)
	while(used < 11){
		s <- getDiceSum()
		if(visited[s] == 0) {
			visited[s] <- 1
			used <- used + 1
		}
		print(visited)
		throws <- throws + 1
	}
	return(throws)
}

getStudy <- function(times){
	ans <- NULL
	for(i in 1 : times){
		ans <- c(ans, getSimulation())
	}
	return(ans)
}

pdf("Pregunta1.pdf")


n <- 10000 # Tomamos 10^4 resultados
values <- getStudy(n) # Obtenemos un arreglo de resultados

plot(cumsum(values) / 1 : n, type = "l") # Hacemos la grafica de los promedios en cada punto de los resultados
