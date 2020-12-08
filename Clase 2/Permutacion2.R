

getUniform <- function(L, R, times = 1) {
	return(floor((R - L + 1) * runif(times)) + L)
}

getRandomPermutation2 <- function(n) {
	ans <- 1
	for(i in 2 : n) { # i elementos en la iteracion i
		ans <- c(ans, i) # Agrego i a P(i-1)
		I = getUniform(1, i) # Obtengo la posicion a swapear
		if(I != i) { # Swap si son diferentes posiciones
			ans[i] = bitwXor(ans[i], ans[I])
			ans[I] = bitwXor(ans[i], ans[I])
			ans[i] = bitwXor(ans[i], ans[I])
		}
	}
	return(ans)
}

print(getRandomPermutation2(20))
