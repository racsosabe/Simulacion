
getUniform <- function(L, R, times = 1) {
	return(floor((R - L + 1) * runif(times)) + L)
}

getRandomPermutation <- function(n) {
	ans <- c(1 : n)
	for(k in n:2){
		I <- getUniform(1, k)
		if(I != k) {
			ans[k] = bitwXor(ans[k], ans[I])
			ans[I] = bitwXor(ans[I], ans[k])
			ans[k] = bitwXor(ans[k], ans[I])
		}
	}
	return(ans)
}

print(getRandomPermutation(30))
