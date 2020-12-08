

getUniform <- function(L, R, times = 1) {
	return(floor((R - L + 1) * runif(times)) + L)
}

getSubset <- function(n, r) {
	ans <- c(1 : n)
	if(2 * r <= n) { # Los ultimos r son de mi conjunto
		limit <- r
	} else { # Los ultimos n - r no son de mi conjunto
		limit <- n - r
	}
	for(i in n : (n - limit + 1)) {
		I = getUniform(1, i)
		if(I != i) {
			ans[i] = bitwXor(ans[i], ans[I])
			ans[I] = bitwXor(ans[i], ans[I])
			ans[i] = bitwXor(ans[i], ans[I])
		}
	}
	if(2 * r <= n) { # Los ultimos r son de mi conjunto
		return(ans[(n - r + 1) : n])
	} else { # Los primeros r son de mi conjunto
		return(ans[1 : r])
	}
}

print(getSubset(1000, 500))
