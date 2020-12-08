
getUniform <- function(L, R, times = 1){
	return(floor((R - L + 1) * runif(times)) + L)
}

getFirstVariable <- function(){ # Unif(1, 10)
	return(getUniform(1, 10))
}

getSecondVariable <- function(){ # 3 * Unif(2, 3)
	return(3 * getUniform(2, 3))
}

getThirdVariable <- function(){ # 3 * Unif(2, 3) + 1
	return(3 * getUniform(2, 3) + 1)
}

getFourthVariable <- function(){ # Always return 8
	return(8)
}

getComposedVariable <- function(times = 1){
	ans <- NULL
	for(i in 1 : times){
		u = runif(1)
		if(u < 0.6){
			ans <- c(ans, getFirstVariable())
		} else if(u < 0.78){
			ans <- c(ans, getSecondVariable())
		} else if(u < 0.92){
			ans <- c(ans, getThirdVariable())
		} else {
			ans <- c(ans, getFourthVariable())
		}
	}
	return(ans)
}

pdf("Pregunta2.pdf")

values <- getComposedVariable(100000)

barplot(table(values) / 100000)
