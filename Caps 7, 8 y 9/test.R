
alpha <- 0.01
lo <- 0
hi <- 1000
for(i in 1 : 10000){
	mi <- (lo + hi) / 2.0
	prob <- 1 - pnorm(mi)
	print(mi)
	print(prob)
	if(2 * prob < alpha){
		hi <- mi
	} else {
		lo <- mi
	}
}

print(lo)
