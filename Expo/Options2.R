# Problem:
# Options over actions - Section 6.7 from Ross' Simulation 3rd Edition book (6.8 from 4th Edition)
# Code Author: Racso Galvan
# Presentation Authors: Guillermo Montañez, Sergio Sanchez
# Policy:
# If there are m days to go, then exercise the option at this time if
#   Current Price > k
# and if for each i = 1, 2, ..., m it holds
#  
#   Current Price > k + (Current Price) * exp(i * alpha) * phi(sigma * sqrt(i) + b_i) - k * phi(b_i)
#
# where phi(x) is the standard normal distribution function and
#   
#   b_i = (i * mu - log(k / (Current Price))) / (sigma * sqrt(i))
#
# Stock Prices model:
#
#   S_i = S_(i-1) * exp(X), where X is a normal random variable with mean mu and standard deviation sigma.
#
# S fractional change has a log-normal distribution with mean (mu + sigma^2 / 2).
# 
# mu <- mean
# sigma <- standard deviation
# k <- fixed stock price
# n <- number of days for the period
# initialPrice <- initial price of the stock

# rm(list = ls()) # Don't use it if not using RStudio

getFormula <- function(Pm, alpha, i, bi, k, sigma){ # Expresión a comparar con Pm
  return(k + Pm * exp(i * alpha) * dnorm(sigma * sqrt(i) + bi) - k * dnorm(bi))
}

getOptionGain <- function(mu, sigma, n, k, initialPrice){
  alpha <- mu + (sigma ^ 2) / 2 # Alpha = esperanza de la log-normal
  if(alpha >= 0){ # Caso óptimo para alpha >= 0
    m <- n
    CurrentPrice <- initialPrice
    while(m > 1){
      x <- rnorm(1, mu, sigma)
      CurrentPrice <- CurrentPrice * exp(x)
      m <- m - 1
    }
    if(CurrentPrice < k){
      return(c(0, 0)) # No compró nada
    } else {
      return(c(CurrentPrice - k, 1)) # Compro en el último dia porque es óptimo
    }
  } else { # Caso en el que la política aplica
    m <- n # Cantidad de dias que faltan para que se acabe el periodo
    CurrentPrice <- initialPrice # Precio inicial de la acción
    choose <- FALSE # Si voy a ejercer la opción o no
    while(m >= 1 && !choose){ # m >= 1 es porque el periodo no ha acabado, !choose es porque aún no ejerzo mi derecho
      if(CurrentPrice > k && m < n){ # Según la política (considerando que no se puede comprar el primer dia)
        choose <- TRUE # Inicializo asumiendo que todos los valores cumplen
        for(i in 1 : m){ # Según la política
          bi <- (i * mu - log(k / CurrentPrice)) / (sigma * sqrt(i))
          if(CurrentPrice <= getFormula(CurrentPrice, alpha, i, bi, k, sigma)){ # Si hay uno que no cumple, no debo elegir este día
            choose <- FALSE
            break
          }
        }
      }
      if(!choose){ # No he elegido este dia para ejercer, genero el nuevo precio de la acción según el x
        x <- rnorm(1, mu, sigma)
        CurrentPrice <- CurrentPrice * exp(x)
        m <- m - 1
      } else { # Ejerzo y termino
        return(c(CurrentPrice - k, m))
      }
    }
  }
  return(c(0, 0)) # En el peor de los casos, no ejerzo y mi ganancia es 0.
}

getPrices <- function(mu, sigma, n, initialPrice){
	ans <- NULL
	CurrentPrice <- initialPrice
	for(i in 1 : n){
		ans <- c(ans, CurrentPrice)
		x <- rnorm(1, mu, sigma)
		CurrentPrice <- CurrentPrice * exp(x)
	}
	return(ans)
}

getOptionExperiment <- function(mu, sigma, n, k, initialPrice, times){ # Generar times experimentos con los parámetros dados
  if(mu + sigma^2 / 2 >= 0){
    print("Solucion Optima")
  } else {
    print("Politica aplicada")
  }
  ansG <- NULL
  ansD <- NULL
  for(i in 1 : times){
    res <- getOptionGain(mu, sigma, n, k, initialPrice)
    ansG <- c(ansG, res[1])
    ansD <- c(ansD, res[2])
  }
  return(c(ansG, ansD))
}

pdf("Options2.pdf") # Erase if using RStudio: if already ran, restart the environment

# Caso de prueba 1

times <- 100000 # 100000 experiments to find average of gain (Takes some time)
mu <- -0.05
sigma <- 0.1
n <- 20
k <- 100
initialPrice <- 101

ans <- getOptionExperiment(mu, sigma, n, k, initialPrice, times)

gains <- ans[1 : times]
days <- ans[(times + 1) : (2 * times)]

plot(1 : times, cumsum(gains) / 1 : times, main = "Gain average simulation", type = "l", xlab = "Number of experiments", ylab = "Gain average")

barplot(table(days), main = "Frequency of optimal number of days left (days = 0 means no execution)")

prices <- getPrices(mu, sigma, n, initialPrice)

plot(1 : n, prices, main = "Price simulation (not related to previous results)", type = "l", xlab = "Day", ylab = "Price in given day")

print(sum(gains) / times)

# Caso de prueba 2

times <- 100000 # 100000 experiments to find average of gain (Takes some time)
mu <- 0.05
sigma <- 0.1
n <- 20
k <- 100
initialPrice <- 101

ans <- getOptionExperiment(mu, sigma, n, k, initialPrice, times)

gains <- ans[1 : times]
days <- ans[(times + 1) : (2 * times)]

plot(1 : times, cumsum(gains) / 1 : times, main = "Gain average simulation", type = "l", xlab = "Number of experiments", ylab = "Gain average")

barplot(table(days), main = "Frequency of optimal number of days left (days = 0 means no execution)")

prices <- getPrices(mu, sigma, n, initialPrice)

plot(1 : n, prices, main = "Price simulation (not related to previous results)", type = "l", xlab = "Day", ylab = "Price in given day")

print(sum(gains) / times)
