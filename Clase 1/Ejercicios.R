rm(list=ls())
# Generador congruencial de n�meros pseudoaleatorios
# ==================================================

# --------------------------------------------------
# initRANDC(semilla,a,c,m)
#   Selecciona el generador congruencial
#   Por defecto RANDU de IBM con semilla del reloj
#   OJO: No se hace ninguna verificaci�n de los par�metros
initRANDC <- function(semilla=as.numeric(Sys.time()), a=7^5, c=0, m=2^31-1) {
  .semilla <<- as.double(semilla) %% m  #C�lculos en doble precisi�n
  .a <<- a
  .c <<- c
  .m <<- m
  return(invisible(list(semilla=.semilla,a=.a,c=.c,m=.m))) #print(initRANDC())
}

# --------------------------------------------------
# RANDC()
#   Genera un valor pseudoaleatorio con el generador congruencial
#   Actualiza la semilla (si no existe llama a initRANDC)
RANDC <- function() {
  if (!exists(".semilla", envir=globalenv())) initRANDC()
  .semilla <<- (.a * .semilla + .c) %% .m
  return(.semilla/.m)
}

# --------------------------------------------------
# RANDCN(n)
#   Genera un vector de valores pseudoaleatorios con el generador congruencial
#   (por defecto de dimensi�n 1000)
#   Actualiza la semilla (si no existe llama a initRANDC)
RANDCN <- function(n=1000) {
  x <- numeric(n)
  for(i in 1:n) x[i]<-RANDC()
  return(x)
  # return(replicate(n,RANDC()))  # Alternativa m�s r�pida    
}

# --------------------------------------------------
# Ejercicios propuestos 1. y 2. de Cap. 3 - Ross
#   Genera un vector de valores pseudoaleatorios con el generador congruencial
#   initRANDC(semilla,a,c,m)
initRANDC(5, 3, 0, 150)
#initRANDC(3,7,0,200)
#initRANDC()
nsim <- 500
u <- RANDCN(nsim)
hist(u, freq = FALSE)
abline(h = 1)                      # Densidad uniforme
# --------------------------------------------------
# C�lculo Aproximado de E[u]=0.5
# 
print(sum(u)/nsim)
nsim = 1:500
uu = cumsum(u) / nsim
print(uu)
plot(nsim, uu, type="l")
abline(h = 0.5)
# --------------------------------------------------
# C�lculo Aproximado de P[0.4<u<0.8]=0.8-0.4=0.4
#   
sum((0.4<u)&(u<0.8))/nsim

