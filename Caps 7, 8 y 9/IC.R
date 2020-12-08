rm(list=ls())
l = 0.1      ##  Longitud mínima requerida del I.C al 100(1-alpha)%
alpha = 0.05 ##  error muestral máximo permitido en la estimación del I.C
x_1 = rnorm(1,0,1);
x_media = c(x_1, x_1 + (rnorm(1,0,1) - x_1)/(1+1))
s2_1 = 0;
s2_varianza=c(s2_1, (1-1/1)*s2_1+(1+1)*(x_media[2] - x_media[1])^2)
longIC = 2 * qnorm(1 - alpha / 2, 0, 1) * sqrt(s2_varianza[2] / 2)
j=2
while(2 * qnorm(1-alpha/2,0,1) * sqrt(s2_varianza[j])/j >= l){   ## qnorm(1-alpha/2,0,1)=z_alpha/2
  x_media = c(x_media, x_media[j] + (rnorm(1,0,1) - x_media[j]) / (j+1))
  s2_varianza = c(s2_varianza, (1-1/j) * s2_varianza[j]+ (j + 1) * (x_media[j+1] - x_media[j]) ^ 2)
  longIC = c(longIC, 2 * qnorm(1-alpha/2,0,1) * sqrt(s2_varianza[j]/j))
  j = j + 1
}
n = 1 : length(longIC)
plot(n, longIC, type="l")
IC=c(x_media[j] - qnorm(1 - alpha/2,0,1) * sqrt(s2_varianza[j]) / j, x_media[j] + qnorm(1 - alpha / 2, 0, 1) * sqrt(s2_varianza[j]) / j)
IC[2] - IC[1]
