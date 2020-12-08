# --------------------------------------------------
# Periodicidad del Metodo Congruencial
a=7; c=3; m=10;
z=matrix(rep(0,m*m),nrow=m,ncol=m, byrow = TRUE)
z[,1]=0:(m-1)
for(i in 1:m) {
  for(j in 2:m){
     z[i,j]<-(a * z[i,j-1] + c) %% m
  }
}
print(z)
# --------------------------------------------------
# Secuencia u_i independientes del Metodo Congruencial
a=137; c=0; m=256;
zVector=matrix(rep(0,1*m),nrow=1,ncol=m, byrow = TRUE)
zVector[1,1]=87 #semilla
for(i in 2:m) {
    zVector[1,i]<-(a * zVector[1,i-1] + c) %% m
}
u=zVector/m
plot(u[1,2:m],u[1,1:m-1],type="l")
# --------------------------------------------------

