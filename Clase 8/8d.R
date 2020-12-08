rm(list = ls())
x_media=c();s2_var=c()
x_media=exp(runif(1,0,1)); x_media=c(x_media,x_media + (exp(runif(1,0,1)) - x_media)/(1+1))
s2_var=0; s2_var=c(s2_var, (1-1/1)*s2_var+(1+1)*(x_media[2] - x_media[1])^2)
j=2
while (sqrt(s2_var[j]/j) >= 0.01){
  x_media=c(x_media, x_media[j] + (exp(runif(1,0,1)) - x_media[j])/(j+1))
  s2_var=c(s2_var, (1-1/j)*s2_var[j]+(j+1)*(x_media[j] - x_media[j-1])^2)
  j = j + 1
}
jj=1:length(x_media)
plot(jj,x_media,type="l",col="blue")
j
x_media[length(x_media)]


##### Variable Antitetica #################
rm(list=ls())
x_media=c();x_mediaVarAnt=c();s2_var=c()
u_1=runif(1,0,1);x_media=exp(u_1)
x_mediaVarAnt=c(x_media,x_media + (exp(1-u_1) - x_media)/(1+1))
s2_var=0; s2_var=c(s2_var, (1-1/1)*s2_var+(1+1)*(x_mediaVarAnt[2] - x_mediaVarAnt[1])^2)
j=2
while (sqrt(s2_var[j]/j) >= 0.01){
  u=runif(1,0,1)
  x_mediaVarAnt[j+1]=x_mediaVarAnt[j] + (exp(u) - x_mediaVarAnt[j])/(j+1)
  x_mediaVarAnt=c(x_mediaVarAnt,x_mediaVarAnt[j+1],x_mediaVarAnt[j+1] + (exp(1-u) - x_mediaVarAnt[j+1])/(j+1+1))
  s2_var[j+1]=(1-1/j)*s2_var[j]+(j+1)*(x_mediaVarAnt[j] - x_mediaVarAnt[j-1])^2
  s2_var=c(s2_var,s2_var[j+1], (1-1/(j+1))*s2_var[j+1]+(j+1+1)*(x_mediaVarAnt[j+1] - x_mediaVarAnt[j+1-1])^2)
  j = j + 2
}
jj=1:length(x_mediaVarAnt)
plot(jj,x_mediaVarAnt,type="l",col="blue")
j
x_mediaVarAnt[length(x_mediaVarAnt)]


##### Variable Control #################
rm(list=ls())
k=3     ### Para este ejemplo en particular se genera por lo menos 3 simulaciones
y_simuladosAux=runif(k,0,1); mu_y=0.5 # Y=Unif(0,1), con E[Y]=0.5
x_simuladosAux=exp(y_simuladosAux)
x_mediaAux=cumsum(x_simuladosAux)/1:k;y_mediaAux=cumsum(y_simuladosAux)/1:k
c_asterisco=-cov(x_simuladosAux,y_simuladosAux)/var(y_simuladosAux)
y_simulados=y_simuladosAux[k];x_simulados=x_simuladosAux[k];
x_media=x_mediaAux[k];y_media=y_mediaAux[k]
xy_media_control=x_media+c_asterisco*(y_media-mu_y)
Var_xy_control=(1/k)*var(x_simuladosAux)*(1-cor(x_simuladosAux,y_simuladosAux)^2)
k=k-2
while(sqrt(Var_xy_control[k]/(k+2))>=0.001){
  y_simulados=c(y_simulados,runif(1,0,1));x_simulados=c(x_simulados,exp(y_simulados[k+1])) 
  x_media=c(x_media,mean(x_simulados)); y_media=c(y_media,mean(y_simulados))
  c_asterisco=-cov(x_simulados,y_simulados)/var(y_simulados)
  xy_media_control=c(xy_media_control,x_media[k]+c_asterisco*(y_media[k]-mu_y))
  Var_xy_control=c(Var_xy_control,(1/(k+1))*var(x_simulados)*(1-cor(x_simulados,y_simulados)^2))
  k=k+1
}
n=1:length(xy_media_control)
plot(n,xy_media_control,type = "l")
print(k+2)
print(xy_media_control[k])

