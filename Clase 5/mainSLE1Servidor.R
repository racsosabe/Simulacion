rm(list = ls())
Ts=function(s){
  # Genera el tiempo de la siguiente "llegada" desde t=s
  # Poisson Homogéneo
  t=s;  lambda=0.5
  t=t-(1/lambda)*log(runif(1,0,1))
  return(t)
}


servidor_1=function(T,lambda1){
##   D-A   : Vector de tiempo de permanencia de los clientes en el sistema  
##   Tp    : Tiempo posterior a T en que sale el Ultimo cliente
##  lambda1: parAmetro de Y ~ Exp(lambda1)=G(y)
t=0;N_A=0;N_D=0;ES=0 # Inicializacion
tA=Ts(0) 
tD=Inf
Tp=0;A=c(); D=c()
while(ES>=0){  
  if((tA <= tD)&&(tA<=T)){
    t=tA            
    N_A=N_A+1
    ES=ES+1
    tA=Ts(t)    
    if(ES==1){
      Y=-(1/lambda1)*log(runif(1,0,1))
      tD=t+Y
    }
    A=c(A,t)      
  }else{
    if((tD<tA)&&(tD<=T)){
        t=tD            
        N_D=N_D+1
        ES=ES-1
        if(ES==0){
            tD=Inf
        }else{
            Y=-(1/lambda1)*log(runif(1,0,1))
            tD=t+Y       
        }
        D=c(D,t)
    }else{
      if((min(tA,tD)>T)&&(ES>0)){
        t=tD
        ES=ES-1
        N_D=N_D+1
        D=c(D,t)
      }else{
        if((min(tA,tD)>T)&&(ES==0)){
          Tp=max(t-T,0)
          break
        }
      }
    }
  }
}
  if(length(A) == 0){ 
    DA = 0; Tp = 0
  }else{
    DA = (D-A)
  }
return(c(Tp,DA))
}




sistema_1_servidor=function(rodadas,lambda1,T){
## [D_A_medioFinal, Tp_medioFinal]=
##   PROGRAMA PRINCIPAL
##
##   D_A_medio  :  tiempo promedio que un cliente pasa en el sistema
##   Tp_medio   :  tiempo promedio posterior a T cuando sale el Ultimo cliente
##   rodadas    :  nUmero de iteraciones  
##   T          :  Intervalo de tiempo despuEs del cual no se permite otras
##                 entradas al sistema
#   lambda1    :  Parametro que controla Tiempo de servicio : Y ~ G(.)=exp(lambda1)

suma=0;longitud=0;Tp=rep(0,rodadas);D_A=c()
for(i in 1:rodadas){
  DA_Tp=servidor_1(T,lambda1)
  if(length(DA_Tp>1)){
    D_A=c(D_A,DA_Tp[2:length(DA_Tp)])
  }
  Tp[i]=DA_Tp[1]
}
n=1:length(D_A)
D_A_medio=cumsum(D_A)/n
m=1:rodadas
Tp_medio=cumsum(Tp)/m
par(mfrow=c(1,2))
plot(n,D_A_medio,type="l",col="blue")
plot(m,Tp_medio,type="l",col="blue")
return(c(Tp_medio[length(Tp_medio)],D_A_medio[length(D_A_medio)]))
}

