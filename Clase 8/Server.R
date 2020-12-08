rm(list = ls())
Ts=function(s){
  # Genera el tiempo de la siguiente "llegada" desde t=s
  # Poisson Homogéneo
  t=s;  lambda=(1/5)     
  t=t-(1/lambda)*log(runif(1,0,1))
  return(t)}


servidor_1=function(T,lambda1){
  ##   D-A   : Vector de tiempo de permanencia de los clientes en el sistema  
  ##   Tp    : Tiempo posterior a T en que sale el Ultimo cliente
  ##  lambda1: parAmetro de Y ~ Exp(lambda1)=G(y)
  t=0;N_A=0;N_D=0;ES=0 # Inicializacion
  tA=Ts(0) 
  tD=Inf
  Tp=0;A=c(); D=c()
  while(ES>=0){  #(t<=T)
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
            Tp=(max(t-T,0))*as.numeric(length(D-A)>0)
            break
          }
        }
      }
    }
  }
  return(Tp)  # return(D_A)
}


sistema_1_servidor=function(lambda1,T){
  ## [D_A_medioFinal, Tp_medioFinal]=
  ##   PROGRAMA PRINCIPAL
  ##
  ##   D_A_medio  :  tiempo promedio que un cliente pasa en el sistema
  ##   Tp_medio   :  tiempo promedio posterior a T cuando sale el Ultimo cliente
  ##   rodadas    :  nUmero de iteraciones  
  ##   T          :  Intervalo de tiempo despuEs del cual no se permite otras
  ##                 entradas al sistema
  #   lambda1    :  Parametro que controla Tiempo de servicio : Y ~ G(.)=exp(lambda1)
  
  Tp_media=c();s2_var=c()
  Tp_media=servidor_1(T,lambda1); Tp_media=c(Tp_media,Tp_media + (servidor_1(T,lambda1) - Tp_media)/(1+1))
  s2_var=0; s2_var=c(s2_var, (1-1/1)*s2_var+(1+1)*(Tp_media[2] - Tp_media[1])^2)
  j=2
  for(j in 2:7){
    Tp_media=c(Tp_media, Tp_media[j] + (servidor_1(T,lambda1) - Tp_media[j])/(j+1))
    s2_var=c(s2_var, (1-1/j)*s2_var[j]+(j+1)*(Tp_media[j+1] - Tp_media[j])^2)
    j = j + 1
  }
  while (sqrt(s2_var[j]/j) >= (5/(1.96))){
    Tp_media=c(Tp_media, Tp_media[j] + (servidor_1(T,lambda1) - Tp_media[j])/(j+1))
    s2_var=c(s2_var, (1-1/j)*s2_var[j]+(j+1)*(Tp_media[j+1] - Tp_media[j])^2)
    j = j + 1
  }
  par(mfrow=c(1,2))
  hist(Tp_media)
  n=1:length(Tp_media)
  plot(n,Tp_media,type="l",col="blue")
  return(c(Tp_media[length(Tp_media)],j))
}
#sistema_1_servidor(1/10,8*3600)
