
b=function(x,y){ 
  #Se usa la función gamma que ya viene incluída en R
  #alpha será x, beta será y
  gamma(x)*gamma(y)/(gamma(x+y))
}

#Función de densidad 
#Recibe de argumentos x,alpha,beta
pdf=function(x,alpha,beta){
  (x^(alpha-1)*(1-x)^(beta-1))/b(alpha,beta)
}

# Método de aceptación-rechazo
#input: alpha, beta, número de éxitos
sim_beta <- function(a,b,n){
  exitos=0 #Contador de éxitos
  puntos=c() #Vector de puntos generados
  totales=0 #Contador de intentos
  if(a>1 & b>1){
    c = pdf((a-1)/(a+b-2),a,b) #Se asigna un valor para c 
    #según los valores de alpha, beta 
  }
  while(exitos<n){ #Mientas los exitos no sean los solicitados 
  
    totales=totales+1 #Se cuenta un intento 
    p_1 = runif(1) #Se genera número aleatorio X entre 0,1 por el soporte de Beta
    
    
    p_2 = runif(1,0,c) #Se genera número aleatorio Y  entre 0,c
    
    if(pdf(p_1,a,b)>=p_2){ #Se evalua el punto con la pdf beta 
      #Si el resultado de evaluar p_1 es mayor al p_2 significa que esta
      #dentro de la f(x) por lo que se acepta p_2, se cuenta un éxito y se agrega 
      #al vector de puntos
      exitos=exitos+1
      puntos=c(puntos,p_1)
    }
    
  }
  return(list(puntos, totales, n))
}

# beta(2,2) n = 10 
b22=sim_beta(2,2,10)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,2)    n = 10')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,2))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,2) n = 100 
b22=sim_beta(2,2,100)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,2)    n = 100')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,2))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,2) n = 200
b22=sim_beta(2,2,200)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,2)    n = 200')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,2))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,2) n = 500
b22=sim_beta(2,2,500)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,2)    n = 500')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,2))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,2) n = 1000
b22=sim_beta(2,2,1000)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,2)    n = 1000')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,2))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,2) n = 2000
b22=sim_beta(2,2,2000)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,2)    n = 2000')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,2))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,2) n = 10000
b22=sim_beta(2,2,10000)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,2)    n = 10000')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,2))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)






# beta(2,5) n = 10 
b22=sim_beta(2,5,10)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,5)    n = 10')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,5))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,5) n = 100 
b22=sim_beta(2,5,100)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,5)    n = 100')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,5))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,5) n = 200
b22=sim_beta(2,5,200)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,5)    n = 200')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,5))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,5) n = 500
b22=sim_beta(2,5,500)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,5)    n = 500')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,5))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,5) n = 1000
b22=sim_beta(2,5,1000)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,5)    n = 1000')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,5))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,5) n = 2000
b22=sim_beta(2,5,2000)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,5)    n = 2000')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,5))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)

# beta(2,5) n = 3000
b22=sim_beta(2,5,3000)
hist(b22[[1]], probability = TRUE, main='')
betaLine=c()
title(main = 'Beta(2,5)    n = 3000')

for(i in seq(0,1,0.01)){
  betaLine=c(betaLine,pdf(i,2,5))
}
lines(seq(0,1,0.01),betaLine,col='red',lwd=2)








# Promedio de totales en los que se llega a nvalues de exitos
nvalues=c(10,100,200,500,1000,2000,3000,4000,5000)
promedio=c()
for(n in nvalues){
  promedio=c(promedio,sim_beta(2,2,n)[[2]])
}
promedio


plot(nvalues,promedio)
