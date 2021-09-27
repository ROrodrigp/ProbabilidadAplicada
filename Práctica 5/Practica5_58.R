#Función Inversa Weibull 
#Recibe como parámetros a lambda, k 
weibull <- function(lambda, k){
  u = runif(1)
  x = lambda*((-log(1-u))^(1/k))
  return(x)
}

dist_weibull <- function(x, lambda, k){
  return ((k/lambda)*((x/lambda)^(k-1))*exp(-(x/lambda)^k))
}

grafica_weibull <- function(lambda, k, n){
  w = c()
  for(i in 1:n){
    w = c(w,weibull(lambda, k))
  }
  x = seq(0,5,0.01)
  y = c()
  for(i in x){
    y = c(y,dist_weibull(i, lambda, k))
  }
  
  hist(w, probability = TRUE, breaks = 100, col="lightpink",
       xlab = '', ylab = '', main = '')
  par(new=TRUE)
  lines(x,y, col="blue", lwd=2)
}

par(new=FALSE)
grafica_weibull(1,5,100)
title(main = 'Weibull(1,5)    n = 100')

par(new=FALSE)
grafica_weibull(1,5,1000)
title(main = 'Weibull(1,5)    n = 1,000')

par(new=FALSE)
grafica_weibull(1,5,100000)
title(main = 'Weibull(1,5)    n = 100,000')


#Función Inversa de Pareto
#Recibe como parametros a,b
pareto <- function(a,b){
  u = runif(1) #Generador de números aleatorios
  x = (b/(1-u)^(1/a))-b
  return(x)
}

dist_pareto <- function(x,a,b){
  return ((a*b**a)/(b+x)**(a+1))
}

grafica_pareto <- function(a, b, n){
  p = c()
  for(i in 1:n){
    p = c(p,pareto(a, b))
  }
  
  x = seq(0,5,0.01)
  y = c()
  for(i in x){
    y = c(y,dist_pareto(i, a, b))
  }
  
  hist(p, probability = TRUE, breaks = 100, col="lightpink",
       xlab = '', ylab = '', main = '')
  par(new=TRUE)
  lines(x,y, col="blue", lwd=2)
}


par(new=FALSE)
grafica_pareto(10,3,100)
title(main = 'Pareto(10,3)    n = 100')

par(new=FALSE)
grafica_pareto(10,3,1000)
title(main = 'Pareto(10,3)    n = 1,000')

par(new=FALSE)
grafica_pareto(10,3,100000)
title(main = 'Pareto(10,3)    n = 100,000')

