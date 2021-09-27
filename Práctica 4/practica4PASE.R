a <- function(x){
  x <- x*100
  if  (sample(1:100,1) <= x){
    return (1)
}
  else{
    return (0)
}

}

b <- function(x,y){
  z <- 0
  for (i in 1:x){
  z <- z + a(y)
  }
  return (z)
}

Intervalo

laura <- function(x,n,p){
  p_x <- factorial(n)/(factorial(x)*factorial(n-x))*p^x*(1-p)^(n-x)

  return(p_x)
}



pilarang= function(n,p){
  start=0
  azar=runif(1)
  intervalo=c()
  for(k in 1:(n+1)){
    end=start+laura(k-1,n,p)
    if(azar<end && azar>start){
      return(k-1)
    }
    intervalo=c(intervalo,end)
    start=end
  }

}


bin=c()

for (k in 1:100000){
  bin=c(bin,pilarang(10,.5))
}

hist(bin,probability=T)

# Probabilidad por número
for (i in 1:11){
  print(i-1)
  print(sum(bin==(i-1))/length(bin))
}
