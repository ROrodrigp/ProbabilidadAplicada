

a <- function(x){
  x <- x*100
  if  (sample(0:100,1) <= x){
    return (1)
  }
  else{
    return (0)
  }
  
}


b <- function(x,y){
  c <- 0
  for (i in 1:x){
    c <- c + a(y)
  } 
  return (c)
}




f_proba <- function(x,n,p){
  p_x <- factorial(n)/(factorial(x)*factorial(n-x))*p^x*(1-p)^(n-x)
  
  return(p_x)
}



d= function(n,p){
  start=0
  azar=runif(1)
  for(k in 1:(n+1)){
    end=start+f_proba(k-1,n,p)
    if(azar<=end && azar>start){
      return(k-1)
    }
    start=end
  }
  
}