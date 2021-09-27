#Ejercicio 1 
Hilbert <- function(n){
  matrix1 <- matrix(c(0), nrow = n, ncol = n) 
  for (i in 1:n){
    for (j in 1:n){
      matrix1[i,j] <- coeficiente_hilbert(i,j)
    }
  }
  return(matrix1)
}

coeficiente_hilbert <- function(i,j){
  x <- 1/(i+j-1)
  return(x)
}

n = 29


m<- Hilbert(n)


m 



det(m)



#Ejercicio 2

Regla_simpson <- function(f,a,b,n){
  h <- (b-a)/n
  sum <- 0
  sum <- sum + f(a)
  sum <- sum + f(b)
  for (i in 1:(n-1)){
    
    if (i%%2 == 0){
      sum <- sum + 2*f(a+i*h)
    }else{
      sum <- sum + 4*f(a+i*h)
    }
  }
  resultado <- (h/3)*(sum)
  return(resultado)
}

f1 <- function(x){
  return(x**2)
}


f3 <- function(x){
  return(exp(-x^2))
}

f4 <- function(x){
  y_1 <- x^3
  y_2 <- 1 + y_1
  y_3 <- sqrt(y_2)
  return (y_3)
}
  

Regla_simpson(f4,0,10,10)