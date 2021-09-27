
calcularPi<-function(n){
  
  gotas<-n #Variable para  el número de gotas que van a caer  
  gotas_cirulo<- 0 #Contador para gotas que caen dentro del círculo 
  gotas_cartulina<-0 #Contador para gotas que caen fuera del círculo 
  
  for (i in 1:n){
    
    #Generamos números aleatorios con runif 
    #Estos se encontrarán entre el rango de -1 a 1 en x,y
    x <-runif(1, min=-1, max=1)
    y<-runif(1, min=-1, max=1)
    
    #Obtenemos la distancia al origen del vector x,y
    distancia_origen<- (x**2) + (y**2)
    
    #Si la distancia es menor o igual a 1 significa que 
    #está dentro del circulo y el contador aumenta
    if (distancia_origen <= 1){
      gotas_cirulo <- gotas_cirulo +1 
    }
    
    gotas_cartulina <- gotas_cartulina +1 
  }
  gotas_cirulo
  gotas_cartulina
  
  #Se obtiene finalmente el valor de pi 
  valorpi <- 4*gotas_cirulo/ gotas_cartulina
  return(valorpi)
  
}

set.seed(10)
print(calcularPi(10))
print(calcularPi(100))
print(calcularPi(1000))
print(calcularPi(10000))
print(calcularPi(100000))


vector_y<-c()
vector_x<-c(seq(1,3000,1))

#Se obtendrá pi para valores de n desde 1 a 20000
set.seed(10)
for (j in 1:3000){
  z<-calcularPi(j)
  vector_y<-c(vector_y,z)
}


pi = 3.1415926535

plot(vector_x,vector_y, xlab = "Número de gotas de lluvia", ylab ="Valor de pi" , pch = 20, cex= 1, col='red')
abline(h=pi, col='blue',lwd=3)


  
vector_y[3000]
print(calcularPi(1000000))












