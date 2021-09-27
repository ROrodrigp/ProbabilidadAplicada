library(scatterplot3d) 


Generador_congruencial <- function(a,c,m,s,n){ #Generador congruencial si te pide ingresar una semilla (s)
  s <- (s%%m)  #Aqui nos aseguramos de poner a s en un valor adecuado 
  numeros <- c(s)  #Agregamos la semilla como primer elemento del vector para poder utilizarla para X_1
  for (i in 2:n){
    numeros[i] <- ((a*numeros[i-1]+c)%%m)  #Aqui llamamos al elemento X_{i-1} para calcular X_i
  }

  numeros <- numeros/m
  return(numeros)
}


a <- Generador_congruencial(2,0,7,1,10)
print(a)

a <- Generador_congruencial(3,0,2**4,1,8)
print(a)


Generador_congruencial2 <- function(a,c,m,n){ #Generador congruencial no pide agregar una semilla
  #Fijamos una seed para obtener siempre los mismo valores
  set.seed(10)
  s <- sample(0:(m-1),1,replace=F) #Obtiene una semilla entre [0,m-1] con s entero
  s <- (s%%m)
  numeros <- c(s)    #Misma idea del codigo anterior
  for (i in 2:n){
    numeros[i] <- ((a*numeros[i-1]+c)%%m) 
  }
  numeros <- numeros/m
  return(numeros)
}



#Codigo para observar los valores dados por nosotros
datos1 <- Generador_congruencial2(a=3**5,c=0,m=3**31-1,n=500000)
datos2 <- Generador_congruencial2(a=5**5,c=0,m=2**31-1,n=1000)
datos3 <- Generador_congruencial2(a=7**5,c=0,m=5**5,n=1000)
#Codigo que llama al histograma
hist(datos1,main=NULL,breaks=100,probability=T)
title('a=3**5, m=3**31-1')
hist(datos2,main=NULL,breaks=100,probability=T)
title('a=5**5, m=2**31-1')
hist(datos3,main=NULL,breaks=100,probability=T)
title('a=7**5, m=5**5')

#Datos para el tercer punto
datos1 <- Generador_congruencial2(a=3**5,c=0,m=3**31-1,n=1000)
datos2 <- Generador_congruencial2(a=5**5,c=0,m=2**31-1,n=1000)
datos3 <- Generador_congruencial2(a=7**5,c=0,m=2**41,n=1000)
g2 <- scatterplot3d(datos1,datos2,datos3,grid = 1,tick.marks=T, box = T,col.axis = "darkblue",col.grid = "lightgreen", main="Grafico en 3D con valores de a y m disitintos")


#Codigo para observar los valores pedidos
y <- Generador_congruencial2(a=7**5,c=0,m=2**31-1,n=500000)
#Codigo que llama al histograma
hist(y,main=NULL, breaks = 100,probability=T)
#Titulos y etiquetas
title('a=7**5, m=2**31-1')

x<- Generador_congruencial2(a=7**5,c=0,m=2**31-1,n=1000)
y <- Generador_congruencial2(a=7**5,c=0,m=2**31-1,n=1000)
z<- Generador_congruencial2(a=7**5,c=0,m=2**31-1,n=1000)
g2 <- scatterplot3d(x,y,z,grid = 1,tick.marks=T, box = T,col.axis = "darkblue",col.grid = "lightgreen", main="Grafico en 3D con valores de a y m disitintos")



#Funcion que usa nuestro generador congruencial
calcularPiCongr<-function(n){
  #Funcion para calcular pi con ayuda de nuestro generador congruencial
  
  
  gotas<-n
  gotas_cirulo<- 0
  gotas_cartulina<-0
  
  #Se eligio esos parametros para el generador congruencial ya que anteriormente
  #Vimos que se distribuian de una buena forma
  rand <- Generador_congruencial2(a=7**5,c=0,m=2**31-1,n=500000)
  for (i in 1:n){
    
    
    x <- rand[i] 
    y <- rand[i+1]
    
    distancia_origen<- (x**2) + (y**2)
    
    if (distancia_origen <= 1){
      gotas_cirulo <- gotas_cirulo +1 
    }
    
    gotas_cartulina <- gotas_cartulina +1 
  }
  gotas_cirulo
  gotas_cartulina
  
  valorpi <- 4*gotas_cirulo/ gotas_cartulina
  return(valorpi)
}


#Funcion que usa runif (Practica 2)
calcularPi <- function(n){
  #n son el numero de gotas a simular
  
  #Creamos nuestros contadores de gotas
  gotas <- n
  #Contador de gotas para el circulo
  gotas_circulo <- 0
  #Contador de gotas para el cuadrado
  gotas_cartulina <- 0
  
  for (i in 1:n){
    
    #Los valores de min = -1 y max = 1
    #Son para simular el cuadrado con esquina
    #[1,1],[1,-1],[-1,1],[-1,-1]
    #Asi tenemos el circulo centrado en el origen
    x <- runif(1, min=-1, max=1)
    y <- runif(1, min=-1, max=1)
    #Runif nos escoge un valor "Real" entre -1 y 1 en este caso
    
    #Variable para saber la distancia al origen
    distancia_origen <- (x**2) + (y**2)
    
    
    if (distancia_origen <= 1){
      gotas_circulo <- gotas_circulo + 1 
    }
    
    gotas_cartulina <- gotas_cartulina + 1 
  }
  gotas_circulo
  gotas_cartulina
  
  #Formula ya dada
  valorpi <- 4*gotas_circulo/ gotas_cartulina
  return (valorpi)
}


#Aqui calculamos el tiempo de ejecucion para nuestra forma de aproximar Pi
inicio= Sys.time()
pi1 <- calcularPiCongr(400000)
final=Sys.time()


#Aqui calculamos el tiempo de ejecucion con ayuda runif de aproximar Pi
inicio2= Sys.time()
pi <- calcularPi(400000)
final2=Sys.time()

#Calculamos la diferencia de los tiempos
deltaTiempo <- (final - inicio) 
alfaTiempo<-  (final2-inicio2)

print(paste("Tiempo con generador lineal congruencial:",deltaTiempo))
print(paste("Tiempo con runif( ) :",alfaTiempo))


#Comparacion de los tiempos de ejecucion
print(paste('Pi con nuestro metodo:', pi1))
print(paste('Pi con metodo de runif:',pi))
print(paste('(Tiempo nuestro)-(Tiempo estandar)' ,deltaTiempo))




