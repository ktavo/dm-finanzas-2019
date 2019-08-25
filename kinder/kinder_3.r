﻿#R para Kindergarten, sala de 3 años 
#ejecutar linea a linea y ver los resultados
#lo minimo de vectores, listas y estructuras de control, para sobrevivir a la materia


#entorno de ejecucion : PC local

#set working directory
setwd( "M:\\datasets\\" )


#opcion nativa en R para cargar un dataset

dataset  <-  fread("201902.txt")

#------------------------------------------------------------------------------
#Vectores


v1  <-  1:10
v1


v2  <-  seq(10)
v2

v3  <-  rep( 3, 5 )
v3

#distribucion 
v4  <-  runif( 5 )
v4


Notar que v1 y v2 son lo mismo


#agrego un elemento a un vector
v1
v1 <- c( v1, 20 )
v1



#accedo por el indice del vector
v2 <- 21:30
v2

v2[ 1 ] 
v2[ 3 ] 
v2[ 4 ] 

#----------------
#sumar un escalar

v1 <-  1:10
v1 <- v1 + 5
v1

#multiplicar por un escalar
v1 <-  1:10
v1 <- v1*2
v1



#----------------
#concateno dos vectores

v1 <- 1:10
v2 <- 21:30



v3 <- c( v1, v2 )
v3

#largo de un vector
length( v3 )

#esto NO funciona
nrow( v3 )




#------------------------------------------------------------------------------
#Listas

alumno  <-  list( edad=33, sexo="F", grado="Economia", sabe_R=TRUE )

alumno
summary( alumno )

alumno$edad

#agrego un campo a la lista
alumno$sabe_Python=FALSE

alumno


alumno[1]
alumno[2]
alumno[3]

is.list( alumno )



#Otra forma de ver la lista
alumno2  <-  list( 33, "F", "Economia", TRUE, FALSE )

names( alumno2 ) <- c( "edad", "sexo", "grado", "sabe_R", "sabe_Python" )

alumno2

#------------------------------------------------------------------------------
#Lista a vector

lprimos <- list(  2,3, 5, 7, 11, 13, 17, 19, 23 )

vprimos <- unlist( lprimos )


lprimos
vprimos
#------------------------------------------------------------------------------
#LOOPS
#por favor, prestar atencion quienes no han programado en su vida


#Aqui vienen distintos loops, que imprimen de diversas formas el resultado


for( i  in   c( "a", "b", "c", "d" )  )
{
  print( i )
}


for( i  in   c( "a", "b", "c", "d" )  )
{
  cat( i )
}



for( i  in   c( "a", "b", "c", "d" )  )
{
  cat( i, " " )
}


for( i  in   1:10  )
{
  cat( i, "\n" )
}


#----------------
#ahora, los temibles Loops Anidados

for( i  in   c( "a", "b", "c", "d" )  )
{
  for( j in 1:5 )
  {
    cat( i,j,  "\n" )
  }
}


#otra forma de verlo

for( i  in   c( "a", "b", "c", "d" )  )
{

  cat( i, " " )

  for( j in 1:5 )
  {
    cat( j,  " " )
  }

  cat("\n" )
}


#------------------------------------------------------------------------------
#apply  y derivados



