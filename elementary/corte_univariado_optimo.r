#Objetivo:  dado un dataset y la funcion de ganancia,  devuelve el corte optimo

#el codigo NO es generico
#se asume que la clase se llama  clase_ternaria  y que los positivos son los BAJA+2

#limpio la memoria
rm(list=ls())
gc()

setwd("E:/UBA/2019-II/DM en Finanzas/Dropbox Prof/datasets")


library("data.table")


#aqui cada alumno debe poner SUS PROPIAS SEMILLAS
ksemilla_azar1   <-  7799901
ksemilla_azar2   <-  56008937

#------------------------------------------------------------------------------
#dados un dataset y el nombre de una columna,  calcula el mejor corte para esa columna
#no hace falta modificar esta funcion

columna_mejorcorte  = function(pcolumna, pdataset)
{
 
  #calculos basicos
  universo     <-  nrow(pdataset )
  pos_total    <-  sum(pdataset$clase01 )
  neg_total    <-  universo -  pos_total
  gan_universo <-  19500*pos_total - 500*neg_total

  #calculo la ganancia de los  na's
  pos_na    <-  sum(  pdataset[ is.na( get(pcolumna) ), clase01 ],  na.rm=TRUE )
  neg_na    <-  sum(  1- pdataset[ is.na( get(pcolumna) ), clase01 ],  na.rm=TRUE  )
  gan_na    <-  19500*pos_na - 500*neg_na


  gan_universo_nonulo <-  gan_universo - gan_na

  #genero el universo de los valores NO NULOS
  univar    <-  pdataset[ !is.na( get(pcolumna) ),  c("clase01", pcolumna, "azar"),  with=FALSE ]
  universo_nonulo <- nrow(univar)

  #este es el caso en que todos los valores de la variable son nulos
  if( universo_nonulo==0 )
  {
    return(  list(  "columna"   = pcolumna,  
                    "valor"     = NA ,
                    "gan_left"  = 0 ,
                    "gan_right" = 0,
                    "gan_na"    = gan_na,
                    "gan_total" = gan_na                    
                 )  
        ) 

  }


  #ordeno creciente por  <pcolumna, azar>
  univar <- univar[ order(get(pcolumna), azar),   c("clase01", pcolumna), with=FALSE]

  gan_acum  <- cumsum( ifelse( univar$clase01, 19500, -500 ) )
  gan_mejor <- pmax( gan_acum, gan_universo_nonulo - gan_acum )

  columna <- as.vector( univar[ , get(pcolumna) ] )
  pos     <-  which.max( gan_mejor )
  val     <-  columna[pos]

  i         <-  match( val,  columna )
  pos_inf   <-  pmax( 1, i-1 ) 
  valor_inf <- columna[pos_inf]
  gan_inf   <- gan_mejor[ pos_inf ]


  i <-  pos
  while( i<=universo_nonulo  &&  columna[i]== val ) i<- i+1 ;
  pos_sup    <- i-1
  valor_sup  <- columna[pos_sup ]
  gan_sup    <- gan_mejor[ pos_sup ]

  if( gan_inf > gan_sup )
  {
    ganancia    <- gan_inf
    valor       <- valor_inf
    pos         <- pos_inf
  }else{
    ganancia    <- gan_sup
    valor       <- valor_sup
    pos         <- pos_sup
  }

  gan_left  <-  gan_acum[ pos ]
  gan_right <-  gan_universo_nonulo - gan_acum[ pos ] 
  


  return(  list(  "columna"   = pcolumna,  
                  "valor"     = valor ,
                  "gan_left"  = gan_left ,
                  "gan_right" = gan_right,
                  "gan_na"    = gan_na,
                  "gan_total" = pmax( gan_left, gan_right, gan_left+gan_na,  gan_right+gan_na)                    
               )  
        ) 
 
}
#------------------------------------------------------------------------------
#dado un dataset,  devuelve el mejor corte desde el punto de vista de la ganancia
#no hace falta modificar esta funcion


dataset_mejorcorte  = function(pdataset)
{
  #creo una variable azar que me va a ser util
  #inicializo el generador de numeros aleatorios
  set.seed( ksemilla_azar2 )
  pdataset[ , azar   := runif(nrow(pdataset)) ]


  #busco el mejor corte del dataset
  metricas <- lapply( colnames( pdataset) , columna_mejorcorte,  pdataset )
  metricas <- rbindlist( metricas )

  metricas <- metricas[ order( -gan_total ) ]
 
  #me quedo con el primero SIN ser clase01 ni clase_ternaria
  m <-  metricas[ columna!="clase01" & columna!="clase_ternaria",  ][1]

  return(  list(  "columna"   = m$columna,  
                  "valor"     = m$valor ,
                  "gan_left"  = m$gan_left ,
                  "gan_right" = m$gan_right,
                  "gan_na"    = m$gan_na,
                  "gan_total" = m$gan_total                    
               )  
        ) 

}

#------------------------------------------------------------------------------
#Aqui comienza el programa principal

#cargo el dataset
dataset <- fread("201902.txt")

#creo una clase que sea 1 cuando es BAJA+2 , y  0 en caso contrario
#esto me simplifica los calculos
dataset[ , clase01:= as.integer(clase_ternaria=="BAJA+2") ]




#-------------------------
#Divido el dataset en training 50% y testing 50%
# training es cuando particion=1,  testing cuando particion=2

set.seed(ksemilla_azar1 )

dataset[, azar :=  runif( nrow(dataset) )]

setorder( dataset,  clase01,  azar )

neg_cant <-  nrow( dataset[ clase01==0 , ] )
pos_cant <-  nrow( dataset[ clase01==1 , ] )

dataset[  , particion:= 1 ]
dataset[  (round(neg_cant/2)+1): neg_cant ,  particion:= 2 ]
dataset[  (neg_cant+1):(neg_cant+1+ round(pos_cant/2)) ,  particion:= 2 ]

nrow( dataset[ particion==1, ] )
nrow( dataset[ particion==2, ] )
#-------------------------

#agrego un campo llamado nodo_arbol
#donde se podra rastear de donde proviene ese nodo
dataset[ , nodo_arbol := "1" ]

#en este momento hay un solo nodo, el "1", que es la raiz

#hago los conteos
hacerConteo  = function()
{
  
  dataset[ , list(  train_cant = sum(  particion==1 ),
                    test_cant  = sum(  particion==2 ),
                    train_pos  = sum(  particion==1 & clase01==1 ),
                    test_pos   = sum(  particion==2 & clase01==1 ),
                    train_neg  = sum(  particion==1 & clase01==0 ),
                    test_neg   = sum(  particion==2 & clase01==0 ),
                    train_gan  = sum(  ifelse( particion==1, ifelse( clase01, 19500, -500),  NA ), na.rm=TRUE),
                    test_gan   = 2*(sum(  ifelse( particion==2, ifelse( clase01, 19500, -500),  NA ), na.rm=TRUE))
  )
  , by="nodo_arbol" ]
}
         

#-------------------------

#Empiezo la construccion del primer nivel del arbol

#primero voy a partir la raiz del arbol
#veo cual es el mejor corte en training
#nodo_arbol=="1"  estoy en la raiz
dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="1",]  )

#---- ALUMNOS   ATENCION
#---- a partir de aqui DEBEN modificar el codigo
#---- ya que les saldra cortes distintos a los mios !

#aplico el corte que se acaba de imprimir
#este es un caso simple, la columna por la que corto no tiene nulos
#si hubiera nulos,  hay que decidir si van para la derecha o la izquierda

dataset[ nodo_arbol=="1" & ttarjeta_visa<=0,  nodo_arbol:= "11" ]
dataset[ nodo_arbol=="1" & ttarjeta_visa>0 ,  nodo_arbol:= "12" ]


#termine el primer nivel del arbol, hago los conteos
hacerConteo()

#-------------------------

#Empiezo la construccion del segundo nivel del arbol

#la hoja izquierda
#nodo_arbol=="11"  estoy en la hoja izquierda raiz
dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="11", ] )

dataset[ nodo_arbol=="11" & mautoservicio<=230.37,  nodo_arbol:= "111" ]
dataset[ nodo_arbol=="11" & mautoservicio> 230.37,  nodo_arbol:= "112" ]


#la hoja derecha
#nodo_arbol=="12"  estoy en la hoja derecha raiz
dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="12", ] )

dataset[ nodo_arbol=="12" & Visa_Finiciomora<=20190211,  nodo_arbol:= "121" ]
dataset[ nodo_arbol=="12" & ( Visa_Finiciomora>20190211 | is.na(Visa_Finiciomora) ) ,  nodo_arbol:= "122" ]


#termine el segundo nivel del arbol, hago los conteos
hacerConteo()


#Pregunta
#Empiezan a notar que en testing
#el oro no es "tan oro" como en lo es training
#y que la piedra no es "tan piedra"  ?
#en este momento cada fibra de su humanidad debe estar resonando con este arbol de decision

#-------------------------

#---- ALUMNOS   ATENCION
#---- a partir de aqui DEBEN seguir cortando las hojas, agregando codigo al script
#---- ya que deben lograr un arbol con 16 hojas
#---- por favor, no actuar como un automata finito,  si a su criterio no vale la pena cortar un nodo del arbo, NO HACERLO

dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="111", ] )
#dataset[ nodo_arbol=="111" & ccajeros_propio_transacciones<=1,  nodo_arbol:= "1111" ]
#dataset[ nodo_arbol=="111" & ( ccajeros_propio_transacciones>1 | is.na(ccajeros_propio_transacciones) ) ,  nodo_arbol:= "1112" ]


dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="112", ] )
#dataset[ nodo_arbol=="112" & mcuenta_corriente_Paquete<=-5031.85,  nodo_arbol:= "1121" ]
#dataset[ nodo_arbol=="112" & ( mcuenta_corriente_Paquete>-5031.85 | is.na(mcuenta_corriente_Paquete) ) ,  nodo_arbol:= "1122" ]



dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="121", ] )
dataset[ nodo_arbol=="121" & marketing_coss_selling<=10,  nodo_arbol:= "1211" ]
dataset[ nodo_arbol=="121" & ( marketing_coss_selling>10 | is.na(marketing_coss_selling) ) ,  nodo_arbol:= "1212" ]


dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="122", ] )
dataset[ nodo_arbol=="122" & tmovimientos_ultimos90dias<=1,  nodo_arbol:= "1221" ]
dataset[ nodo_arbol=="122" & ( tmovimientos_ultimos90dias>1 | is.na(tmovimientos_ultimos90dias) ) ,  nodo_arbol:= "1222" ]

hacerConteo()

#Resultados
#--------------------------11
#--------------121--------------------122
#---------1211------1212---------1221------1222

dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="1211", ] )
dataset[ nodo_arbol=="1211" & mcuenta_corriente_Paquete<=1,  nodo_arbol:= "12111" ]
dataset[ nodo_arbol=="1211" & ( mcuenta_corriente_Paquete>1 | is.na(mcuenta_corriente_Paquete) ) ,  nodo_arbol:= "12112" ]


dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="1212", ] )
#Gan = 0

dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="1221", ] )
dataset[ nodo_arbol=="1221" & mcaja_ahorro_Paquete<=1,  nodo_arbol:= "12211" ]
dataset[ nodo_arbol=="1221" & ( mcaja_ahorro_Paquete>1 | is.na(mcaja_ahorro_Paquete) ) ,  nodo_arbol:= "12212" ]

dataset_mejorcorte( dataset[ particion==1 & nodo_arbol=="1222", ] )
dataset[ nodo_arbol=="1222" & mcheques_emitidos_rechazados<=130162.5,  nodo_arbol:= "12221" ]
dataset[ nodo_arbol=="1222" & ( mcheques_emitidos_rechazados>130162.5 | is.na(mcheques_emitidos_rechazados) ) ,  nodo_arbol:= "12222" ]

hacerConteo()

#Resultados
#------------------------------11
#-----------------121--------------------------------122
#-------1211----------------1212---------1221----------------1222
#--12111---12112---------------------12121--12122-------12221---12222



