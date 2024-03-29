#Objetivo1:  mostrar como se divide en training y testing
#Objetivo2:  mostrar como se calcula la ganancia y el AUC
#Objetivo3:  que la clase vea la gran varianza que hay de las ganancias al utilizar distintas semillas

#Modelo con libreria  rpart
#Entreno con training y mido la ganancia en testing   (una verdadera porqueria que genera mucha varianza)



#source("M:\\R\\elementary\\MiPrimerModelo_01.r")

#limpio la memoria
rm(list=ls())
gc()


library("rpart")
library("data.table")
library("dplyr")
library("rpart.plot")
library("gtools")
library("bitops")
library("caTools")
library("gplots")
library("ROCR")


setwd("E:/UBA/2019-II/DM en Finanzas/Dropbox Prof/datasets")


#Parametros entrada
karchivo_entrada      <-  "201902.txt"
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c(kcampo_id)


ktraining_prob        <-  0.70
ksemilla_azar         <- 9123437  # poner aqui SU propia semilla aleatoria
#91237-> 4596667 

#Parametros salida
karchivo_imagen       <-  "..\\work\\modelo_01.jpg"


#constantes de la funcion ganancia del problema
kprob_corte           <-      0.025
kganancia_acierto     <-  19500 
kganancia_noacierto   <-   -500

#------------------------------------------------------
#Esta funcion calcula la ganancia de una prediccion
#Quedarse solo con las predicciones con probabilidad mayor a  kprob_corte (0.025)
#Si es un acierto  sumar  kganancia_acierto    (+19500) 
#Si NO es acierto  sumar  kganancia_noacierto  (  -500)

fmetrica_ganancia_rpart  = function(probs, clases)
{
  
  return( sum(   (probs > kprob_corte ) * 
                   ifelse(clases== kclase_valor_positivo, kganancia_acierto, kganancia_noacierto)   
  )
  )
  
}
#------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_rpart  = function(probs, clases)
{
  testing_binaria  <-  as.numeric(clases == kclase_valor_positivo )
  pred             <-  ROCR::prediction( probs, testing_binaria, label.ordering=c(0, 1))
  auc_testing      <-  ROCR::performance(pred,"auc"); 
  
  return(unlist(auc_testing@y.values))
  
}
#------------------------------------------------------


#cargo los datos
dataset <- fread(karchivo_entrada, header=TRUE, sep=kcampos_separador)

#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ] 

#Divido el dataset en training 70% y testing 30%  , usando la libreria dplyr
dataset  <- as.data.table(mutate(dataset, idtempo =  row_number()))

set.seed(ksemilla_azar )
dataset_training <- as.data.table(dataset %>%
                                    group_by(!!as.name(kclase_nomcampo)) %>%
                                    sample_frac(ktraining_prob) %>%
                                    ungroup)
dataset_testing  <- as.data.table(anti_join(dataset, dataset_training, by = "idtempo"))

dataset_training[ ,  idtempo := NULL    ] 
dataset_testing[ ,  idtempo := NULL    ] 



nrow( dataset[ clase_ternaria=='BAJA+1' , ] ) / nrow(dataset)
nrow( dataset_training[ clase_ternaria=='BAJA+1' , ] ) / nrow(dataset_training)
nrow( dataset_testing[ clase_ternaria=='BAJA+1' , ] ) / nrow(dataset_testing)


# generacion del modelo
formula  <-  formula(paste(kclase_nomcampo, "~ ."))

t0       <-  Sys.time()
modelo   <-  rpart(formula,   data = dataset_training,   cp=0.005,  xval=0)
t1       <-  Sys.time()

tcorrida <-  as.numeric( t1 - t0, units = "secs")
print( tcorrida)


#impresion un poco mas elaborada del arbol
jpeg(file = karchivo_imagen,  width = 12, height = 4, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()




#aplico el modelo a datos nuevos
testing_prediccion  <- predict( modelo, dataset_testing , type = "prob")

#calculo la ganancia en testing
ganancia_testing <-  fmetrica_ganancia_rpart(testing_prediccion[ ,kclase_valor_positivo],  dataset_testing[ , get(kclase_nomcampo)])

#normalizo la ganancia de testing
ganancia_testing_normalizada  <-  ganancia_testing / (1 - ktraining_prob)
cat("Ganancia Testing Normalizada : ", ganancia_testing_normalizada, "\n") 

#comparar en clase las ganancias que obtienen los alumnos ! 



auc_testing  <-  fmetrica_auc_rpart(testing_prediccion[ ,kclase_valor_positivo],  dataset_testing[ , get(kclase_nomcampo)])

cat("AUC Testing : ", auc_testing , "\n") 
