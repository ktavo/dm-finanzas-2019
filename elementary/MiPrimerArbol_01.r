#Arbol con libreria  rpart
#se trabaja con constantes para ordenar el codigo fuente

#source("M:\\R\\elementary\\MiPrimerArbol_01.r")
#limpio la memoria
rm(list=ls())
gc()

library("data.table")
library("rpart")
library("rpart.plot")

setwd("E:/UBA/2019-II/DM en Finanzas/Dropbox Prof/datasets")

#Parametros entrada
karchivo_entrada      <-  "201902.txt"
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c(kcampo_id)

#Parametros salida
karchivo_imagen       <-  "..\\work\\arbol_01_bis.jpg"

#cargo los datos
dataset <- fread(karchivo_entrada)

#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ]

# generacion del modelo
formula  <-  formula(paste(kclase_nomcampo, "~ ."))
t0       <-  Sys.time()
modelo   <-  rpart(formula,   data = dataset,   cp=0.01,   xval=0)
t1       <-  Sys.time()
tcorrida <-  as.numeric( t1 - t0, units = "secs")
print( tcorrida)

#sum( dataset[ modelo[1], ganancia] )
ftable(dataset$clase_ternaria)

#Profundidad del árbol (raíz es 0)
nodes <- as.numeric(rownames(modelo$frame))
max(rpart:::tree.depth(nodes))
summary(modelo)

#Get final node count
table(modelo$where)
typeof(table(modelo$where))

#table(modelo$where)[1]
#table(modelo$where)[2]

#El universo agrupado por  clase_ternaria
#Muestra los resultados para Baja+1 Baja+2 y Continua
ftable(dataset$clase_ternaria)
#!!!calculo la ganancia


#impresion un poco mas elaborada del arbol
jpeg(file = karchivo_imagen,  width = 6, height = 4, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

