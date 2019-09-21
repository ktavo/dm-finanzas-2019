#Objetivo:  mostrar ganancia de predicados


#install.packages('data.table', dependencies=TRUE, repos='http://cran.rstudio.com/')
rm(list=ls())

library('data.table')


setwd("E:/UBA/2019-II/DM en Finanzas/Dropbox Prof/datasets")

#cargo los datos
dataset  <-  fread("201902.txt")


#El universo agrupado por  clase_ternaria
#Muestra los resultados para Baja+1 Baja+2 y Continua
ftable(dataset$clase_ternaria)


#!!!calculo la ganancia
dataset[  , ganancia:= -500 ]
dataset[ clase_ternaria=='BAJA+2' ,   ganancia:= 19500]
sum( dataset$ganancia )


#!!!Conteo de la clase
dataset[  , clase:= 0 ]
dataset[ clase_ternaria=='BAJA+2' ,   clase:= 1]
sum( dataset$clase )


#Otra forma de ver el universo
#Muestra igual Baja+1 baja+2 y Continua
ftable(dataset[  ,clase_ternaria])

#Basico
summary( dataset)
by(dataset, dataset$clase_ternaria, summary)


#Corte por edad
hist(dataset[ ,cliente_edad] )
boxplot(cliente_edad  ~ clase_ternaria,  data=dataset)

#Menor de 33
ftable(dataset[ cliente_edad <=33, clase_ternaria])
#Mayor a 33
ftable(dataset[ cliente_edad  >33, clase_ternaria])



#Ganancia (Ganancia objetivo -> 21'157.500)
sum( dataset[ cliente_edad <=33, ganancia] )
sum( dataset[ cliente_edad >33, ganancia] )
sum( dataset[ mcuentas_saldo <= -120000, ganancia] )
sum( dataset[ Visa_mconsumototal > 200000, ganancia] )
sum( dataset[ ttarjeta_visa == 0, ganancia] )
sum( dataset[ ttarjeta_master == 0, ganancia] )
sum( dataset[ Visa_cuenta_estado == 11, ganancia] )
sum( dataset[ Visa_cuenta_estado == 19, ganancia] )


#Lift (Lift objetivo -> 173.14)
(sum( dataset[ cliente_edad <=33, clase]) / nrow( dataset[ cliente_edad <=33, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))
(sum( dataset[ cliente_edad >33, clase]) / nrow( dataset[ cliente_edad >33, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))
(sum( dataset[ mcuentas_saldo <= -120000, clase]) / nrow( dataset[ mcuentas_saldo <= -120000, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))
(sum( dataset[ Visa_mconsumototal > 200000, clase]) / nrow( dataset[ Visa_mconsumototal > 200000, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))
(sum( dataset[ ttarjeta_visa == 0, clase]) / nrow( dataset[ ttarjeta_visa == 0, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))
(sum( dataset[ ttarjeta_master == 0, clase]) / nrow( dataset[ ttarjeta_master == 0, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))
(sum( dataset[ Visa_cuenta_estado == 11, clase]) / nrow( dataset[ Visa_cuenta_estado == 11, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))
(sum( dataset[ Visa_cuenta_estado == 19, clase]) / nrow( dataset[ Visa_cuenta_estado == 19, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))


	
#Corte por mcuentas_saldo
hist(dataset[ ,mcuentas_saldo], xlim=c(0,12000000) )
boxplot(mcuentas_saldo  ~ clase_ternaria, data=dataset)
boxplot(mcuentas_saldo  ~ clase_ternaria, data=dataset, outline=FALSE)

ftable(dataset[ mcuentas_saldo <= -120000, clase_ternaria])
ftable(dataset[ mcuentas_saldo >  -120000, clase_ternaria])




#Corte por Visa_mconsumototal
hist(dataset[ ,Visa_mconsumototal] )
boxplot(Visa_mconsumototal  ~ clase_ternaria, data=dataset, outline=FALSE)

ftable(dataset[ Visa_mconsumototal <= 20000, clase_ternaria])
ftable(dataset[ Visa_mconsumototal >  20000, clase_ternaria])
ftable(dataset[ is.na(Visa_mconsumototal)  , clase_ternaria])



#Tabla de contingencia , ya que  ttarjeta_visa  es una variable DISCRETA
table( dataset$ttarjeta_visa,  dataset$clase_ternaria, useNA = 'ifany')

dataset[, list( gan=sum(ganancia), lift= (sum(clase)/(.N))/(1085/187861) ), by = ttarjeta_visa]



#Tabla de contingencia , ya que  Visa_cuenta_estado  es una variable DISCRETA
table( dataset$Visa_cuenta_estado,  dataset$clase_ternaria, useNA = 'ifany')

dataset[, list( gan=sum(ganancia), lift= (sum(clase)/(.N))/(1085/187861) ), by = Visa_cuenta_estado]



#Tabla de contingencia , ya que  Master_cuenta_estado  es una variable DISCRETA
table( dataset$Master_cuenta_estado,  dataset$clase_ternaria, useNA = 'ifany')


dataset[, list( gan=sum(ganancia), lift= (sum(clase)/(.N))/(1085/187861) ), by = Master_cuenta_estado]




#algo un poco mas avanzado 
#install.packages('caret', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(ggplot2)
library(caret)

d2 <- as.data.table(downSample( dataset, as.factor(dataset$clase_ternaria), list = FALSE, yname = "clase_ternaria2"))

d3 <-  d2[Visa_mconsumototal < 120000 & mcuentas_saldo < 120000, ]


#C(BAJA+1->green,BAJA+2->red, CONTINUA->blue)
my_cols <- c("#00FF00", "#FF0000", "#0000FF")

pairs(d3[ , c("mcuentas_saldo", "Visa_mconsumototal") ], pch = 19,  cex = 1,
       col = my_cols[ as.factor(d3$clase_ternaria) ],
      lower.panel=NULL)

