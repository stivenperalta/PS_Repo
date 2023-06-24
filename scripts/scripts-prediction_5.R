############################# problem set 1 ###################################
# Autores: Sergio Jimenez
# SI ENCUENTRA ALGUN ERROR O SUGERENCIA POR FAVOR CONTACTEME
# correo: sa.jimenezo1@uniandes.edu.co
# fecha: 24/06/2023
###############################################################################

rm(list = ls()) # Limpiar Rstudio
options(scipen = 20,  digits=3) # establezco la notacion científica y el número de decimales
require(pacman)
# Cargo varios paquetes al tiempo
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx,
       rstudioapi, readxl, openxlsx, stargazer) 

#Cargo base preliminar de trabajo
GEIH <- read_excel("../stores/GEIH")
GEIH <- GEIH %>%
  filter(!is.na(salario_real_hora_imputado)) %>%
  mutate(estrato2 = estrato^2,
         educacion_tiempo2 = educacion_tiempo^2,
         edad3 = edad^3) %>%
  rename(mujer=sexo)

#ESTADISTICAS DESCRIPTIVAS
stargazer(data.frame(GEIH), header=FALSE, type='text',title="Variables en el Data Set")

###############################################################################
######################### PUNTO 5 - PREDICCION ################################

#generacion de grupo train - test
#Definimos semilla para hacerlo reproducible
set.seed(777)

#usar 70% del dataset como entrenamiento y 30% como test
sample <- sample(c(TRUE, FALSE), nrow(GEIH), replace=TRUE, prob=c(0.7,0.3))
head(sample)

train  <- GEIH[sample, ] 
test   <- GEIH[!sample, ] 

###############################################################################
#   Especificacion de modelos
##############################################################################

# especificacion 1: salario ~ edad + edad2
model1<-lm(log_salario_hora_imputado~edad + edad2,data=train)
summary(model1)
test$model1<-predict(model1,newdata = test)
with(test,mean((log_salario_hora_imputado-model1)^2))

# especificacion 2: salario ~ mujer + edad + edad2 + educacion_tiempo + ocupacion
model2<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo + as.factor(ocupacion),data=train)
summary(model2)
test$model2<-predict(model2,newdata = test)
with(test,mean((log_salario_hora_imputado-model2)^2))

# especificacion 3: salario ~ mujer + edad + edad2 + educacion_tiempo  + estrato 
model3<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo  + estrato,data=train)
summary(model3)
test$model3<-predict(model3,newdata = test)
with(test,mean((log_salario_hora_imputado-model3)^2))

# especificacion 4: salario ~ mujer + edad + edad2 + educacion_tiempo  + estrato + estrato2
model4<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo  + estrato + estrato2,data=train)
summary(model4)
test$model5<-predict(model4,newdata = test)
with(test,mean((log_salario_hora_imputado-model5)^2))

# especificacion 5: salario ~ mujer + edad + edad2 + educacion_tiempo  + estrato + (mujer*edad)
model5<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo  + estrato + mujer * edad,data=train)
summary(model5)
test$model5<-predict(model5,newdata = test)
with(test,mean((log_salario_hora_imputado-model5)^2))

# especificacion 6: salario ~ mujer + edad + edad2 + educacion_tiempo + educacion_tiempo2 + ocupacion
model6<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo + educacion_tiempo2 + as.factor(ocupacion),data=train)
summary(model6)
test$model6<-predict(model6,newdata = test)
with(test,mean((log_salario_hora_imputado-model6)^2))

# especificacion 7: salario ~ mujer + edad + edad2 + edad3 + educacion_tiempo  + estrato + estrato2
model7<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + edad3 + educacion_tiempo  + estrato + estrato2,data=train)
summary(model7)
test$model7<-predict(model7,newdata = test)
with(test,mean((log_salario_hora_imputado-model7)^2))

