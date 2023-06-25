############################# problem set 1 ###################################
# Autores: Sergio Jimenez
# SI ENCUENTRA ALGUN ERROR O SUGERENCIA POR FAVOR CONTACTEME
# correo: sa.jimenezo1@uniandes.edu.co
# Autores: Sergio Jimenez y Andrea Clavijo
# SI ENCUENTRA ALGUN ERROR O SUGERENCIA POR FAVOR CONTACTEME
# correo: sa.jimenezo1@uniandes.edu.co/ ay.clavijo@uniandes.edu.co
# fecha: 24/06/2023
###############################################################################

rm(list = ls()) # Limpiar Rstudio
options(scipen = 20,  digits=3) # establezco la notacion cient√≠fica y el n√∫mero de decimales
require(pacman)
# Cargo varios paquetes al tiempo
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx,
       rstudioapi, readxl, openxlsx, stargazer) 

#Cargo base preliminar de trabajo
GEIH <- read_excel("../stores/GEIH")
#pero antes vamos a convertir algunas variables a categÛricas y crear variables para la especificaciÛn de modelos con variable sno lineales:

GEIH$estrato <- factor(GEIH$estrato)
GEIH$ocupacion <- factor(GEIH$ocupacion)
GEIH <- GEIH %>%
  filter(!is.na(salario_real_hora_imputado)) %>%
  mutate(educacion_tiempo2 = educacion_tiempo^2,
         edad3 = edad^3)%>%
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
MSE_model1<-with(test,mean((log_salario_hora_imputado-model1)^2))
MSE_model1

# especificacion 2: salario ~ mujer + edad + edad2 + educacion_tiempo + ocupacion
model2<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo + as.factor(ocupacion),data=train)
summary(model2)
test$model2<-predict(model2,newdata = test)
MSE_model2<-with(test,mean((log_salario_hora_imputado-model2)^2))
MSE_model2

# especificacion 3: salario ~ mujer + edad + edad2 + educacion_tiempo  + estrato 
model3<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo + estrato,data=train)
summary(model3)
test$model3<-predict(model3,newdata = test)
MSE_model3<-with(test,mean((log_salario_hora_imputado-model3)^2))
MSE_model3

# especificacion 4: salario ~ mujer + edad + edad2 + educacion_tiempo  + estrato + estrato2
model4<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo  + estrato + estrato2,data=train)
summary(model4)
test$model5<-predict(model4,newdata = test)
MSE_model4<-with(test,mean((log_salario_hora_imputado-model5)^2))
MSE_model4

# especificacion 5: salario ~ mujer + edad + edad2 + educacion_tiempo  + estrato + (mujer*edad)
model5<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo  + estrato + mujer * edad,data=train)
summary(model5)
test$model5<-predict(model5,newdata = test)
with(test,mean((log_salario_hora_imputado-model5)^2))
MSE_model5<-with(test,mean((log_salario_hora_imputado-model5)^2))
MSE_model5

# especificacion 6: salario ~ mujer + edad + edad2 + educacion_tiempo + educacion_tiempo2 + ocupacion
model6<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo + educacion_tiempo2 + as.factor(ocupacion),data=train)
summary(model6)
test$model6<-predict(model6,newdata = test)
MSE_model6<-with(test,mean((log_salario_hora_imputado-model6)^2))
MSE_model6

# especificacion 7: salario ~ mujer + edad + edad2 + edad3 + educacion_tiempo  + estrato + estrato2
model7<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + edad3 + educacion_tiempo  + estrato + estrato2,data=train)
summary(model7)
test$model7<-predict(model7,newdata = test)
MSE_model7<-with(test,mean((log_salario_hora_imputado-model7)^2))
MSE_model7

#Se presentan las estimaciones de todos los modelos 
stargazer(model1,model2,model3,model4,model5,model6,model7, summary = TRUE, type = "text")

#Los resultados de la predicciÛn (MSE) de los modelos para los modelos previos y nuevos se condensan a continuaciÛn:
MSE_table<-c(MSE_model1, MSE_model2, MSE_model3, MSE_model4, MSE_model5,MSE_model6,MSE_model7)
x_label<-c('Modelo 1','Modelo 2', 'Modelo 3', 'Modelo 4', 'Modelo 5','Modelo 6','Modelo 7')
MSEtabla<-data.frame(Columna1 = x_label,Columna2 = MSE_table)
dataframe <- data.frame(Columna1 = MSEtabla$Columna1, Columna2 = MSEtabla$Columna2)
colnames(dataframe) <- c("Modelo", "MSE")
print(dataframe)

#Ahora se grafican los MSE de cada uno de los modelos predichos para poder compararlos
ggplot(data=MSEtabla, aes(x = x_label, y = MSE_table, group=1)) + 
  geom_line() +  
  geom_point() +
  ggtitle("MSE de los modelos especificados") +
  ylab("MSE") +
  xlab ("N˙mero de modelo")

#Posteriormente, para tener idea de cu·les modelos tienen el MSE m·s bajo
ordenMSE <- dataframe[order(dataframe$MSE), ]
View(ordenMSE)


