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
GEIH$relacion_laboral <- factor(GEIH$relacion_laboral)
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
#   EspecificaciÛn de modelos
##############################################################################

# especificaciÛn 1: salario ~ edad + edad2
model1<-lm(log_salario_hora_imputado~edad + edad2,data=train)
summary(model1)
test$model1<-predict(model1,newdata = test)
MSE_model1<-with(test,mean((log_salario_hora_imputado-model1)^2))
MSE_model1

# especificaciÛn 2: salario ~ mujer 
model2<-lm(log_salario_hora_imputado~mujer,data=train)
summary(model2)
test$model2<-predict(model2,newdata = test)
MSE_model2<-with(test,mean((log_salario_hora_imputado-model2)^2))
MSE_model2

# especificaciÛn 3: salario ~ mujer+edad2+educacion_tiempo+relacion_laboral+tamaÒo_empresa

model3<-lm(log_salario_hora_imputado~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+tamaÒo_empresa,data=train)
summary(model3)
test$model3<-predict(model3,newdata = test)
MSE_model3<-with(test,mean((log_salario_hora_imputado-model3)^2))
MSE_model3

# especificaciÛn 4: salario ~ mujer + edad + edad2 + educacion_tiempo + relaciÛn laboral
model4<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo + as.facto(relacion_laboral),data=train)
summary(model4)
test$model4<-predict(model4,newdata = test)
MSE_model4<-with(test,mean((log_salario_hora_imputado-model4)^2))
MSE_model4

# especificacion 5: salario ~ mujer + edad + edad2 + educacion_tiempo  + estrato + (InteracciÛn entre edad y gÈnero) 
model5<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo  + estrato + edad * mujer,data=train)
summary(model5)
test$model5<-predict(model5,newdata = test)
MSE_model5<-with(test,mean((log_salario_hora_imputado-model5)^2))
MSE_model5

# especificacion 6: salario ~ mujer + edad + edad2 + educacion_tiempo  + (quitando estrato y dejando interacciÛn entre edad y gÈnero)
model6<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo + mujer * edad,data=train)
summary(model6)
test$model6<-predict(model6,newdata = test)
with(test,mean((log_salario_hora_imputado-model6)^2))
MSE_model6<-with(test,mean((log_salario_hora_imputado-model6)^2))
MSE_model6

# especificacion 7: salario ~ mujer + edad +  educacion_tiempo (InteracciÛn entre edad y gÈnero, quitando edad^2)
model7<-lm(log_salario_hora_imputado~ mujer + edad + educacion_tiempo + edad * mujer,data=train)
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

# De todos los modelos presentados, los que tienen un menor MSE son los modelos nuevos X  Es decir, en donde se tiene un mejor performance en la predicciÛn. 
# Sin embargo, los MSE son muy similares a los de los dem·s modelos, especialmente el modelo previo X a y los nuevos X, X y X. 

# Apalancamiento
install.packages("caret")

alpha <- c()
u <- c()
h <- c()

#El modelo con menor error cuadr·tico medio se calcula nuevamente #AquÌ dependiendo de lo que nos de en los modelos se debe estimar nuevamente
bestmodel<-lm(lningresoh ~ sex + age + age2 + educ + lnexperp + relab + estrato1, data = test_data)

#Calcular el leverage para el modelo con el menor MSE
alphass <- c()
for (j in 1:nrow(test_data)) {
  u_j <- bestmodel$residual[j]
  h_j <- lm.influence(bestmodel)$hat[j]
  alpha <- u_j/(1-h_j)
  alphass <- c(alphass, alpha)
} 



#Teniendo en cuenta que es posible que un leverage mayor a 1 o menor que -1 se podrÌa considerar alto, se calcula lo siguiente:
alphass<-data.frame(alphass)
leverage<-alphass[alphass$alphass>=1|alphass<=-1,]
leverage<-data.frame(leverage)
lvpercentage<-((nrow(leverage)/nrow(alphass)*100))
xlabel_alpha<-1:nrow(test_data)
xlabel_alpha<-data.frame(xlabel_alpha)
alphass<-cbind(alphass, xlabel_alpha)
view(lvpercentage)

# Se grafican los resultados obtenidos
ggplot(data=alphass, aes(x = xlabel_alpha, y = alphass, group=1)) + 
  geom_point() + 
  ggtitle("Leverage para el modelo con mejor mÈtrica de MSE")

#Se consultan los valores m·ximos y mÌnimos
max(alphass$alphass)
min(alphass$alphass)


# LOOCV para el modelo con mejor performance predictivo, es decir, el mnX (FALTA DEFINIRLO)
GEIHSO$lnexperp <- log(GEIHSO$experp)

modelLOOCV1 <- train(lningresoh ~ sex + age + age2 + educ + lnexperp + relab + estrato1, 
                     data = GEIHSO,
                     method = "lm",
                     trControl = trainControl(method = "LOOCV"))

# Resultados 
modelLOOCV1

RMSE_modelLOOCV1<-modelLOOCV1$results
RMSE_modelLOOCV1<-RMSE_modelLOOCV1$RMSE
RMSE_modelLOOCV1<-mean(RMSE_modelLOOCV1)

view(RMSE_modelLOOCV1)
# 0.6448116

# LOOCV para el segundo modelo con mejor performance predictivo, es decir, el mn3 (FALTA DEFINIRLO)
modelLOOCV2 <- train(lningresoh ~ sex + age + educ + experp + I(experp^2) + relab + estrato1, 
                     data = GEIHSO,
                     method = "lm",
                     trControl = trainControl(method = "LOOCV"))

# Resultados 
modelLOOCV2

RMSE_modelLOOCV2<-modelLOOCV2$results
RMSE_modelLOOCV2<-RMSE_modelLOOCV2$RMSE
RMSE_modelLOOCV2<-mean(RMSE_modelLOOCV2)

view(RMSE_modelLOOCV2)
# 0.6469727



