############################## Problem Set 1 ###################################
# Autores: David Stiven Peralta M, Jazmine Roxana Galdos G
# Si encuentra alg?n error o tiene sugerencias por favor cont?cteme
# correo: ds.peralta@uniandes.edu.co
# fecha: 20/06/2023

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

options(scipen = 20,  digits=5)
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, rstudioapi, stargazer, boot, openxlsx) # Cargar varios paquetes al tiempo


#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Import filtered data ----------------------------------------------------

GEIH <- read_excel("../stores/GEIH")
summary(GEIH)
names(GEIH)

# Question 3- Estimating the Age-wage profile profile--------


#Model: log(w) = β1 + β2Age + β3Age2 + u

reg_w_age<-lm(formula=log_salario_hora~edad+edad2, data=PREGUNTA3)
reg_w_age_mujer<-lm(formula=log_salario_hora~edad+edad2, subset=sexo==0, data=PREGUNTA3)
reg_w_age_hombre<-lm(formula=log_salario_hora~edad+edad2, subset=sexo==1, data=PREGUNTA3)

#reg_w_age<-lm(formula=log_salario_hora~edad+edad2, data=PREGUNTA3, weights=fex_c)
#reg_w_age2<-lm(formula=logw~age+age2+ educ +sex +urbano+formal,data=PREGUNTA3, subset=sueldo_mens!=0,weights=fex_c)
summary(reg_w_age)

##AGREGAR MUJER Y HOMBRE EN TABLA- AIC BIC
stargazer(reg_w_age,type="text",title="Tabla X: Resultado de la Regresión Salario-Edad", keep=c("edad","edad2"), 
          dep.var.labels="Log(salario)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq"), out="../views/age_wage1.html", add.lines=list(c('Variables de Control', 'No')))

coefs_w_age<-reg_w_age$coef
b1_w_age<-coefs_w_age[1]
b2_w_age<-coefs_w_age[2]
b3_w_age<-coefs_w_age[3]

edad_mean<-mean(PREGUNTA3$edad)
edad_mea2<-mean(PREGUNTA3$edad2)

#Predict yhat
PREGUNTA3$yhat<-ifelse(PREGUNTA3$log_salario_hora2!=0, predict(reg_w_age), 0)
#PREGUNTA3$yhat<-predict(reg_w_age2)


#Model in sample fit #EXTRA
summ_PREGUNTA3 <- PREGUNTA3 %>%  
  group_by(
    edad, edad2
  ) %>%  
  summarize(
    mean_y = mean(log_salario_hora),
    yhat_reg = mean(yhat), .groups="drop"
  ) 

ggplot(summ_PREGUNTA3) + 
  geom_point(
    aes(x = edad, y = mean_y),
    color = "5", size = 2
  ) + 
  geom_line(
    aes(x = edad, y = yhat_reg), 
    color = "8", size = 1.5
  ) + 
  labs(
    title = "Log Sueldo por Edad",
    x = "Edad",
    y = "Log Sueldos"
  ) +
  theme_bw()

#Model: log(w) = β1 + β2Age + β3Age2 + u
#Prediction

edad_max<- (-b2_w_age/(2*b3_w_age))
edad_max

#standard errors

model_wage_age_fn<- function(data, index, edad_barra=mean(edad)) {
                    f<- lm(formula=log_salario_hora~edad+edad2, data, subset=index)
                    
                    coefs<-f$coefficients
                    b2<-coefs[2]
                    b3<-coefs[3]
                    
                    edad_max_bt<--(b2_w_age/(2*b3_w_age))
                    return(edad_max_bt)
}

model_wage_age_fn(PREGUNTA3,1:nrow(PREGUNTA3))

err_est_wage_age<-boot(PREGUNTA3,model_wage_age_fn,R=1000)
err_est_wage_age

g3 <- ggplot(db, aes(x=edad, y=predic2a)) + 
  geom_point(col = "red" , size = 1.5) +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='brown1') +
  theme_light()


# Question 4: The gender earnings GAP -------------------------------------

#Model1: log(w) = β1 + β2Female + u




