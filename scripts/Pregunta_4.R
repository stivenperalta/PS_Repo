# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

options(scipen = 20,  digits=10)
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, rstudioapi, stargazer, boot, openxlsx, knitr) # Cargar varios paquetes al tiempo


#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Import filtered data ----------------------------------------------------

GEIH <- read_excel("../stores/GEIH")
summary(GEIH$log_salario_hora)
names(GEIH)
GEIH<-GEIH %>% rename (mujer="sexo")
GEIH<-GEIH[!is.na(GEIH$log_salario_hora),] #para poder correr todo el código


# Question 4: The gender earnings GAP -------------------------------------

#Model1: log(w) = β1 + β2Female + u

reg_w_fem<-lm(formula=log_salario_hora~mujer, data=GEIH) #modelo general
reg_w_fem2<-lm(formula=log_salario_hora~mujer, subset=mujer==1, data=GEIH) #modelo con controles
reg_w_fem3<-lm(formula=log_salario_hora~edad+edad2, subset=mujer==0, data=GEIH) #modelo con mas controles

reg_w_age$AIC<-AIC(reg_w_age) #Akaike para modelo general
reg_w_age_mujer$AIC<-AIC(reg_w_age_mujer) #Akaike para modelo mujeres
reg_w_age_hombre$AIC<-AIC(reg_w_age_hombre) #Akaike para modelo hombres

#Con los tres modelos
stargazer(reg_w_age, reg_w_age_mujer, reg_w_age_hombre, type="text",title="Tabla 3.1: Regresión Salario-Edad", keep=c("edad","edad2"), 
          dep.var.labels="Log(salario)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq","aic"), out="../views/age_wage2.html",
          add.lines=list(c("AIC", round(AIC(reg_w_age),1), round(AIC(reg_w_age_mujer),1), round(AIC(reg_w_age_hombre),1))))



