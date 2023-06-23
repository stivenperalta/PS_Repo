############################## Problem Set 1 ###################################
# Autores: David Stiven Peralta M, Jazmine Roxana Galdos G
# Si encuentra alg?n error o tiene sugerencias por favor cont?cteme
# correo: ds.peralta@uniandes.edu.co
# fecha: 20/06/2023

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

options(scipen = 20,  digits=1)
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, rstudioapi, stargazer) # Cargar varios paquetes al tiempo


#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()


# Extracting Data ---------------------------------------------------------
my_url<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
html_code<-read_html(my_url)
GEIH <- html_code %>% html_nodes("table") %>% .[[1]]
GEIH<-GEIH %>% html_table()
head(GEIH)


# Organizing data ---------------------------------------------------------

GEIH<- subset(GEIH, select=c(directorio, secuencia_p, ingtot, ingtotes, ocu, pet, mes, age, p6210, p6210s1, depto, clase, oficio, fex_c, fex_dpto, totalHoursWorked, sex, p6050, maxEducLevel, p6426, formal, p6500, cuentaPropia, y_ingLab_m, y_ingLab_m_ha, y_salary_m, y_salary_m_hu, y_total_m, y_total_m_ha))

colnames(GEIH)[colnames(GEIH) == ""] ="index"
colnames(GEIH)[colnames(GEIH) == "p6210"] = "educ"
colnames(GEIH)[colnames(GEIH) == "p6210s1"] = "grad_educ"
colnames(GEIH)[colnames(GEIH) == "clase"] = "urbano"
colnames(GEIH)[colnames(GEIH) == "p6050"] = "parentesco_jh"
colnames(GEIH)[colnames(GEIH) == "p6426"] = "tiempo_trab"
colnames(GEIH)[colnames(GEIH) == "p6500"] = "sueldo_mens" #es sueldo por hora pero se usa esa variable solo para temas de código

GEIH$llave<- paste(GEIH$directorio, GEIH$secuencia_p)
GEIH<-as.data.frame(GEIH)

# Cleaning/Filtering data -------------------------------------------------

suel_prom_hog<-aggregate(sueldo_mens~directorio+secuencia_p, data=GEIH ,FUN=(function (x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))
suel_prom_hog$llave<- paste(suel_prom_hog$directorio, suel_prom_hog$secuencia_p)

if (is.na(GEIH$sueldo_mens)) {
  GEIH$sueldo_mens[match(suel_prom_hog$llave, GEIH$llave)]<- suel_prom_hog$sueldo_mens
}

GEIH<- GEIH[GEIH$sueldo_mens !=0,] #sacamos los 0

names(GEIH)
#missing<-is.na(GEIH$p6500)
#GEIH<-subset(GEIH, subset=!missing) #sacamos los NA


# Import filtered data ----------------------------------------------------

load("../stores/GEIH.RData")
summary(GEIH)
# Question 3- Estimating the Age-wage profile profile--------


#Model: log(w) = β1 + β2Age + β3Age2 + u

#Variable Salario:
#GEIH<-GEIH%>% mutate(logw=log(sueldo_mens))
#GEIH<-GEIH%>% mutate(age2=age^2)
#summary(GEIH$logw)
GEIH<-GEIH%>%mutate(edad2=edad^2)

reg_w_age<-lm(formula=log_salario_hora2~edad+edad2, data=GEIH, weights=fex_c)
#reg_w_age2<-lm(formula=logw~age+age2+ educ +sex +urbano+formal,data=GEIH, subset=sueldo_mens!=0,weights=fex_c)
summary(reg_w_age)

coefs_w_age<-reg_w_age$coef
b0_w_age<-coefs_w_age[1]
b1_w_age<-coefs_w_age[2]
b2_w_age<-coefs_w_age[3]

age_mean<-mean(age)
age_mea2<-mean(age2)

stargazer(reg_w_age,type="text",title="Tabla X: Resultado de la Regresión Salario-Edad", keep=c("edad","edad2"), 
          dep.var.labels="Log(salario)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq"), out="../views/age_wage1.html", add.lines=list(c('Variables de Control', 'No')))



#Prediction
GEIH$yhat1<-ifelse(GEIH$log_salario_hora2!=0, predict(reg_w_age), 0)
#GEIH$yhat2<-ifelse(GEIH$log_salario_hora2!=0, predict(reg_w_age2), 0)


summ_GEIH <- GEIH %>%  
  group_by(
    edad, edad2
  ) %>%  
  summarize(
    mean_y = mean(log_salario_hora2),
    yhat_reg = mean(yhat1), .groups="drop"
  ) 

ggplot(summ_GEIH) + 
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




# Question 4: The gender earnings GAP -------------------------------------

#Model1: log(w) = β1 + β2Female + u




