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
table1 <- html_code %>% html_nodes("table") %>% .[[1]]
table1<-table1 %>% html_table()
head(table1)


# Organizing data ---------------------------------------------------------

table1<- subset(table1, select=c(directorio, secuencia_p, ingtot, ingtotes, ocu, pet, mes, age, p6210, p6210s1, depto, clase, oficio, fex_c, fex_dpto, totalHoursWorked, sex, p6050, maxEducLevel, p6426, formal, p6500, cuentaPropia, y_ingLab_m, y_ingLab_m_ha, y_salary_m, y_salary_m_hu, y_total_m, y_total_m_ha))

colnames(table1)[colnames(table1) == ""] ="index"
colnames(table1)[colnames(table1) == "p6210"] = "educ"
colnames(table1)[colnames(table1) == "p6210s1"] = "grad_educ"
colnames(table1)[colnames(table1) == "clase"] = "urbano"
colnames(table1)[colnames(table1) == "p6050"] = "parentesco_jh"
colnames(table1)[colnames(table1) == "p6426"] = "tiempo_trab"
colnames(table1)[colnames(table1) == "p6500"] = "sueldo_mens"

table1$llave<- paste(table1$directorio, table1$secuencia_p)
table1<-as.data.frame(table1)

# Cleaning/Filtering data -------------------------------------------------

suel_prom_hog<-aggregate(sueldo_mens~directorio+secuencia_p, data=table1 ,FUN=(function (x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))
suel_prom_hog$llave<- paste(suel_prom_hog$directorio, suel_prom_hog$secuencia_p)

if (is.na(table1$sueldo_mens)) {
  table1$sueldo_mens[match(suel_prom_hog$llave, table1$llave)]<- suel_prom_hog$sueldo_mens
}

table1<- table1[table1$sueldo_mens !=0,] #sacamos los 0

names(table1)
#missing<-is.na(table1$p6500)
#table1<-subset(table1, subset=!missing) #sacamos los NA


# Question 3- Estimating the Age-wage profile profile--------


#Model: log(w) = β1 + β2Age + β3Age2 + u

#Variable Salario:
table1<-table1%>% mutate(logw=log(sueldo_mens))
table1<-table1%>% mutate(age2=age^2)
summary(table1$logw)

reg_w_age<-lm(formula=logw~age+age2, data=table1, subset=sueldo_mens!=0,weights=fex_c)
reg_w_age2<-lm(formula=logw~age+age2+ educ +sex +urbano+formal,data=table1, subset=sueldo_mens!=0,weights=fex_c)

stargazer(reg_w_age, reg_w_age2,type="text", keep=c("age","age2") ,title="Tabla X: Resultado de la Regresión Salario-Edad", 
          dep.var.labels="Log(salario)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq"), out="../views/age_wage1.html", add.lines=list(c('Variables de Control', 'No','Si')),notes="Las variables de control incluyen educación, género, urbano rural e informal", notes.align = "l")

#Prediction
table1$yhat1<-ifelse(table1$sueldo_mens!=0, predict(reg_w_age), 0)
table1$yhat2<-ifelse(table1$sueldo_mens!=0, predict(reg_w_age2), 0)


summ_table1 <- table1 %>%  
  group_by(
    age, age2
  ) %>%  
  summarize(
    mean_y = mean(logw),
    yhat_reg = mean(yhat1), .groups="drop"
  ) 

ggplot(summ_table1) + 
  geom_point(
    aes(x = age, y = mean_y),
    color = "5", size = 2
  ) + 
  geom_line(
    aes(x = age, y = yhat_reg), 
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




