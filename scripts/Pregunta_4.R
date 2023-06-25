# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

options(scipen = 20,  digits=10)
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, rstudioapi, stargazer, boot, openxlsx, knitr, readxl, kableExtra) # Cargar varios paquetes al tiempo


#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Import filtered data ----------------------------------------------------

GEIH <- read_excel("../stores/GEIH") #ajustar ruta store
summary(GEIH)
GEIH$relacion_laboral[is.na(GEIH$relacion_laboral)]<-9 #reemplazamos los missings por 9
GEIH<-GEIH[complete.cases(GEIH[,c("tamaño_empresa")]),] #imputamos las personas sin información del tamaño de la empresa
names(GEIH)

# Question 4: The gender earnings GAP -------------------------------------


# Model 1 -----------------------------------------------------------------


#Model1: log(w) = β1 + β2Female + u

reg_w_fem<-lm(formula=log_salario_hora_imputado~mujer, data=GEIH) #modelo general
reg_w_fem$AIC<-AIC(reg_w_fem) #Akaike para modelo general


# Model 2: FWL ------------------------------------------------------------

#Modelo2: log(w) = β1 + β2Female + relacion_laboral + eduacion+ edad+ edad2 + tamaño empresa + u [usando FWL]

GEIH<-GEIH %>% mutate(muj_res=lm(mujer~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa),GEIH)$residuals,#capturamos los residuales de mujer
                      sal_res=lm(log_salario_hora_imputado~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+tamaño_empresa,GEIH)$residuals) #capturamos los residuales del salario

reg_fwl1<-lm(sal_res~muj_res,GEIH) #el coeficiente de mujer debería salir igual que si lo corremos como lm(log_salario_hora_imputado~mujer+edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa))

#probamos para ver si sale el mismo coeficiente
lm(log_salario_hora_imputado~mujer+edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa),GEIH)
reg_fwl1



# Results table -----------------------------------------------------------
#Con los dos modelos
stargazer(reg_w_fem, reg_fwl1, type="text",title="Tabla 4.1: Regresión Salario-Mujer", 
          dep.var.labels=c("Ln(salario)","Ln(salario)"),covariate.labels=c("Mujer","Mujer FWL"), omit.stat=c("ser","f","adj.rsq","aic"), 
          out="../views/age_fem.html", keep= c("mujer", "muj_res"),
          add.lines=list(c("AIC", round(AIC(reg_w_fem),1), round(AIC(reg_fwl1),1)),c('Variables de Control', 'No','Si')), 
          notes=c("El modelo FWL (2) ha sido calculado con las variable de control", 
          "edad, educación, ocupación y tamaño de la empresa."), notes.align="c")


# Model 3: Bootstrap and FWL ----------------------------------------------

#Utilizamos bootstrap para calcular los coeficientes y errores estándares
#Función para Bootstrap
model_fwl_boot<- function(data, index) {
  f<- lm(formula=sal_res~muj_res, data, subset=index)
  
  coefs<-f$coefficients
  b2<-coefs[2]
  
  return(b2)
}

set.seed(12345) #para que sea reproducible
model_fwl_boot(GEIH,1:nrow(GEIH)) #para verificar que nos de el mismo coeficiente de mujer

err_est_fwl_boot<-boot(GEIH,model_fwl_boot,R=1000)
err_est_fwl_boot


# Extracting Coefficients -------------------------------------------------
# Extraemos el coeficiente b2 y el error estándard
t1_stat <- err_est_fwl_boot$t0
se_t1 <- sd(err_est_fwl_boot$t)
fwl_boot<-c(t1_stat,se_t1)

#Extraemos el coeficientes de antiguas regresiones
t1_reg<-reg_w_fem$coef[2]
se_reg<-coef(summary(reg_w_fem))[, "Std. Error"][2]
reg_muj<-c(t1_reg,se_reg)

t1_fwl<-reg_fwl1$coef[2]
se_fwl<-coef(summary(reg_fwl1))[, "Std. Error"][2]
reg_fwl<-c(t1_fwl,se_fwl)  

tags<-c("Mujer","Std. Error")

#Juntamos todos los resultados de las regresiones
comparison.df<-format(data.frame(Modelo=tags,Regression=reg_muj,FWL=reg_fwl,FWL_BOOT=fwl_boot),digits=3)

# Exportar la tabla
ruta <- "../views/tabla_comp.html" 
tabla_comp <- kable(comparison.df, format = "html", align = "c", caption = "Tabla 4.2: Comparación de Modelos") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  cat(tabla_comp, file = ruta)
tabla_comp







