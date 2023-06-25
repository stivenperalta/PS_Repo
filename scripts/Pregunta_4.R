# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

options(scipen = 20,  digits=10)
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, rstudioapi, stargazer, boot, openxlsx, knitr, readxl) # Cargar varios paquetes al tiempo


#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Import filtered data ----------------------------------------------------

GEIH <- read_excel("../stores/GEIH") #ajustar ruta store
summary(GEIH)
GEIH$relacion_laboral[is.na(GEIH$relacion_laboral)]<-9 #reemplazamos los missings por 9
GEIH$tamaño_empresa[is.na(GEIH$tamaño_empresa)]<-1
names(GEIH)

# Question 4: The gender earnings GAP -------------------------------------

#Model1: log(w) = β1 + β2Female + u

reg_w_fem<-lm(formula=log_salario_hora_imputado~mujer, data=GEIH) #modelo general
reg_w_fem$AIC<-AIC(reg_w_fem) #Akaike para modelo general


#Modelo2: log(w) = β1 + β2Female + relacion_laboral + eduacion+ edad+ edad2 + tamaño empresa + u [usando FWL]

GEIH<-GEIH %>% mutate(muj_res=lm(mujer~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+tamaño_empresa,GEIH)$residuals,#capturamos los residuales de mujer
                      sal_res=lm(log_salario_hora_imputado~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+tamaño_empresa,GEIH)$residuals) #capturamos los residuales del salario

reg_fwl1<-lm(sal_res~muj_res,GEIH) #el coeficiente de mujer debería salir igual que si lo corremos como lm(log_salario_hora_imputado~mujer+edad+edad2+educacion_tiempo+as.factor(relacion_laboral))

#Con los dos modelos
stargazer(reg_w_fem, reg_fwl1, type="text",title="Tabla 4.1: Regresión Salario-Mujer", 
          dep.var.labels=c("Ln(salario)","Ln(salario)"),covariate.labels=c("Mujer","Mujer FWL"),omit.stat=c("ser","f","adj.rsq","aic"), out="../views/age_fem.html",
          add.lines=list(c("AIC", round(AIC(reg_w_fem),1), round(AIC(reg_fwl1),1)),c('Variables de Control', 'No','Si')), 
          notes=c("El modelo FWL (2) ha sido calculado con las variable de control", 
          "edad, educación, ocupación y tamaño de la empresa."), notes.align="c")

#Utilizamos bootstrap para calcular los coeficientes y errores estándares

set.seed(12345)
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

se_fwl<- apply(err_est_fwl_boot$t,2,sd)[1] #grabamos el valor del error estándar en el objeto se

# tabla manual con resultados de bootstrap
bootstrap_table <- data.frame(
  estimador = -0.01926,
  bias = -0.00007,
  std.error = 0.01159)

bootstrap_table

# Exportar la tabla
ruta <- "../views/fwl_table.html" 
fwl_boot <- kable(bootstrap_table, format = "html", align = "c", caption = "Estimacion Bootstrap FWL") %>%
  kable_styling() %>%
  row_spec(1, bold = FALSE)
cat(fwl_boot, file = ruta)






