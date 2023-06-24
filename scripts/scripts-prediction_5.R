rm(list = ls()) # Limpiar Rstudio
options(scipen = 20,  digits=3) # establezco la notacion científica y el número de decimales
require(pacman)
# Cargo varios paquetes al tiempo
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx,
       rstudioapi, readxl, openxlsx, stargazer) 

#Cargo base preliminar de trabajo
GEIH <- read_excel("GEIH")

GEIH <- GEIH %>%
  filter(!is.na(salario_real_hora_imputado))

#ESTADISTICAS DESCRIPTIVAS
stargazer(data.frame(GEIH), header=FALSE, type='text',title="Variables en el Data Set")

#DISTRIBUCION DEL SALARIO
plot(density(GEIH$log_salario_hora_imputado))

#generacion de grupo train - test
#Definimos semilla para hacerlo reproducible
set.seed(777)

#usar 70% del dataset como entrenamiento y 30% como test
sample <- sample(c(TRUE, FALSE), nrow(GEIH), replace=TRUE, prob=c(0.7,0.3))
head(sample)

train  <- GEIH[sample, ] 
test   <- GEIH[!sample, ] 

#generacion de 5 especificaciones

##Se incluyen aqui los modelos de los otros puntos


