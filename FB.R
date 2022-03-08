
#librerias a usar

library(RMySQL)
library(dplyr)
library(TSA)
library(ggplot2)
library(lubridate)

#Conexion con la base se datos 
MyDataBase <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "ftes_brot",
  host = "127.0.0.1",
  username = "root",
  password = "370067")
dbListTables(MyDataBase) # muestra las tablas de la base de datos
dbListFields(MyDataBase, 'titular')#ver campos de la tabla titular



#Primero queremos saber cuantos trabajadores activos se encuentran registados 
#en la clinica FB

DataDB <- dbGetQuery(MyDataBase, "select FECHA_NACIMIENTO, APELLIDO_PATERNO, APELLIDO_MATERNO, NOMBRE, GENERO, COLONIA, TIPO_DE_DIRECTO, REGIMEN, BENEFICIARIOS_REG from tItular where ESTATUS = 'VIGENTE' AND CAP = 'NO' ")

#Filtro para traer trabajadores activos y los agrupamos por genero

ta<- DataDB %>% filter(TIPO_DE_DIRECTO== "TRABAJADOR") %>% group_by(GENERO) %>% summarise(total= n())

#vemos grafica de barras

barplot(height = ta$total, names = ta$GENERO, col = c("red", "green"), ylab = "Genero", xlab = "Frecuencia", main = "Relaci髇 trabajadores derechohabientes", sub= "CMF FB"
       ,horiz = TRUE)



#Creamos una serie de tiempo para ver cuando 
#se dieron de alta ante el issste los trabajadores

alta <- dbGetQuery(MyDataBase, "select ALTA1, count(ALTA1) AS ACTIVOS from titular GROUP BY ALTA1 ORDER BY ALTA1")
View(alta)


DB<-alta %>% filter(ALTA1 >= 2000/01/01 ) 
View(DB) 

activo<-DB  %>% group_by(year(ALTA1),month(month(ALTA1)))%>% summarise(total=sum(ACTIVOS))                         


alta <- ts(activo[, 3], start = 2000, freq = 12)
alta
class(alta)


plot(alta, ylab = "CANTIDAD", xlab = "Tiempo", 
     main = "CANTIDAD DE ALTAS DE TRABAJADOR", 
     sub = "ISSSTE FB")
#points(y = alta, x = time(alta),
#       pch = as.vector(season(alta)))


#Realizamos una descomposicion aditiva para ver tendencia, 
#estacionalidad y ruido

alta.decom <- decompose(alta)

plot(alta.decom, xlab = "Tiempo", 
     sub = "Descomposici贸n de los datos de alta de trabajadores")

#Sacamos las componentes

Tendencia <- alta.decom$trend
Estacionalidad <- alta.decom$seasonal
Aleatorio <- alta.decom$random

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de altas de trabajadores", 
        ylab = "alta de trabajadores", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")


# Funci贸n para buscar un "buen" modelo (no basarse 煤nicamente en los resultados de aplicar la funci贸n)

get.best.arima <- function(x.ts, maxord = c(1, 1, 1, 1, 1, 1)){
  best.aic <- 1e8
  n <- length(x.ts)
  for(p in 0:maxord[1])for(d in 0:maxord[2])for(q in 0:maxord[3])
    for(P in 0:maxord[4])for(D in 0:maxord[5])for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p, d, q),
                   seas = list(order = c(P, D, Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2*fit$loglik + (log(n) + 1)*length(fit$coef)
      if(fit.aic < best.aic){
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p, d, q, P, D, Q)
      }
    }
  list(best.aic, best.fit, best.model)
}

# Nuevo ajuste a los datos de la serie transformada de producci贸n 
# de electricidad

best.arima <- get.best.arima(log(alta),
                                  maxord = c(2, 2, 2, 2, 2, 2))

best.fit <- best.arima[[2]]  # Modelo
best.arima[[3]] # Tipo de modelo (贸rdenes)
best.fit
best.arima[[1]] # AIC
###

# ACF para residuales del ajuste

acf(resid(best.fit), main = "")
title(main = "Correlograma de los residuales del ajuste")

###
# Predicci贸n

pr <- predict(best.fit, 60)$pred 
ts.plot(cbind(window(alta, start = 2000),
              exp(pr)), col = c("blue", "red"), xlab = "")
title(main = "Predicci贸n para la serie alta de trabajadores",
      xlab = "a駉",
      ylab = "alta de trabajadores")





#Ahora quiero ver cuantos derechohabientes pensionados y trabajadores se tienen por colonia
dbListFields(MyDataBase, 'titular')#ver campos de la tabla titular
DataDB1 <- dbGetQuery(MyDataBase, "select COLONIA from titular where CAP='NO' AND ESTATUS = 'VIGENTE' AND TIPO_DE_DIRECTO")

TOTAL <- DataDB1 %>% group_by(COLONIA) %>% count(COLONIA,sort=TRUE)
View(TOTAL)
#DataDB1 <- dbGetQuery(MyDataBase, "select COLONIA, count(COLONIA) as TOTAL from titular where CAP = 'NO' group by COLONIA order by total desc ")


dev.off()

#nos desconectamos de la bdd
dbDisconnect(MyDataBase)

