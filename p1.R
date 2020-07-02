# Proyecto AED

install.packages("tidyverse")
install.packages("ggplot2")

library(tidyverse)

# Casos activos #########################################
# fuente de datos: https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosActivosPorComuna.csv
# se leen los datos desde la url entregada en formato raw
raw.activos <- read.csv(url("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosActivosPorComuna.csv"))
#head(raw)

# se extraen los datos de la región del Maule
maule.activos <- raw.activos[raw.activos$Region == "Maule",]
str(maule.activos)

# se obtienen los nombres de las columnas que son fechas
# maule.colnames <- colnames(maule.activos)[6:ncol(maule.activos)]
# maule.colnames

#ggplot(data = maule.activos, aes(x=Comuna, y=colnames(maule.activos)[ncol(maule.activos)])) +
#  geom_bar(stat = "identity", width = 0.5) +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# ggplot(data = maule.activos, aes(x=c(colnames(maule.activos)[6:ncol(maule.activos)]), y=Region)) +
#   geom_bar(stat = "identity", width = 0.5) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Casos fallecidos ######################################
# fuente de datos: https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosFallecidosPorComuna.csv
raw.fallec <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosFallecidosPorComuna.csv")
maule.fallec <- raw.fallec[raw.fallec$Region == "Maule",]
maule.fallec.simple <- maule.fallec[c(3, 5, ncol(maule.fallec))]
maule.fallec.simple

# Casos acumulados ######################################
# fuente de datos: https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosAcumuladosPorComuna.csv
raw.acum <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosAcumuladosPorComuna.csv")
maule.acum <- raw.acum[raw.acum$Region == "Maule",]

vals <- data.frame("Maule", 7, "Total", "")
names(vals) <- c("Region", "Codigo.region", "Comuna", "Codigo.comuna")
totales <- mapply(sum, maule.acum[,5:ncol(maule.acum)], na.rm = TRUE)
row.totales <- c(vals, totales)
maule.acum.tot <- rbind(maule.acum, row.totales) # agregada la fila con totales




# Construcción tabla ####################################
# para la tabla son necesarias la columna Comuna (3), Población (5), Confirmados (a la última fecha), 
# incremento (dif ultima fecha - fecha anterior) tasa (última fila), que corresponde a
# los confirmados/100k habitantes
ultima.col.acum <- ncol(maule.acum.tot)-1
tabla.cols1 <- maule.acum.tot[,c(3, 5, ultima.col.acum)]
# calcular el aumento usando la penúltima columna y la anterior
tabla.cols2 <- maule.acum.tot[,ultima.col.acum] - maule.acum.tot[,ultima.col.acum-1]
tabla.cols3 <- maule.fallec.simple[,ncol(maule.fallec.simple)]
tabla.cols4 <- maule.acum.tot[, ncol(maule.acum.tot)]

tabla <- cbind(tabla.cols1,tabla.cols2, tabla.cols3, tabla.cols4)

names(tabla) <- c("Comuna", "Poblacion", "Confirmados", "Incremento", "Fallecidos", "Casos.100k")
row.names(tabla) <- c(1:nrow(tabla))
tabla
t.tabla <- t(tabla)
t.tabla.colnames <- t.tabla[1,]
as.factor(t.tabla.colnames)
t.tabla <- t.tabla[2:nrow(t.tabla),]
colnames(t.tabla) <- t.tabla.colnames # se asignan las comunas como nombre de las columnas

t.tabla

# ggplot(data = tabla, aes(fill = Fallecidos, y = Confirmados, x = Comuna)) +
#   geom_bar(position = "stack", stat = "identity", width = 0.5) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



# intento 1
ggplot(data = tabla) +
  geom_bar(mapping = aes(x = Comuna, fill = Fallecidos)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#intento 2
ggplot(data = tabla) +
  geom_bar(mapping = aes(x = Comuna, fill = Confirmados), position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

tabla

# intento 3
ggplot(data = tabla, aes(x = Comuna, y = Confirmados, fill = Fallecidos)) +
  geom_bar(position = "stack", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# intento 4
t1<- tabla[1:(nrow(tabla)-1),] # le quitamos la fila con el total

ggplot(data=t1, aes(x = Comuna, y = Confirmados, fill = Fallecidos)) +
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# internto 5 $$$$$
ggplot(data = t1, aes(x = Comuna, y = Confirmados, color = as.factor(Fallecidos))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#t.tabla2 <- as.data.frame(t.tabla)

# intento 6 con facetas
ggplot(data = t1, aes(x = Comuna, y = Confirmados, fill = Fallecidos)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~Fallecidos)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# ggplot ####################################
ggplot(data = t1, aes(x = Comuna, y = Confirmados, color = Fallecidos)) +
  geom_line()
