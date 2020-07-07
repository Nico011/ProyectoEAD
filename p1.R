# Proyecto AED

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gridExtra")

library(tidyverse)
library(gridExtra)

setwd(getwd()) # archivos serán guardados en el directorio de trabajo.
# Son 2 archivos de salida: uno con la tabla que contiene la información de la región del Maule,
# el otro contiene los distintos gráficos de barras y líneas.
# Ejecutar getwd() para conocer el directorio de trabajo.

# Info #################
# Licencia: https://github.com/MinCiencia/Datos-COVID19/blob/master/LICENSE
# Fuentes: Ministerio de Ciencias, Ministerio de Salud
# Fuente de datos: (https://github.com/MinCiencia/Datos-COVID19)


# Casos activos #########################################
# Fuente de datos: Informe epidemiológico
# https://github.com/MinCiencia/Datos-COVID19/blob/master/input/InformeEpidemiologico/CasosActivosPorComuna.csv
# se leen los datos desde la url entregada en formato raw
raw.activos <- read.csv(url("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosActivosPorComuna.csv"))


# se extraen los datos de la región del Maule
maule.activos <- raw.activos[raw.activos$Region == "Maule",]


# Casos fallecidos ######################################
# fuente de datos: Informe epidemiológico
# https://github.com/MinCiencia/Datos-COVID19/blob/master/input/InformeEpidemiologico/CasosFallecidosPorComuna.csv
raw.fallec <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosFallecidosPorComuna.csv")
maule.fallec <- raw.fallec[raw.fallec$Region == "Maule",]
maule.fallec.simple <- maule.fallec[c(3, 5, ncol(maule.fallec))]

# Casos acumulados ######################################
# fuente de datos: Informe epidemiológico
# https://github.com/MinCiencia/Datos-COVID19/blob/master/input/InformeEpidemiologico/CasosAcumuladosPorComuna.csv
raw.acum <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosAcumuladosPorComuna.csv")
maule.acum <- raw.acum[raw.acum$Region == "Maule",]

#vals <- data.frame("Maule", 7, "Total", "")
#names(vals) <- c("Region", "Codigo.region", "Comuna", "Codigo.comuna")
#totales <- mapply(sum, maule.acum[,5:ncol(maule.acum)], na.rm = TRUE)
#row.totales <- c(vals, totales)
#maule.acum.tot <- rbind(maule.acum, row.totales) # agregada la fila con totales
#se quitó maule.acum.total porque ahora si tiene una fila para los totales

# Considerar la última fila que tiene el total de la región
tot.reg <- maule.acum[nrow(maule.acum), 6:(ncol(maule.acum)-1)] # le quitamos la columna Tasa

# calcular los casos nuevos usando los casos acumulados
# sea d(n) un día cualquiera y d_ac(n) los casos acumulados en el mismo día,
# los casos nuevos del día n sería d(n) = d_ac(n)-d_ac(n-1)
casos.nuevos <- tot.reg
casos.nuevos[,2:ncol(casos.nuevos)] <- tot.reg[,2:ncol(casos.nuevos)] - tot.reg[,1:(ncol(casos.nuevos)-1)]

# Construcción tabla ####################################
# para la tabla son necesarias la columna Comuna (3), Población (5), Confirmados (a la última fecha), 
# incremento (dif ultima fecha - fecha anterior) tasa (última fila), que corresponde a
# los confirmados/100k habitantes
ultima.col.acum <- ncol(maule.acum)-1
tabla.cols1 <- maule.acum[,c(3, 5, ultima.col.acum)]

# calcular el aumento usando la penúltima columna y la anterior
tabla.cols2 <- maule.acum[,ultima.col.acum] - maule.acum[,ultima.col.acum-1]
tabla.cols3 <- maule.fallec.simple[,ncol(maule.fallec.simple)]
tabla.cols4 <- maule.acum[, ncol(maule.acum)]

tabla <- cbind(tabla.cols1,tabla.cols2, tabla.cols3, tabla.cols4)

names(tabla) <- c("Comuna", "Poblacion", "Confirmados", "Incremento", "Fallecidos", "Casos.100k")
row.names(tabla) <- c(1:nrow(tabla))

t1<- tabla[1:(nrow(tabla)-1),] # le quitamos la fila con el total
t1

pdf("salida-tabla.pdf", height=12, width=10)
grid.table(t1)
dev.off() # cerrar el pdf

pdf("salida-graficos.pdf", height=10, width=15)

# Gráfico casos confirmados y fallecidos por comuna ###############

ggplot(mapping = aes(x, y)) +
  geom_bar(data = data.frame(x = reorder(t1$Comuna, -t1$Confirmados), y = t1$Confirmados), stat = "identity", fill = "yellow", width = 0.7) +
  geom_bar(data = data.frame(x = reorder(t1$Comuna, -t1$Confirmados), y = t1$Fallecidos), stat = "identity", fill = "black", width = 0.7) +
  scale_fill_manual("Casos", values = c("Confirmados" = "yellow", "Fallecidos" = "black"), aesthetics = "fill") +
  labs(title = "Casos por comuna", subtitle = "Región del Maule", x = "Comuna", y = "Casos confirmados (am) y fallecidos (neg)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggsave("salida-graficos.pdf", width = 13, height = 10, units = "cm")


# Gráfico casos nuevos #########################
fechas <- colnames(casos.nuevos)

mat.casos.nuevos <- as.data.frame(t(as.matrix(casos.nuevos)))
new.casos.nuevos <- cbind(mat.casos.nuevos,fechas)

colnames(new.casos.nuevos) <- c("Casos.nuevos", "Fecha")

new.casos.nuevos$Fecha <- gsub("X","", as.character(new.casos.nuevos$Fecha))
new.casos.nuevos$Fecha <- gsub("\\.","-", as.character(new.casos.nuevos$Fecha))

# cambiar a formato fecha y luego al formato de fecha usado en Chile
new.casos.nuevos$Fecha <- format(as.Date(new.casos.nuevos$Fecha, format = "%Y-%m-%d"), "%d/%m/%Y")

ggplot(mapping = aes(x, y)) +
  geom_bar(data = data.frame(
    x = as.character(as.Date(new.casos.nuevos$Fecha, format = "%d/%m/%Y")), 
    y = new.casos.nuevos$Casos.nuevos),
    width = 0.7,
    stat = "identity", fill = "#336699") +
  labs(title = "Casos nuevos", subtitle = "Región del Maule", x = "Fecha", y = "Casos nuevos") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Tabla de datos estandarizada ####################

# fuente de datos: Minsal (Producto 3)
# https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto3/TotalesPorRegion_std.csv
raw.nac.covid <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/TotalesPorRegion_std.csv")
raw.nac.covid <- as.data.frame(raw.nac.covid)

maule.covid <- raw.nac.covid[which(raw.nac.covid$Region == "Maule"),]


# Gráfico líneas ############
maule.covid.2 <- maule.covid[which(
  ((maule.covid$Categoria == "Casos acumulados") |
     (maule.covid$Categoria == "Fallecidos totales") |
     (maule.covid$Categoria == "Casos nuevos totales") |
     (maule.covid$Categoria == "Casos activos confirmados"))),]

# Tratamiento de valores NA
which(is.na(maule.covid.2) & maule.covid.2$Categoria == "Casos activos confirmados")
which(is.na(maule.covid.2) & maule.covid.2$Categoria == "Casos acumulados")
which(is.na(maule.covid.2) & maule.covid.2$Categoria == "Casos nuevos totales")
which(is.na(maule.covid.2) & maule.covid.2$Categoria == "Fallecidos totales")
# existen valores NA en Casos activos confirmados y Fallecidos totales

# Los valores NA en Fallecidos totales corresponden a la cifra de fallecidos 
# antes de que se registre el primer caso)
# Por lo tanto se reemplazan por 0
maule.covid.2[is.na(maule.covid.2) & maule.covid.2$Categoria == "Fallecidos totales"] = 0


length(which(is.na(maule.covid.2) & maule.covid.2$Categoria == "Casos activos confirmados"))
# para los Casos activos confirmados se registran +100 NA, los que serán omitods
maule.covid.2 <- na.omit(maule.covid.2)

ggplot(data=maule.covid.2, aes(x=Fecha, y=Total, group=Categoria, colour=Categoria)) +
  geom_line(size=1.5) +
  labs(title = "Casos por fecha", subtitle = "Región del Maule") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = c(0.1, 0.85))


# Grafico barras casos acumulados vs fallecidos #################

maule.covid.simple <- maule.covid[which(
    ((maule.covid$Categoria == "Casos acumulados") |
    (maule.covid$Categoria == "Fallecidos totales"))),]

# revisión de valores NA
which(is.na(maule.covid.simple))
# Los valores NA corresponden a la cifra de fallecidos 
# antes de que se registre el primer caso)
# Por lo tanto se reemplazan por 0
maule.covid.simple[is.na(maule.covid.simple)] = 0

ggplot(maule.covid.simple, aes(x=Fecha, y=Total, fill=Categoria)) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(title = "Casos por fecha", subtitle = "Región del Maule") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = c(0.1, 0.85)) +
  scale_fill_manual("Casos", values = 
                      c("Casos activos confirmados" = "blue", "Fallecidos totales" = "black", "Casos acumulados" = "yellow"), aesthetics = "fill") 

dev.off() # cerrar el pdf