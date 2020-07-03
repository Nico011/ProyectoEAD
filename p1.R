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
#str(maule.activos)

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

# Casos acumulados ######################################
# fuente de datos: https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosAcumuladosPorComuna.csv
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
tot.reg

# calcular los casos nuevos usando los casos acumulados
# sea d(n) un día cualquiera y d_ac(n) los casos acumulados en el mismo día,
# los casos nuevos de el día n sería d(n) = d_ac(n)-d_ac(n-1)
casos.nuevos <- tot.reg
casos.nuevos[,2:ncol(casos.nuevos)] <- tot.reg[,2:ncol(casos.nuevos)] - tot.reg[,1:(ncol(casos.nuevos)-1)]
casos.nuevos

# gráfico casos nuevos #########################
fechas <- colnames(casos.nuevos)

mat.casos.nuevos <- as.data.frame(t(as.matrix(casos.nuevos)))
new.casos.nuevos <- cbind(mat.casos.nuevos,fechas)
#dim(new.casos.nuevos)

colnames(new.casos.nuevos) <- c("Casos.nuevos", "Fecha")

new.casos.nuevos$Fecha <- gsub("X","", as.character(new.casos.nuevos$Fecha))
new.casos.nuevos$Fecha <- gsub("\\.","-", as.character(new.casos.nuevos$Fecha))

# cambiar a formato fecha y luego al formato de fecha usado en Chile
new.casos.nuevos$Fecha <- format(as.Date(new.casos.nuevos$Fecha, format = "%Y-%m-%d"), "%d/%m/%Y")

ggplot(mapping = aes(x, y)) +
  geom_bar(data = data.frame(
    x = as.character(as.Date(new.casos.nuevos$Fecha, format = "%d/%m/%Y")), 
    y = new.casos.nuevos$Casos.nuevos), 
    stat = "identity", fill = "#336699") +
  labs(title = "Casos nuevos", subtitle = "Región del Maule", x = "Fecha", y = "Casos nuevos") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Gráfico línas continuas ############################
ggplot(mapping = aes(x, y)) +
  geom_line(data = data.frame(
    x = as.character(as.Date(new.casos.nuevos$Fecha, format = "%d/%d/%Y")),
    y = new.casos.nuevos$Casos.nuevos),
    stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

?geom_line()

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
tabla
t.tabla <- t(tabla)
t.tabla.colnames <- t.tabla[1,]
as.factor(t.tabla.colnames)
t.tabla <- t.tabla[2:nrow(t.tabla),]
colnames(t.tabla) <- t.tabla.colnames # se asignan las comunas como nombre de las columnas

t.tabla

t1<- tabla[1:(nrow(tabla)-1),] # le quitamos la fila con el total


# intento 1
# ggplot(data = tabla) +
#   geom_bar(mapping = aes(x = Comuna, fill = Fallecidos)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#intento 2
# ggplot(data = tabla) +
#   geom_bar(mapping = aes(x = Comuna, fill = Confirmados), position = "fill") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# intento 3
# ggplot(data = tabla, aes(x = Comuna, y = Confirmados, fill = Fallecidos)) +
#   geom_bar(position = "stack", stat = "identity") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# intento 4

# ggplot(data=t1, aes(x = Comuna, y = Confirmados, fill = Fallecidos)) +
#   geom_bar(stat = "identity")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# internto 5 $$$$$
# ggplot(data = t1, aes(x = Comuna, y = Confirmados, color = as.factor(Fallecidos))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#t.tabla2 <- as.data.frame(t.tabla)

# intento 6 con facetas
# ggplot(data = t1, aes(x = Comuna, y = Confirmados, fill = Fallecidos)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   facet_wrap(~Fallecidos) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# intento 7
# t1.order <- t1[order(-t1$Confirmados),]
# 
# ggplot(mapping = aes(x, y)) +
#   geom_bar(data = data.frame(x = t1.order$Comuna, y = t1.order$Confirmados), stat = "identity", fill = "yellow") +
#   geom_bar(data = data.frame(x = t1.order$Comuna, y = t1.order$Fallecidos), stat = "identity", fill = "black") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# intento 8 para ordenar las columnas
# ggplot(mapping = aes(x, y)) +
#   geom_bar(data = data.frame(x = reorder(t1$Comuna, -t1$Confirmados), y = t1$Confirmados), stat = "identity", fill = "yellow") +
#   geom_bar(data = data.frame(x = reorder(t1$Comuna, -t1$Confirmados), y = t1$Fallecidos), stat = "identity", fill = "black") +
#   labs(title = "Casos por comuna", subtitle = "Región del Maule", x = "Comuna", y = "Nro. de casos") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Gráfico casos confirmados y fallecidos por comuna ###############
# intento 9 agregando leyenda
ggplot(mapping = aes(x, y)) +
  geom_bar(data = data.frame(x = reorder(t1$Comuna, -t1$Confirmados), y = t1$Confirmados), stat = "identity", fill = "yellow") +
  geom_bar(data = data.frame(x = reorder(t1$Comuna, -t1$Confirmados), y = t1$Fallecidos), stat = "identity", fill = "black") +
  scale_fill_manual("Casos", values = c("Confirmados" = "yellow", "Fallecidos" = "black"), aesthetics = "fill") +
  labs(title = "Casos por comuna", subtitle = "Región del Maule", x = "Comuna", y = "Casos confirmados (am) y fallecidos (neg)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

