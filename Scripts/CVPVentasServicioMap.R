rm(list=ls())
setwd("~")

require(rgdal)
require(rgeos)
require(sp)
require(maptools)
require(readstata13)
require(RColorBrewer)
require(readxl)

dir1 <- "C:\\Users\\user\\Desktop\\CursoDataScience\\5_Mapas\\5_Mapas\\Data\\Mapa 1\\estados"
##dir2 <- "C:\\Users\\user\\Desktop\\CursoDataScience\\5_Mapas\\5_Mapas\\Data\\Mapa 1\\municipios"
dir3 <- "C:\\Users\\user\\Desktop\\CursoDataScience\\5_Mapas\\5_Mapas\\Data\\Mapa 1\\carreteraMex"
dir4 <- "C:\\Users\\user\\Desktop\\CursoDataScience\\5_Mapas\\5_Mapas\\Data\\Mapa 1"
dir5 <- "C:\\Users\\user\\Desktop\\5_Mapas\\5_Mapas\\Maps\\Mapa 1"

estados <- readOGR(paste(dir1, "ESTADOS.shp", sep="\\"), "ESTADOS", stringsAsFactors=F)
##munics <- readOGR(paste(dir2, "MUNICIPIOS.shp", sep="\\"), "MUNICIPIOS", stringsAsFactors=F)

carreteras <- readOGR(paste(dir3, "carretera.shp", sep="\\"), "carretera", stringsAsFactors=F)
carreteras <- spTransform(carreteras, CRS(proj4string(estados)))

str(carreteras)

estados@data$inegi <- paste(estados@data$CVE_ENT, sep="")
estados@data <- estados@data[c("inegi")]

#Por que se cambiaron los valores a NA? Ya no salen YaY!

data.complete <-read.csv(paste(dir4,"CVPVentasServicio.csv", sep="\\"), stringsAsFactor=F)
data.complete$inegi <- formatC(as.numeric(data.complete$CVE_ENT), width = 2, format = "d", flag = "0")
estados@data <- data.frame(estados@data, data.complete[match(estados@data[,"inegi"], data.complete[,"inegi"]),])#todas las filas donde hagan match
rm(data.complete)
str(estados@data)

#CVE_ENT transformar a 2 espacios
#municipios y estados, ver si estados tiene inegi y ver cual utilizar 
#A sacar los quantiles

summary(as.numeric(estados@data$VExperienciaClienteAcum))
quantile(as.numeric(estados@data$VExperienciaClienteAcum), probs=c(0, 25, 50, 75)/100, na.rm=TRUE)
brks <- c(0, 59.90, 78.0, 81.9, 84.3, 100)
legtext <- c("0", "50 a 75", "76 a 80", "81 a 85", "86 a 100")
col.5 <- c("#fdae61", "#d7191c", "#ffffbf", "#a6d96a", "#1a9641")
col.missing <- "#ADADAD"



png(paste(dir5, "1_VExperienciaClienteAcum.png", sep="\\"), width=6, height=6, units="in", res=1000)
plot(estados, col= col.missing, lwd=0.001) 
plot(estados, col=col.5[findInterval(estados@data$VExperienciaClienteAcum, brks, all.inside=TRUE)], lwd=0.2, border ="#333333", add=T) 
plot(estados, lwd=0.5, border ="#252525", add=T) 
legend("bottomleft", "(x,y)", legend= legtext, cex=0.5, fill=col.5, bty="n") 
title("Índice de experiencia del cliente", sub="Fuente: Encuestas 2016. Corte: Diciembre, 2016") 
dev.off()
