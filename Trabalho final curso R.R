#Trabalho final Curso_R


library(tidyverse)
library (dplyr)
library(sf)
library(geobr)
library(ggthemes)
library(leaflet)	
library(reactable)


FOM=read.csv ("FOM.csv" , header = TRUE,
 sep = ",")
br= st_as_sf(read_state(code_state="all"))
l = "white", color = "black")


#########################
#########################
#####MAPA GGPLOT#########
ggplot(br)+
geom_sf(fill="gray98") +
coord_sf(xlim = c(-40, -60), ylim = c(-33, -19), expand = FALSE) +
geom_point(data = FOM.a,
	aes(x=long1, y = lat1),shape=21, fill="green",alpha = 0.7, size=1.9) +
  xlab("Longitude") + ylab("Latitude") +
ggtitle("Florestas Ombrófila Mista \n no Brasil") +
 theme(axis.title.y = element_text(size = 10),
	axis.title.x=element_text(size= 10),
	title=element_text(size=12),
	plot.title.position = "plot" ,
	legend.text=element_text(size=8),
	legend.position= "top",
	legend.title= element_text(size=10),
	plot.title = element_text(hjust = 0.5))+
theme_hc()

ggsave("map.jpeg")


#########################
#########################
#####MAPA INTERATIVO#####



Temp=as.numeric(FOM.a$temp)
Eco= FOM.a$ecoreg


content = paste (sep = "<br/>","Município", FOM.a$county, "Temp Médio ºC",
	format(round(Temp,2), nsmall=2) ,
	"Eco-região",  Eco)
estados <- read_state (code_state="all")


FOM.a|>
  leaflet() |>
  addProviderTiles(provider = providers$Esri.WorldImagery) |> 
  addMarkers(
    lng = ~ long1,
    lat = ~ lat1,
    popup = content,
    clusterOptions = markerClusterOptions()) |> 
  addPolygons(
    data = leaflet::getMapData(leaflet::leaflet(estados))