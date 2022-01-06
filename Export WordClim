setwd("C:/Users/Pedro/Dropbox/Mestrado/Dados/Similaridade floristica")# Função para mudar o diretório de comando
library(raster)
library(sp)
install.packages ("ggfortify")
library (ggfortify)
library (vegan)
library (ggplot2)

#########################################################################################################
###########Já tem uma matriz baixada, não precisa fazer esse procedimento################################
#########################################################################################################


citation(c("ggfortify","sp","raster"))

citation("raster")
in
r <- getData("worldclim",var="bio",res=2.5)
r.1 <- r[[c(1:11)]]#função para indexação para selecionar colunas
r.2 <- r[[c(12:19)]]#função para indexação para selecionar colunas
corre=cor (dads.clim)
cor.test (dads.clim[,14],dads.clim[,22])
head(r)
write.csv(corre,"corre.csv")
dim(corre)
dir ()
coord <- read.table ("coordenadas.txt", header = TRUE,
 sep = "\t", row.names=1)
dim(coord)
View (coord)
clim<-extract (r, coord)
clim.temp<-extract (r.1, coord)
clim.prec<-extract (r.2, coord)
coord
#head(clim.prec)

dads.temp=cbind.data.frame(coord,clim.temp)
dads.prec=cbind.data.frame(coord,clim.prec)
dads.clim=cbind.data.frame(coord,clim)


alt=matrix(NA,16,1)
colnames(alt)<-c("Alt")
alt [,1]<- c(1609,1790,1307,1011,803,705, 1511, 1650,1774,1647,1023,806,856,
	706,1116,1046)
dads.env=cbind (coord,alt,clim)
head (dads.env)
View (dads.env)
names (dads.env) <- c("long","lat", "Alt","temp.med","range.dia","iso.metric", 
"tem.sasonal","max.tep.mês.q","min.tem.mês.f","rang.tem.ano",
"temp.médi.quartil.umido","temp.médi.quartil.seco",
"temp.médi.quartil.quente","temp.méd.quartil.frio","preci.anual",
 "preci.m.um", "preci.m.sec", "preci.sasonal","prec.quartil.umido",
"prec.quartil.seco","prec.quartil.quente","prec.quartil.frio")


dads.temp=cbind (coord,alt,clim.temp)
names (dads.temp) <- c("long","lat", "Alt","temp.med","range.dia","iso.metric", 
"tem.sasonal","max.tep.mês.q","min.tem.mês.f","rang.tem.ano",
"temp.médi.quartil.umido","temp.médi.quartil.seco",
"temp.médi.quartil.quente","temp.méd.quartil.frio")

dads.prec=cbind (coord,alt,clim.prec)
names (dads.prec) <- c("long","lat", "Alt","preci.anual",
 "preci.m.um", "preci.m.sec", "preci.sasonal","prec.quartil.umido",
"prec.quartil.seco","prec.quartil.quente","prec.quartil.frio")
 

dads.env.p= decostand (dads.env, 			#função padroniza dados de contagem
		method="range")	#argumento para escolha da padronização

dads.temp.p= decostand (dads.temp, 			#função padroniza dados de contagem
		method="range")	#argumento para escolha da padronização

dads.prec.p= decostand (dads.prec, 			#função padroniza dados de contagem
		method="range")	#argumento para escolha da padronização




b=princomp (dads.env)

princomp (dads.temp)

princomp (dads.prec)

clim<-extract (r, coord)
#head(clim)
dads.clim=cbind.data.frame(coord,alt,clim)
dads.clim[1,1]
plot (dads.clim)


#########################################################################################################
################################### Tabelas WC###################################

write.table(dads.env, "dads.env.csv",dec=",")
dads.env2 <- read.table ("dads.env.par.txt", header = TRUE,
 sep = "\t", row.names=1, dec = ",")
dir ()
dads.env2= decostand (dads.env2, 			#função padroniza dados de contagem
		method="range")	#argumento para escolha da padronização
					# interessante utilizar log +1 "log"
					#Calculo de logb(x)+1, onde b é a base e x a matriz
					#outra padronização é a escalarização completa 
					#ou ranging "range" no R
					#formula yi'= yi – ymin/ymax – ymin
					#Argumento para base log do método "log"


head(dads.env2 [,c(1,2,3,15:22)])
pca.env=princomp(dads.env2)
pca.temp=princomp(dads.env2 [,c(1:14)])
pca.prec=princomp(dads.env2 [,c(1,2,3,15:22)])

plot (pca.env)
plot (pca.tem)
plot (pca.prec)

summary (pca.env)


autoplot(pca.env,loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

autoplot(pca.temp,loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

autoplot(pca.prec,loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, type ="t")


dir()


