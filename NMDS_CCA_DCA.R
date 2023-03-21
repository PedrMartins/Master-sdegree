setwd("C:/Users/Pedro/Dropbox/Mestrado/Dados/Similaridade floristica")
#/Similaridade floristica")# Função para mudar o diretório de comando
install.packages ("vegan")# função para baixar pacotes
library (vegan)#funçao disponibiliza os pacotes 
#library (MASS)
#dir ()
#Detalhe as matrizes no R devem estar com as espécies na 
#colune e parcelas nas linhas

################################################################################
################################################################################
##############################ELEGÂNCIA EM BAIXAR PACOTES#######################

pkg <- c('flora')
pkg <- pkg[!pkg%in%installed.packages()]
install.packages ("pkg")
################################################################################
################################################################################
dir ()
##########
#espécies e localidades
dads.np =  read.table ("Ma_ab_sp_rare.txt", header = TRUE,
 sep = "\t", row.names=1, dec = ",")#dados de campos do jordão, Bananal, barra do chapeu, Itabera
#
dads.TD =  read.table ("Similaridade floristica TUDO.txt", header = TRUE,
 sep = "\t", row.names=1, dec = ",")#dados de campos do jordão, Bananal, barra do chapeu, Itabera
dads.TD[is.na(dads.TD)] <- 0
dads.TD= t(dads.TD)
dim (dads.TD)
View (dads.TD)

#
dim(dads.np)
#retirando Ba e ipiranga (PRipi)
dim (dads.np)
#dads.sud_orig <- dads.np[c(2:8),]
#dads.sud_orig<- dads.sud_orig[,colSums (dads.sud_orig)>0]
#dads.sud_orig<- dads.sud_orig[-3,]
#dads.sud_orig<- dads.sud_orig[,colSums (dads.sud_orig)>0]
#write.csv(dads.sud_orig, , header=TRUE)
#write.table (dads.sud_orig, "dads.sud.csv",
# sep = "\t", dec=",") #melhorar
#dads.Ba <- dads.np[4,]
#dads.Ba<- dads.Ba[,colSums (dads.Ba)>0]
#dim (dads.Ba)
dads.np<-dads.np[-4,]
dim (dads.np)
dads.np<- dads.np[,colSums (dads.np)>0]
dim (dads.np)
dads.np<-dads.np[-11,]
dim (dads.np)
dads.np<- dads.np[,colSums (dads.np)>0]
dim (dads.np)
rownames(dads.np)<- c("Campos do \n Jordão2-SP", "Delfim Moreira \n Faz. São Fran.-MG",
				 "Delfim Moreira \n Faz. Bartira-MG", "Barra do Chapéu-SP", "Itaberá-SP",
				"Campos do \n Jordão-SP", "Baependi-MG", "Baependi1-MG", 
				 "Baependi2-MG", "Castro-PR","Campo Tenente-PR", "Tibagi1-PR",
				"Tibagi2-PR","Ventania-PR")
#View (dads.np)
#class (b)
#write.table (b,"nomes.csv")



#########
#Localidades x coord 
dads.coord= read.table ("coordenadas.txt", header = TRUE,
 sep = "\t", row.names=1, dec=",")

dim (dads.coord)
dads.coord<-dads.coord[-4,]
dim (dads.coord)
dads.coord<-dads.coord[-11,]
dim (dads.coord)



#########
#Localidade x var.amb
dads.env =  read.table ("Env.loc.txt", header = TRUE,
 sep = "\t", row.names=1, dec = ",")
names (dads.env) <- c("Long","Lat", "Alt","temp. \n média","range.dia","iso.metric", 
"tem.sasonal","max.temp. \n mês quente","min.temp. \n mês frio","rang.tem.ano",
"temp.médi.quartil.umido","temp.médi.quartil.seco",
"temp.médi.quartil.quente","temp.méd.quartil.frio","preci. \n anual",
 "preci. \n  mês.úmi", "preci.m.sec", "preci.sasonal","prec.quartil.umido",
"prec.quartil.seco","prec.quartil.quente","prec.quartil.frio")
#d.t=t(dads.env)


#View(d.t)
#dads.env<-dads.env[,-c(1:3)]
#dads.env<-dads.env[,-c(1,2,3,5:6,10:14,18:22)]
dads.env<-dads.env[,-c(1:3,5:7,10:14,17:22)]
dim (dads.env)
dads.env<-dads.env[-4,]
dim (dads.env)
dads.env<-dads.env[-11,]
dim (dads.env)


View (d.t)
#############Diver.Tax
######em processo! 
dir ()
tax=read.table("Diver_tax.txt"  , header = TRUE, 
sep = "\t", row.names=1)
taxdis <- taxa2dist(tax, varstep=TRUE)

#as.matrix(taxdis)
#View (as.matrix(taxdis))
#x11()
#plot(hclust(taxdis), hang = -1)

#p=vegdist (dads.sp)
#plot(hclust(p, method="average"), hang=-1)


diversity(dads.np, index="shannon")
diversity(dads.np, index="simpson")


rowSums (dads.np)

#View (as.matrix(taxdis))
#View (tax)
labels <- as.factor(rownames(dads.np))
n<- length(labels)
D.T <-taxondive(dads.np, taxdis)
plot (D.T$Species,D.T$D)
with(D.T, text(Species-.4, D-.1, as.character(rownames(dads.np)),
pos = 4, cex = 0.8))

write.table (D.T, "distax.txt")
#summary (D.T)
#D.T [,c(1:7)]
#D.T
#str (D.T)
#range (D.T$Dstar)
cor= c(rep("gray11",3),rep("gray57",2),rep("gray11",4),rep("gray57",5))
plot(D.T,pch=19,cex = 1.7, col=cor,xlim = c(min(D.T$Species)-10,max(D.T$Species))+15,
ylim = c(min(D.T$EDplus-D.T$sd.Dplus*2)-1,max(D.T$sd.Dplus*2+D.T$EDplus)+1))
with(D.T, text(Species-.4, Dplus-.1, as.character(rownames(dads.np)),
pos = 4, cex = 0.8))
#px=as.data.frame(D.T$Species)
#py=as.data.frame(D.T$Dplus)
#pontos=cbind(px,py)
#points (pontos,, pch=20, cex=1.5)




#plot(D.T,pch=19,cex = 1.7,xlim = c(min(D.T$Species),max(D.T$Species)),
# ylim = c(min(D.T$EDplus-D.T$sd.Dplus*2),max(D.T$sd.Dplus*2+D.T$EDplus)))


plot (D.T$Species,D.T$Dplus, type="n", xlim=c(0,200),ylim=c(90,100) )
plot (D.T, xlim=c(0,200),ylim=c(90,100))
abline (v= , h= )
c=D.T$EDplus
x=sort (px[,1])
y=sort (py[,1])
abline (v= y)

x=c(D.T$Species)
y=c(D.T$Dplus[c(1:8)])
points (x, y, pch= c(19,18,17,21,22,23,25,15))


dim (tax)
plot.taxondive
dim (dads.pxa)

b=metaMDS(taxdis)
plot (b)
####################################################################################################################################
############################################ PADRONIZAÇÃO DOS DADOS ##################################################
######ESPECIES E PARCELA
dads.sp= decostand (dads.np, 			#função padroniza dados de contagem
		method="log",	#argumento para escolha da padronização
					# interessante utilizar log +1 "log"
					#Calculo de logb(x)+1, onde b é a base e x a matriz
					#outra padronização é a escalarização completa 
					#ou ranging "range" no R
					#formula yi'= yi – ymin/ymax – ymin
		logbase=10)		#Argumento para base log do método "log"

#
stand.sp= decostand (dads.TD, 			#função padroniza dados de contagem
		method="log",	#argumento para escolha da padronização
					# interessante utilizar log +1 "log"
					#Calculo de logb(x)+1, onde b é a base e x a matriz
					#outra padronização é a escalarização completa 
					#ou ranging "range" no R
					#formula yi'= yi – ymin/ymax – ymin
		logbase=10)		#Argumento para base log do método "log"
head (stand.sp)
#
mat.env =	decostand (dads.env, 			#função padroniza dados de contagem
		method="range")	#argumento para escolha da padronização

#A análise NMDS com log 10 respondeu melhor, teve um stress menor
##########################

#View (dads.gen)
#summary (dads)
########################################Detalhes do método log###################################
#log: logarithmic transformation as suggested by Anderson et al. (2006): logb(x) + 1 for		#
#x > 0, where b is the base of the logarithm; zeros are left as zeros. Higher bases give less	#
#weight to quantities and more to presences, and logbase = Inf gives the presence/absence		#
#scaling. Please note this is not log(x + 1). Anderson et al. (2006) suggested this for their	#
#(strongly) modified Gower distance (implemented as method = "altGower" in vegdist),		#
#but the standardization can be used independently of distance indices.					#
############################################################################################################################################
####################################################################################################################################
############################################EXPLORAÇÃO DOS DADOS EM MULTIVARIADAS ##################################################




####NMDS spp x parcela
#Points são os scores do NMDS?
(dads.Nmds <-metaMDS (dads.sp,			#argumento para os dados
				distance= "bray", #argumento define distancia
				trace=FALSE,	
				engine="monoMDS",	#argumento define qual método de nmds será usado
				k=3,#argumento sobre as dimensões do nmds
				binary=FALSE, 
				wascore=TRUE))#argumento indica que a matriz é de PxA
str (dads.Nmds)
dim(stand.sp)
ordiplot (dads.Nmds, type="t", dis= "sites")
#View (dads.TD)
loc=c(rep("Serra Mant", 4), "Sul SP",rep("Serra Mant", 2),rep("PR", 39)
	,rep("RS", 29),rep("SC", 134),"Sul SP",rep("Serra Mant",2))


#d.loc=data.frame (loc)
#dim(d.loc)
#View (d.loc)

cor= colorRampPalette(c("darkblue","lightblue"))
#cor (5)

ordihull(dads.Nmds, groups=loc, col=cor(5),
draw="polygon", label=TRUE, alpha=120)#função para 

s=envfit (dads.Nmds,mat.env)
plot (s)
# função retorna dados do ordenação alem de 
# possibilitar análise das espécies
#plot (dads.NmdsP, type="t", dis="site", xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
#head (dads.NmdsP$points)
#scores(dads.NmdsP)
#str (dads.NmdsP)



##########################################################################
################(ANOSIM) ANÁILISE DE SIMILARIDADE#########################

nomes.sp.par = data.frame(areas=c(rep("Mantiqueira",3),rep("Plan.Paraense",2),
			rep("Mantiqueira",4),rep("Plan.Paraense",5)))


#dads.sp
SP.par <- with(nomes.sp.par, anosim(dads.sp, areas))
plot (SP.par)
#summary(SP.par)

################################################################################################

#### CA E DCA

#DCA sp x parcela
(DCA.sp=decorana(dads.sp))
(CCA.sp= cca (dads.sp,mat.env))# decorana é função para Detrended corresp ondence analysis
#chisq.test(dads.np/sum(dads.np))	# O test chi quadrado representa a inercia do CA, 
			#já que esse usa chi quadrado como distância
eng.env= envfit (CCA.sp, mat.env)
eng.envDCA= envfit (DCA.sp, mat.env)
#b=matrix (nrow=293,ncol=3, )
#b [,1]<-c(rep (-22.5092,100),rep (-22.6836, 43),
#		rep (-24,4731, 50),rep (-23,8619, 50),
#		rep (-22,7394, 50))
#b [,2]<-c(rep (-45.28,100),rep (-44.3233, 43),
#		rep (-49.0244, 50),rep (-49,1372, 50),
#		rep (-45.5914, 50))
#b [,3]<-c(rep (1790,50),rep (1307,50)
#		,rep (1011, 43),
#		rep (803, 50),rep (705, 50),
#		rep (1467, 50))
#site.lat.long.alt=cbind (site, b)
#lat.dca=cor.test(site.lat.long.alt[,5],site.lat.long.alt[,1],method="pearson")
#lat.dca
#long.dca=cor.test(site.lat.long.alt[,6],site.lat.long.alt[,1],method="pearson")
#long.dca
#alt.dca=cor.test(site.lat.long.alt[,7],site.lat.long.alt[,1],method="pearson")
#alt.dca
#x=lm(site.lat.long.alt[,7]~site.lat.long.alt[,1])
#summary (x)
##################################################################################################


####################################################################################################################################
####################################################################################################################################
#######################################FUNÇÕES GRÁFICAS#############################################################################


par (mfrow=c(1,1)#divisão janela gráfica direira da virgula divide por linha
			# esquerda por coluna
	,mar = c(5,5,2,1)# margem
	, cex.axis=1.5 # tamanho fonte eixos
	, cex.lab=2 #tamanho fonte legenda
	, cex.main=2.5 #tamanho fonte título
	,family="mono"# fonte da letra
	, las=1,# orientação dos números eixo y
	tcl=0.3# orientação traços do eixo
	,mgp=c(3,0.3,0))# distância do titulo, legenda e linha
#args(x11)

###ARGUMENTOS UTEIS DA FUNC PLOT ()
#a função tem um argumento display = c("sites","species")esse argumento 
#permite ver só espécies ou as parcelas
#argumeto type= c("points","text","none ou n")a forma 
#como será representado no gráfico

#####GRÁFICO DCA
######para salvar

setwd("C:/Users/Pedro/Dropbox/Mestrado/Dados/Gráficos")

#DCA
jpeg(filename = "Gráfica&DCA1.jpg", width = 500, height = 500, # função salva gráficos em .jpg
     units = "px", quality = 75,
     bg = "white", restoreConsole = TRUE)

###parâmetros gráficos

par (mfrow=c(1,1)#divisão janela gráfica direira da virgula divide por linha
			# esquerda por coluna
	,mar = c(5,5,2,1)# margem
	, cex.axis=1.5 # tamanho fonte eixos
	, cex.lab=2 #tamanho fonte legenda
	, cex.main=2.5 #tamanho fonte título
	,family="mono"# fonte da letra
	, las=1,# orientação dos números eixo y
	tcl=0.3# orientação traços do eixo
	,mgp=c(3,0.3,0))# distância do titulo, legenda e linha


######
#DCA spp x loc
#View(data.frame("SP"=names(dads.sp),"Abrev"=n.abrev,"DCA1"=DCA.dads$cproj[,1]))
n.abrev = make.cepnames(names(dads.sp)) #função abrevia nomes em latim para sp
names=names (dads.sp)
#Nxsig=data.frame (names, n.abrev)
#View (Nxsig)
#write.table (Nxsig, "siglas.csv",
# sep = "\t", dec=",") #melhorar
s.col= colSums (dads.sp)#função soma as colunas
ordiplot (DCA.sp, type ="n", xlim=c(-3.5,4),ylim=c(-3,3)) #plota um gráfico do DCA vazio
gra.DCA = orditorp(DCA.sp # objeto com o DCA
		, dis = "sp"# display só para espécies
		, lab=n.abrev # etiqueta com os nomes abreviados
		, priority=s.col # dá prioridade para os nomes pela soma das colunas
		, pcol = rgb (0,0,0,0.5) #colere os pontos do gráfico
		, pch=20  #símbolo dos pontos
		, cex=1
		, air=1)

#CCA.s=CCA.sp$CCA$u[,c(1,2)]
#pont.1=as.data.frame(CCA.s)
#View(pont.1)
#xrange.1=range (pont.1[,1])
#yrange.1=range (pont.1[,2])
#points(pont.1 [1,], xlim=xrange.1,ylim=yrange.1,col= #Campos do Jordão2
#"gray11",pch=18, cex =1)
#points(pont.1 [2,], xlim=xrange.1,ylim=yrange.1,col= #Fsf-MG
#"gray11",pch=16, cex =1)
#points(pont.1 [3,], xlim=xrange.1,ylim=yrange.1,col= #Fb
#"gray11",pch=17, cex =1)
#points(pont.1 [4,], xlim=xrange.1,ylim=yrange.1,col= #Barra do Chapéu
#"gray57",pch=15, cex =1)
#points(pont.1 [5,], xlim=xrange.1,ylim=yrange.1,col= #Itaberá
#"gray57",pch=15, cex =1)
#points(pont.1 [6,], xlim=xrange.1,ylim=yrange.1,col= #Campos 1
#"gray11",pch=16, cex =1)
#points(pont.1 [7,], xlim=xrange.1,ylim=yrange.1,col= #Baependi
#"gray11",pch=18, cex =1)
#points(pont.1 [8,], xlim=xrange.1,ylim=yrange.1,col= #Baependi1
#"gray11",pch=16, cex =1)
#points(pont.1 [9,], xlim=xrange.1,ylim=yrange.1,col= #Baependi2
#"gray11",pch=17, cex =1)
#points(pont.1 [10,], xlim=xrange.1,ylim=yrange.1,col= #PRcastro
#"gray57",pch=15, cex =1)
#points(pont.1 [11,], xlim=xrange.1,ylim=yrange.1,col= #PRTene
#"gray57",pch=15, cex =1)
#points(pont.1 [12,], xlim=xrange.1,ylim=yrange.1,col= #PRtiba1
#"gray57",pch=16, cex =1)
#points(pont.1 [13,], xlim=xrange.1,ylim=yrange.1,col= #PRtiba4
#"gray57",pch=17, cex =1)
#points(pont.1 [14,], xlim=xrange.1,ylim=yrange.1,col= #PRvent
#"gray57",pch=15, cex =1)

loc=c(rep("Manti", 3),rep("Extr", 2),rep("Manti", 4),
	rep("Extr", 5))
#View (dads.sp)

cor=c(col="salmon2",col="paleturquoise1")
ordihull(DCA.sp, groups=loc, draw="polygon", col=cor, label=FALSE, alpha=120)#função para 

#plot (eng.envDCA)

legend("topright",c("Plan.Paranaense","Mantiqueira"), cex=1,bty="n",
pch=20,col=c("salmon2","paleturquoise1"))


#text(-3,4 #função adiciona um texto ao gráfico, 
#			#arg 1º define a localização, usa-se a função locator para
#			#adicionar de uma forma interativa
#	,"a" #texto a ser escrito
#	,cex=1.2		#tamanho da fonte
#	, family = "mono") #tipo da fonte
#

#View(data.frame(table (names(dads.pxa))))
#sort(s.col,decr=TRUE )# soma das colunas em ordem crescente
#View (dads.pxa)

dev.off()

##taxon distinctiness


jpeg(filename = "Taxdis.jpg", width = 500, height = 500, # função salva gráficos em .jpg
     units = "px", quality = 75,
     bg = "white", restoreConsole = TRUE)

###parâmetros gráficos

par (mfrow=c(1,1)#divisão janela gráfica direira da virgula divide por linha
			# esquerda por coluna
	,mar = c(5,5,2,1)# margem
	, cex.axis=1.5 # tamanho fonte eixos
	, cex.lab=2 #tamanho fonte legenda
	, cex.main=2.5 #tamanho fonte título
	,family="mono"# fonte da letra
	, las=1,# orientação dos números eixo y
	tcl=0.3# orientação traços do eixo
	,mgp=c(3,0.3,0))# distância do titulo, legenda e linha



cor= c(rep("gray11",3),rep("gray57",2),rep("gray11",4),rep("gray57",5))
plot(D.T,pch=19,cex = 1.7, col=cor,xlim = c(min(D.T$Species)-30,max(D.T$Species))+30,
ylim = c(min(D.T$EDplus-D.T$sd.Dplus*2)-1,max(D.T$sd.Dplus*2+D.T$EDplus)+1))
with(D.T, text(Species-.4, Dplus-.1, as.character(rownames(dads.np)),
pos = 4, cex = 0.8))

legend("topright",c("Plan.Paranaense","Mantiqueira"), cex=1,bty="n",
pch=20,col=c("grey 57","grey 11"))


#text(43,99 #função adiciona um texto ao gráfico, 
#			#arg 1º define a localização, usa-se a função locator para
#			#adicionar de uma forma interativa
#	,"b" #texto a ser escrito
#	,cex=1.2		#tamanho da fonte
#	, family = "mono") #tipo da fonte
#
dev.off()
################################


setwd("C:/Users/Pedro/Dropbox/Mestrado/Dados/Gráficos")

#CCA
jpeg(filename = "Gráfica&CCA.jpg", width = 750, height = 500, # função salva gráficos em .jpg
     units = "px", quality = 75,
     bg = "white", restoreConsole = TRUE)

###parâmetros gráficos

par (mfrow=c(1,1)#divisão janela gráfica direira da virgula divide por linha
			# esquerda por coluna
	,mar = c(5,5,2,1)# margem
	, cex.axis=1.5 # tamanho fonte eixos
	, cex.lab=2 #tamanho fonte legenda
	, cex.main=2.5 #tamanho fonte título
	,family="mono"# fonte da letra
	, las=1,# orientação dos números eixo y
	tcl=0.3# orientação traços do eixo
	,mgp=c(3,0.3,0))# distância do titulo, legenda e linha

#####################
#####################


#CCA spp x loc
#View(data.frame("SP"=names(dads.sp),"Abrev"=n.abrev,"DCA1"=DCA.dads$cproj[,1]))
#n.abrev = make.cepnames(names(dads.sp)) #função abrevia nomes em latim para sp
#s.col= colSums (dads.sp)#função soma as colunas
ordiplot (CCA.sp, type ="t", dis="site", xlim=c(-1.5,1.5), ylim=c(-2,2), cex=) #plota um gráfico do DCA vazio
#gra.DCA = orditorp(DCA.sp # objeto com o DCA
#		, dis = "sp"# display só para espécies
#		, lab=n.abrev # etiqueta com os nomes abreviados
#		, priority=s.col # dá prioridade para os nomes pela soma das colunas
#		, pcol = rgb (0,0,0,0.5) #colere os pontos do gráfico
#		, pch=20  #símbolo dos pontos
#		, cex=1
#		, air=1.5)


#CCA.s=CCA.sp$CCA$u[,c(1,2)]
#pont.1=as.data.frame(CCA.s)
#View(pont.1)
#xrange.1=range (pont.1[,1])
#yrange.1=range (pont.1[,2])
#points(pont.1 [1,], xlim=xrange.1,ylim=yrange.1,col= #Campos do Jordão2
#"gray11",pch=18, cex =1)
#points(pont.1 [2,], xlim=xrange.1,ylim=yrange.1,col= #Fsf-MG
#"gray11",pch=16, cex =1)
#points(pont.1 [3,], xlim=xrange.1,ylim=yrange.1,col= #Fb
#"gray11",pch=17, cex =1)
#points(pont.1 [4,], xlim=xrange.1,ylim=yrange.1,col= #Barra do Chapéu
#"gray57",pch=15, cex =1)
#points(pont.1 [5,], xlim=xrange.1,ylim=yrange.1,col= #Itaberá
#"gray57",pch=15, cex =1)
#points(pont.1 [6,], xlim=xrange.1,ylim=yrange.1,col= #Campos 1
#"gray11",pch=16, cex =1)
#points(pont.1 [7,], xlim=xrange.1,ylim=yrange.1,col= #Baependi
#"gray11",pch=18, cex =1)
#points(pont.1 [8,], xlim=xrange.1,ylim=yrange.1,col= #Baependi1
#"gray11",pch=16, cex =1)
#points(pont.1 [9,], xlim=xrange.1,ylim=yrange.1,col= #Baependi2
#"gray11",pch=17, cex =1)
#points(pont.1 [10,], xlim=xrange.1,ylim=yrange.1,col= #PRcastro
#"gray57",pch=15, cex =1)
#points(pont.1 [11,], xlim=xrange.1,ylim=yrange.1,col= #PRTene
#"gray57",pch=15, cex =1)
#points(pont.1 [12,], xlim=xrange.1,ylim=yrange.1,col= #PRtiba1
#"gray57",pch=16, cex =1)
#points(pont.1 [13,], xlim=xrange.1,ylim=yrange.1,col= #PRtiba4
#"gray57",pch=17, cex =1)
#points(pont.1 [14,], xlim=xrange.1,ylim=yrange.1,col= #PRvent
#"gray57",pch=15, cex =1)

loc=c(rep("Manti", 3),rep("Extr", 2),rep("Manti", 4),
	rep("Extr", 5))

plot (eng.env)

cor=c(col="salmon2",col="paleturquoise1")
ordihull(CCA.sp, groups=loc, draw="polygon",
 col=cor, label=FALSE, alpha=120)#função para 

legend("topright",c("Plan. Paranaense","Mantiqueira"), cex=1,bty="n",
pch=20,col=c("salmon2","paleturquoise1"))


#text(4,6.3 #função adiciona um texto ao gráfico, 
#			#arg 1º define a localização, usa-se a função locator para
#			#adicionar de uma forma interativa
#	,"spp Matriz \n Abundância" #texto a ser escrito
#	,cex=0.9		#tamanho da fonte
#	, family = "mono") #tipo da fonte

####################################


dev.off()

plot (CCA.sp, type="t", dis="site")

#####GRÁFICO DE ORDENAÇÃO NMDS

######para salvar

setwd("C:/Users/Pedro/Dropbox/Mestrado/Dados/Gráficos")

#NMDS

###parâmetros gráficos
#NMDS spp x parcela
#spp x Par
jpeg(filename = "NMDS_anosim.jpg", width = 800, height = 500, # função salva gráficos em .jpg
     units = "px",  bg = "transparent", 
	restoreConsole = TRUE, pointsize=12)

par (mfrow=c(1,2)#divisão janela gráfica direira da virgula divide por linha
			# esquerda por coluna
	,mar = c(5,5,2,1)# margem
	, cex.axis=1.5 # tamanho fonte eixos
	, cex.lab=2 #tamanho fonte legenda
	, cex.main=2.5 #tamanho fonte título
	,family="mono"# fonte da letra
	, las=1,# orientação dos números eixo y
	tcl=0.3# orientação traços do eixo
	,mgp=c(3,0.3,0))# distância do titulo, legenda e linha

#NMDS spp x parcela
#View(dads.sp)

ordiplot(dads.Nmds$points, type = "t", 
xlim=c(-1.3,1),ylim=c(-0.8,0.7), cex=0.8,
choices=c(1,2)) # gráfico de ordenação NMDS
#pont= as.data.frame(dads.Nmds$points)
#xrange.ns<-range(pont[,1])
#yrange.ns<-range(pont[,2])
#points(pont [1,], xlim=xrange.ns,ylim=yrange.ns,cex=1.5,col= #Campos do Jordão2
#"gray11",pch=19)
#points(pont [2,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #Fazenda São Francisco Delfim
#"gray11",pch=18)
#points(pont [3,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #Fazsenda Batirra Delfim
#"gray11",pch=17)
#points(pont [4,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #Barra do Chapel
#"gray57",pch=20)
#points(pont [5,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #Itaberá
#"gray57",pch=17)
#points(pont [6,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #Campos do Jordão
#"gray11",pch=16)
#points(pont [7,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #Baependi
#"gray11",pch=19)
#points(pont [8,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #Baependi1
#"gray11",pch=18)
#points(pont [9,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #Baependi2
#"gray11",pch=17)
#points(pont [10,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #PRcastro
#"gray57",pch=20)
#points(pont [11,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #PRtene
#"gray57",pch=17)
#points(pont [12,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #PRtiba1
#"gray57",pch=16)
#points(pont [13,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #PRtiba4
#"gray57",pch=17)
#points(pont [14,], xlim=xrange.ns,ylim=yrange.ns, cex=1.5,col= #PRvent
#"gray57",pch=16)


loc=c(rep("Manti", 3),rep("Extr", 2),rep("Manti", 4),
	rep("Extr", 5))
#View (dads.sp)

cor=c(col="salmon2",col="paleturquoise1")
ordihull(dads.Nmds, groups=loc, draw="polygon", col=cor, label=FALSE, alpha=120)#função para 
legend("topright",c("Plan. Paranaense","Mantiqueira"), cex=1,bty="n",
pch=20,col=c("salmon2","paleturquoise1"))


###############ADD curva de nível no gráfico de NMDS#########################
#elev= c(14.1,16,16,18.2,19.8,14.1,14.2,14.2,14.2,16.8,18.5,18.5,16.7,17.6)
#ordisurf(dads.Nmds,elev,main="",col="black", add=TRUE)
x11()
#############################################################################

text(-0.9,-1.4 #função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"stress=0.10" #texto a ser escrito
	,cex=1 		#tamanho da fonte
	, family = "mono") #tipo da fonte



text(-1.15,1.3 #função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"a" #texto a ser escrito
	,cex=1.7		#tamanho da fonte
	, family = "mono") #tipo da fonte



par (mar=c(5,7,2,1), xaxt="n")
plot(SP.par,ylab="Ranking de Dissimilaridade \n Bray-Curtis",
	col=c(rep(rgb(0,0,0,0.4))), xlab=NULL)


mtext( #função plota textos nas áreas ao redor do gráfico
	c("Entre\n grupos", "Serra da \n Mantiqueira", "Plan. \n Paranaense"), #primeiro argumento refere oa texto plotado
	side= 1, #argumento localiza no gráfico "1" abaixo
	cex=1,line=1.8,
	at=c(1,2,3,4,5,6,7))

text(0.6,85 #função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"b" #texto a ser escrito
	,cex=1.7		#tamanho da fonte
	, family = "mono") #tipo da fonte


dev.off()

x11()
plot (dads.Nmds, type="t", dis="site")
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################


#NMDS spp x área
#spp x area
jpeg(filename = "Stressplot_spxAre.jpg", width = 500, height = 750, # função salva gráficos em .jpg
     units = "px",  bg = "transparent", 
	restoreConsole = TRUE, pointsize=12)

par (mfrow=c(2,1)#divisão janela gráfica direira da virgula divide por linha
			# esquerda por coluna
	,mar = c(5,5,2,1)# margem
	, cex.axis=1.5 # tamanho fonte eixos
	, cex.lab=2 #tamanho fonte legenda
	, cex.main=2.5 #tamanho fonte título
	,family="mono"# fonte da letra
	, las=1,# orientação dos números eixo y
	tcl=0.3# orientação traços do eixo
	,mgp=c(3,0.3,0))# distância do titulo, legenda e linha

###stress spp área
stressplot (dads.NmdsA, dads.distA)
text(0.93,0.97 #função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"a" #texto a ser escrito
	,cex=2		#tamanho da fonte
	, family = "mono") #tipo da fonte

ordiplot(dads.NmdsA$points, type = "n", xlim=c(-0.6,0.6), ylim=c(-0.4,0.6)) # gráfico de ordenação NMDS
pont= as.data.frame(dads.NmdsA$points)
xrange<-range(pont[,1])
yrange<-range(pont[,2])
points(pont [6,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Campos do Jordão
rgb(0.5,0,0,0.4),pch=19)
points(pont [3,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Bananal
rgb(0,0.5,0,0.4),pch=18)
points(pont [5,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Itaberá
rgb(0,0,0.5,0.4),pch=17)
points(pont [4,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Barra do Chapel
rgb(0.5,0,0.5,0.4),pch=20)
points(pont [1,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Fazenda São Francisco Delfim
rgb(0.5,0.5,0,0.6),pch=17)
points(pont [2,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Fazsenda Batirra Delfim
rgb(0,0.5,0.5,0.4),pch=16)
points(pont [7,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Baependi
rgb(0,0,0,0.4),pch=18)

legend("topleft",c("CJ-SP","Ba-SP", "It-SP","BC-SP" ,"fsfD-MG", "fbD-MG", 
"Bp-MG"), cex=1, bty="n",
pch=c(19,18,17,20,17,16,18),col=c(rgb(0.5,0,0,0.6),rgb(0,0.5,0,0.6),
rgb(0,0,0.5,0.6),rgb(0.5,0,0.5,0.6),rgb(0.5,0.5,0,0.6),rgb(0,0.5,0.5,0.6),
rgb(0,0,0,0.6)))

text(-0.6,-0.35 #função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"stress=0.00" #texto a ser escrito
	,cex=0.9 		#tamanho da fonte
	, family = "mono") #tipo da fonte

text(0.7,0.58 #função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"b" #texto a ser escrito
	,cex=2		#tamanho da fonte
	, family = "mono") #tipo da fonte

dev.off()	

x11()
plot ()


##############################################################
###################
#NMDS gen x área

#gen x area
jpeg(filename = "Stressplot_genxAre.jpg", width = 500, height = 750, # função salva gráficos em .jpg
     units = "px",  bg = "transparent", 
	restoreConsole = TRUE, pointsize=12)

par (mfrow=c(2,1)#divisão janela gráfica direira da virgula divide por linha
			# esquerda por coluna
	,mar = c(5,5,2,1)# margem
	, cex.axis=1.5 # tamanho fonte eixos
	, cex.lab=2 #tamanho fonte legenda
	, cex.main=2.5 #tamanho fonte título
	,family="mono"# fonte da letra
	, las=1,# orientação dos números eixo y
	tcl=0.3# orientação traços do eixo
	,mgp=c(3,0.3,0))# distância do titulo, legenda e linha


###stress gen e áreas
stressplot(dads.NmdsGA, dads.gen)
text(0.75,1.5 #função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"a" #texto a ser escrito
	,cex=2		#tamanho da fonte
	, family = "mono") #tipo da fonte

ordiplot(dads.NmdsGA$points, type = "n", # gráfico de ordenação NMDS
xlim=c(-1,0.8), ylim=c(-0.6,0.6))

pont= as.data.frame(dads.NmdsGA$points)
xrange<-range(pont[,1])
yrange<-range(pont[,2])
points(pont [6,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Campos do Jordão
rgb(0.5,0,0,0.4),pch=19)
points(pont [3,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Bananal
rgb(0,0.5,0,0.4),pch=18)
points(pont [5,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Itaberá
rgb(0,0,0.5,0.4),pch=17)
points(pont [4,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Barra do Chapel
rgb(0.5,0,0.5,0.4),pch=20)
points(pont [1,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Fazenda São Francisco Delfim
rgb(0.5,0.5,0,0.4),pch=17)
points(pont [2,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Fazsenda Batirra Delfim
rgb(0,0.5,0.5,0.4),pch=16)
points(pont [7,], xlim=xrange,ylim=yrange, ,cex=2.5,col= #Fazsenda Batirra Delfim
rgb(0,0,0,0.4),pch=18)


legend("topleft",c("CJ-SP","Ba-SP", "It-SP", "BC-SP","fsfD-MG", "fbD-MG", 
"Bp-MG"), cex=1, bty="n",
pch=c(19,18,17,20,17,16,18),col=c(rgb(0.5,0,0,0.4),rgb(0,0.5,0,0.6),
rgb(0,0,0.5,0.6),rgb(0.5,0,0.5,0.6),rgb(0.5,0.5,0,0.6),rgb(0,0.5,0.5,0.6),
rgb(0,0,0,0.6)))

text(-0.8,-0.55 #Função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"stress=0.00" #texto a ser escrito
	,cex=0.9 		#tamanho da fonte
	, family = "mono") #tipo da fonte

text(0.7,0.55 #Função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"b" #texto a ser escrito
	,cex=2 		#tamanho da fonte
	, family = "mono") #tipo da fonte

dev.off()


#NMDS gen X parcela

#genxpar
jpeg(filename = "Stressplot_genxPar.jpg", width = 500, height = 750, # função salva gráficos em .jpg
     units = "px",  bg = "transparent", 
	restoreConsole = TRUE, pointsize=12)

par (mfrow=c(2,1)#divisão janela gráfica direira da virgula divide por linha
			# esquerda por coluna
	,mar = c(5,5,2,1)# margem
	, cex.axis=1.5 # tamanho fonte eixos
	, cex.lab=2 #tamanho fonte legenda
	, cex.main=2.5 #tamanho fonte título
	,family="mono"# fonte da letra
	, las=1,# orientação dos números eixo y
	tcl=0.3# orientação traços do eixo
	,mgp=c(3,0.3,0))# distância do titulo, legenda e linha

###stress gen parcela
stressplot (dads.NmdsGP, dads.dist.gen)
text(0.8,2.9 #função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"a" #texto a ser escrito
	,cex=2		#tamanho da fonte
	, family = "mono") #tipo da fonte

#View(dads.np.gen)
ordiplot(dads.NmdsGP$points, type = "n") # gráfico de ordenação NMDS
pont= as.data.frame(dads.NmdsGP$points)
xrange<-range(pont[,1])
yrange<-range(pont[,2])
points(pont [244:293,], xlim=xrange,ylim=yrange, ,cex=1.5,col= #Campos do Jordão
rgb(0.5,0,0,0.4),pch=19)
points(pont [101:143,], xlim=xrange,ylim=yrange, ,cex=1.5,col= #Bananal
rgb(0,0.5,0,0.4),pch=18)
points(pont [194:243,], xlim=xrange,ylim=yrange, ,cex=1.5,col= #Itaberá
rgb(0,0,0.5,0.4),pch=17)
points(pont [144:193,], xlim=xrange,ylim=yrange, ,cex=1.5,col= #Barra do Chapel
rgb(0.5,0,0.5,0.4),pch=20)
points(pont [1:50,], xlim=xrange,ylim=yrange, ,cex=1.5,col= #Fazenda São Francisco Delfim
rgb(0.5,0.5,0,0.4),pch=17)
points(pont [51:100,], xlim=xrange,ylim=yrange, ,cex=1.5,col= #Fazsenda Batirra Delfim
rgb(0,0.5,0.5,0.4),pch=16)
loc=c(rep("FsfD-MG", 50),rep("FbD-MG", 50),rep("BA-SP", 43),rep("BC-SP", 50),
	rep("It-SP", 50),rep("CJ-SP", 50))
cor=c(rgb(0,0.5,0,0.2),rgb(0.5,0,0.5,0.2),rgb(0.5,0,0,0.2)
	,rgb(0,0.5,0.5,0.2), rgb(0.5,0.5,0,0.2),rgb(0,0,0.5,0.2))
ordihull(dads.NmdsGP, groups=loc, draw="polygon", col=cor, label=FALSE)#função para 
#delimitar os grupos na ordenação


legend("topleft", bty="n",
c("CJ-SP","Ba-SP", "It-SP", "BC-SP","fsfD-MG", "fbD-MG"), cex=1,
pch=c(19,18,17,20,17,16),col=c(rgb(0.5,0,0,0.6),rgb(0,0.5,0,0.6),
rgb(0,0,0.5,0.6),rgb(0.5,0,0.5,0.6),rgb(0.5,0.5,0,0.6),rgb(0,0.5,0.5,0.6)))

text(-1.5,-1.2 #função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"stress=0.18" #texto a ser escrito
	,cex=0.9 		#tamanho da fonte
	, family = "mono") #tipo da fonte

text(2,1.3 #Função adiciona um texto ao gráfico, 
			#arg 1º define a localização, usa-se a função locator para
			#adicionar de uma forma interativa
	,"b" #texto a ser escrito
	,cex=2 		#tamanho da fonte
	, family = "mono") #tipo da fonte


dev.off()

################ SALVANDO GRÁFICO

##DICA IMPORTANTE, quando for salvar o gráfico refazer os passos da
## funça par () e o gráfico em si, a funça jpeg abre um arquivo jpg, evc irá mexer 
## dentro dele, quando finalizar usar a função dev.off ()


######################################################################################################################################################
################################################## EXPORTANDO DADOS ###########################################################################
View (as.data.frame(as.matrix(dads.dist)))
data4.dist=as.data.frame(as.matrix(dads4.distjac))

write.table (as.matrix(dads.dist), "Matrix de distancia araucária.csv",
 sep = "\t", dec=",") #melhorar
write.table (data4.dist, "Matrix de distancia 4 áreas jac.csv",
 sep = "\t", dec=",")

write.table (summary(as.matrix(dads.dist)[,1:39]), "resumo de distancia araucária.csv",
 sep = "\t", dec=",")

##############################################################
##############################################################
### estatísticas 

sum (dads.np[1:39,])#CJ
sum (dads.np[40:82,])#Bananal
sum (dads.np[83:132,])#Itaberá
sum (dads.np[133:182,])#Barra do Chapeu
sum (dads.np[183:232,])#fsf
sum (dads.np[233:282,])#fb

dim (dads.np)
dim (dads.6ar.np)
dim (dads.gen.np)
dim (dads.gen6ar.np)

dist.stats =diag(as.matrix(dads.dist))
View(as.matrix(dads.dist)[1:39,1:39])


dads.dist [1,1]

boxplot (dads.dist)

dads.dist

citation ("vegan")
