install.packages ("VennDiagram")
library (VennDiagram)


setwd("C:/Users/Pedro/Dropbox/Mestrado/Dados/Similaridade floristica")
diag.venn =  read.table ("VennDiagra.txt", header = TRUE,
 sep = "\t", dec = ",")

head (manti)
manti <- diag.venn[diag.venn$Manti>0,]
sul_sp <- diag.venn[diag.venn$sul_sp>0,]
sul <- diag.venn[diag.venn$Sul>0,]


###mantiqueira x Sul
cor=colorRampPalette(c("darkblue","lightblue"))
cor (3)

# Generate 3 sets of 200 words
set1 <- paste(manti$parcela,manti$Manti, sep= " ")
set2 <- paste(sul$parcela,sul$Sul, sep =" ")
set3 <- paste(sul_sp$parcela,sul_sp$sul_sp, sep =" ")
 

list(set1,set2,set3)

venn.diagram(
  x = list(set1,set2,set3),
  category.names = c("Mantiqueira" , "Sul" , "Sul São Paulo"),
  filename = 'venn_diagramm.jpg',
  output=FALSE,
        height = 600 , 
        width = 650 , 
        resolution = 300,
        compression = "lzw",
        lwd = 2,
        lty = 'blank',
        fill = cor (3),
        # Numbers
        cex = .4,
        fontfamily = "sans", 
	cat.cex=0.5,
	cat.dist=c(0.065, 0.055, 0.045),
	cat.fontfamily = "sans",
	cat.pos = c(-27, 27, 155))

