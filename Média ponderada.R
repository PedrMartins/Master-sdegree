## função média ponderada terminar
meanp=function (v,w,pop,as_numeric=FALSE)	# v=variável
						#w=peso
						#as_numeric-> caso esteja usando a função para peso
            #pop=população n
            
{	if (class (v)!="numeric")
		{ stop("Variável não numérica")}
	if (as_numeric==TRUE)
		{w=as.numeric (w)}
	if (class (w) !="numeric")
		{stop ("Peso não numérico")}
	if (length (v)!= length (w))
		{stop("colunas não com tamananhos diferentes")}
	m_p=sum (v*w)/length (pop)
	return (m_p)
	
}	




library (dplyr)

count
