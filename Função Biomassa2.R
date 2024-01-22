biomass= function (Dap, Alt=NULL, Dens=NULL, CAP=FALSE, 
Methods = c("Arv.DAP", 									#a função deve ser exigente apenas para a 
												#entrada do dado de DAP, alt e dens podem 
												#ser opcionais, or argumento Methods, permite 
												#ao usuário escolher qual das variávies 
												#deseja usar na análise dos dados
"Arv.DAP.Alt", " Arv.DAP.Alt.Dens", "palm.DAP", "palm.DAP.Alt"), 		#sendo cada função especifica para do 
												# argumentos que o usuário possuí, sendo só DAP,
												#DAP e ALtura, Ou DAP, Altura e Densidade da madeira
grafico = FALSE)										#Gráfico colocado como falso para
												# que o usuário escolha se a
												# função terá uma saída gráfica
{
	if (class (Dap) != "numeric")							# Teste para verificar se os dados são numéricos
		{stop ("DAP não númérico \n Função não Opera dados naõ numéricos")}#caso não a função para
	if (anyNA (Dap)){stop ("DAP com valores faltantes")}			#Função não opera com valores faltantes
	if (CAP == TRUE)									# se CAP for verdadeiro deve ser 
												#converter o objeto Dap
												# Diâmetro à altura do Peito
	{
		Dap= c(Dap)/pi								#conversão de CAP para DAP
	}
	if (Methods == "Arv.DAP") 							#caso seja utilizada a função 
												#para dados só com DAP
	{
		if (is.null(Alt)==FALSE | is.null(Dens)==FALSE)			#teste lógico para analizar se 
												#há dados a mais na função
		{stop ("Equação não calcula Altura e/ou Densidade da madeira")}#Função não permitir uso 
												#de dados de altura ou densidade		
		a= -0.370 									#Parametro para cálculo de biomassa de 
												#árvore inteira segundo referência 2
		b1= 0.333 									#Parâmetro para cálculo de biomassa de 
												#árvore inteira segundo referência 2

		b2 = 0.933									#Parâmetro para cálculo de biomassa de 
												#árvore inteira segundo referência 2

		b3= -0.122									#Pârametro para cálculo de biomassa de 
												#árvore inteira segundo referência 2

		bio= exp (a +(b1*(log(Dap))^2)+(b2*(log(Dap))^2)+(b3*(log(Dap)^2))) #Cálculo de Biomassa referência 2

		
		cat ("Equação alométrica ideal para árvores de florestas trópicais 
		úmidas,levando em consideração todo o tamanho da árvores\n")		#Mensagem informativa sobre o uso da equação.
	
		if (grafico == TRUE)								#Caso o usuário desejar que retorne um gráfico
		{
			x11()										#Abre uma nova janela gráfica
			par (mar=c(5,5,2,2),bty = "l", las= 1,				#Muda os pârametros gerais do gráfico
			cex.lab=1.4, cex =1.2, tcl=0.2)					#Alterando Margem (mar), as linhas laterais (bty)
													#o tamanho da legenda dos eixos e do título	
			line=lm(Dap~bio+I(bio^2)+I(bio^3))					#equação linear dos dados e resultados da biomasas 
			co.m= coef(line)								#coeficientes da equação
			plot (Dap, bio, pch=19, col=rgb(0,0.7,0,0.3), xlab="DAP(cm)",#Plota um gráfico de disperção entre Biomassa e DAP
			ylab= "Biomassa(kg)")							#Utiliza os pontos de bolinha sólidos, verdes e com 30% 	
													#Transparência, Biomassa em kg e DAP em cm
			curve (co.m[1]+co.m[2]*x+co.m[3]*x^2+co.m [4]*x^3, 		#Adiciona uma linha de tendência exponencial no gráfico
			add=T, lty=1, col=rgb (1,0,0.7)) 					#de cor roxa
			
		}
		return (data.frame(Dap, bio))						#retorna uma lista com os 
												#resultados DAP e BIOMASSA em um data.frame
	}
	if (Methods =="Arv.DAP.Alt")							#Cálcula dados de DAP e Altura para biomassa
	{
		if (is.null(Dens)==FALSE)						#Função não cálcula dados de densidade
		{stop ("Equação não calcula Densidade da madeira")}		#Função  não permite uso 
												# de dados de densidade
		if (anyNA (Dap)|| anyNA(Alt))						#Teste lógico para verificar se há dados faltantes

		{stop ("DAP ou Altura comtém 	
		valores faltantes")}							#Função não opera com valores faltantes
		if (is.null(Alt)==TRUE)							#função para caso não haja dados de 
												#de Altura nessa opção de método
		{stop("Dados de Altura Nulo")}					#não permite 
		
		if (length (Dap) != length (Alt)) 					#teste lógico para premissa dos dados 
												#inseridos
		{stop ("comprimento das variáveis DAP e ALTURA não são iguais")} #Casos não forem iguais a função para 
												#e emite essa messagem
		if (class (Alt) != "numeric")						#teste loogico para os dados de altura,
												#função só opera dados númericos
		{stop ("Altura não númérico \n Função não Opera dados naõ numéricos")}#Para a função e retorna mensagem de erro
		a= -3.282									#Pârametro para cálculo de biomassa de 
												#árvore inteira segundo referência 3

		b= 0.95									#Pârametro para cálculo de biomassa de 
												#árvore inteira segundo referência 3

		bio= exp (a+(b*(log(Dap)^2)*Alt))					#Cálculo de Biomassa referência 3

		cat ("Equação alométrica ideal para árvores de florestas trópicais 
		úmidas,levendo em consideração a Altura e DAP, Para essa funções é ideal ter as 
		mediadas exatas de alturas das árvores\n")			#Mensagem informativa sobre o uso da equação.
		if (grafico == TRUE)							#Caso o usuário desejar que retorne um gráfico
		{
			x11()										#Abre uma nova janela gráfica
			par (mar=c(5,5,2,2),bty = "l", las= 1,				#Muda os pârametros gerais do gráfico
			cex.lab=1.4, cex =1.2, tcl=0.2)					#Alterando Margem (mar), as linhas laterais (bty)
													#o tamanho da legenda dos eixos e do título
			line=lm(Dap~bio+I(log(bio)^2))					#equação linear dos dados e resultados da biomasas
			co.m= coef(line)								#coeficientes da equação
			plot (Dap, bio, pch=19, col=rgb(0,0.7,0,0.3), xlab="DAP(cm)",#Plota um gráfico de disperção entre Biomassa e DAP
			ylab= "Biomassa(kg)")							#Utiliza os pontos de bolinha sólidos, verdes e com 30% 	
													#Transparência, Biomassa em kg e DAP em cm
			curve (co.m[1]+co.m[2]*x+co.m[3]*x^2, 					#Adiciona uma linha de tendência exponencial no gráfico
			add=T, lty=1, col=rgb (1,0,0.7)) 					#de cor roxa
		}
		return (data.frame(Dap,Alt,bio))						#retorna uma lista com os 
													#resultados DAP, Altura e BIOMASSA em um data.frame

	}
	if (Methods =="Arv.DAP.Alt.Dens")							#Calcula dados de DAP, Altura e Densidade da madeira para biomassa
	{
		if (anyNA (Dap)|| anyNA(Alt)|| anyNA (Dens))			#Teste lógico para verificar se há dados faltantes
		{stop ("DAP, Altura ou Densidade contém 	
		valores faltantes")}							#Função não opera com valores faltantes
		if (is.null(Alt)==TRUE||is.null(Dens)==TRUE)			#função para caso não haja dados de 
												#de Altura nessa opção de método
		{stop("Dados de Altura ou Densidade Nulo")}			#não permite 
		
		if (length (Dap) != length (Alt)) 					#teste lógico para premissa dos dados 
												#inseridos
		{stop ("comprimento das variáveis DAP e ALTURA não são iguais")} #Casos não forem iguais a função para 
												#e emite essa messagem
		if (length (Dap) != length (Dens)) 						#teste lógico para premissa dos dados 
												#inseridos
		{stop ("comprimento das variáveis DAP e DENSIDADE não são iguais")} #Casos não forem iguais a função para 
												#e emite essa messagem
		if (length (Dens) != length (Alt)) 						#teste lógico para premissa dos dados 
												#inseridos
		{stop ("comprimento das variáveis DENSIDADE e ALTURA não são iguais")} #Casos não forem iguais a função para 
												#e emite essa messagem
		if (class (Alt) != "numeric"|class (Dens) != "numeric")	#teste loogico para os dados de altura,
												#função só opera dados númericos
		{stop ("Altura ou Densidade não númérico \n Função não Opera dados naõ numéricos")}
		
		a= -2.977									#Pârametro para cálculo de biomassa de 
												#árvore inteira segundo referência 2

	
		bio= Dens*exp (a+log((Dens*((Dap)^2)*Alt)))			#Cálculo de Biomassa referência 2
		
		cat ("Equação alométrica ideal para árvores de florestas trópicais 
		úmidas,levando em consideração todo o tamanho da árvores e a densidade da madeira\n")#Mensagem informativa sobre o uso da equação.

		if (grafico == TRUE)							#Caso o usuário desejar que retorne um gráfico
		{
			x11()										#Abre uma nova janela gráfica
			par (mar=c(5,5,2,2),bty = "l", las= 1,				#Muda os parametros gerais do gráfico
			cex.lab=1.4, cex =1.2, tcl=0.2)					#Alterando Margem (mar), as linhas laterais (bty)
													#o tamanho da legenda dos eixos e do título
			line=lm(Dap~bio+I(log(Dap)^2)+I(Dens*Alt))			#equação linear dos dados e resultados da biomasas
			co.m= coef(line)								#coeficientes da equação
			plot (Dap, bio, pch=19, col=rgb(0,0.7,0,0.3), xlab="DAP(cm)",#Plota um gráfico de disperção entre Biomassa e DAP
			ylab= "Biomassa(kg)")							#Utiliza os pontos de bolinha sólidos, verdes e com 30% 	
													#Transparência, Biomassa em kg e DAP em cm
			curve (co.m[1]+co.m[2]*x+co.m[3]*x^2+co.m[4]*x^3,		#Adiciona uma linha de tendência exponencial no gráfico
			add=T, lty=1, col=rgb (1,0,0.7)) 					#de cor roxa

			
		}
	return (data.frame(Dap, Alt, Dens, bio))						#retorna uma lista com os 
													#resultados DAP, Altura e BIOMASSA em um data.frame
				
	}
	if (Methods == "palm.DAP")							#caso seja utilizada a função 
												#para dados só com DAP

	{
		if (is.null(Alt)==FALSE | is.null(Dens)==FALSE)			#teste lógico para analizar se 
												#há dados a mais na função
		{stop ("Equação não calcula Altura e/ou Densidade da madeira")}#Funçãonão permitir uso 
												#de dados de altura ou densidade
		RDap= sqrt (Dap)								#Raiz Quadrada para os valores de DAP
		a=6.6666									#Pârametro para cálculo de biomassa de 
												#árvore inteira segundo referência 4

		b=12.826									#Pârametro para cálculo de biomassa de 
												#árvore inteira segundo referência 4

		bio= a+((b*(RDap))*(log(Dap)))					#Cálculo de Biomassa referência 4
			
		cat ("Equação alométrica ideal para Palmeiras em de florestas trópicais úmidas\n")#Mensagem informativa sobre o uso da equação.
		if (grafico == TRUE)								#Caso o usuário desejar que retorne um gráfico
		{
			x11()										#Abre uma nova janela gráfica
			par (mar=c(5,5,2,2),bty = "l", las= 1,				#Muda os parametros gerais do gráfico
			cex.lab=1.4, cex =1.2, tcl=0.2)					#Alterando Margem (mar), as linhas laterais (bty)
													#o tamanho da legenda dos eixos e do título
			line=lm(Dap~bio+I(RDap*Dap))						#equação linear dos dados e resultados da biomasas
			co.m= coef(line)								#coeficientes da equação
			plot (Dap, bio, pch=19, col=rgb(0,0.7,0,0.3), xlab="DAP(cm)",#Plota um gráfico de disperção entre Biomassa e DAP
			ylab= "Biomassa(kg)")							#Utiliza os pontos de bolinha sólidos, verdes e com 30% 	
													#Transparência, Biomassa em kg e DAP em cm
			curve (co.m[1]+co.m[2]*x+co.m[3]*x^2, 				#Adiciona uma linha de tendência exponencial no gráfico
			add=T, lty=1, col=rgb (1,0,0.7)) 					#de cor roxa


		}
	return (list (data.frame(Dap, bio)))					#retorna uma lista com os 
												#resltados DAP e BIOMASSA em um data.frame
	}
	if (Methods == "palm.DAP.Alt")						#Cálcula dados de DAP e Altura para biomassa
	{
			if (is.null(Dens)==FALSE)						#Função não calcula dados de densidade
			{stop ("Equação não calcula Densidade da madeira")}		#Função  não permite uso 
													# de dados de densidade
			if (anyNA (Dap)|| anyNA(Alt))						#Teste lógico para verificar se há dados faltantes

			{stop ("DAP ou Altura comtém 	
			valores faltantes")}							#Função não opera com valores faltantes
			if (is.null(Alt)==TRUE)							#função para caso não haja dados de 
													#de Altura nessa opção de método	
			{stop("Dados de Altura Nulo")}					#não permite 
		
			if (length (Dap) != length (Alt)) 					#teste lógico para premissa dos dados 
													#inseridos
			{stop ("comprimento das variáveis DAP e ALTURA não são iguais")} #Casos não forem iguais a função para 
													#e emite essa messagem
			if (class (Dap) != "numeric"|class (Alt) != "numeric")	#Função só opera com dados numéricos
			{stop ("DAP ou Altura não númérico \n Função não Opera dados naõ numéricos")}#Para a função e retorna mensagem de erro
		a=-6.3789									#Pârametro para cálculo de biomassa de 
												#árvore inteira segundo referência 5

		b1=-0.877									#Pârametro para cálculo de biomassa de 
												#árvore inteira segundo referência 5

		b2=2.151									#Pârametro para cálculo de biomassa de 
												#árvore inteira segundo referência 5
									
		bio= exp(a+(b1*log(1/Dap))+(b2*log(Alt)))				#Cálculo de Biomassa referência 5
			
		cat ("Equação alométrica ideal para palmeiras em florestas trópicais 
		úmidas,levando em consideração todo o tamanho da árvores\n")#Mensagem informativa sobre o uso da equação.

		if (grafico == TRUE)							#Caso o usuário desejar que retorne um gráfico
		{
			x11()										#Abre uma nova janela gráfica
			par (mar=c(5,5,2,2),bty = "l", las= 1,				#Muda os parametros gerais do gráfico
			cex.lab=1.4, cex =1.2, tcl=0.2)					#Alterando Margem (mar), as linhas laterais (bty)
													#o tamanho da legenda dos eixos e do título
			line=lm(Dap~bio+I(log(1/Dap)^2)+I(log(Alt)^3))			#equação linear dos dados e resultados da biomasas
			co.m= coef(line)								#coeficientes da equação
			plot (Dap, bio, pch=19, col=rgb(0,0.7,0,0.3), xlab="DAP(cm)",#Plota um gráfico de disperção entre Biomassa e DAP
			ylab= "Biomassa(kg)")							#Utiliza os pontos de bolinha sólidos, verdes e com 30% 	
													#Transparência, Biomassa em kg e DAP em cm
			curve (co.m[1]+co.m[2]*x+co.m[3]*x^2+co.m[4]*x^3, 		#Adiciona uma linha de tendência exponencial no gráfico
			add=T, lty=1, col=rgb (1,0,0.7)) 					#de cor roxa


		}
		return (data.frame(Dap,Alt,bio))						#retorna uma lista com os 
													#resultados DAP, Altura e BIOMASSA em um data.frame

	}
	
}
			


#Referências

	#1 Vieira et al. (2008) Estimation of biomass and carbon stocks: the case of the Atlantic Forest, Biota Neotropica,2 (10), 21-29
	#2 Chaves et al. (2005) Tree allometry and improved estimation of carbon stocks and balance in tropical forests, Oecologia, (145), 87-99
	#3 Chambers et al. (2001) Tree damage, allometric relationships, and above-graound net primary production in central Amazon forest, Forest Ecology and Management, (152), 73-84 	
	#4 Saldarriaga et al. (1988)LONG-TERM CHRONOSEQUENCE OF FOREST SUCCESSION IN THE UPPER RIO NEGRO OF COLOMBIA AND VENEZUELA, Journal of Ecology, (76), 938-958 
	#5 Brow et al. (1989) Biomass Estimation Methods for Tropical Forest with Applications of Forest Inventory Data, 4 (35), 881-902 

			

