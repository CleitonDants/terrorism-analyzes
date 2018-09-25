#####################################################################
#						PACOTES/BIBLIOTECAS							#
#####################################################################
install.packages("RColorBrewer")
require("RColorBrewer")
library(ggplot2)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#####################################################################
#								DEFAULT								#
#####################################################################
options(scipen=10)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#####################################################################
#								BANCO								#
#####################################################################
#Carregando o banco e guardando-o na variável “terrorismo”
#Trocar o endereço pela localização do banco no seu respectivo computador
terrorism <- read.csv("C:/Users/ricar/Google Drive/BK_PC/EACH-MQAAE1/MQA_Trabalho/globalterrorismdb_0718dist.csv", header=FALSE)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#####################################################################
#							MISSING DATA							#
#####################################################################

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#####################################################################
#						ANÁLISES POR VARIÁVEL						#
#####################################################################
#-------------------------------------------------------------------#
#																	#
#>>>>>>>>>>>>>>>>>>>>		Regiões Atacadas	<<<<<<<<<<<<<<<<<<<<#
#																	#
#-------------------------------------------------------------------#
#-------------------------Grafico de Barras-------------------------#
#-------------------------------------------------------------------#
#Capturando a coluna 11 do banco, criando um sumário da mesma, e delimitando um número máximo de elementos para o sumário
regioes <- summary(terrorism$V11[-1], maxsum = 10)
#Criando um gráfico de barras a partir dos dados da variável “regioes", especificando o título, e as cores de cada barra (delimitadas em um vetor)
barplot(
	regioes,
	main = "Barplot de Atentados Terroristas por Região (1970-2017)",
	las = 2,
	col = c("darkred", "darkblue", "darkgreen", "darkgrey", "saddlebrown", "gold", "darkorange2", "darkorchid", "darkturquoise", "white"),
	names.arg = ""
)
#Criando uma legenda para o gráfico, definindo a posição dela no gráfico, passando os nomes dos elementos do gráfico, e logo após as respectivas cores de cada elemento (delimitadas em um vetor). Também foi especificada a largura da legenda (3), e o tamanho da fonte (0.8)
legend(
	"topright",
	names(regioes),
	fill = c("darkred", "darkblue", "darkgreen", "darkgrey", "saddlebrown", "gold", "darkorange2", "darkorchid", "darkturquoise", "white"),
	text.width = 3,
	cex = 0.8
)
#-------------------------------------------------------------------#
#-------------------------Gráfico de Pizza--------------------------#
#-------------------------------------------------------------------#
pie(
	regioes,
	main = "Gráfico de Setores de Atentados Terroristas por Região (1970-2017)",
	col = c("darkred", "darkblue", "darkgreen", "darkgrey", "saddlebrown", "gold", "darkorange2", "darkorchid", "darkturquoise", "white")
)

#-------------------------------------------------------------------#
#------------------------------Boxplot------------------------------#
#-------------------------------------------------------------------#
regioes <- summary(terrorism$V11[-1])[-8]

boxplot(
	regioes,
	main = "Boxplot de Atentados Terroristas por Região (1970-2017)",
	las = 2
)
#-------------------------------------------------------------------#
#---------Mínimo-1ºQuartil-Mediana-Média-3ºQuartil-Máximo-----------#
#-------------------------------------------------------------------#
summary(
	regioes
)
#-------------------------------------------------------------------#
#--------------------Variancia-Desvio Padrão------------------------#
#-------------------------------------------------------------------#
writeLines(
	paste(
		"Variancia: ", var(regioes),
		"\nDesvio Padrão: ", sd(regioes)
	)
)
#------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------#
#																	#
#>>>>>>>>>>>>>>>>>>		Atentados por Ano		<<<<<<<<<<<<<<<<<<<<#
#																	#
#-------------------------------------------------------------------#
#-------------------------Grafico de Barras-------------------------#
#-------------------------------------------------------------------#
anos <- summary(terrorism$V2)[-48]
cores <- c()
gerar_grad <- colorRampPalette(c("darkgreen", "gold3", "gold4", "red4", "darkred"))
gradiente <- gerar_grad(47)

for (i in 1:47){
	cores[i] = gradiente[(47/anos["2014"])*anos[i]]
}

barplot(
	anos,
	main = "Barplot de Atentados Terroristas por Ano (1970-2017)",
	las = 2,
	col = cores
)
#-------------------------------------------------------------------#
#-------------------------Gráfico de Pizza--------------------------#
#-------------------------------------------------------------------#
pie(
	anos,
	main = "Gráfico de Setores de Atentados Terroristas por Ano (1970-2017)",
	col = c("darkred", "darkblue", "darkgreen", "darkgrey", "saddlebrown", "gold", "darkorange2", "darkorchid", "darkturquoise", "white")
)
#-------------------------------------------------------------------#
#------------------------------Boxplot------------------------------#
#-------------------------------------------------------------------#
boxplot(
	anos,
	main = "Boxplot de Atentados Terroristas por Ano (1970-2017)",
	las = 2
)
#-------------------------------------------------------------------#
#---------Mínimo-1ºQuartil-Mediana-Média-3ºQuartil-Máximo-----------#
#-------------------------------------------------------------------#
summary(
	anos
)
#-------------------------------------------------------------------#
#--------------------Variancia-Desvio Padrão------------------------#
#-------------------------------------------------------------------#
writeLines(
	paste(
		"Variancia: ", var(anos),
		"\nDesvio Padrão: ", sd(anos)
	)
)
#------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------#
#																	#
#>>>>>>>>>>>>>>>>>>		Métodos Usados		<<<<<<<<<<<<<<<<<<<<<<<<#
#																	#
#-------------------------------------------------------------------#
#-------------------------Grafico de Barras-------------------------#
#-------------------------------------------------------------------#
metodos <- summary((terrorism[-1,])$V30, maxsum=7)

barplot(
	metodos,
	main = "Barplot de Métodos de Ataque (1970-2017)",
	las = 2,
	col = brewer.pal(n=7, name="Set1"),
	names.arg = ""
)
legend(
	"topright",
	names(metodos),
	fill = brewer.pal(n=7, name="Set1"),
	text.width = 3,
	cex = 1
)

#-------------------------------------------------------------------#
#-------------------------Gráfico de Pizza--------------------------#
#-------------------------------------------------------------------#
pie(
	metodos,
	main = "Gráfico de Setores de Métodos de Ataque (1970-2017)",
	col = brewer.pal(n=7, name="Set1")
)
#-------------------------------------------------------------------#
#------------------------------Boxplot------------------------------#
#-------------------------------------------------------------------#
metodos <- summary((terrorism[-1,])$V30)[-3]

boxplot(
	metodos,
	main = "Boxplot de Métodos de Ataque (1970-2017)",
	las = 2
)
#-------------------------------------------------------------------#
#---------Mínimo-1ºQuartil-Mediana-Média-3ºQuartil-Máximo-----------#
#-------------------------------------------------------------------#
summary(
	metodos
)
#-------------------------------------------------------------------#
#--------------------Variancia-Desvio Padrão------------------------#
#-------------------------------------------------------------------#
writeLines(
	paste(
		"Variancia: ", var(metodos),
		"\nDesvio Padrão: ", sd(metodos)
	)
)
#------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------#
#																	#
#>>>>>>>>>>>>>>>>>>>>		Objetivos		<<<<<<<<<<<<<<<<<<<<<<<<#
#																	#
#-------------------------------------------------------------------#
#-------------------------Grafico de Barras-------------------------#
#-------------------------------------------------------------------#
objetivos <- summary((terrorism[-1,])$V36, maxsum=14)

barplot(
	objetivos[1:7],
	main = "Barplot dos 7 Maiores Objetivos dos Atentados (1970-2017)",
	las = 2,
	col = brewer.pal(n=7, name="Paired"),
	names.arg = ""
)
legend(
	"topright",
	names(objetivos[1:7]),
	fill = brewer.pal(n=7, name="Paired"),
	text.width = 1.5,
	cex = 1
)

barplot(
	c(objetivos[1],objetivos[8:14]),
	main = "Barplot de Outros Objetivos dos Atentados Comparados ao Maior Objetivo (1970-2017)",
	las = 2,
	col = brewer.pal(n=8, name="Paired"),
	names.arg = ""
)
legend(
	"topright",
	names(c(objetivos[1],objetivos[8:14])),
	fill = brewer.pal(n=8, name="Paired"),
	text.width = 2	,
	cex = 1
)

#-------------------------------------------------------------------#
#-------------------------Gráfico de Pizza--------------------------#
#-------------------------------------------------------------------#
pie(
	c(objetivos[1],objetivos[8:14]),
	main = "Gráfico de Setores de Outros Objetivos dos Atentados Comparados ao Maior Objetivo (1970-2017)",
	col = brewer.pal(n=8, name="Set1")
)
#-------------------------------------------------------------------#
#------------------------------Boxplot------------------------------#
#-------------------------------------------------------------------#
objetivos <- summary((terrorism[-1,])$V36)[-16]

boxplot(
	objetivos,
	main = "Boxplot dos Objetivos dos Ataques (1970-2017)",
	las = 2
)
#-------------------------------------------------------------------#
#---------Mínimo-1ºQuartil-Mediana-Média-3ºQuartil-Máximo-----------#
#-------------------------------------------------------------------#
summary(
	objetivos
)
#-------------------------------------------------------------------#
#--------------------Variancia-Desvio Padrão------------------------#
#-------------------------------------------------------------------#
writeLines(
	paste(
		"Variancia: ", var(objetivos),
		"\nDesvio Padrão: ", sd(objetivos)
	)
)
#------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------#
#																	#
#>>>>>>>>>>>>>>>>>>>		Mortes por Ano		<<<<<<<<<<<<<<<<<<<<#
#																	#
#-------------------------------------------------------------------#
#-------------------------Grafico de Barras-------------------------#
#-------------------------------------------------------------------#
mortesAno <- c()

for(i in 1970:2017){
    mortesAno[i] = sum(as.numeric(terrorism[terrorism$V2==i,]$V99))
}

cores <- c()
gerar_grad <- colorRampPalette(c("darkgreen", "gold3", "gold4", "red4", "darkred"))
gradiente <- gerar_grad(48)

for (i in 1:48){
	set = ceiling((48/mortesAno[2014])*mortesAno[(i+1969)])
	if (set != 0){
		cores[i] = gradiente[set]
	}
}

barplot(
    mortesAno[1970:2017],
    main = "Barplot de Mortes por Ano (1970-2017)",
    las = 2,
    col = cores,
    names.arg = 1970:2017
)


#-------------------------------------------------------------------#
#------------------------------Grafico------------------------------#
#-------------------------------------------------------------------#
plot(
	mortesAno,
	main = "Gráfico de Mortes por Ano (1970-2017)",
	las = 2,
	xlim = c(1970,2017),
	ylab = "",
	xlab = "",
	type = "l",
	lwd = 2
)

#-------------------------------------------------------------------#
#------------------------------Boxplot------------------------------#
#-------------------------------------------------------------------#
boxplot(
	mortesAno[1970:2017],
	main = "Boxplot de Mortes por Ano (1970-2017)",
	las = 2
)
#-------------------------------------------------------------------#
#---------Mínimo-1ºQuartil-Mediana-Média-3ºQuartil-Máximo-----------#
#-------------------------------------------------------------------#
summary(
	mortesAno[1970:2017]
)
#-------------------------------------------------------------------#
#--------------------Variancia-Desvio Padrão------------------------#
#-------------------------------------------------------------------#
writeLines(
	paste(
		"Variancia: ", var(mortesAno[1970:2017]),
		"\nDesvio Padrão: ", sd(mortesAno[1970:2017])
	)
)
#------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------#
#																	#
#>>>>>>>>>>>>		Ataques(mult, indiv, suc, doub)		<<<<<<<<<<<<#
#																	#
#-------------------------------------------------------------------#
#-------------------------Graficos de Pizza-------------------------#
#-------------------------------------------------------------------#
atkMult <- summary(terrorism$V26[-1])[-1]

pie(
    atkMult,
    main = "Gráfico de Setores de Atentados Conectados X Atentados Não Conectados (1970-2017)",
    col = c("red3", "green4"),
    labels = c(
    			paste(
    				"Não Conectados: ",
    				round(
    					atkMult[1]/(sum(atkMult[1],atkMult[2]))*100
					), "%"
				),
               paste(
               		"Conectados: ",
                    round(
                    	atkMult[2]/(sum(atkMult[1],atkMult[2]))*100
                	), "%"
               )
          )
)

atkIndiv <- summary(terrorism$V69[-1])

pie(
    atkIndiv,
    main = "Gráfico de Setores de Atentados Individuais X Atentados Coletivos (1970-2017)",
    col = c("red3", "green4"),
    labels = c(
    			paste(
    				"Coletivos: ",
    				round(
    					atkIndiv[1]/(sum(atkIndiv[1],atkIndiv[2]))*100, 3
					), "%"
				),
               paste(
               		"Individuais: ",
                    round(
                    	atkIndiv[2]/(sum(atkIndiv[1],atkIndiv[2]))*100, 3
                	), "%"
               )
          )
)

atkSuc <- summary(terrorism$V27[-1])

pie(
    atkSuc,
    main = "Gráfico de Setores de Atentados Bem Sucedidos X Atentados Mal Sucedidos (1970-2017)",
    col = c("red3", "green4"),
    labels = c(
    			paste(
    				"Mal Sucedidos: ",
    				round(
    					atkSuc[1]/(sum(atkSuc[1],atkSuc[2]))*100
					), "%"
				),
               paste(
               		"Bem Sucedidos: ",
                    round(
                    	atkSuc[2]/(sum(atkSuc[1],atkSuc[2]))*100
                	), "%"
               )
          )
)

atkDoub <- summary(terrorism$V23[-1])[-1]

pie(
    atkDoub,
    main = "Gráfico de Setores de Atentados Duvidosos de Serem Terrorismo (1970-2017)",
    col = c("grey", "red3", "green4"),
    labels = c(
    			paste(
    				"Não Avaliados: ",
    				round(
    					atkDoub[1]/(sum(atkDoub[1],atkDoub[2],atkDoub[3]))*100
					), "%"
				),
    			paste(
    				"Não há Dúvidas: ",
    				round(
    					atkDoub[2]/(sum(atkDoub[1],atkDoub[2],atkDoub[3]))*100
					), "%"
				),
               paste(
               		"Há Dúvidas: ",
                    round(
                    	atkDoub[3]/(sum(atkDoub[1],atkDoub[2],atkDoub[3]))*100
                	), "%"
               )
          )
)
#------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------#
#																	#
#>>>>>>>>>>>>>>>		Nacionalidade dos Alvos		<<<<<<<<<<<<<<<<#
#																	#
#-------------------------------------------------------------------#
#-------------------------Grafico de Barras-------------------------#
#-------------------------------------------------------------------#
natAlvos <- summary((terrorism[-1,])$V42, maxsum=9)[-9]

barplot(
	natAlvos,
	main = "Barplot das 8 Maiores Nacionalidades Alvos de Atentados (1970-2017)",
	las = 2,
	col = brewer.pal(n=8, name="Dark2"),
	names.arg = ""
)
legend(
	"topright",
	names(natAlvos),
	fill = brewer.pal(n=8, name="Dark2"),
	text.width = 3,
	cex = 0.85
)

#-------------------------------------------------------------------#
#-------------------------Gráfico de Pizza--------------------------#
#-------------------------------------------------------------------#
pie(
	natAlvos,
	main = "Gráfico de Setores das 8 Maiores Nacionalidades dos Alvos de Atentados (1970-2017)",
	col = brewer.pal(n=8, name="Dark2")
)
#-------------------------------------------------------------------#
#------------------------------Boxplot------------------------------#
#-------------------------------------------------------------------#
natAlvos <- summary((terrorism[-1,])$V42, maxsum=220)[-131]

boxplot(
	natAlvos,
	main = "Boxplot das Nacionalidades dos Alvos de Atentados (1970-2017)",
	las = 2
)
#-------------------------------------------------------------------#
#---------Mínimo-1ºQuartil-Mediana-Média-3ºQuartil-Máximo-----------#
#-------------------------------------------------------------------#
summary(
	natAlvos
)
#-------------------------------------------------------------------#
#--------------------Variancia-Desvio Padrão------------------------#
#-------------------------------------------------------------------#
writeLines(
	paste(
		"Variancia: ", var(natAlvos),
		"\nDesvio Padrão: ", sd(natAlvos)
	)
)
#------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------#
#																	#
#>>>>>>>>>>>>>>>>>>		Grupos Responsáveis		<<<<<<<<<<<<<<<<<<<<#
#																	#
#-------------------------------------------------------------------#
#-------------------------Grafico de Barras-------------------------#
#-------------------------------------------------------------------#
grupos <- sort(summary((terrorism[-1,])$V59, maxsum=4000), decreasing=TRUE)[1:8]

barplot(
	grupos,
	main = "Barplot dos Grupos Responsáveis pelos Atentados (1970-2017)",
	las = 2,
	col = brewer.pal(n=8, name="Dark2"),
	names.arg = ""
)
legend(
	"topright",
	names(grupos),
	fill = brewer.pal(n=8, name="Dark2"),
	text.width = 3,
	cex = 0.85
)

#-------------------------------------------------------------------#
#-------------------------Gráfico de Pizza--------------------------#
#-------------------------------------------------------------------#
grupos <- sort(summary((terrorism[-1,])$V59, maxsum=4000), decreasing=TRUE)[-3538]

pie(
	grupos,
	main = "Gráfico de Setores dos Grupos Responsáveis pelos Atentados (1970-2017)",
	col = brewer.pal(n=10, name="Paired")
)
#-------------------------------------------------------------------#
#------------------------------Boxplot------------------------------#
#-------------------------------------------------------------------#
boxplot(
	grupos,
	main = "Boxplot dos Grupos Responsáveis pelos Atentados (1970-2017)",
	las = 2
)
#-------------------------------------------------------------------#
#---------Mínimo-1ºQuartil-Mediana-Média-3ºQuartil-Máximo-----------#
#-------------------------------------------------------------------#
summary(
	grupos
)
#-------------------------------------------------------------------#
#--------------------Variancia-Desvio Padrão------------------------#
#-------------------------------------------------------------------#
writeLines(
	paste(
		"Variancia: ", var(grupos),
		"\nDesvio Padrão: ", sd(grupos)
	)
)
#------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------#
#																	#
#>>>>>>>>>>>>>>>>>>>		Tipos de Armas		<<<<<<<<<<<<<<<<<<<<#
#																	#
#-------------------------------------------------------------------#
#-------------------------Grafico de Barras-------------------------#
#-------------------------------------------------------------------#
armas <- sort(summary((terrorism[-1,])$V83), decreasing=TRUE)[-13]

barplot(
	armas,
	main = "Barplot dos Tipos de Armas Usadas (1970-2017)",
	las = 2,
	col = c(brewer.pal(n=10, name="Accent"),"orange","red"),
	names.arg = ""
)
legend(
	"topright",
	names(armas),
	fill = c(brewer.pal(n=10, name="Accent"),"orange","red"),
	text.width = 5.5,
	cex = 0.8
)

#-------------------------------------------------------------------#
#-------------------------Gráfico de Pizza--------------------------#
#-------------------------------------------------------------------#
pie(
	armas,
	main = "Gráfico de Setores dos Tipos de Armas Usadas (1970-2017)",
	col = c(brewer.pal(n=10, name="Accent"),"orange","red")
)
#-------------------------------------------------------------------#
#------------------------------Boxplot------------------------------#
#-------------------------------------------------------------------#
boxplot(
	armas,
	main = "Boxplot dos Tipos de Armas Usadas (1970-2017)",
	las = 2
)
#-------------------------------------------------------------------#
#---------Mínimo-1ºQuartil-Mediana-Média-3ºQuartil-Máximo-----------#
#-------------------------------------------------------------------#
summary(
	armas
)
#-------------------------------------------------------------------#
#--------------------Variancia-Desvio Padrão------------------------#
#-------------------------------------------------------------------#
writeLines(
	paste(
		"Variancia: ", var(armas),
		"\nDesvio Padrão: ", sd(armas)
	)
)
#------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------#
#																	#
#>>>>>>>>>>>		Mortes/Ano por Atentados/Ano		<<<<<<<<<<<<#
#																	#
#-------------------------------------------------------------------#
#------------------------Grafico de Dispersão-----------------------#
#-------------------------------------------------------------------#
anos <- summary(terrorism$V2)[-48]
anos2[1970:2017] = c(anos[1:33],0,anos[34:47]);

mortesAno <- c()
for(i in 1970:2017){
    mortesAno[i] = sum(as.numeric(terrorism[terrorism$V2==i,]$V99))
}

plot(
	anos2[1970:2017],
	mortesAno[1970:2017],
	ylab = "Mortes por Ano",
	xlab = "Atentados por Ano",
	main = "Gráfico de Dispersão de Mortes/Anos por Atentados/Ano"
)