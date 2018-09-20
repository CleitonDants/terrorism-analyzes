#Carregando o banco e guardando-o na variável “terrorismo”
terrorism <- read.csv("C:/Users/ricar/Google Drive/BK_PC/EACH-MQAAE1/MQA_Trabalho/globalterrorismdb_0718dist.csv", header=FALSE)
#Capturando a coluna 11 do banco, criando um sumário da mesma, e delimitando um número máximo de elementos para o sumário
regioes <- summary(terrorism$V11, maxsum = 10)
#Criando um gráfico de barras a partir dos dados da variável “regioes", especificando o título, as cores de cada barra (delimitadas em um vetor), e o texto dos nomes de cara barra na parte inferior
barplot(
	regioes,
	main = "Atentados Terroristas por Países (1970-2018)",
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