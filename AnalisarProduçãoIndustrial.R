library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa balanca ao mês(01/1995 - 11/2019 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")

print(arq)


#Transformar o arquivo em uma série temporal
industria = ts(arq$prod_industrial_mensal,start = c(2002,1), end=c(2019,11), frequency=12)

plot(industria)
print(industria)
#-------------------------------------------------------------------------------------

autoplot(industria, ylab = "Índice da Produção Industrial", xlab = "Tempo", main = "Índice da Produção Industrial 2002/2019")
#A série, visualmente, aparenta ter bastantes sazonalidade, possui variação similar durante o ano. 
#Aparentemente em 2009 o índice teve uma brusca queda.
#Em 2014 a sazonalidade foi mantida, porém teve uma visivel queda geral dos índices,
#provavelmente por reflexo da crise econômica.

#Esse índice:
#O Índice Base Fixa Mensal (número-índice) compara a produção do mês de referência do índice
#com a média mensal produzida no ano base da pesquisa (2012). Foi realizado o encadeamento das
#séries de Índices de Base Fixa, encerradas em fevereiro de 2014 (base média 2002 = 100), com 
#a série que se iniciou em janeiro de 2012 (base 2012 = 100), mantendo-se inalteradas as comparações
#mês contra igual mês do ano anterior, acumulado no ano e acumulado nos últimos 12 meses.
#São produzidos índices com e sem ajuste sazonal para a indústria geral, seções industriais
#(extrativa e transformação), atividades industriais e categorias de uso e índices sem ajuste
#sazonal para 103 grupos e classes selecionados da indústria. O ajuste sazonal das séries é 
#obtido utilizando-se o método X-12 ARIMA.

#-------------------------------------------------------------------------------------

split.screen( c(1,2))
screen(1)
hist(industria, xlab = "Índice", ylab = "Frequência", main = "Histograma produção")
screen(2)
boxplot(industria, ylab = "Mediana", main = "Boxplot produção")
close.screen(all=T)
summary(industria)
#Histograma para ver como os dados estão distribuídos
#A maior parte do tempo a índice permaneceu entre 85 e 100.
#Gerar um boxplot para entender como que o índice está distribuído
#Como visto no histograma a mediana está por volta de 92 e  não há outliers
#   Min.  1st Qu. Median    Mean   3rd Qu.   Max. 
# 69.70   85.55   92.10    92.16   98.90  112.60 


#-------------------------------------------------------------------------------------
#Vamos decompor a série para entender melhor como se comporta a produção
dec = decompose(industria)
autoplot(dec, main = "Decomposição do Índice da Produção Industrial 2002/2019", xlab="Tempo")  
#Existe um padrão de sazonalidade. Pelo gráfco ela é bem regular  
#Possui uma tendência de tímida recuperação aparente no período 2015-2019.
#A tendência parece acomppanhar as crises econômicas externas e internas.

#-------------------------------------------------------------------------------------
#Até agora nos vimos que a índice médio da produção é de cerca de 92.
#Existe um padrão sazonal frequente
#O período analisado demonstra uma ligeira recuperação após a queda em 2014

#-------------------------------------------------------------------------------------
#Vamos analisar a tendência com mais cuidado:
autoplot(dec$trend)
#O índice apresentou um expressivo aumento até 2010, houve uma queda, porém com rápida recuperação
#No final de 2013 iniciou-se uma grande queda que provavelmente ocorreu por conta da crise
#Estamos em aparente processo de recuperação.
autoplot(window(dec$trend, start=c(2013,9)))
#Essa forte queda na produção começou em setembro de 2013, estabilizou durante 2016
#voltando a apresentar tímida recuperação desde então.


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupação sazonal 2002-2019 (cada ano é uma cor)
ggseasonplot(industria)  
#Aqui se confirma que existe um padrão bem regular na sazonalidade  
#Mesmo com a diferença de índice as sazonalidades demontram que há um aumento da produção
#entre Fevereito e Setembro e uma Queda entre Outubro e Janeiro (Provavel causa férias e períodos pré-carnaval) 





