library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa inpc ao mês(03/1999 - 01/2020 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Transformar o arquivo em uma série temporal
inpc = ts(arq$inpc_mensal,start = c(1979,4), end=c(2019,12), frequency=12)

plot(inpc)
print(inpc)
#Aqui cabe uma observação quanto ao valores extremos nas décadas de 1980 e 1990, que dizem 
#respeito ao período de hiperinflação, portanto iremos considerar o mesmo período da taxa
#selic 01/1999 - 12/2019
inpcSemInflacao = window(inpc,start = c(1999,1), end=c(2019,12))
print(inpcSemInflacao)
plot(inpcSemInflacao)

#-------------------------------------------------------------------------------------

autoplot(inpcSemInflacao, ylab = "Percentual da taxa INPC", xlab = "Tempo", main = "Taxa INPC 1999/2019")
#A série, visualmente, não aparenta ter sazonalidade, possui muita variação e excetuando um período em 
#2002 se manteve com aumentos mesnsais entre 0 e 1.0%

#-------------------------------------------------------------------------------------

split.screen( c(1,2))
screen(1)
hist(inpcSemInflacao, xlab = "Percentual", ylab = "Frequência", main = "Histograma INPC")
screen(2)
boxplot(inpcSemInflacao, ylab = "Mediana", main = "Boxplot INPC")
close.screen(all=T)
summary(inpcSemInflacao)
#Histograma para ver como os dados estão distribuídos
#A maior parte do tempo a taxa sobe cerca de 0,5%am.
#Gerar um boxplot para entender como que a ocupação está distribuída
#Como visto no histograma a mediana está por volta de 0,5% e uma taxa acima de
#1,5%(em torno de 1,5%) já é considerada alta(Outlier)
#   Min.  1st Qu. Median    Mean  3rd Qu.    Max. 
#-0.3000  0.2375  0.4750  0.5237  0.7100  3.3900 


#-------------------------------------------------------------------------------------
#Vamos decompor a série para entender melhor como se comporta a inpc
dec = decompose(inpcSemInflacao)
autoplot(dec, main = "Decomposição Taxa INPC 1999/2019", xlab="Tempo")  
#Existe um padrão de sazonalidade. Pelo gráfco ela é bem regular  
#Não possui uma tendência aparente no período

#-------------------------------------------------------------------------------------
#Até agora nos vimos que a taxa média da inpc é de cerca de 0,5%am.
#Vimos que taxas acima de 1,5% é exceção e não regra;
#Existe um padrão sazonal frequente
#No período analisado existe uma certa estabilidade

#-------------------------------------------------------------------------------------
#Vamos analisar a tendência com mais cuidado:
autoplot(dec$trend)
#A taxa varia bastante ao longo dos anos, porém em meados de 2002 e também 2013 apresentou uma
#forte aceleração conincide com crises econômicas e conturbados períodos eleitorais.
#Em 2016 iniciou-se uma sequência de desacelerações mensais.
autoplot(window(dec$trend, start=c(2015,7)), ylab = "Tendência da taxa INPC", xlab = "Tempo", main = " Tendência INPC 1999/2019")
#Essa tendência de sesaceleração do inpc começou em julho de 2015, estabilizou durante 2017
#voltando a acelerar em 2018 chegando a 1,2%am, em dezembro de 2019 .


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupação sazonal 2000-2020 (cada ano é uma cor)
ggseasonplot(inpcSemInflacao)  
#Aqui se confirma que existe um padrão bem regular na sazonalidade  
#As acelerções permanecem entre 0% e 1% ao longo do ano
#No ano de 2002 o outlier fica bem evidente  

#Ver a Taxa entre 1999 e 2003:
ggseasonplot(window(inpc, start=c(2002,01), end = c(2003, 12)))
#Confirmação dos dados anteriores, apresentando apenas dados mais limpos





