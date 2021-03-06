library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa inpc ao m�s(03/1999 - 01/2020 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Transformar o arquivo em uma s�rie temporal
inpc = ts(arq$inpc_mensal,start = c(1979,4), end=c(2019,12), frequency=12)

plot(inpc)
print(inpc)
#Aqui cabe uma observa��o quanto ao valores extremos nas d�cadas de 1980 e 1990, que dizem 
#respeito ao per�odo de hiperinfla��o, portanto iremos considerar o mesmo per�odo da taxa
#selic 01/1999 - 12/2019
inpcSemInflacao = window(inpc,start = c(1999,1), end=c(2019,12))
print(inpcSemInflacao)
plot(inpcSemInflacao)

#-------------------------------------------------------------------------------------

autoplot(inpcSemInflacao, ylab = "Percentual da taxa INPC", xlab = "Tempo", main = "Taxa INPC 1999/2019")
#A s�rie, visualmente, n�o aparenta ter sazonalidade, possui muita varia��o e excetuando um per�odo em 
#2002 se manteve com aumentos mesnsais entre 0 e 1.0%

#-------------------------------------------------------------------------------------

split.screen( c(1,2))
screen(1)
hist(inpcSemInflacao, xlab = "Percentual", ylab = "Frequ�ncia", main = "Histograma INPC")
screen(2)
boxplot(inpcSemInflacao, ylab = "Mediana", main = "Boxplot INPC")
close.screen(all=T)
summary(inpcSemInflacao)
#Histograma para ver como os dados est�o distribu�dos
#A maior parte do tempo a taxa sobe cerca de 0,5%am.
#Gerar um boxplot para entender como que a ocupa��o est� distribu�da
#Como visto no histograma a mediana est� por volta de 0,5% e uma taxa acima de
#1,5%(em torno de 1,5%) j� � considerada alta(Outlier)
#   Min.  1st Qu. Median    Mean  3rd Qu.    Max. 
#-0.3000  0.2375  0.4750  0.5237  0.7100  3.3900 


#-------------------------------------------------------------------------------------
#Vamos decompor a s�rie para entender melhor como se comporta a inpc
dec = decompose(inpcSemInflacao)
autoplot(dec, main = "Decomposi��o Taxa INPC 1999/2019", xlab="Tempo")  
#Existe um padr�o de sazonalidade. Pelo gr�fco ela � bem regular  
#N�o possui uma tend�ncia aparente no per�odo

#-------------------------------------------------------------------------------------
#At� agora nos vimos que a taxa m�dia da inpc � de cerca de 0,5%am.
#Vimos que taxas acima de 1,5% � exce��o e n�o regra;
#Existe um padr�o sazonal frequente
#No per�odo analisado existe uma certa estabilidade

#-------------------------------------------------------------------------------------
#Vamos analisar a tend�ncia com mais cuidado:
autoplot(dec$trend)
#A taxa varia bastante ao longo dos anos, por�m em meados de 2002 e tamb�m 2013 apresentou uma
#forte acelera��o conincide com crises econ�micas e conturbados per�odos eleitorais.
#Em 2016 iniciou-se uma sequ�ncia de desacelera��es mensais.
autoplot(window(dec$trend, start=c(2015,7)), ylab = "Tend�ncia da taxa INPC", xlab = "Tempo", main = " Tend�ncia INPC 1999/2019")
#Essa tend�ncia de sesacelera��o do inpc come�ou em julho de 2015, estabilizou durante 2017
#voltando a acelerar em 2018 chegando a 1,2%am, em dezembro de 2019 .


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupa��o sazonal 2000-2020 (cada ano � uma cor)
ggseasonplot(inpcSemInflacao)  
#Aqui se confirma que existe um padr�o bem regular na sazonalidade  
#As aceler��es permanecem entre 0% e 1% ao longo do ano
#No ano de 2002 o outlier fica bem evidente  

#Ver a Taxa entre 1999 e 2003:
ggseasonplot(window(inpc, start=c(2002,01), end = c(2003, 12)))
#Confirma��o dos dados anteriores, apresentando apenas dados mais limpos





