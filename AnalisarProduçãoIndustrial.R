library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa balanca ao m�s(01/1995 - 11/2019 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")

print(arq)


#Transformar o arquivo em uma s�rie temporal
industria = ts(arq$prod_industrial_mensal,start = c(2002,1), end=c(2019,11), frequency=12)

plot(industria)
print(industria)
#-------------------------------------------------------------------------------------

autoplot(industria, ylab = "�ndice da Produ��o Industrial", xlab = "Tempo", main = "�ndice da Produ��o Industrial 2002/2019")
#A s�rie, visualmente, aparenta ter bastantes sazonalidade, possui varia��o similar durante o ano. 
#Aparentemente em 2009 o �ndice teve uma brusca queda.
#Em 2014 a sazonalidade foi mantida, por�m teve uma visivel queda geral dos �ndices,
#provavelmente por reflexo da crise econ�mica.

#Esse �ndice:
#O �ndice Base Fixa Mensal (n�mero-�ndice) compara a produ��o do m�s de refer�ncia do �ndice
#com a m�dia mensal produzida no ano base da pesquisa (2012). Foi realizado o encadeamento das
#s�ries de �ndices de Base Fixa, encerradas em fevereiro de 2014 (base m�dia 2002 = 100), com 
#a s�rie que se iniciou em janeiro de 2012 (base 2012 = 100), mantendo-se inalteradas as compara��es
#m�s contra igual m�s do ano anterior, acumulado no ano e acumulado nos �ltimos 12 meses.
#S�o produzidos �ndices com e sem ajuste sazonal para a ind�stria geral, se��es industriais
#(extrativa e transforma��o), atividades industriais e categorias de uso e �ndices sem ajuste
#sazonal para 103 grupos e classes selecionados da ind�stria. O ajuste sazonal das s�ries � 
#obtido utilizando-se o m�todo X-12 ARIMA.

#-------------------------------------------------------------------------------------

split.screen( c(1,2))
screen(1)
hist(industria, xlab = "�ndice", ylab = "Frequ�ncia", main = "Histograma produ��o")
screen(2)
boxplot(industria, ylab = "Mediana", main = "Boxplot produ��o")
close.screen(all=T)
summary(industria)
#Histograma para ver como os dados est�o distribu�dos
#A maior parte do tempo a �ndice permaneceu entre 85 e 100.
#Gerar um boxplot para entender como que o �ndice est� distribu�do
#Como visto no histograma a mediana est� por volta de 92 e  n�o h� outliers
#   Min.  1st Qu. Median    Mean   3rd Qu.   Max. 
# 69.70   85.55   92.10    92.16   98.90  112.60 


#-------------------------------------------------------------------------------------
#Vamos decompor a s�rie para entender melhor como se comporta a produ��o
dec = decompose(industria)
autoplot(dec, main = "Decomposi��o do �ndice da Produ��o Industrial 2002/2019", xlab="Tempo")  
#Existe um padr�o de sazonalidade. Pelo gr�fco ela � bem regular  
#Possui uma tend�ncia de t�mida recupera��o aparente no per�odo 2015-2019.
#A tend�ncia parece acomppanhar as crises econ�micas externas e internas.

#-------------------------------------------------------------------------------------
#At� agora nos vimos que a �ndice m�dio da produ��o � de cerca de 92.
#Existe um padr�o sazonal frequente
#O per�odo analisado demonstra uma ligeira recupera��o ap�s a queda em 2014

#-------------------------------------------------------------------------------------
#Vamos analisar a tend�ncia com mais cuidado:
autoplot(dec$trend)
#O �ndice apresentou um expressivo aumento at� 2010, houve uma queda, por�m com r�pida recupera��o
#No final de 2013 iniciou-se uma grande queda que provavelmente ocorreu por conta da crise
#Estamos em aparente processo de recupera��o.
autoplot(window(dec$trend, start=c(2013,9)))
#Essa forte queda na produ��o come�ou em setembro de 2013, estabilizou durante 2016
#voltando a apresentar t�mida recupera��o desde ent�o.


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupa��o sazonal 2002-2019 (cada ano � uma cor)
ggseasonplot(industria)  
#Aqui se confirma que existe um padr�o bem regular na sazonalidade  
#Mesmo com a diferen�a de �ndice as sazonalidades demontram que h� um aumento da produ��o
#entre Fevereito e Setembro e uma Queda entre Outubro e Janeiro (Provavel causa f�rias e per�odos pr�-carnaval) 





