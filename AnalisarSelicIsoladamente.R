library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa selic ao m�s(03/1999 - 01/2020 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Transformar o arquivo em uma s�rie temporal
selic = ts(arq$meta_selic,start = c(1999,3), end=c(2019,12), frequency=12)

plot(selic)

#-------------------------------------------------------------------------------------

autoplot(selic, ylab = "Percentual da taxa Selic", xlab = "Tempo", main = "Taxa Selic 1999/2019")
#A s�rie, visualmente, n�o aparenta ter sazonalidade, possui alguma varia��o e parece ter uma
#tend�ncia de queda

#-------------------------------------------------------------------------------------
#Divis�o de tela para apresenta��o de Histograma e Boxplot
split.screen( c(1,2))
screen(1)
hist(selic, xlab = "Percentual", ylab = "Frequ�ncia", main = "Histograma Selic")
screen(2)
boxplot(selic, ylab = "Mediana", main = "Boxplot Selic")
close.screen(all=T)
summary(selic)
#Histograma para ver como os dados est�o distribu�dos
#A maior parte do tempo a taxa estava entre 10% e 16,75%.
#Gerar um boxplot para entender como que a ocupa��o est� distribu�da
#Como visto no histograma a mediana est� por volta de 12,75% e uma taxa acima de
#26%(em torno de 26/27%) j� � considerada alta(Outlier)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.50   10.00   12.75   13.54   16.75   42.00 


#-------------------------------------------------------------------------------------
#Vamos decompor a s�rie para entender melhor como se comporta a selic
dec = decompose(selic)
autoplot(dec, main = "Decomposi��o Taxa Selic 1999/2019", xlab="Tempo")  
#Existe um padr�o de sazonalidade. Pelo gr�fco ela � bem regular.  
#A tend�ncia tem forte varia��o por�m com queda durante o per�odo.

#-------------------------------------------------------------------------------------
#At� agora vimos que a taxa m�dia da selic � de cerca de 13%aa.
#Vimos que taxas acima de 25% � exce��o e n�o regra;
#Existe um padr�o sazonal frequente
#No per�odo analisado existe uma tend�ncia de queda da taxa(Not�cia boa)

#-------------------------------------------------------------------------------------
#Vamos analisar a tend�ncia com mais cuidado:
autoplot(dec$trend)
#A taxa vem caindo ao longo dos anos, por�m em meados de 2013 aprasentou um forte aumento que conincide com
#o in�cio da crise econ�mica. E, 2016 iniciou-se uma queda acentuada.
autoplot(window(dec$trend, start=c(2016,01)))
#Essa tend�ncia de queda da taxa Selici come�ou no in�cio de 2016, estabilizou no segundo semestre de
# 2018 e em 2019 voltou a cair chegando a 4,5%aa, taxa in�dida no pa�s.


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupa��o sazonal 1999-2020 (cada ano � uma cor)
ggseasonplot(selic)  
#Aqui se confirma que existe um padr�o bem regular na sazonalidade  
#Apenas dois anos apresentam n�o lineares 1999 e 2003
#Nos demais anos h� diferenca de taxa, por�m essas se mant�m com certa estabilidade durante o ano
#As linhas demosntram, tamb�m, uma queda ao longo dos anos.

#Ver a Taxa entre 1999 e 2003:
ggseasonplot(window(selic, start=c(1999,01), end = c(2003, 12)))
#Confirma��o dos dados anteriores, apresentando apenas dados mais limpos





