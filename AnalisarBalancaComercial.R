library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)
library(stringr)

#Ler o arquivo com os dados da taxa balancaComercial ao m�s(01/1999 - 01/2020 Mensal)
arq = read.csv2(file.choose(), header = T, sep = ";", dec = ",")
print(arq)


#Transformar o arquivo em uma s�rie temporal
balancaComercial = ts(arq$balanca_comercial,start = c(1995,1), end=c(2019,11), frequency=12)

plot(balancaComercial)
print(balancaComercial)

#-------------------------------------------------------------------------------------

autoplot(balancaComercial, ylab = "Valor da Balan�a Comercial", xlab = "Tempo", main = "Balan�a Comercial 1995/2019")
#A s�rie, visualmente, n�o aparenta ter sazonalidade, possui bastante varia��o no per�odo.
#Apresenta picos positivos em 2006 e 2018, e pico negativo em 2014.

#-------------------------------------------------------------------------------------

split.screen( c(1,2))
screen(1)
hist(balancaComercial, xlab = "Percentual", ylab = "Frequ�ncia", main = "Histograma Balan�a Comercial")
screen(2)
boxplot(balancaComercial, ylab = "Mediana", main = "Boxplot Balan�a Comercial")
close.screen(all=T)
summary(balancaComercial)
#Histograma para ver como os dados est�o distribu�dos
#A maior parte do tempo o saldo da balan�a Comercial tende ao equil�brio ou ligeira perda,
#ou seja, quando importa mais do que exporta.
#Gerar um boxplot para entender como que a ocupa��o est� distribu�da
#Como visto no histograma a mediana est� um pouco abaixo do equil�brio
#D�ficits acima de 6 bilh�es s�o considerados fora do padr�o (outliers).
#     Min.  1st Qu.  Median    Mean   3rd Qu.    Max. 
#-7763.40 -1355.00  -294.00   -86.89  1744.15  4598.00  


#-------------------------------------------------------------------------------------
#Vamos decompor a s�rie para entender melhor como se comporta a balancaComercial
dec = decompose(balancaComercial)
autoplot(dec, main = "Decomposi��o �ndice da Balan�a Comercial 1995/2019", xlab="Tempo")  
#Existe um padr�o de sazonalidade. Pelo gr�fico ela � bem regular  
#Os valores atingiram um pico entre 2005 e 2007 quando come�ou a apresentar seguidos 
#d�fcitis, provavel reflexo de crise econ�mica, inicialmente externa e logo ap�s interna. 

#-------------------------------------------------------------------------------------
#At� agora nos vimos que o valor m�dio do saldo da Balan�a Comercial � de US$86 milh�es de d�ficit.
#Existe um padr�o sazonal frequente
#No per�odo entre 2016 e 2019 existe uma tend�ncia de estabilidade em super�vit

#-------------------------------------------------------------------------------------
#Vamos analisar a tend�ncia com mais cuidado:
autoplot(dec$trend)
#O saldo cai bastante a partir de 2006, por�m no final de 2014 apresentou uma forte recupera��o 
#essa queda conincide com a crise econ�mica.
autoplot(window(dec$trend, start=c(2014,09)))
#Essa tend�ncia aumento no saldo da balan�a Comercial come�ou em setembro de 2014, 
#mantendo certa estabilidade em 2016.


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupa��o sazonal 1995-2019 (cada ano � uma cor)
ggseasonplot(balancaComercial)  
#Esse gr�fico demonstra que n�o existe um padr�o bem regular na sazonalidade  

#Ver a Saldo entre 1995 e 2000:
ggseasonplot(window(balancaComercial, start=c(2016,01), end = c(2019, 12)))
#Confirma��o dos dados anteriores, apresentando apenas dados mais limpos






