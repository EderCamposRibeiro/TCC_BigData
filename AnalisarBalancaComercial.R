library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)
library(stringr)

#Ler o arquivo com os dados da taxa balancaComercial ao mês(01/1999 - 01/2020 Mensal)
arq = read.csv2(file.choose(), header = T, sep = ";", dec = ",")
print(arq)


#Transformar o arquivo em uma série temporal
balancaComercial = ts(arq$balanca_comercial,start = c(1995,1), end=c(2019,11), frequency=12)

plot(balancaComercial)
print(balancaComercial)

#-------------------------------------------------------------------------------------

autoplot(balancaComercial, ylab = "Valor da Balança Comercial", xlab = "Tempo", main = "Balança Comercial 1995/2019")
#A série, visualmente, não aparenta ter sazonalidade, possui bastante variação no período.
#Apresenta picos positivos em 2006 e 2018, e pico negativo em 2014.

#-------------------------------------------------------------------------------------

split.screen( c(1,2))
screen(1)
hist(balancaComercial, xlab = "Percentual", ylab = "Frequência", main = "Histograma Balança Comercial")
screen(2)
boxplot(balancaComercial, ylab = "Mediana", main = "Boxplot Balança Comercial")
close.screen(all=T)
summary(balancaComercial)
#Histograma para ver como os dados estão distribuídos
#A maior parte do tempo o saldo da balança Comercial tende ao equilíbrio ou ligeira perda,
#ou seja, quando importa mais do que exporta.
#Gerar um boxplot para entender como que a ocupação está distribuída
#Como visto no histograma a mediana está um pouco abaixo do equilíbrio
#Déficits acima de 6 bilhões são considerados fora do padrão (outliers).
#     Min.  1st Qu.  Median    Mean   3rd Qu.    Max. 
#-7763.40 -1355.00  -294.00   -86.89  1744.15  4598.00  


#-------------------------------------------------------------------------------------
#Vamos decompor a série para entender melhor como se comporta a balancaComercial
dec = decompose(balancaComercial)
autoplot(dec, main = "Decomposição Índice da Balança Comercial 1995/2019", xlab="Tempo")  
#Existe um padrão de sazonalidade. Pelo gráfico ela é bem regular  
#Os valores atingiram um pico entre 2005 e 2007 quando começou a apresentar seguidos 
#défcitis, provavel reflexo de crise econômica, inicialmente externa e logo após interna. 

#-------------------------------------------------------------------------------------
#Até agora nos vimos que o valor médio do saldo da Balança Comercial é de US$86 milhões de déficit.
#Existe um padrão sazonal frequente
#No período entre 2016 e 2019 existe uma tendência de estabilidade em superávit

#-------------------------------------------------------------------------------------
#Vamos analisar a tendência com mais cuidado:
autoplot(dec$trend)
#O saldo cai bastante a partir de 2006, porém no final de 2014 apresentou uma forte recuperação 
#essa queda conincide com a crise econômica.
autoplot(window(dec$trend, start=c(2014,09)))
#Essa tendência aumento no saldo da balança Comercial começou em setembro de 2014, 
#mantendo certa estabilidade em 2016.


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupação sazonal 1995-2019 (cada ano é uma cor)
ggseasonplot(balancaComercial)  
#Esse gráfico demonstra que não existe um padrão bem regular na sazonalidade  

#Ver a Saldo entre 1995 e 2000:
ggseasonplot(window(balancaComercial, start=c(2016,01), end = c(2019, 12)))
#Confirmação dos dados anteriores, apresentando apenas dados mais limpos






