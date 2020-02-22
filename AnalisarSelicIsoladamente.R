library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa selic ao mês(03/1999 - 01/2020 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Transformar o arquivo em uma série temporal
selic = ts(arq$meta_selic,start = c(1999,3), end=c(2019,12), frequency=12)

plot(selic)

#-------------------------------------------------------------------------------------

autoplot(selic, ylab = "Percentual da taxa Selic", xlab = "Tempo", main = "Taxa Selic 1999/2019")
#A série, visualmente, não aparenta ter sazonalidade, possui alguma variação e parece ter uma
#tendência de queda

#-------------------------------------------------------------------------------------
#Divisão de tela para apresentação de Histograma e Boxplot
split.screen( c(1,2))
screen(1)
hist(selic, xlab = "Percentual", ylab = "Frequência", main = "Histograma Selic")
screen(2)
boxplot(selic, ylab = "Mediana", main = "Boxplot Selic")
close.screen(all=T)
summary(selic)
#Histograma para ver como os dados estão distribuídos
#A maior parte do tempo a taxa estava entre 10% e 16,75%.
#Gerar um boxplot para entender como que a ocupação está distribuída
#Como visto no histograma a mediana está por volta de 12,75% e uma taxa acima de
#26%(em torno de 26/27%) já é considerada alta(Outlier)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.50   10.00   12.75   13.54   16.75   42.00 


#-------------------------------------------------------------------------------------
#Vamos decompor a série para entender melhor como se comporta a selic
dec = decompose(selic)
autoplot(dec, main = "Decomposição Taxa Selic 1999/2019", xlab="Tempo")  
#Existe um padrão de sazonalidade. Pelo gráfco ela é bem regular.  
#A tendência tem forte variação porém com queda durante o período.

#-------------------------------------------------------------------------------------
#Até agora vimos que a taxa média da selic é de cerca de 13%aa.
#Vimos que taxas acima de 25% é exceção e não regra;
#Existe um padrão sazonal frequente
#No período analisado existe uma tendência de queda da taxa(Notícia boa)

#-------------------------------------------------------------------------------------
#Vamos analisar a tendência com mais cuidado:
autoplot(dec$trend)
#A taxa vem caindo ao longo dos anos, porém em meados de 2013 aprasentou um forte aumento que conincide com
#o início da crise econômica. E, 2016 iniciou-se uma queda acentuada.
autoplot(window(dec$trend, start=c(2016,01)))
#Essa tendência de queda da taxa Selici começou no início de 2016, estabilizou no segundo semestre de
# 2018 e em 2019 voltou a cair chegando a 4,5%aa, taxa inédida no país.


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupação sazonal 1999-2020 (cada ano é uma cor)
ggseasonplot(selic)  
#Aqui se confirma que existe um padrão bem regular na sazonalidade  
#Apenas dois anos apresentam não lineares 1999 e 2003
#Nos demais anos há diferenca de taxa, porém essas se mantém com certa estabilidade durante o ano
#As linhas demosntram, também, uma queda ao longo dos anos.

#Ver a Taxa entre 1999 e 2003:
ggseasonplot(window(selic, start=c(1999,01), end = c(2003, 12)))
#Confirmação dos dados anteriores, apresentando apenas dados mais limpos





