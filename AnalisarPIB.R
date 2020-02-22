library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)
library(stringr)

#Ler o arquivo com os dados da taxa pib ao mês(01/1990 - 11/2019 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Transformar o arquivo em uma série temporal
pib = ts(arq$pib_mensal,start = c(1990,1), end=c(2019,11), frequency=12)

plot(pib)
print(pib)

#-------------------------------------------------------------------------------------

autoplot(pib, ylab = "Valor do PIB", xlab = "Tempo", main = "PIB 1990/2019")
#A série, visualmente, aparenta ter ligeira sazonalidade, porém fica difícil determinar em 
#qual período. O gráfico mostra evolução grande no período analisado, que podemos atribuir
#a um crescimento natural da economia, pois as variações não coincidem com as dos índices 
#analisados até o momento.

#-------------------------------------------------------------------------------------

split.screen( c(1,2))
screen(1)
hist(pib, xlab = "Percentual", ylab = "Frequência", main = "Histograma PIB")
screen(2)
boxplot(pib, ylab = "Mediana", main = "Boxplot PIB")
close.screen(all=T)
summary(pib)
#Histograma para ver como os dados estão distribuídos
#A a média do pib foi 230 milhões e a media 171 milhões
#Gerar um boxplot para entender como que a ocupação está distribuída
#Não foram registrados outliers.
#   Min.  1st Qu. Median    Mean  3rd Qu.    Max. 
#    0.2  80478.9 171370.9 230990.2 396721.0 627852.6  


#-------------------------------------------------------------------------------------
#Vamos decompor a série para entender melhor como se comporta a pib
dec = decompose(pib)
autoplot(dec, main = "Decomposição do  valor do PIB 1992/2019", xlab="Tempo")  
#Existe um padrão de sazonalidade. Pelo gráfco ela é bem regular  
#Possui uma forte tendência de aumento sem oscilações expressivas

#-------------------------------------------------------------------------------------
#Vamos analisar a tendência com mais cuidado:
autoplot(dec$trend)
#O índice sobe bastante ao longo dos anos.

#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupação sazonal 1990-2019 (cada ano é uma cor)
ggseasonplot(pib)  
#Aqui se confirma que existe um padrão bem regular na sazonalidade  
#Os valores sobem bastante ano após ano. Esse aummento parece ter mais a ver com
#o crecimento normal da população/atividade econômica do que ter uma relação direta
#com outro fatores/índices da economia.
#Talvez seria o caso de buscar algum índice que possa medir o PIB sem levar em consideração 
#esse aumento que aparentemente é natural.






