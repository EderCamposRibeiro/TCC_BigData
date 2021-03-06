library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)
library(stringr)

#Ler o arquivo com os dados da taxa pib ao m�s(01/1990 - 11/2019 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Transformar o arquivo em uma s�rie temporal
pib = ts(arq$pib_mensal,start = c(1990,1), end=c(2019,11), frequency=12)

plot(pib)
print(pib)

#-------------------------------------------------------------------------------------

autoplot(pib, ylab = "Valor do PIB", xlab = "Tempo", main = "PIB 1990/2019")
#A s�rie, visualmente, aparenta ter ligeira sazonalidade, por�m fica dif�cil determinar em 
#qual per�odo. O gr�fico mostra evolu��o grande no per�odo analisado, que podemos atribuir
#a um crescimento natural da economia, pois as varia��es n�o coincidem com as dos �ndices 
#analisados at� o momento.

#-------------------------------------------------------------------------------------

split.screen( c(1,2))
screen(1)
hist(pib, xlab = "Percentual", ylab = "Frequ�ncia", main = "Histograma PIB")
screen(2)
boxplot(pib, ylab = "Mediana", main = "Boxplot PIB")
close.screen(all=T)
summary(pib)
#Histograma para ver como os dados est�o distribu�dos
#A a m�dia do pib foi 230 milh�es e a media 171 milh�es
#Gerar um boxplot para entender como que a ocupa��o est� distribu�da
#N�o foram registrados outliers.
#   Min.  1st Qu. Median    Mean  3rd Qu.    Max. 
#    0.2  80478.9 171370.9 230990.2 396721.0 627852.6  


#-------------------------------------------------------------------------------------
#Vamos decompor a s�rie para entender melhor como se comporta a pib
dec = decompose(pib)
autoplot(dec, main = "Decomposi��o do  valor do PIB 1992/2019", xlab="Tempo")  
#Existe um padr�o de sazonalidade. Pelo gr�fco ela � bem regular  
#Possui uma forte tend�ncia de aumento sem oscila��es expressivas

#-------------------------------------------------------------------------------------
#Vamos analisar a tend�ncia com mais cuidado:
autoplot(dec$trend)
#O �ndice sobe bastante ao longo dos anos.

#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupa��o sazonal 1990-2019 (cada ano � uma cor)
ggseasonplot(pib)  
#Aqui se confirma que existe um padr�o bem regular na sazonalidade  
#Os valores sobem bastante ano ap�s ano. Esse aummento parece ter mais a ver com
#o crecimento normal da popula��o/atividade econ�mica do que ter uma rela��o direta
#com outro fatores/�ndices da economia.
#Talvez seria o caso de buscar algum �ndice que possa medir o PIB sem levar em considera��o 
#esse aumento que aparentemente � natural.






