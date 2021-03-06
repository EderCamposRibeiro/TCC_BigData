library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa emprego ao m�s(03/1999 - 01/2020 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Transformar o arquivo em uma s�rie temporal
emprego = ts(arq$emprego_formal,start = c(1992,1), end=c(2019,11), frequency=12)

plot(emprego)
print(emprego)

#O �ndice:
#A s�rie mensal do �ndice de estoque do emprego formal � calculada a partir do estoque cuja base
#de recupera��o � divulgada pelo Cadastro Geral de Empregados e Desempregados (Caged) e dos saldos
#mensais de admiss�o l�quida de demiss�o j� incorporando informa��es de declara��es recebidas fora
#do prazo. A s�rie de estoque � transformada em n�mero �ndice com o valor igual a 100 em dezembro 
#de 2001, m�s anterior ao das divulga��es das declara��es fora do prazo.

#-------------------------------------------------------------------------------------

autoplot(emprego, ylab = "�ndice de Emprego Formal", xlab = "Tempo", main = "�ndice do Emprego Formal 1992/2019")
#A s�rie, visualmente, aparenta ter ligeira sazonalidade, por�m fica dif�cil determinar em 
#qual per�odo. O gr�fico mostra evolu��o grande no per�odo por�m ligeiro recuo em 2015,
#ap�s isso uma recupera��o suave.


#-------------------------------------------------------------------------------------

split.screen( c(1,2))
screen(1)
hist(emprego, xlab = "Percentual", ylab = "Frequ�ncia", main = "Histograma Emprego")
screen(2)
boxplot(emprego, ylab = "Mediana", main = "Boxplot Emprego")
close.screen(all=T)
summary(emprego)
#Histograma para ver como os dados est�o distribu�dos
#A maior parte do tempo o �ndice de emprego ent� em 100 e tamb�m tendo sua segunda maior frequ�ncia em
#torno de 180.Na verdade, esse �ndice parece acompanhar a tend�ncia natural do pib, por�m, mostrou, em 2014/2015
#que pode sofrer varia��es em tempo de crise.
#Gerar um boxplot para entender como que a ocupa��o est� distribu�da
#Como visto no histograma a mediana est� por volta de 123 e uma taxa acima de
#n�o apresenta outliers.
#   Min.  1st Qu. Median    Mean  3rd Qu.    Max. 
#  94.27   99.69  123.08  133.98  174.82  189.65 


#-------------------------------------------------------------------------------------
#Vamos decompor a s�rie para entender melhor como se comporta a emprego
dec = decompose(emprego)
autoplot(dec, main = "Decomposi��o �ndice de Emprego Formal 1992/2019", xlab="Tempo")  
#Existe um padr�o de sazonalidade. Pelo gr�fco ela � bem regular  
#Possui uma forte tend�ncia de aumento, queda em 2014/2015 e recupera��o a partir de 2017

#-------------------------------------------------------------------------------------
#At� agora nos vimos que o �ndice m�dio da emprego � de cerca de 133.
#Existe um padr�o sazonal frequente
#No per�odo analisado existe uma tend�ncia de aumento

#-------------------------------------------------------------------------------------
#Vamos analisar a tend�ncia com mais cuidado:
autoplot(dec$trend)
#O �ndice sobe bastante ao longo dos anos, por�m no final de 2014 appresentou uma ligeira queda
#essa queda conincide com a crise econ�mica.
#Em 2017 iniciou-se uma t�mida recupera��o.
autoplot(window(dec$trend, start=c(2014,08)))
#Essa tend�ncia de queda do emprego come�ou em agosto de 2014, voltando a subir em 2017


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupa��o sazonal 1992-2019 (cada ano � uma cor)
ggseasonplot(emprego)  
#Aqui se confirma que existe um padr�o bem regular na sazonalidade  
#Os indices mostram ligeira subida durante o ano e queda em novembro






