library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa selic ao m�s(03/1999 - 01/2020 Mensal)
arqselic = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arqselic)

#Ler o arquivo com os dados da Emprego ao m�s(01/1995 - 11/2019 Mensal)
arqemprego = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arqemprego)

#Separar os dados em Treino e Teste
#Faremos um processo de Hold-out (Dividir os dados entre treino e teste)
#Criaremos os tr�s modelos, fazer a previs�o accurancy para comparar os �ndices
#OBS: Nesta primeira an�lise preditiva iremos utilizar seis modelos preditivos.
#Analisaremos qual obteve a melhor performance dentre eles:

# Modelo ARIMA utilizando auto.arima;
# Modelo ETS
# Modelo de Redes Neurais
# Modelo ARIMA Produ��o empregol utilizando previsao auto.arima Selic;
# Modelo ARIMA Produ��o empregol utilizando previsao ets Selic;
# Modelo ARIMA Produ��o empregol utilizando previsao nnetar Selic;

#Primeiro passo: Dividir a s�rie temporal entre treino e teste:
#Vamos usar dados de 1999-2017 para treino e de 2018-2020 para teste
#18 anos para treino e 3 anos para teste afim de gerar 12 meses de forecast para testar
# e prever as demais s�ries temporais da economia.

selic = ts(arqselic$meta_selic, start = c(1999,3), end = c(2019,11), frequency = 12)
selictreino = window(selic, start = c(1999,3), end = c(2017,12))
selicteste = window(selic, start = c(2017,12), end = c(2019,11))
emprego = ts(arqemprego$emprego_formal, start = c(1992,1), end = c(2019,11), frequency = 12)
emprego2 = window(emprego, start = c(1999,3), end = c(2019,11))
empregotreino = window(emprego, start = c(1992,1), end = c(2017,12))
empregoteste = window(emprego, start = c(2017,12), end = c(2019,11))
#Essas outras divis�es foram criadas para melhorar a apresenta��o dos dados e resultados
selicparcial = window(selic, start = c(2014,12), end = c(2019,11))
empregoparcial = window(emprego, start = c(2013,12), end = c(2019,11))
empregoparcial2 = window(emprego, start = c(2014,12), end = c(2019,11))
#somar mais 12 mesese de 4.5 para gerar plot ideal
selicparcialplot = ts(c(selicparcial,4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5), start = c(2014,12), end = c(2020,12), frequency = 12)
#somar mais 12 mesese de 180.65 para gerar plot ideal
empregoparcialplot = ts(c(empregoparcial,180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65), start = c(2013,12), end = c(2020,12), frequency = 12)
empregoparcialplot2 = ts(c(empregoparcial2,180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65, 180.65), start = c(2014,12), end = c(2020,12), frequency = 12)

#Imprimir Comparativo Selic x emprego
par(mfrow=c(2,1), mar=c(3,10,3,6))
plot(selictreino)
lines(selicteste, col = "red")
legend("topright", legend=c("selictreino", "selicteste"),
       lty=1, col=c("2","1"), bty="s", pch=19)
title("SELIC Treino x SELIC Teste")
plot(empregotreino)
lines(empregoteste, col = "red")
legend("topleft", legend=c("empregotreino", "empregoteste"),
       lty=1, col=c("2","1"), bty="n", pch=19)
title("Emprego Treino x Emprego Teste")
par(mfrow=c(1,1), mar=c(5,4,4,2))

#Criando modelo Arima
modeloarimaSELIC = auto.arima(selictreino, trace = T, stepwise = F, approximation = F)
modeloarimaemprego = auto.arima(empregotreino, trace = T, stepwise = F, approximation = F)

#Selecionamos trace = True para listrar os modelos Arima poss�veis,
#isso ajuda a visualizar o processo e a mostrar o resultado
#Selecionamos stepwise = True para testarmos todos os modelos
#Selecionamos approximation = False para n�o realizar proxima��es.

#Result SELIC:
#Best model: ARIMA(1,1,2)(1,0,0)[12] 
#Arima (1,1,2) (na parte n�o sazonal)
#Result Produ��o empregol:
#Best model: ARIMA(2,1,1)(0,1,1)[12]  

#Criar previsao para 2018,01-2020,12: 
preverarimaSELIC = forecast(modeloarimaSELIC, h=36)
preverarimaemprego = forecast(modeloarimaemprego, h=36)
#Criando modelo ets
modeloetsSELIC = ets(selictreino)
modeloetsemprego = ets(empregotreino)
preveretsSELIC = forecast(modeloetsSELIC, h=36)
preveretsemprego = forecast(modeloetsemprego, h=36)
#Criando modelos para Redes Neurais
modelonnetarSELIC = nnetar(selictreino)
modelonnetaremprego = nnetar(empregotreino)
prevernnetarSELIC = forecast(modelonnetarSELIC, h=36)
prevernnetaremprego = forecast(modelonnetaremprego, h=36)

#Ver graficamente os modelos para as duas s�ries:

par(mfrow=c(2,1), mar=c(3,10,3,6))
plot(selicparcialplot,ylab = "Percentual")
lines(preverarimaSELIC$mean, col="green")
lines(preveretsSELIC$mean, col="red")
lines(prevernnetarSELIC$mean, col="blue")
legend("bottomleft", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19)
title("Previs�o Taxa Selic entre 2018 e 2020")
plot(empregoparcialplot, ylab = "Percentual")
lines(preverarimaemprego$mean, col="green")
lines(preveretsemprego$mean, col="red")
lines(prevernnetaremprego$mean, col="blue")
legend("bottomleft", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19)
title("Previs�o da Produ��o empregol entre 2018 e 2020")
par(mfrow = c(1, 1), mar=c(5,4,4,2))

#Comparativo Realidade x Previsao emprego:
par(mfrow=c(2,2))
plot(empregotreino, xlab = "Anos", ylab = "Percentual")
lines(empregoteste, col = "red")
legend("topleft", legend=c("empregotreino", "empregoteste"),
       lty=1, col=c("2","1"), bty="n", pch=19)
title("emprego entre 2014 e 2020")
plot(preverarimaemprego)
plot(preveretsemprego)
plot(prevernnetaremprego)
par(mfrow = c(1, 1), mar=c(5,4,4,2))

#Agora vamos fazer a previs�o usando a SELIC como vari�vel independente
#Aqui a inte��o n�o � fazer uma avalia��o dos m�todos e sim fazer a predi��o 
#para os pr�ximos 12 meses(2020)

print(preverarimaSELIC$mean)  #Redu��o na taxa Selic
print(preveretsSELIC$mean)    #Estagna��o na taxa Selic
print(prevernnetarSELIC$mean) #Aumento na taxa Selic

arimaSelic2020 = ts(window(preverarimaSELIC$mean, start = c(2020,1), end= c(2020,12)))
etsSelic2020 = ts(window(preveretsSELIC$mean, start = c(2020,1), end= c(2020,12)))
nnetarSelic2020 = ts(window(prevernnetarSELIC$mean, start = c(2020,1), end= c(2020,12)))

modeloempregoxSELIC_arima = auto.arima(emprego2, xreg = selic, trace = T, stepwise = F, approximation = F)
print(modeloempregoxSELIC_arima)
preverempregoxSELIC_arima = forecast(modeloempregoxSELIC_arima, xreg = arimaSelic2020)
preverempregoxSELIC_ets = forecast(modeloempregoxSELIC_arima, xreg = etsSelic2020)
preverempregoxSELIC_nnetar = forecast(modeloempregoxSELIC_arima, xreg = nnetarSelic2020)

print(preverempregoxSELIC_arima$mean)
print(preverempregoxSELIC_ets$mean)
print(preverempregoxSELIC_nnetar$mean)

print(empregoparcialplot2)

plot(empregoparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverempregoxSELIC_arima$mean, col = "red")
lines(preverarimaemprego$mean, col = "blue")
legend("topright", legend=c("empregoxSELIC", "emprego"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previs�o Emprego x SELIC - Redu��o")

plot(empregoparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverempregoxSELIC_ets$mean, col = "red")
lines(preveretsemprego$mean, col = "blue")
legend("topright", legend=c("empregoxSELIC", "emprego"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previs�o Emprego x SELIC - Estagna��o")

plot(empregoparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverempregoxSELIC_nnetar$mean, col = "red")
lines(prevernnetaremprego$mean, col = "blue")
legend("topright", legend=c("empregoxSELIC", "emprego"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previs�o Emprego x SELIC - Aumento")


