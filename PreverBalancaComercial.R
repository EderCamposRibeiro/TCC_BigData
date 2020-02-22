library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa selic ao mês(03/1999 - 01/2020 Mensal)
arqselic = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arqselic)

#Ler o arquivo com os dados da Balança Comercial ao mês(01/1995 - 11/2019 Mensal)
arqbalanca = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arqbalanca)

#Separar os dados em Treino e Teste
#Faremos um processo de Hold-out (Dividir os dados entre treino e teste)
#Criaremos os três modelos, fazer a previsão accurancy para comparar os índices
#OBS: Nesta primeira análise preditiva iremos utilizar seis modelos preditivos.
#Analisaremos qual obteve a melhor performance dentre eles:

# Modelo ARIMA utilizando auto.arima;
# Modelo ETS
# Modelo de Redes Neurais
# Modelo ARIMA Balança Comercial utilizando previsao auto.arima Selic;
# Modelo ARIMA Balança Comercial utilizando previsao ets Selic;
# Modelo ARIMA Balança Comercial utilizando previsao nnetar Selic;

#Primeiro passo: Dividir a série temporal entre treino e teste:
#Vamos usar dados de 1999-2017 para treino e de 2018-2020 para teste
#18 anos para treino e 3 anos para teste afim de gerar 12 meses de forecast para testar
# e prever as demais séries temporais da economia.

selic = ts(arqselic$meta_selic, start = c(1999,3), end = c(2019,11), frequency = 12)
selictreino = window(selic, start = c(1999,3), end = c(2017,12))
selicteste = window(selic, start = c(2017,12), end = c(2019,11))
balanca = ts(arqbalanca$balanca_comercial, start = c(1995,1), end = c(2019,11), frequency = 12)
balancatreino = window(balanca, start = c(1995,1), end = c(2017,12))
balancateste = window(balanca, start = c(2017,12), end = c(2019,11))
#Essas outras divisões foram criadas para melhorar a apresentação dos dados e resultados
selicparcial = window(selic, start = c(2014,12), end = c(2019,11))
balancaparcial = window(balanca, start = c(1999,3), end = c(2019,11))
balancaparcial2 = window(balanca, start = c(2014,12), end = c(2019,11))
#somar mais 12 mesese de 4.5 para gerar plot ideal
selicparcialplot = ts(c(selicparcial,4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5), start = c(2014,12), end = c(2020,12), frequency = 12)
#somar mais 12 mesese de 1.22 para gerar plot ideal
balancaparcialplot = ts(c(balancaparcial,711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6), start = c(2013,12), end = c(2020,12), frequency = 12)
balancaparcialplot2 = ts(c(balancaparcial2,711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6, 711.6), start = c(2014,12), end = c(2020,12), frequency = 12)

#Imprimir Comparativo Selic x balanca
par(mfrow=c(2,1), mar=c(3,10,3,6))
plot(selictreino)
lines(selicteste, col = "red")
legend("topright", legend=c("selictreino", "selicteste"),
       lty=1, col=c("2","1"), bty="s", pch=19)
title("SELIC Treino x SELIC Teste")
plot(balancatreino)
lines(balancateste, col = "red")
legend("bottomleft", legend=c("balancatreino", "balancateste"),
       lty=1, col=c("2","1"), bty="n", pch=19)
title("Balança Treino x Balança Teste")
par(mfrow=c(1,1), mar=c(5,4,4,2))

#Criando modelo Arima
modeloarimaSELIC = auto.arima(selictreino, trace = T, stepwise = F, approximation = F)
modeloarimabalanca = auto.arima(balancatreino, trace = T, stepwise = F, approximation = F)

#Selecionamos trace = True para listrar os modelos Arima possíveis,
#isso ajuda a visualizar o processo e a mostrar o resultado
#Selecionamos stepwise = True para testarmos todos os modelos
#Selecionamos approximation = False para não realizar proximações.

#Result SELIC:
#Best model: ARIMA(1,1,2)(1,0,0)[12] 
#Arima (1,1,2) (na parte não sazonal)
#Result Balança:
#Best model: ARIMA(0,1,1)(2,0,1)[12]

#Criar previsao para 2018,01-2020,12: 
preverarimaSELIC = forecast(modeloarimaSELIC, h=36)
preverarimabalanca = forecast(modeloarimabalanca, h=36)
#Criando modelo ets
modeloetsSELIC = ets(selictreino)
modeloetsbalanca = ets(balancatreino)
preveretsSELIC = forecast(modeloetsSELIC, h=36)
preveretsbalanca = forecast(modeloetsbalanca, h=36)
#Criando modelos para Redes Neurais
modelonnetarSELIC = nnetar(selictreino)
modelonnetarbalanca = nnetar(balancatreino)
prevernnetarSELIC = forecast(modelonnetarSELIC, h=36)
prevernnetarbalanca = forecast(modelonnetarbalanca, h=36)

#Ver graficamente os modelos para as duas séries:

par(mfrow=c(2,1), mar=c(3,10,3,6))
plot(selicparcialplot,ylab = "Percentual")
lines(preverarimaSELIC$mean, col="green")
lines(preveretsSELIC$mean, col="red")
lines(prevernnetarSELIC$mean, col="blue")
legend("topright", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19)
title("Previsão Taxa Selic entre 2018 e 2020")
plot(balancaparcialplot, ylab = "Percentual")
lines(preverarimabalanca$mean, col="green")
lines(preveretsbalanca$mean, col="red")
lines(prevernnetarbalanca$mean, col="blue")
legend("bottomright", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19)
title("Previsão Balança Comercial entre 2018 e 2020")
par(mfrow = c(1, 1), mar=c(5,4,4,2))

#Comparativo Realidade x Previsao balanca:
par(mfrow=c(2,2))
plot(balancatreino, xlab = "Anos", ylab = "Percentual")
lines(balancateste, col = "red")
legend("bottomleft", legend=c("balancatreino", "balancateste"),
       lty=1, col=c("2","1"), bty="n", pch=19)
title("balanca entre 2014 e 2020")
plot(preverarimabalanca)
plot(preveretsbalanca)
plot(prevernnetarbalanca)
par(mfrow = c(1, 1), mar=c(5,4,4,2))

#Agora vamos fazer a previsão usando a SELIC como variável independente
#Aqui a inteção não é fazer uma avaliação dos métodos e sim fazer a predição 
#para os próximos 12 meses(2020)

print(preverarimaSELIC$mean)  #Redução na taxa Selic
print(preveretsSELIC$mean)    #Estagnação na taxa Selic
print(prevernnetarSELIC$mean) #Aumento na taxa Selic

arimaSelic2020 = ts(window(preverarimaSELIC$mean, start = c(2020,1), end= c(2020,12)))
etsSelic2020 = ts(window(preveretsSELIC$mean, start = c(2020,1), end= c(2020,12)))
nnetarSelic2020 = ts(window(prevernnetarSELIC$mean, start = c(2020,1), end= c(2020,12)))

modelobalancaxSELIC_arima = auto.arima(balancaparcial, xreg = selic, trace = T, stepwise = F, approximation = F)
print(modelobalancaxSELIC_arima)
preverbalancaxSELIC_arima = forecast(modelobalancaxSELIC_arima, xreg = arimaSelic2020)
preverbalancaxSELIC_ets = forecast(modelobalancaxSELIC_arima, xreg = etsSelic2020)
preverbalancaxSELIC_nnetar = forecast(modelobalancaxSELIC_arima, xreg = nnetarSelic2020)

print(preverbalancaxSELIC_arima$mean)
print(preverbalancaxSELIC_ets$mean)
print(preverbalancaxSELIC_nnetar$mean)

print(balancaparcialplot2)

plot(balancaparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverbalancaxSELIC_arima$mean, col = "red")
lines(preverarimabalanca$mean, col = "blue")
legend("topright", legend=c("balancaxSELIC", "balanca"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão balanca x SELIC - Redução")

plot(balancaparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverbalancaxSELIC_ets$mean, col = "red")
lines(preveretsbalanca$mean, col = "blue")
legend("topright", legend=c("balancaxSELIC", "balanca"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão balanca x SELIC - Estagnação")

plot(balancaparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverbalancaxSELIC_nnetar$mean, col = "red")
lines(prevernnetarbalanca$mean, col = "blue")
legend("topright", legend=c("balancaxSELIC", "balanca"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão balanca x SELIC - Aumento")


