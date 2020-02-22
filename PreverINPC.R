library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa selic ao mês(03/1999 - 01/2020 Mensal)
arqselic = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arqselic)

#Ler o arquivo com os dados do INPC ao mês(04/1979 - 12/2019 Mensal)
arqinpc = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arqinpc)

#Separar os dados em Treino e Teste
#Faremos um processo de Hold-out (Dividir os dados entre treino e teste)
#Criaremos os três modelos, fazer a previsão accurancy para comparar os índices
#OBS: Nesta primeira análise preditiva iremos utilizar seis modelos preditivos.
#Analisaremos qual obteve a melhor performance dentre eles:

# Modelo ARIMA utilizando auto.arima;
# Modelo ETS
# Modelo de Redes Neurais
# Modelo ARIMA INPC utilizando previsao auto.arima Selic;
# Modelo ARIMA INPC utilizando previsao ets Selic;
# Modelo ARIMA INPC utilizando previsao nnetar Selic;

#Primeiro passo: Dividir a série temporal entre treino e teste:
#Vamos usar dados de 1999-2017 para treino e de 2018-2020 para teste
#18 anos para treino e 3 anos para teste afim de gerar 12 meses de forecast para testar
# e prever as demais séries temporais da economia.

selic = ts(arqselic$meta_selic, start = c(1999,3), end = c(2019,12), frequency = 12)
selictreino = window(selic, start = c(1999,3), end = c(2017,12))
selicteste = window(selic, start = c(2017,12), end = c(2019,12))
inpc = ts(arqinpc$inpc_mensal, start = c(1999,3), end = c(2019,12), frequency = 12)
inpctreino = window(inpc, start = c(1999,3), end = c(2017,12))
inpcteste = window(inpc, start = c(2017,12), end = c(2019,12))
#Essas outras divisões foram criadas para melhorar a apresentação dos dados e resultados
selicparcial = window(selic, start = c(2014,12), end = c(2019,12))
inpcparcial = window(inpc, start = c(2013,12), end = c(2019,12))
inpcparcial2 = window(inpc, start = c(2014,12), end = c(2019,12))
#somar mais 12 mesese de 4.5 para gerar plot ideal
selicparcialplot = ts(c(selicparcial,4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5), start = c(2014,12), end = c(2020,12), frequency = 12)
#somar mais 12 mesese de 1.22 para gerar plot ideal
inpcparcialplot = ts(c(inpcparcial,1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22), start = c(2013,12), end = c(2020,12), frequency = 12)
inpcparcialplot2 = ts(c(inpcparcial2,1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22), start = c(2014,12), end = c(2020,12), frequency = 12)

#Imprimir Comparativo Selic x INPC
par(mfrow=c(2,1), mar=c(3,10,3,6))
plot(selictreino)
lines(selicteste, col = "red")
legend("topright", legend=c("selictreino", "selicteste"),
       lty=1, col=c("2","1"), bty="s", pch=19)
title("SELIC Treino x SELIC Teste")
plot(inpctreino)
lines(inpcteste, col = "red")
legend("topright", legend=c("inpctreino", "inpcteste"),
       lty=1, col=c("2","1"), bty="s", pch=19)
title("INPC Treino x INPC Teste")
par(mfrow=c(1,1), mar=c(5,4,4,2))

#Criando modelo Arima
modeloarimaSELIC = auto.arima(selictreino, trace = T, stepwise = F, approximation = F)
modeloarimaINPC = auto.arima(inpctreino, trace = T, stepwise = F, approximation = F)

#Selecionamos trace = True para listrar os modelos Arima possíveis,
#isso ajuda a visualizar o processo e a mostrar o resultado
#Selecionamos stepwise = True para testarmos todos os modelos
#Selecionamos approximation = False para não realizar proximações.

#Result SELIC:
#Best model: ARIMA(1,1,2)(1,0,0)[12] 
#Arima (1,1,2) (na parte não sazonal)
#Result INPC:
#Best model: Best model: ARIMA(2,1,1)

#Criar previsao para 2018,01-2020,12: 
preverarimaSELIC = forecast(modeloarimaSELIC, h=36)
preverarimaINPC = forecast(modeloarimaINPC, h=36)
#Criando modelo ets
modeloetsSELIC = ets(selictreino)
modeloetsINPC = ets(inpctreino)
preveretsSELIC = forecast(modeloetsSELIC, h=36)
preveretsINPC = forecast(modeloetsINPC, h=36)
#Criando modelos para Redes Neurais
modelonnetarSELIC = nnetar(selictreino)
modelonnetarINPC = nnetar(inpctreino)
prevernnetarSELIC = forecast(modelonnetarSELIC, h=36)
prevernnetarINPC = forecast(modelonnetarINPC, h=36)

#Ver graficamente os modelos para as duas séries:

par(mfrow=c(2,1), mar=c(3,10,3,6))
plot(selicparcialplot,ylab = "Percentual")
lines(preverarimaSELIC$mean, col="green")
lines(preveretsSELIC$mean, col="red")
lines(prevernnetarSELIC$mean, col="blue")
legend("topright", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19)
title("Previsão Taxa Selic entre 2018 e 2020")
plot(inpcparcialplot, ylab = "Percentual")
lines(preverarimaINPC$mean, col="green")
lines(preveretsINPC$mean, col="red")
lines(prevernnetarINPC$mean, col="blue")
legend("topright", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19)
title("Previsão INPC entre 2018 e 2020")
par(mfrow = c(1, 1), mar=c(5,4,4,2))

#Comparativo Realidade x Previsao INPC:
par(mfrow=c(2,2))
plot(inpctreino, xlab = "Anos", ylab = "Percentual")
lines(inpcteste, col = "red")
legend("topright", legend=c("inpctreino", "inpcteste"),
       lty=1, col=c("2","1"), bty="n", pch=19)
title("INPC entre 2014 e 2020")
plot(preverarimaINPC)
plot(preveretsINPC)
plot(prevernnetarINPC)
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

modeloINPCxSELIC_arima = auto.arima(inpc, xreg = selic, trace = T, stepwise = F, approximation = F)
print(modeloINPCxSELIC_arima)
preverINPCxSELIC_arima = forecast(modeloINPCxSELIC_arima, xreg = arimaSelic2020)
preverINPCxSELIC_ets = forecast(modeloINPCxSELIC_arima, xreg = etsSelic2020)
preverINPCxSELIC_nnetar = forecast(modeloINPCxSELIC_arima, xreg = nnetarSelic2020)

print(preverINPCxSELIC_arima$mean)
print(preverINPCxSELIC_ets$mean)
print(preverINPCxSELIC_nnetar$mean)

print(inpcparcialplot2)

plot(inpcparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverINPCxSELIC_arima$mean, col = "red")
lines(preverarimaINPC$mean, col = "blue")
legend("topright", legend=c("INPCxSELIC", "INPC"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão INPC x SELIC - Redução")

plot(inpcparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverINPCxSELIC_ets$mean, col = "red")
lines(preveretsINPC$mean, col = "blue")
legend("topright", legend=c("INPCxSELIC", "INPC"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão INPC x SELIC - Estagnação")

plot(inpcparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverINPCxSELIC_nnetar$mean, col = "red")
lines(prevernnetarINPC$mean, col = "blue")
legend("topright", legend=c("INPCxSELIC", "INPC"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão INPC x SELIC - Aumento")


