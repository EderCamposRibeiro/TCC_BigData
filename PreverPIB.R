library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa selic ao mês(03/1999 - 01/2020 Mensal)
arqselic = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arqselic)

#Ler o arquivo com dados do PIB ao mês(01/1990 - 11/2019 Mensal)
arqpib = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arqpib)

#Separar os dados em Treino e Teste
#Faremos um processo de Hold-out (Dividir os dados entre treino e teste)
#Criaremos os três modelos, fazer a previsão accurancy para comparar os índices
#OBS: Nesta primeira análise preditiva iremos utilizar seis modelos preditivos.
#Analisaremos qual obteve a melhor performance dentre eles:

# Modelo ARIMA utilizando auto.arima;
# Modelo ETS
# Modelo de Redes Neurais
# Modelo ARIMA Produção  utilizando previsao auto.arima Selic;
# Modelo ARIMA Produção  utilizando previsao ets Selic;
# Modelo ARIMA Produção  utilizando previsao nnetar Selic;

#Primeiro passo: Dividir a série temporal entre treino e teste:
#Vamos usar dados de 1999-2017 para treino e de 2018-2020 para teste
#18 anos para treino e 3 anos para teste afim de gerar 12 meses de forecast para testar
# e prever as demais séries temporais da economia.

selic = ts(arqselic$meta_selic, start = c(1999,3), end = c(2019,11), frequency = 12)
selictreino = window(selic, start = c(1999,3), end = c(2017,12))
selicteste = window(selic, start = c(2017,12), end = c(2019,11))
pib = ts(arqpib$pib_mensal, start = c(1990,1), end = c(2019,11), frequency = 12)
pib2 = window(pib, start = c(1999,3), end = c(2019,11))
pibtreino = window(pib, start = c(1992,1), end = c(2017,12))
pibteste = window(pib, start = c(2017,12), end = c(2019,11))
#Essas outras divisões foram criadas para melhorar a apresentação dos dados e resultados
selicparcial = window(selic, start = c(2014,12), end = c(2019,11))
pibparcial = window(pib, start = c(2013,12), end = c(2019,11))
pibparcial2 = window(pib, start = c(2014,12), end = c(2019,11))
#somar mais 12 mesese de 4.5 para gerar plot ideal
selicparcialplot = ts(c(selicparcial,4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5), start = c(2014,12), end = c(2020,12), frequency = 12)

#somar mais 12 mesese de 180.65 para gerar plot ideal
pibparcialplot = ts(c(pibparcial, 627545.9, 627545.9, 627545.9, 627545.9, 627545.9, 627545.9, 627545.9, 627545.9, 627545.9, 627545.9, 627545.9, 627545.9, 627545.9 ), start = c(2013,12), end = c(2020,12), frequency = 12)
pibparcialplot2 = ts(c(pibparcial2,628000.0, 700000.0, 700000.0, 700000.0, 800000.0, 800000.0, 800000.0, 900000.0, 900000.0, 900000.0, 1000000.0, 1000000.0, 1000000.0), start = c(2014,12), end = c(2020,12), frequency = 12)


#Imprimir Comparativo Selic x PIB
par(mfrow=c(2,1), mar=c(3,10,3,6))
plot(selictreino)
lines(selicteste, col = "red")
legend("topright", legend=c("selictreino", "selicteste"),
       lty=1, col=c("2","1"), bty="s", pch=19)
title("SELIC Treino x SELIC Teste")
plot(pibtreino)
lines(pibteste, col = "red")
legend("topleft", legend=c("pibtreino", "pibteste"),
       lty=1, col=c("2","1"), bty="n", pch=19)
title("PIB Treino x PIB Teste")
par(mfrow=c(1,1), mar=c(5,4,4,2))

#Criando modelo Arima
modeloarimaSELIC = auto.arima(selictreino, trace = T, stepwise = F, approximation = F)
modeloarimapib = auto.arima(pibtreino, trace = T, stepwise = F, approximation = F)

#Selecionamos trace = True para listrar os modelos Arima possíveis,
#isso ajuda a visualizar o processo e a mostrar o resultado
#Selecionamos stepwise = True para testarmos todos os modelos
#Selecionamos approximation = False para não realizar proximações.

#Result SELIC:
#Best model: ARIMA(1,1,2)(1,0,0)[12] 
#Arima (1,1,2) (na parte não sazonal)
#Result Produção PIB:
#Best model: ARIMA(4,1,0)(0,1,1)[12]  

#Criar previsao para 2018,01-2020,12: 
preverarimaSELIC = forecast(modeloarimaSELIC, h=36)
preverarimapib = forecast(modeloarimapib, h=36)
#Criando modelo ets
modeloetsSELIC = ets(selictreino)
modeloets = ets(pibtreino)
print(pibtreino)
preveretsSELIC = forecast(modeloetsSELIC, h=36)
preveretspib = forecast(modeloets, h=36)
#Criando modelos para Redes Neurais
modelonnetarSELIC = nnetar(selictreino)
modelonnetarpib = nnetar(pibtreino)
prevernnetarSELIC = forecast(modelonnetarSELIC, h=36)
prevernnetarpib = forecast(modelonnetarpib, h=36)

#Ver graficamente os modelos para as duas séries:

par(mfrow=c(2,1), mar=c(3,10,3,6))
plot(selicparcialplot,ylab = "Percentual")
lines(preverarimaSELIC$mean, col="green")
lines(preveretsSELIC$mean, col="red")
lines(prevernnetarSELIC$mean, col="blue")
legend("topright", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19)
title("Previsão Taxa Selic entre 2018 e 2020")
plot(pibparcialplot2, ylab = "Percentual")
lines(preverarimapib$mean, col="green")
lines(preveretspib$mean, col="red")
lines(prevernnetarpib$mean, col="blue")
legend("topleft", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19)
title("Previsão do PIB entre 2018 e 2020")
par(mfrow = c(1, 1),mar=c(5,4,4,2))

#Comparativo Realidade x Previsao PIB:
par(mfrow=c(2,2))
plot(pibtreino, xlab = "Anos", ylab = "Percentual")
lines(pibteste, col = "red")
legend("topleft", legend=c("pibtreino", "pibteste"),
       lty=1, col=c("2","1"), bty="n", pch=19)
title("PIB entre 2014 e 2020")
plot(preverarimapib)
plot(preveretspib)
plot(prevernnetarpib)
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

modelopibxSELIC_arima = auto.arima(pib2, xreg = selic, trace = T, stepwise = F, approximation = F)
print(modelopibxSELIC_arima)
preverpibxSELIC_arima = forecast(modelopibxSELIC_arima, xreg = arimaSelic2020)
preverpibxSELIC_ets = forecast(modelopibxSELIC_arima, xreg = etsSelic2020)
preverpibxSELIC_nnetar = forecast(modelopibxSELIC_arima, xreg = nnetarSelic2020)

print(preverpibxSELIC_arima$mean)
print(preverpibxSELIC_ets$mean)
print(preverpibxSELIC_nnetar$mean)

print(pibparcialplot2)

plot(pibparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverpibxSELIC_arima$mean, col = "red")
lines(preverarimapib$mean, col = "blue")
legend("topleft", legend=c("PIBxSELIC", "PIB"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão PIB x SELIC - Redução")

plot(pibparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverpibxSELIC_ets$mean, col = "red")
lines(preveretspib$mean, col = "blue")
legend("topleft", legend=c("pibxSELIC", "pib"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão PIB x SELIC - Estagnação")

plot(pibparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverpibxSELIC_nnetar$mean, col = "red")
lines(prevernnetarpib$mean, col = "blue")
legend("topleft", legend=c("PIBxSELIC", "PIB"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão PIB x SELIC - Aumento")


