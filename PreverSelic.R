library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa selic ao mês(03/1999 - 01/2020 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Separar os dados em Treino e Teste
#Faremos um processo de Hold-out (Dividir os dados entre treino e teste)
#Criaremos os três modelos, fazer a previsão accurancy para comparar os índices
#OBS: Nesta primeira análise preditiva iremos utilizar Três modelos preditivos.
#Analisaremos qual obteve a melhor performance dentre eles:

# Modelo ARIMA utilizando auto.arima;
# Modelo ETS
# Modelo de Redes Neurais

#Primeiro passo: Dividir a série temporal entre treino e teste:
#Vamos usar dados de 1999-2017 para treino e de 2018-2020 para teste
#18 anos para treino e 3 anos para teste afim de gerar 12 meses de forecast para testar
# e prever as demais séries temporais da economia.

#Gera uma time Series compreta do arquivo:
selic = ts(arq$meta_selic, start = c(1999,3), end = c(2020,1), frequency = 12)

#Divide em treino e Teste:
selictreino = window(selic, start = c(1999,3), end = c(2017,12))
selicteste = window(selic, start = c(2017,12), end = c(2019,12))
selicparcial = window(selic, start = c(2014,12), end = c(2019,12))
selicparcialplot = ts(c(selicparcial,4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5), start = c(2014,12), end = c(2020,12), frequency = 12)

plot(selicparcialplot, xlab = "Anos", ylab = "Percentual")
lines(selicteste, col = "red")
legend("topright", legend=c("selictreino", "selicteste"),
       lty=1, col=c("2","1"), bty="s", pch=19)
title("Taxa Selic entre 2014 e 2020")



#Criando modelo Arima
modeloarima = auto.arima(selictreino, trace = T, stepwise = F, approximation = F)

#Selecionamos trace = True para listrar os modelos Arima possíveis,
#isso ajuda a visualizar o processo e a mostrar o resultado
#Selecionamos stepwise = True para testarmos todos os modelos
#Selecionamos approximation = False para não realizar proximações.

#Result:
#Best model: ARIMA(1,1,2)(1,0,0)[12] 
#Arima (1,1,2) (na parte não sazonal)
#Criar previsao para 2018,01-2020,12 
preverarima = forecast(modeloarima, h=36)
#Criando modelo ets
modeloets = ets(selictreino)
preverets = forecast(modeloets, h=36)
#Criando modelos para Redes Neurais
modelonnetar = nnetar(selictreino)
prevernnetar = forecast(modelonnetar, h=36)


#Ver graficamente os três modelos:

plot(selicparcialplot, xlab = "Anos", ylab = "Percentual")
lines(preverarima$mean, col="green")
lines(preverets$mean, col="red")
lines(prevernnetar$mean, col="blue")
legend("topright", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="s", pch=19)
title("Previsão Taxa Selic entre 2018 e 2020")


#Os três modelos apresentam comportamentos distintos
#Podemos comparar as duas previsões com os dados de treino

#3º - Comparar os dois modelos--------------------------------------------------------
#Qual é o mais adequado?????

accuracy(preverarima, selicteste)
#                     ME      RMSE       MAE        MPE      MAPE      MASE        ACF1 Theil's U
#Training set 0.02224705 0.6567259 0.3948922  0.1588482  2.850028 0.1236385 -0.08903279        NA
#Test set     0.79597479 0.9933987 0.8134570 12.8814604 13.146035 0.2546888  0.90308943  4.162081

accuracy(preverets, selicteste)
#                     ME      RMSE       MAE       MPE      MAPE      MASE       ACF1 Theil's U
#Training set 0.03065998 0.6398329 0.3567124 0.0933716  2.430724 0.1116847 0.02015344        NA
#Test set     0.44065715 0.6899302 0.6388196 6.2306299 10.381180 0.2000108 0.74683122  2.964449

accuracy(prevernnetar, selicteste)
#                        ME      RMSE       MAE          MPE      MAPE       MASE        ACF1 Theil's U
#Training set  0.0001610623 0.4122546 0.2848631  -0.09716997  2.214777 0.08918903 -0.05134655        NA
#Test set     -2.7836431839 3.2447192 2.7841157 -47.41058901 47.417339 0.87169093  0.85156321  14.81975

#Observando o ME os valores para Ets estão menores
#Observando o RMSE os valores para Ets estão menores
#Observando o MAE os valores para Ets estão menores
#Observando o MPE os valores para Ets estão menores
#Observando o MAPE os valores para Ets estão menores
#Observando o MASE os valores para Ets estão menores
#Observando o ACF1 os valores para Ets estão menores

#Todos os índices de performance dão "ganho" para o modelo Ets
#Porem o mais interessante aqui é que temos três cenários distintos
#para realizar previsões. Com viés de alta, estabilidade e viés de baixa, respectivamente.

#previsao
par(mfrow=c(2,2))
plot(selicparcialplot)
lines(preverarima$mean, col="green")
lines(preverets$mean, col="red")
lines(prevernnetar$mean, col="blue")
legend("topright", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19)
plot(preverarima)
plot(preverets)
plot(prevernnetar)

