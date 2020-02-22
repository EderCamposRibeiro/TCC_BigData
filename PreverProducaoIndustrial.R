library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa selic ao mês(03/1999 - 01/2020 Mensal)
arqselic = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arqselic)

#Ler o arquivo com os dados da Balança Comercial ao mês(01/2002 - 11/2019 Mensal)
arqindustria = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arqindustria)

#Separar os dados em Treino e Teste
#Faremos um processo de Hold-out (Dividir os dados entre treino e teste)
#Criaremos os três modelos, fazer a previsão accurancy para comparar os índices
#OBS: Nesta primeira análise preditiva iremos utilizar seis modelos preditivos.
#Analisaremos qual obteve a melhor performance dentre eles:

# Modelo ARIMA utilizando auto.arima;
# Modelo ETS
# Modelo de Redes Neurais
# Modelo ARIMA Produção Industrial utilizando previsao auto.arima Selic;
# Modelo ARIMA Produção Industrial utilizando previsao ets Selic;
# Modelo ARIMA Produção Industrial utilizando previsao nnetar Selic;

#Primeiro passo: Dividir a série temporal entre treino e teste:
#Vamos usar dados de 1999-2017 para treino e de 2018-2020 para teste
#18 anos para treino e 3 anos para teste afim de gerar 12 meses de forecast para testar
# e prever as demais séries temporais da economia.

selic = ts(arqselic$meta_selic, start = c(1999,3), end = c(2019,11), frequency = 12)
selic2 = window(selic, start = c(2002,1), end = c(2019,11))
selictreino = window(selic, start = c(1999,3), end = c(2017,12))
selicteste = window(selic, start = c(2017,12), end = c(2019,11))
industria = ts(arqindustria$prod_industrial_mensal, start = c(2002,1), end = c(2019,11), frequency = 12)
industriatreino = window(industria, start = c(2002,1), end = c(2017,12))
industriateste = window(industria, start = c(2017,12), end = c(2019,11))
#Essas outras divisões foram criadas para melhorar a apresentação dos dados e resultados
selicparcial = window(selic, start = c(2014,12), end = c(2019,11))
industriaparcial = window(industria, start = c(2013,12), end = c(2019,11))
industriaparcial2 = window(industria, start = c(2014,12), end = c(2019,11))
#somar mais 12 mesese de 4.5 para gerar plot ideal
selicparcialplot = ts(c(selicparcial,4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5), start = c(2014,12), end = c(2020,12), frequency = 12)
#somar mais 12 mesese de 1.22 para gerar plot ideal
industriaparcialplot = ts(c(industriaparcial,88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0), start = c(2013,12), end = c(2020,12), frequency = 12)
industriaparcialplot2 = ts(c(industriaparcial2,88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0, 88.0), start = c(2014,12), end = c(2020,12), frequency = 12)

#Imprimir Comparativo Selic x industria
par(mfrow=c(2,1), mar=c(3,10,3,6))
plot(selictreino)
lines(selicteste, col = "red")
legend("topright", legend=c("selictreino", "selicteste"),
       lty=1, col=c("2","1"), bty="s", pch=19)
title("SELIC Treino x SELIC Teste")
plot(industriatreino)
lines(industriateste, col = "red")
legend("topleft", legend=c("industriatreino", "industriateste"),
       lty=1, col=c("2","1"), bty="n", pch=19)
title("Indústria Treino x Indústria Teste")
par(mfrow=c(1,1), mar=c(5,4,4,2))

#Criando modelo Arima
modeloarimaSELIC = auto.arima(selictreino, trace = T, stepwise = F, approximation = F)
modeloarimaindustria = auto.arima(industriatreino, trace = T, stepwise = F, approximation = F)

#Selecionamos trace = True para listrar os modelos Arima possíveis,
#isso ajuda a visualizar o processo e a mostrar o resultado
#Selecionamos stepwise = True para testarmos todos os modelos
#Selecionamos approximation = False para não realizar proximações.

#Result SELIC:
#Best model: ARIMA(1,1,2)(1,0,0)[12] 
#Arima (1,1,2) (na parte não sazonal)
#Result Produção Industrial:
#Best model: ARIMA(2,1,2)(0,1,1)[12]

#Criar previsao para 2018,01-2020,12: 
preverarimaSELIC = forecast(modeloarimaSELIC, h=36)
preverarimaindustria = forecast(modeloarimaindustria, h=36)
#Criando modelo ets
modeloetsSELIC = ets(selictreino)
modeloetsindustria = ets(industriatreino)
preveretsSELIC = forecast(modeloetsSELIC, h=36)
preveretsindustria = forecast(modeloetsindustria, h=36)
#Criando modelos para Redes Neurais
modelonnetarSELIC = nnetar(selictreino)
modelonnetarindustria = nnetar(industriatreino)
prevernnetarSELIC = forecast(modelonnetarSELIC, h=36)
prevernnetarindustria = forecast(modelonnetarindustria, h=36)

#Ver graficamente os modelos para as duas séries:

par(mfrow=c(2,1), mar=c(3,10,3,6))
plot(selicparcialplot,ylab = "Percentual")
lines(preverarimaSELIC$mean, col="green")
lines(preveretsSELIC$mean, col="red")
lines(prevernnetarSELIC$mean, col="blue")
legend("bottomleft", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19)
title("Previsão Taxa Selic entre 2018 e 2020")
plot(industriaparcialplot, ylab = "Percentual")
lines(preverarimaindustria$mean, col="green")
lines(preveretsindustria$mean, col="red")
lines(prevernnetarindustria$mean, col="blue")
legend("bottomleft", legend=c("Arima", "Ets","Redes Neurais"),
       lty=1, col=c("green","red","blue"), bty="n", pch=19, text.width =0.001)
title("Previsão da Produção Industrial entre 2018 e 2020")
par(mfrow = c(1, 1), mar=c(5,4,4,2))


#Comparativo Realidade x Previsao industria:
par(mfrow=c(2,2))
plot(industriatreino, xlab = "Anos", ylab = "Percentual")
lines(industriateste, col = "red")
legend("topleft", legend=c("industriatreino", "industriateste"),
       lty=1, col=c("2","1"), bty="n", pch=19)
title("industria entre 2014 e 2020")
plot(preverarimaindustria)
plot(preveretsindustria)
plot(prevernnetarindustria)
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

modeloindustriaxSELIC_arima = auto.arima(industria, xreg = selic2, trace = T, stepwise = F, approximation = F)
print(modeloindustriaxSELIC_arima)
preverindustriaxSELIC_arima = forecast(modeloindustriaxSELIC_arima, xreg = arimaSelic2020)
preverindustriaxSELIC_ets = forecast(modeloindustriaxSELIC_arima, xreg = etsSelic2020)
preverindustriaxSELIC_nnetar = forecast(modeloindustriaxSELIC_arima, xreg = nnetarSelic2020)

print(preverindustriaxSELIC_arima$mean)
print(preverindustriaxSELIC_ets$mean)
print(preverindustriaxSELIC_nnetar$mean)

print(industriaparcialplot2)

plot(industriaparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverindustriaxSELIC_arima$mean, col = "red")
lines(preverarimaindustria$mean, col = "blue")
legend("topright", legend=c("industriaxSELIC", "industria"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão Indústria x SELIC - Redução")

plot(industriaparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverindustriaxSELIC_ets$mean, col = "red")
lines(preveretsindustria$mean, col = "blue")
legend("topright", legend=c("industriaxSELIC", "industria"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão Indústria x SELIC - Estagnação")

plot(industriaparcialplot2, xlab = "Anos", ylab = "Percentual")
lines(preverindustriaxSELIC_nnetar$mean, col = "red")
lines(prevernnetarindustria$mean, col = "blue")
legend("topright", legend=c("industriaxSELIC", "industria"),
       lty=1, col=c("red","blue"), bty="n", pch=19)
title("Previsão Indústria x SELIC - Aumento")


