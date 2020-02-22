library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa emprego ao mês(03/1999 - 01/2020 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Transformar o arquivo em uma série temporal
emprego = ts(arq$emprego_formal,start = c(1992,1), end=c(2019,11), frequency=12)

plot(emprego)
print(emprego)

#O índice:
#A série mensal do índice de estoque do emprego formal é calculada a partir do estoque cuja base
#de recuperação é divulgada pelo Cadastro Geral de Empregados e Desempregados (Caged) e dos saldos
#mensais de admissão líquida de demissão já incorporando informações de declarações recebidas fora
#do prazo. A série de estoque é transformada em número índice com o valor igual a 100 em dezembro 
#de 2001, mês anterior ao das divulgações das declarações fora do prazo.

#-------------------------------------------------------------------------------------

autoplot(emprego, ylab = "Índice de Emprego Formal", xlab = "Tempo", main = "Índice do Emprego Formal 1992/2019")
#A série, visualmente, aparenta ter ligeira sazonalidade, porém fica difícil determinar em 
#qual período. O gráfico mostra evolução grande no período porém ligeiro recuo em 2015,
#após isso uma recuperação suave.


#-------------------------------------------------------------------------------------

split.screen( c(1,2))
screen(1)
hist(emprego, xlab = "Percentual", ylab = "Frequência", main = "Histograma Emprego")
screen(2)
boxplot(emprego, ylab = "Mediana", main = "Boxplot Emprego")
close.screen(all=T)
summary(emprego)
#Histograma para ver como os dados estão distribuídos
#A maior parte do tempo o índice de emprego entá em 100 e também tendo sua segunda maior frequência em
#torno de 180.Na verdade, esse índice parece acompanhar a tendência natural do pib, porém, mostrou, em 2014/2015
#que pode sofrer variações em tempo de crise.
#Gerar um boxplot para entender como que a ocupação está distribuída
#Como visto no histograma a mediana está por volta de 123 e uma taxa acima de
#não apresenta outliers.
#   Min.  1st Qu. Median    Mean  3rd Qu.    Max. 
#  94.27   99.69  123.08  133.98  174.82  189.65 


#-------------------------------------------------------------------------------------
#Vamos decompor a série para entender melhor como se comporta a emprego
dec = decompose(emprego)
autoplot(dec, main = "Decomposição Índice de Emprego Formal 1992/2019", xlab="Tempo")  
#Existe um padrão de sazonalidade. Pelo gráfco ela é bem regular  
#Possui uma forte tendência de aumento, queda em 2014/2015 e recuperação a partir de 2017

#-------------------------------------------------------------------------------------
#Até agora nos vimos que o índice médio da emprego é de cerca de 133.
#Existe um padrão sazonal frequente
#No período analisado existe uma tendência de aumento

#-------------------------------------------------------------------------------------
#Vamos analisar a tendência com mais cuidado:
autoplot(dec$trend)
#O índice sobe bastante ao longo dos anos, porém no final de 2014 appresentou uma ligeira queda
#essa queda conincide com a crise econômica.
#Em 2017 iniciou-se uma tímida recuperação.
autoplot(window(dec$trend, start=c(2014,08)))
#Essa tendência de queda do emprego começou em agosto de 2014, voltando a subir em 2017


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupação sazonal 1992-2019 (cada ano é uma cor)
ggseasonplot(emprego)  
#Aqui se confirma que existe um padrão bem regular na sazonalidade  
#Os indices mostram ligeira subida durante o ano e queda em novembro






