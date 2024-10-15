#########################
#Pacotes
#########################

#########################
#instalação
#########################
install.packages("dplyr")
install.packages("fpp2")
install.packages("forecast")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("Metrics")
install.packages("tsibble")
install.packages("tsibbledata")
install.packages("TTR")

###########################
#Carregamento
###########################
library(dplyr)
library(fpp2)
library(forecast)
library(ggplot2)
library(lubridate)
library(Metrics)
library(tsibble)
library(tsibbledata)
library(TTR)
############################

################################
#PREVISÃO DE SÉRIES TEMPORAIS
################################

######################################
# I - Carregamento e análise do dados
######################################

Vendas_cerveja <- ausbeer

print(Vendas_cerveja)

#grafico linha
autoplot(Vendas_cerveja)

#decomposição da serie

Vendas_cerveja %>% decompose() %>% autoplot()

######################################
# II - Métodos e modelos de previsão 
######################################

#######################
# Divisão dos dados
#######################
0.8 * length(Vendas_cerveja)
174/4
1956+43

cerveja_treino <- window(Vendas_cerveja, end = c(1999, 4))
cerveja_teste <- window(Vendas_cerveja, start = c(2000,1))

######################
# 1 - previsor Naive
######################

prev_naive_teste <- naive(cerveja_teste)

# Vendo os valores previstos:
prev_naive_teste

# Calculandoo o erro:
mape(prev_naive_teste$fitted[2:42], cerveja_teste[2:42])*100 #começa no 2 pq o naive desconsidera o primeiro

#gráfico:
autoplot(prev_naive_teste)

##############################
# 2 - previsor Naive Sazonal
##############################

prev_snaive_teste <- snaive(cerveja_teste)

prev_snaive_teste

# cálculo do erro 
mape(prev_snaive_teste$fitted[5:42], cerveja_teste[5:42])*100

#gráfico
autoplot(prev_snaive_teste)

################################
# 3 - previsor de médias móveis
################################

# escolha da janela (ecolher jaanela com menor MAPE)
janela <- 2 #O 2 é o melhor pq deu resultado menor
media_movel_treino <- SMA(cerveja_treino, janela)
mape(cerveja_treino[janela:176], media_movel_treino[janela:176])*100

#previsões na amostra de teste
prev_media_movel <- SMA(cerveja_teste, janela)

#erro
mape(cerveja_teste[janela:42], prev_media_movel[janela:42])*100

#########################################
# 4 - Amortecimento exponencial simples
#########################################

#treino (encontrando alfa)
aes_treino <- ses(cerveja_treino)
summary(aes_treino)

alfa_aes <- 0.1562 

prev_aes_teste <- ses(cerveja_teste, alpha = alfa_aes)

#erro 
mape(prev_aes_teste$ fitted, cerveja_teste)*100

#gráfico
autoplot(prev_aes_teste)

##################################
# 5 - Método de Holt (tendência)
##################################

#Encontrando alfa e bata
holt_treino <- holt(cerveja_treino)
summary(holt_treino)

alfa_holt <- 0.0643
beta_holt <- 0.0249

#previsoes na amostra de teste
prev_holt_teste <- holt(cerveja_teste, alpha = alfa_holt, beta = beta_holt)

#erro
mape(prev_holt_teste$fitted, cerveja_teste)*100

#grafico
autoplot(prev_holt_teste)

#########################################################
# 6 - Método de Holt-Winters (tendência e sazonalidade)
#########################################################

#encontrando os valores de alfa, beta e gama
hw_treino <- hw(cerveja_treino)
summary(hw_treino)

alfa_hw <- 0.2284
beta_hw <- 0.0375
gama_hw <- 0.2031


#Previsoes na amostra de teste
prev_hw_teste <- hw(cerveja_teste, alpha = alfa_hw, beta = beta_hw, gamma = gama_hw)

#erro
mape(prev_hw_teste$fitted, cerveja_teste)*100

#grafico
autoplot(prev_hw_teste)

#vendo os numeros
prev_hw_teste






















