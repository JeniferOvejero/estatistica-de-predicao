library(tidyverse)
library(forecast)
library(lubridate)
library(data.table)
base <- fread(input = paste0("ipca.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
plot(base$IPCA,type="s")

constante <- 0.00000001 #constante consideravelmente baixa para evitar divisões por zero
base$IPCA <- base$IPCA + constante


# SUAVIZACAO EXPONENCIAL SIMPLES (SES) ########################################
alpha1 <- ses(base$IPCA, alpha = 0.1)
alpha2 <- ses(base$IPCA, alpha = 0.5)
alpha3 <- ses(base$IPCA, alpha = 0.9)

#calculo dos erros de cada ajuste
list(alpha1, alpha2, alpha3) %>% map(accuracy)

#gráfico do ajuste
plot(base$IPCA,type="s")
lines(fitted(alpha1), col="blue")
lines(fitted(alpha2), col="red")
lines(fitted(alpha3), col="green")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("serie original",
         expression(alpha == 0.1),
         expression(alpha == 0.5),
         expression(alpha == 0.9)),
       pch=1)

alpha_otimo <- ses(base$IPCA)
summary(alpha_otimo)
#alpha = 0.7307 

# SUAVIZACAO EXPONENCIAL DE HOLT ##############################################
beta1 <- holt(base$IPCA, alpha = 0.6, beta = 0.4)
summary(beta1)
beta2 <- holt(base$IPCA)
summary(beta2)
#alpha = 0.7274 
#beta  = 1e-04

#calculo dos erros de cada ajuste
list(beta1, beta2) %>% map(accuracy)

#gráfico do ajuste
plot(base$IPCA,type="s")
lines(fitted(beta1), col = "blue")
lines(fitted(beta2), col = "red")

# SUAVIZACAO EXPONENCIAL DE HOLT-WINTER #######################################
base_ts <- ts(base$IPCA, frequency=12, start=c(1980,1))
base_ts
#A série temporal contém valores negativos, portanto consideraremos um modelo de
#sazonalidade aditiva, já que o modelo com sazonalidade multiplicativa assume
#que os valores da série temporal e seus componentes (tendência, sazonalidade)
#são estritamente positivos

gama_ad <- hw(base_ts, seasonal = "additive")

# calculo dos erros do ajuste
list(gama_ad) %>% map(accuracy)
summary(gama_ad)

# analise grafica do ajuste
plot(base_ts)
lines(fitted(gama_ad), col = "blue")

# COMPARACAO GERAL#############################################################

list(alpha_otimo, beta2, gama_ad) %>% map(accuracy)
