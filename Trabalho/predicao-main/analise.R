library(tidyverse)
library(forecast)
library(lubridate)
library(data.table)

# ITAIPU

base <- fread(input = paste0("itaipu_mensal_oficial.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 

base$tempo <- seq(1:nrow(base))
base$intervalo <- as.factor(base$mes)
base$afluencia <- as.numeric(base$afluencia)

# Visualização gráfica
plot(base$tempo,base$afluencia,xlab="Período de Tempo", ylab="Afluência")
lines(base$tempo,base$afluencia, col = "black")

# SUAVIZACAO EXPONENCIAL SIMPLES (SES) ########################################
alpha1 <- ses(base$afluencia, alpha = 0.1)
alpha2 <- ses(base$afluencia, alpha = 0.5)
alpha3 <- ses(base$afluencia, alpha = 0.9)

#calculo dos erros de cada ajuste
itaipu_resultados_SES <- list(alpha1, alpha2, alpha3) %>% map(accuracy)

itaipu_tabela_SES <- do.call(rbind, lapply(seq_along(itaipu_resultados_SES), function(i) {
  data.frame(Modelo = paste("Modelo", i), itaipu_resultados_SES[[i]])
}))
rownames(itaipu_tabela_SES) <- c("alpha1", "alpha2", "alpha3")

#gráfico do ajuste
plot(base$afluencia,type="s")
lines(fitted(alpha1), col="blue")
lines(fitted(alpha2), col="red")
lines(fitted(alpha3), col="green")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("serie original",
         expression(alpha == 0.1),
         expression(alpha == 0.5),
         expression(alpha == 0.9)),
       pch=1)

alpha_otimo <- ses(base$afluencia)
summary(alpha_otimo)
# alpha = 0.8332 

# SUAVIZACAO EXPONENCIAL DE HOLT ##############################################
beta1 <- holt(base$afluencia, alpha = 0.6, beta = 0.4)
summary(beta1)
beta2 <- holt(base$afluencia)
summary(beta2)
# beta1 = 0.4
# beta  = 0.0043 

#calculo dos erros de cada ajuste
itaipu_resultados_holt <- list(beta1, beta2) %>% map(accuracy)

itaipu_tabela_holt <- do.call(rbind, lapply(seq_along(itaipu_resultados_holt), function(i) {
  data.frame(Modelo = paste("Modelo", i), itaipu_resultados_holt[[i]])
}))
rownames(itaipu_tabela_holt) <- c("beta1", "beta2")

#gráfico do ajuste
plot(base$afluencia,type="s", ylim = c(0, 25000))
lines(fitted(beta1), col = "blue")
lines(fitted(beta2), col = "red")
legend("topleft", lty = 1, col = c(1, "blue", "red"),
       legend = c("Série Original", "beta1", "beta2"),
       pch = 1)

# SUAVIZACAO EXPONENCIAL DE HOLT-WINTER #######################################
base_ts <- ts(base$afluencia, frequency=12, start=c(1980,1))
base_ts

gama_ad <- hw(base_ts, seasonal = "additive")
gama_mult <- hw(base_ts, seasonal = "multiplicative")

# calculando o erro de cada ajuste
itaipu_tabela_holtwinter <- list(gama_ad, gama_mult) %>% map(accuracy)

itaipu_tabela_holtwinter <- do.call(rbind, lapply(seq_along(itaipu_tabela_holtwinter), function(i) {
  data.frame(Modelo = paste("Modelo", i), itaipu_tabela_holtwinter[[i]])
}))
rownames(itaipu_tabela_holtwinter) <- c("gama_ad", "gama_mult")

# analise grafica do ajuste
plot(base_ts, , ylim = c(0, 25000))
lines(fitted(gama_ad), col = "blue")
lines(fitted(gama_mult), col = "red")
legend("topleft", lty = 1, col = c(1, "blue", "red"),
       legend = c("Série Original", "gama_ad", "gama_mult"),
       pch = 1)

# COMPARACAO GERAL ITAIPU ######################################################

itaipu_tabela_geral <- list(alpha_otimo, beta2, gama_ad) %>% map(accuracy)

itaipu_tabela_geral <- do.call(rbind, lapply(seq_along(itaipu_tabela_geral), function(i) {
  data.frame(Modelo = paste("Modelo", i), itaipu_tabela_geral[[i]])
}))
rownames(itaipu_tabela_geral) <- c("alpha_otimo", "beta2", "gama_ad")

# SOBRADINHO

base <- fread(input = paste0("sobradinho_mensal_oficial.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 

base$tempo <- seq(1:nrow(base))
base$intervalo <- as.factor(base$mes)
base$afluencia <- as.numeric(base$afluencia)

# Visualização gráfica
plot(base$tempo,base$afluencia,xlab="Período de Tempo", ylab="Afluência")
lines(base$tempo,base$afluencia, col = "black")

# SUAVIZACAO EXPONENCIAL SIMPLES (SES) ########################################
alpha1 <- ses(base$afluencia, alpha = 0.1)
alpha2 <- ses(base$afluencia, alpha = 0.5)
alpha3 <- ses(base$afluencia, alpha = 0.9)

#calculo dos erros de cada ajuste
sobradinho_resultados_SES <- list(alpha1, alpha2, alpha3) %>% map(accuracy)

sobradinho_tabela_SES <- do.call(rbind, lapply(seq_along(sobradinho_resultados_SES), function(i) {
  data.frame(Modelo = paste("Modelo", i), sobradinho_resultados_SES[[i]])
}))
rownames(sobradinho_tabela_SES) <- c("alpha1", "alpha2", "alpha3")

#gráfico do ajuste
plot(base$afluencia,type="s")
lines(fitted(alpha1), col="blue")
lines(fitted(alpha2), col="red")
lines(fitted(alpha3), col="green")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("serie original",
         expression(alpha == 0.1),
         expression(alpha == 0.5),
         expression(alpha == 0.9)),
       pch=1)

alpha_otimo <- ses(base$afluencia)
summary(alpha_otimo)
# alpha = 0.9999 

# SUAVIZACAO EXPONENCIAL DE HOLT ##############################################
beta1 <- holt(base$afluencia, alpha = 0.6, beta = 0.4)
summary(beta1)
beta2 <- holt(base$afluencia)
summary(beta2)
# beta1 = 0.4
# beta  = 0.007

#calculo dos erros de cada ajuste
sobradinho_resultados_holt <- list(beta1, beta2) %>% map(accuracy)

sobradinho_tabela_holt <- do.call(rbind, lapply(seq_along(sobradinho_resultados_holt), function(i) {
  data.frame(Modelo = paste("Modelo", i), sobradinho_resultados_holt[[i]])
}))
rownames(sobradinho_tabela_holt) <- c("beta1", "beta2")

#gráfico do ajuste
plot(base$afluencia,type="s", ylim = c(-1000, 8000))
lines(fitted(beta1), col = "blue")
lines(fitted(beta2), col = "red")
legend("topleft", lty = 1, col = c(1, "blue", "red"),
       legend = c("Série Original", "beta1", "beta2"),
       pch = 1)

# SUAVIZACAO EXPONENCIAL DE HOLT-WINTER #######################################
base_ts <- ts(base$afluencia, frequency=12, start=c(1980,1))
base_ts

gama_ad <- hw(base_ts, seasonal = "additive")
gama_mult <- hw(base_ts, seasonal = "multiplicative")

# calculando o erro de cada ajuste
sobradinho_tabela_holtwinter <- list(gama_ad, gama_mult) %>% map(accuracy)

sobradinho_tabela_holtwinter <- do.call(rbind, lapply(seq_along(sobradinho_tabela_holtwinter), function(i) {
  data.frame(Modelo = paste("Modelo", i), sobradinho_tabela_holtwinter[[i]])
}))
rownames(sobradinho_tabela_holtwinter) <- c("gama_ad", "gama_mult")

# analise grafica do ajuste
plot(base_ts, , ylim = c(0, 7000))
lines(fitted(gama_ad), col = "blue")
lines(fitted(gama_mult), col = "red")
legend("topleft", lty = 1, col = c(1, "blue", "red"),
       legend = c("Série Original", "gama_ad", "gama_mult"),
       pch = 1)

# COMPARACAO GERAL SOBRADINHO ##################################################

sobradinho_tabela_geral <- list(alpha_otimo, beta2, gama_ad) %>% map(accuracy)

sobradinho_tabela_geral <- do.call(rbind, lapply(seq_along(sobradinho_tabela_geral), function(i) {
  data.frame(Modelo = paste("Modelo", i), sobradinho_tabela_geral[[i]])
}))
rownames(sobradinho_tabela_geral) <- c("alpha_otimo", "beta2", "gama_ad")

