library(data.table)

base <- fread(input = paste0("car_base.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=".")
base <- base[base$drivewheel != "4wd", ]

#organizando
library(dplyr)
base <- base %>%
  rename(tração_da_roda = drivewheel,
         largura_do_carro = carwidth,
         preço = price)

base <- base %>%
  mutate(tração_da_roda = case_when(
    tração_da_roda == "rwd" ~ "traseira",
    tração_da_roda == "fwd" ~ "dianteira",
    TRUE ~ tração_da_roda
  ))

coeficiente_correlacao <- cor(base$largura_do_carro, base$preço)


# Ajuste do modelo de regressão linear com interação
modelo <- lm(preço ~ largura_do_carro * tração_da_roda, data = base)
# Resumo do modelo
summary(modelo)

#grafico de interação no modelo
library(ggplot2)
ggplot(base, aes(x = largura_do_carro, y = preço, color = tração_da_roda)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interação entre Largura do Carro e Tração da Roda Traseira",
       x = "Largura do Carro",
       y = "Price",
       color = "Tração da Roda") +
  theme_minimal()

# Intervalo de confiança e de predição para os dados da base
tração_da_roda <- base$tração_da_roda
largura_do_carro <- base$largura_do_carro
preço <- base$preço

IC1 <- predict(modelo, interval="confidence",level = 0.95)
IC2 <- predict(modelo, interval="predict",level = 0.95)

new <- data.frame(tração_da_roda,largura_do_carro,preço,IC1,IC2)


# Predição para traça dianteira e tem largura de 70 polegadas 
novo = data.frame(tração_da_roda="dianteira", largura_do_carro=70)
predict(modelo, novo, interval="confidence")
predict(modelo, novo, interval="predict")
