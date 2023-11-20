# CARREGAR O PACOTE
library(data.table)
# LEITURA DA BASE
base <- fread(input = paste0("salarios.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
# Classificação das variáveis qualitativas
base$Gerencia_CAT <- as.factor(base$Gerencia)
base$Educacional_CAT <- as.factor(base$Educacional)

# Gráfico do efeito da variável quantitativa
library(ggplot2)
ggplot(data = base, aes(x = Experiencia, y = Salario)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Gráfico do efeito da Experiência",
       x = "Experiência",
       y = "Salário")

# Gráfico da interação
ggplot(data = base, aes(x = Experiencia, y = Salario, color = Gerencia_CAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Gráfico da Interação",
       x = "Experiência",
       y = "Salário")

# Modelo
modelo1 <- lm(Salario ~ Experiencia*Gerencia_CAT, data=base)
summary(modelo1)
library(car)
Anova(modelo1)


