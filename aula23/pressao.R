# LEITURA DA BASE
library(data.table)
base <- fread(input = paste0("pressao.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 

# Modelo
modelo <- glm(pressao ~ peso + sexo + tabag, family = binomial(), data=base)
summary(modelo)
prob <- predict(modelo, base, type = "response")
base1 <- cbind(base,prob)

# Razão de Chances
OR <- data.frame(exp(modelo$coefficients))
IC <- data.frame(exp(confint(modelo)))
IC_OR <- cbind(OR[-1,],IC[-1,])
colnames(IC_OR) <- c("OR","2.5%","97.5%")

# Reordenando os nives da variavel Sexo
library(dplyr)
library(forcats)
base <- base %>% 
        mutate(sexo = sexo %>% 
                      fct_relevel("Masc"))

# Modelo
modelo <- glm(pressao ~ peso + sexo + tabag, family = binomial(), data=base)
summary(modelo)

# Razão de Chances
OR <- data.frame(exp(modelo$coefficients))
IC <- data.frame(exp(confint(modelo)))
IC_OR <- cbind(OR[-1,],IC[-1,])
colnames(IC_OR) <- c("OR","2.5%","97.5%")

# PREDICAO
novo <- data.frame(sexo="Fem", peso=50, tabag="Nao")
predict(modelo, novo, type = "response")

