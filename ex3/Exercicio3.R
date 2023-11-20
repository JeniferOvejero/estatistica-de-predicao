library(data.table)
base <- fread(input = paste0("selecao.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(base)

# seleção de variaveis modelo 1
m0 = lm(y ~ 1, data = base)
m1 = step(m0,list(lower = ~ 1,
                  upper = ~ x1+x2+x3+x4+x5+x6+x7+x8+x9),
          direction = "forward")

# Qualidade do Ajuste (análise de resíduos)
plot(fitted(m1), rstandard(m1))
abline(0, 0)
anova(m1)
summary(m1)
anova(m1)

# transformação das variáveis
base$x1sqrt = base$x1 ^ 2
base$x2sqrt = base$x2 ^ 2
base$x3sqrt = base$x3 ^ 2
base$x4sqrt = base$x4 ^ 2
base$x5sqrt = base$x5 ^ 2
base$x6sqrt = base$x6 ^ 2
base$x7sqrt = base$x7 ^ 2
base$x8sqrt = base$x8 ^ 2
base$x9sqrt = base$x9 ^ 2

# seleção de variaveis modelo 2
m0 = lm(y ~ 1, data = base)
m2 = step(m0, list(lower = ~ 1,
                   upper = ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 +
                     x1sqrt + x2sqrt + x3sqrt + x4sqrt + x5sqrt + x6sqrt + x7sqrt + x8sqrt + x9sqrt),
          direction = "forward")

# Qualidade do Ajuste (análise de resíduos)
plot(fitted(m2), rstandard(m2))
abline(0, 0)
anova(m2)
summary(m2)

# Modelo Final
m3 = lm(y ~ x2 + x4 + x5 + x7 + x8 + x9 +  x5sqrt + x7sqrt + x8sqrt + x9sqrt, data = base)

# Qualidade do Ajuste (análise de resíduos)
plot(fitted(m3), rstandard(m3))
abline(0, 0)
anova(m3)
summary(m3)