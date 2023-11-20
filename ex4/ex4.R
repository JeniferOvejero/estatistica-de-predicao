library(data.table)
base = fread(input = paste0("Deslocamento.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 
base <- as.data.frame(lapply(base, as.factor))
colnames(base)

#modelo
m0 = glm(desloc ~ 1, data = base, family=binomial())
modelo = step(m0, list(lower = ~1,
                   upper = ~escola + sexo + idade + imc + tr + pa + t_livre),
          direction = "forward")
#desloc ~ escola + idade

summary(modelo)
#p-valores abaixo de 0.05

# Razão de Chances
OR = data.frame(exp(modelo$coefficients))
IC = data.frame(exp(confint(modelo)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")

print(IC_OR)
exp(modelo$coefficients)

#A chance de alunos de escolas particulares serem passivos em  deslocamento é 55.15 vezes maior do que alunos de escolas públicas.
#A chance de alunos de 10-12 anos serem passivos em  deslocamento é 0.36 vezes menor do que alunos de 7-9 anos.