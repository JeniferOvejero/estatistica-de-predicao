install.packages("basedosdados")
library("basedosdados")

# Defina o seu projeto no Google Cloud
set_billing_id("delta-era-405021")

# Para carregar o dado direto no R
query <- bdplyr("br_ana_reservatorios.sin")
df <- bd_collect(query)

#Mapa dos reservatórios
#https://dadosabertos.ana.gov.br/datasets/7d53f4b435ed4fdfaf11d493c644ce70/explore?location=-7.482760%2C-38.201355%2C7.45
#https://www.ana.gov.br/sar/sin
#https://clima.inmet.gov.br/GraficosClimatologicos

#ITAIPU
#SOBRADINHO

#Clean data
#Group by month
#Select variables
#Create time series for each

sobradinho <- subset(df, nome_reservatorio == "SOBRADINHO")
itaipu <- subset(df, nome_reservatorio == "ITAIPU")

sobradinho$mensal <- format(as.Date(sobradinho$data), "%Y-%m")
sobradinho_mensal <- aggregate(cbind(cota, afluencia, defluencia)  ~ mensal, data = sobradinho, FUN = mean)

itaipu$mensal <- format(as.Date(itaipu$data), "%Y-%m")
itaipu_mensal <- aggregate(cbind(cota, afluencia, defluencia)  ~ mensal, data = itaipu, FUN = mean)


sobradinho_oficial$ano <- format(as.Date(sobradinho$data), "%Y")
sobradinho_oficial$mes <- format(as.Date(sobradinho$data), "%m")
sobradinho_oficial <- aggregate(cbind(cota, afluencia, defluencia)  ~ mensal, data = sobradinho_oficial, FUN = mean)

sobradinho_oficial <- sobradinho

# Carregar o pacote tidyr
library(tidyr)

# Suponha que você tenha um data frame chamado df e a coluna que deseja separar se chama 'data'
# df <- data.frame(data = c("2000-01", "2000-02", "2000-03"))

# Usar a função separate para dividir a coluna 'data' em duas novas colunas 'ano' e 'mes'
df_separado <- separate(sobradinho_oficial, "mensal", into = c("ano", "mes"), sep = "-")
sobradinho_oficial = df_separado


itaipu_oficial <- itaipu

itaipu_oficial$ano <- format(as.Date(itaipu$data), "%Y")
itaipu_oficial$mes <- format(as.Date(itaipu$data), "%m")
itaipu_oficial <- aggregate(cbind(cota, afluencia, defluencia)  ~ mensal, data = itaipu_oficial, FUN = mean)

df_separado <- separate(itaipu_oficial, "mensal", into = c("ano", "mes"), sep = "-")
itaipu_oficial = df_separado



sobradinho_oficial$tempo <- seq(1:nrow(sobradinho_oficial))
sobradinho_oficial$intervalo <- as.factor(sobradinho_oficial$mes)

# Visualização gráfica
plot(sobradinho_oficial$tempo,sobradinho_oficial$afluencia,xlab="Período de Tempo", ylab="Afluência")
lines(sobradinho_oficial$tempo,sobradinho_oficial$afluencia, col = "black")







#######################################################################################################



itaipu_oficial$mensal <- format(as.Date(itaipu$data), "%Y-%m")
itaipu_mensal <- aggregate(cbind(cota, afluencia, defluencia)  ~ mensal, data = itaipu, FUN = mean)


write.csv(sobradinho_mensal, file = "sobradinho_mensal.csv", row.names = FALSE)
write.csv(itaipu_mensal, file = "itaipu_mensal.csv", row.names = FALSE)

base$tempo <- seq(1:nrow(base))
base$I <- as.factor(base$TRIM)



