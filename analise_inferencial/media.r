# --- 1. Carregar Bibliotecas ---
library(ggplot2)
library(scales) # Para formatar eixos (ex: labels = comma)

# --- 2. Carregar os Dados ---
# Certifique-se de que o arquivo "carros.csv" está no seu diretório de trabalho
carros <- read.csv("carros.csv", stringsAsFactors = FALSE)

# 2. Limpar a coluna 'cavalo_potencia'
# (Removendo vírgulas e convertendo para numérico)
carros$cavalo_potencia <- gsub(",", "", carros$cavalo_potencia)
carros$cavalo_potencia <- as.numeric(carros$cavalo_potencia)

# 3. Criar um vetor limpo de cavalos de potência (removendo NAs)
# [cite_start](Conforme sua metodologia de remoção de NAs [cite: 43-44])
hp_limpo <- na.omit(carros$cavalo_potencia)

# 4. Definir a Hipótese
# H0: mu = 300
# H1: mu != 300
valor_referencia_mu <- 300

# 5. Executar o Teste T para Média de uma Amostra
# alternative = "two.sided" é o padrão, e significa que testamos "diferente de"
teste_media <- t.test(hp_limpo, 
                      mu = valor_referencia_mu, 
                      alternative = "two.sided")

# 6. Exibir os resultados completos do teste
print(teste_media)