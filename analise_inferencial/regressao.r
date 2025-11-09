# --- 1. Carregar Bibliotecas ---
library(ggplot2)
library(scales) 
library(dplyr)

## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

# --- 3. Limpeza dos Dados (Preço e Potência) ---

# Limpando a coluna 'preco' [cite: 610-623]
carros$preco <- gsub("\\$", "", carros$preco)
carros$preco <- gsub(",", "", carros$preco)
carros$preco <- gsub(" ", "", carros$preco) # Remove espaços extras
carros$preco <- as.numeric(carros$preco)

# Limpando a coluna 'cavalo_potencia' [cite: 768]
carros$cavalo_potencia <- gsub(",", "", carros$cavalo_potencia)
carros$cavalo_potencia <- as.numeric(carros$cavalo_potencia)

# Criar um dataframe limpo, removendo NAs
# (Conforme sua metodologia de remoção de NAs [cite: 43-44])
carros_reg <- na.omit(carros[, c("preco", "cavalo_potencia")])


# ---. Modelo 2: Regressão Log-Linear (RECOMENDADO) ---

# Construir o modelo log-linear (log10 do preço)
modelo_log <- lm(log10(preco) ~ cavalo_potencia, data = carros_reg)

# Analisar o modelo log-linear (R-squared de 0.8195)
# Use este summary para a sua hipótese!
print(summary(modelo_log))
par(mfrow = c(2,2))
plot(modelo_log, pch = 19)
# Plotar o modelo log-linear (Note o scale_y_log10)
# Este gráfico corresponde à sua Figura 9 [cite: 371-376]
ggplot(carros_reg, aes(x = cavalo_potencia, y = preco)) +
  geom_point(alpha = 0.5, color = "pink") +
  geom_smooth(method = "lm", col = "steelblue", linetype = "dashed", formula = y ~ x) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Regressão: Preço (Log) vs. Potência",
       subtitle = "Eixo Y em escala logarítmica (log10)",
       x = "Cavalos de Potência (HP)",
       y = "Preço (US$)") +
  theme_minimal()

