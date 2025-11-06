# --- 1. Carregar Bibliotecas ---
library(ggplot2)
library(scales) # Para formatar eixos (ex: labels = comma)

# --- 2. Carregar os Dados ---
# Certifique-se de que o arquivo "carros.csv" está no seu diretório de trabalho
carros <- read.csv("carros.csv", stringsAsFactors = FALSE)

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

# --- 4. Modelo 1: Regressão Linear Simples (Preco ~ Potência) ---

# Construir o modelo
modelo_simples <- lm(preco ~ cavalo_potencia, data = carros_reg)

# Analisar o modelo (R-squared de 0.2991)
print(summary(modelo_simples))

# Plotar o modelo simples
ggplot(carros_reg, aes(x = cavalo_potencia, y = preco)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", col = "red", formula = y ~ x) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Regressão Linear: Preço vs. Potência",
       x = "Cavalos de Potência (HP)",
       y = "Preço (US$)") +
  theme_minimal()


# --- 5. Modelo 2: Regressão Log-Linear (RECOMENDADO) ---

# Construir o modelo log-linear (log10 do preço)
modelo_log <- lm(log10(preco) ~ cavalo_potencia, data = carros_reg)

# Analisar o modelo log-linear (R-squared de 0.8195)
# Use este summary para a sua hipótese!
print(summary(modelo_log))

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