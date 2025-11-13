library(ggplot2)
library(scales) 
library(dplyr)

carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

# --- Limpeza dos Dados (Preço e Potência) ---
carros$preco <- gsub("\\$", "", carros$preco)
carros$preco <- gsub(",", "", carros$preco)
carros$preco <- gsub(" ", "", carros$preco)
carros$preco <- as.numeric(carros$preco)

carros$cavalo_potencia <- gsub(",", "", carros$cavalo_potencia)
carros$cavalo_potencia <- as.numeric(carros$cavalo_potencia)

carros_reg <- na.omit(carros[, c("preco", "cavalo_potencia")])


# ---. Regressão Log-Linear ---

modelo_log <- lm(log10(preco) ~ cavalo_potencia, data = carros_reg)

print(summary(modelo_log))
par(mfrow = c(2,2))
plot(modelo_log, pch = 19)
ggplot(carros_reg, aes(x = cavalo_potencia, y = preco)) +
  geom_point(alpha = 0.5, color = "pink") +
  geom_smooth(method = "lm", col = "steelblue", linetype = "dashed", formula = y ~ x) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Regressão: Preço (Log) vs. Potência",
       subtitle = "Eixo Y em escala logarítmica (log10)",
       x = "Cavalos de Potência (HP)",
       y = "Preço (US$)") +
  theme_minimal()

