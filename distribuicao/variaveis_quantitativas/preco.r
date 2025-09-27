library(ggplot2)
library(dplyr)
library(scales)

## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)

# --- Limpeza da coluna 'preco' ---
carros$preco <- gsub(",", "", carros$preco)        # remove vírgulas
carros$preco <- gsub("\\$", "", carros$preco)      # se tiver cifrão
carros$preco <- gsub(" ", "", carros$preco)        # remove espaços em branco
carros$preco <- as.numeric(carros$preco)           # converte para número

str(carros)
names(carros)

# --- Estatísticas descritivas ---
cat("\nEstatísticas descritivas do preço:\n")
summary(carros$preco)
cat("Média: ", mean(carros$preco, na.rm = TRUE), "\n")
cat("Mediana: ", median(carros$preco, na.rm = TRUE), "\n")
cat("Desvio padrão: ", sd(carros$preco, na.rm = TRUE), "\n")
cat("Mínimo: ", min(carros$preco, na.rm = TRUE), "\n")
cat("Máximo: ", max(carros$preco, na.rm = TRUE), "\n")

# --- Tabela de frequência (em classes) ---
breaks <- c(0, 50000, 100000, 200000, 500000, 1000000, 2000000, 5000000, Inf)
labels <- c("≤ 50 mil", "50–100 mil", "100–200 mil", "200–500 mil", 
            "500 mil–1 mi", "1–2 mi", "2–5 mi", "> 5 mi")

carros$faixa_preco <- cut(carros$preco,
                          breaks = breaks,
                          labels = labels,
                          include.lowest = TRUE)

tabela_freq <- table(carros$faixa_preco)
tabela_freq_relativa <- prop.table(tabela_freq) * 100

print(tabela_freq)
print(round(tabela_freq_relativa, 2))

# --- 5. Histograma (escala logarítmica para lidar com outliers) ---
ggplot(carros, aes(x = preco)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  scale_x_log10(labels = comma) +
  labs(title = "Distribuição do Preço dos Carros (escala log10)",
       x = "Preço (US$)",
       y = "Frequência") +
  theme_minimal(base_size = 14)

# --- Boxplot ---
ggplot(carros, aes(x = "", y = preco)) +   # <- aqui adicionamos x = ""
  geom_boxplot(fill = "lightgreen") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue") +
  scale_y_log10(labels = comma) +
  labs(title = "Boxplot do Preço dos Carros (escala log10)",
       x = "",
       y = "Preço (US$)") +
  theme_minimal()

