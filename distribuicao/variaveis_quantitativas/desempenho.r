# --- Pacotes ---
library(ggplot2)
library(dplyr)
library(scales)

# --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)

# --- 2. Limpeza e conversão ---
# Converte "2,50" → 2.50 (numérico)
carros$perfomance <- as.numeric(gsub(",", ".", carros$perfomance))

str(carros)
names(carros)

# --- Estatísticas descritivas ---
cat("\nEstatísticas do desempenho (0–100 km/h):\n")
print(summary(carros$perfomance))
cat("Média:", mean(carros$perfomance, na.rm = TRUE), "s\n")
cat("Mediana:", median(carros$perfomance, na.rm = TRUE), "s\n")
cat("Desvio padrão:", sd(carros$perfomance, na.rm = TRUE), "s\n")

# --- Tabela de frequência em classes ---
# Cria faixas de 1s
breaks <- seq(floor(min(carros$perfomance, na.rm = TRUE)),
              ceiling(max(carros$perfomance, na.rm = TRUE)), by = 1)

carros$faixa_perf <- cut(carros$perfomance, breaks = breaks, include.lowest = TRUE)

tabela_freq <- table(carros$faixa_perf)
tabela_freq_relativa <- prop.table(tabela_freq) * 100

cat("\nTabela de frequência absoluta:\n")
print(tabela_freq)
cat("\nTabela de frequência relativa (%):\n")
print(round(tabela_freq_relativa, 2))

# --- Histograma + Densidade ---
ggplot(carros, aes(x = perfomance)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribuição do Tempo de Aceleração (0–100 km/h)",
       x = "Tempo (s)", y = "Densidade")


# --- Boxplot do desempenho ---
ggplot(carros, aes(x = "", y = perfomance)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.color = "red", outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue") +
  labs(title = "Boxplot do Tempo de Aceleração (0–100 km/h)",
       x = "",
       y = "Tempo (s)") +
  theme_minimal(base_size = 14)

