# --- 1. Pacotes ---
library(ggplot2)
library(dplyr)
library(scales) # para formatar eixos em reais

# --- 2. Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Converter variáveis para numéricas
# --- Limpeza da coluna 'preco' ---
carros$preco <- gsub(",", "", carros$preco)        # remove vírgulas
carros$preco <- gsub("\\$", "", carros$preco)      # se tiver cifrão
carros$preco <- gsub(" ", "", carros$preco)        # remove espaços em branco
carros$preco <- as.numeric(carros$preco)           # converte para número
carros$perfomance <- as.numeric(gsub(",", ".", carros$perfomance))

# --- Gráfico de Dispersão com Escala Logarítmica no Preço ---
ggplot(carros, aes(x = perfomance, y = preco)) +
  geom_point(alpha = 0.6, color = "pink") +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linetype = "dashed") +
  scale_y_log10(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Relação entre Preço e Desempenho (0–100 km/h)",
    subtitle = "Em escala logarítmica",
    x = "Tempo de aceleração (s)",
    y = "Preço (US$)"
  ) +
  theme_minimal(base_size = 14)

# --- Gráfico de Linha de Tendência por Faixas de Desempenho ---
faixas <- carros %>%
  mutate(faixa_perf = cut(perfomance, breaks = seq(0, 35, by = 5))) %>%
  group_by(faixa_perf) %>%
  summarise(preco_medio = mean(preco, na.rm = TRUE))

ggplot(faixas, aes(x = faixa_perf, y = preco_medio)) +
  geom_col(fill = "orange") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Preço Médio por Faixa de Desempenho",
    x = "Faixa de tempo de aceleração (s)",
    y = "Preço médio dos carros (US$)"
  ) +
  theme_minimal(base_size = 14)


