library(dplyr)
library(ggplot2)
library(readr)

dados_carros <- read_csv("carros.csv")

dados_carros <- dados_carros %>%
  mutate(
    preco_numerico = as.numeric(gsub(",", "", preco))
  )

preco_medio_por_marca <- dados_carros %>%
  group_by(marca) %>%
  summarise(
    preco_medio = mean(preco_numerico, na.rm = TRUE)
  ) %>%
  arrange(desc(preco_medio))

ggplot(data = preco_medio_por_marca, aes(x = reorder(marca, -preco_medio), y = preco_medio)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = scales::dollar(preco_medio, prefix = "$", big.mark = ",")), 
            vjust = -0.5, size = 3) +
  labs(
    title = "Análise do Preço Médio (USD) dos Carros por Marca",
    x = "Marca",
    y = "Preço Médio (USD)"
  ) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )