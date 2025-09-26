## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

## --- Tabela de frequência: variável qualitativa (marca) ---
table(carros$marca)

# Criar dataframe de frequências
df_marcas <- as.data.frame(table(carros$marca))
colnames(df_marcas) <- c("marca", "freq")

# Reordenar marcas pela frequência
df_marcas$marca <- reorder(df_marcas$marca, -df_marcas$freq)

# Gráfico de barras com ggplot2
ggplot(df_marcas, aes(x = marca, y = freq)) +
  geom_bar(stat = "identity", fill = "tomato", color = "black") +
  labs(title = "Distribuição das Marcas (Ordenadas)",
       x = "Marcas",
       y = "Frequência") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

