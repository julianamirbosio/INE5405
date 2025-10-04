## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

library(ggplot2)

## --- Tabela de frequência: variável qualitativa (tipo de motor) ---
table(carros$tipo_de_motor)

# Criar dataframe de frequências
df_tipo_de_motor <- as.data.frame(table(carros$tipo_de_motor))
colnames(df_tipo_de_motor) <- c("tipo_de_motor", "freq")

# Reordenar tipo de motor pela frequência
df_tipo_de_motor$tipo_de_motor <- reorder(df_tipo_de_motor$tipo_de_motor, -df_tipo_de_motor$freq)

# Gráfico de barras com ggplot2
ggplot(df_tipo_de_motor, aes(x = tipo_de_motor, y = freq)) +
  geom_bar(stat = "identity", fill = "tomato", color = "black") +
  labs(title = "Distribuição dos Tipos de Motores (Ordenados)",
       x = "Tipos de Motores",
       y = "Frequência") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


