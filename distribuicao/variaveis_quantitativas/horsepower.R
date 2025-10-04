library(ggplot2)
library(readr)

# Carregando o dataset
carros <- read_csv("carros.csv")

# Criando o histograma
ggplot(carros, aes(x = cavalo_potencia)) +
  geom_histogram(binwidth = 20, fill = "orange", color = "tomato") +
  labs(
    title = "Distribuição da Potência dos Motores",
    x = "Cavalos de Potência (HP)",
    y = "Frequência (Contagem de Carros)"
  ) +
  theme_minimal()

# Histograma com curva de densidade sobreposta
ggplot(carros, aes(x = cavalo_potencia)) +
  geom_histogram(aes(y = ..density..), binwidth = 25, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribuição da Potência dos Motores com Curva de Densidade",
    x = "Cavalos de Potência (HP)",
    y = "Densidade"
  ) +
  theme_classic()

# Criando o box plot
ggplot(carros, aes(y = cavalo_potencia)) +
  geom_boxplot(fill = "mediumseagreen", color = "black", alpha = 0.8) +
  labs(
    title = "Resumo da Distribuição de Potência dos Motores",
    x = "", # Remove o rótulo do eixo x
    y = "Cavalos de Potência (HP)"
  ) +
  coord_flip() + # Deixa o gráfico na horizontal para melhor visualização
  theme_light()