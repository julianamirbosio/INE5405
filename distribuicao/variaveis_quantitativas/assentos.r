## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

library(ggplot2)

ggplot(carros, aes(x = factor(assentos))) +
  geom_bar(fill = "tomato") +
  labs(title = "Distribuição Número de Assentos",
       x = "Número de Assentos",
       y = "Frequência") +
  theme_minimal(base_size = 14)


ggplot(carros, aes(y = assentos)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot do Número de Assentos",
       y = "Número de Assentos") +
  theme_minimal(base_size = 14)




