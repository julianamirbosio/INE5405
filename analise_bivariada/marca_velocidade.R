## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

library(ggplot2)

ggplot(carros, aes(x = marca, y = velocidade_maxima)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Velocidade Máxima por Marca",
       x = "Marca",
       y = "Velocidade Máxima (km/h)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







