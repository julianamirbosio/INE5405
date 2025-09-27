## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

library(ggplot2)

ggplot(carros, aes(x = marca, y = assentos)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Número de Assentos por Marca",
       x = "Marca",
       y = "Número de Assentos") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(carros, aes(x = marca, fill = factor(assentos))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição de Assentos por Marca",
       x = "Marca",
       y = "Frequência",
       fill = "Assentos") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(carros, aes(x = marca, y = assentos)) +
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.6, color = "tomato") +
  labs(title = "Carros (Assentos x Marca)",
       x = "Marca",
       y = "Número de Assentos") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

ggplot(carros, aes(x = marca, y = assentos)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Número de Assentos por Marca",
       x = "Marca",
       y = "Número de Assentos") +
  scale_y_continuous(breaks = seq(min(carros$assentos, na.rm = TRUE),
                                  max(carros$assentos, na.rm = TRUE), 1)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




