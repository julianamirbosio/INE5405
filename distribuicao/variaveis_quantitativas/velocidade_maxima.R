## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

ggplot(carros, aes(x = velocidade_maxima)) +
  geom_density(fill = "orange", alpha = 0.6) +
  labs(title = "Densidade da Velocidade Máxima dos Carros",
       x = "Velocidade Máxima (km/h)",
       y = "Densidade") +
  theme_minimal(base_size = 14)

ggplot(carros, aes(x = "", y = velocidade_maxima)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "red") +
  labs(title = "Boxplot da Velocidade Máxima dos Carros",
       x = "",
       y = "Velocidade Máxima (km/h)") +
  theme_minimal(base_size = 14)

