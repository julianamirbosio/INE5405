library(ggplot2)
library(scales) 

carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

carros$cavalo_potencia <- gsub(",", "", carros$cavalo_potencia)
carros$cavalo_potencia <- as.numeric(carros$cavalo_potencia)

# (Removendo NAs)
hp_limpo <- na.omit(carros$cavalo_potencia)

# --- Executar o Teste T para Média de uma Amostra ---
# H0: mu = 300
# H1: mu != 300
valor_referencia_mu <- 300

teste_media <- t.test(hp_limpo, 
                      mu = valor_referencia_mu, 
                      alternative = "two.sided")

print(teste_media)

hp_df <- data.frame(potencia = hp_limpo)

media_amostra <- teste_media$estimate

linhas_info <- data.frame(
  Tipo = c("Média Hipotética (mu)", "Média da Amostra (x)"),
  Valor = c(valor_referencia_mu, media_amostra)
)

ggplot(hp_df, aes(x = potencia)) +
  geom_histogram(aes(y = ..density..), binwidth = 25, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(col = "blue", size = 1) +

  geom_vline(data = linhas_info, 
             aes(xintercept = Valor, color = Tipo, linetype = Tipo), 
             size = 1.2) +
  
  scale_color_manual(values = c("Média Hipotética (mu)" = "tomato", "Média da Amostra (x)" = "darkgreen")) +
  scale_linetype_manual(values = c("Média Hipotética (mu)" = "dotted", "Média da Amostra (x)" = "dashed")) +
  
  coord_cartesian(xlim = c(0, 800)) +
  
  labs(title = "Visualização do Teste T: Potência dos Carros",
       subtitle = "Distribuição da Potência (HP) vs. Média Hipotética (300 HP)",
       x = "Cavalos de Potência (HP)",
       y = "Densidade",
       color = "Legenda", 
       linetype = "Legenda") + 
  theme_minimal() +
  theme(legend.position = "top") 

