library(ggplot2)
library(scales) 
library(dplyr)

carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

#--- Limpar a Coluna 'tipo_de_motor' ---
motores_limpo <- na.omit(carros$tipo_de_motor)
motores_limpo <- motores_limpo[motores_limpo != ""] # Remove strings vazias

# --- Calcular 'n' e 'x' para o teste ---

# n = Número total de observações (Total de carros com motor listado)
n <- length(motores_limpo)

# x = Número de "sucessos" (Contagem de motores "i4")
x <- sum(motores_limpo == "i4")

# --- Definir a Hipótese ---
# H0: p <= 0.30
# H1: p > 0.30
valor_proporcao_h0 <- 0.30

teste_proporcao <- prop.test(x = x, 
                             n = n, 
                             p = valor_proporcao_h0, 
                             alternative = "greater",
                             correct = TRUE)

print(teste_proporcao)

prop_motores_df <- data.frame(tipo_de_motor = motores_limpo) %>%
  count(tipo_de_motor) %>%
  mutate(
    proporcao = n / sum(n),
    destaque = ifelse(tipo_de_motor == "i4", "Destaque (i4)", "Outros")
  )

ggplot(prop_motores_df, aes(x = reorder(tipo_de_motor, -proporcao), y = proporcao, fill = destaque)) +
  geom_bar(stat = "identity", color = "black") +
  
  geom_hline(yintercept = valor_proporcao_h0, 
             linetype = "dashed", 
             color = "#e41a1c",
             size = 1.2) +
  
  annotate(geom = "text", 
           x = 5, # Posição x da anotação
           y = 0.32, # Posição y (um pouco acima da linha)
           label = paste("H0: p \u2264", valor_proporcao_h0), # \u2264 é o símbolo <= 
           color = "#e41a1c",
           fontface = "bold") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  scale_fill_manual(values = c("Destaque (i4)" = "steelblue", "Outros" = "grey80")) +
  
  labs(title = "Visualização do Teste de Proporção: Motores 'i4'",
       subtitle = paste("Proporção Observada (", round(teste_proporcao$estimate * 100, 1), "%) vs. Hipótese Nula (30%)", sep=""),
       x = "Tipo de Motor",
       y = "Proporção no Dataset") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "none") 