# --- 1. Carregar Bibliotecas ---
library(ggplot2)
library(scales) 
library(dplyr)

## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

#--- 3. Limpar a Coluna 'tipo_de_motor' ---
  # (Apenas removendo NAs ou valores em branco, se houver)
  motores_limpo <- na.omit(carros$tipo_de_motor)
motores_limpo <- motores_limpo[motores_limpo != ""] # Remove strings vazias

# --- 4. Calcular 'n' e 'x' para o teste ---

# n = Número total de observações (Total de carros com motor listado)
n <- length(motores_limpo)

# x = Número de "sucessos" (Contagem de motores "i4")
x <- sum(motores_limpo == "i4")

# --- 5. Definir a Hipótese ---
# H0: p <= 0.25
# H1: p > 0.25
valor_proporcao_h0 <- 0.30

# --- 6. Executar o Teste de Proporção para uma Amostra ---
# Usamos alternative = "greater" porque nossa H1 é "maior que"
teste_proporcao <- prop.test(x = x, 
                             n = n, 
                             p = valor_proporcao_h0, 
                             alternative = "greater",
                             correct = TRUE) # correct = TRUE aplica a Correção de Continuidade de Yates

# --- 7. Exibir os resultados completos do teste (no console) ---
print(teste_proporcao)

# --- 8. GERAR GRÁFICO DE APOIO (NOVO) ---

# Preparar os dados para o ggplot (calcular proporção de TODAS as categorias)
prop_motores_df <- data.frame(tipo_de_motor = motores_limpo) %>%
  count(tipo_de_motor) %>%
  mutate(
    proporcao = n / sum(n),
    # Criar uma coluna para destacar o 'i4'
    destaque = ifelse(tipo_de_motor == "i4", "Destaque (i4)", "Outros")
  )

# Plotar o gráfico de barras de proporção
ggplot(prop_motores_df, aes(x = reorder(tipo_de_motor, -proporcao), y = proporcao, fill = destaque)) +
  geom_bar(stat = "identity", color = "black") +
  
  # Adicionar a linha da Hipótese Nula (p = 0.25)
  geom_hline(yintercept = valor_proporcao_h0, 
             linetype = "dashed", 
             color = "#e41a1c", # Vermelho
             size = 1.2) +
  
  # Anotação para a linha da hipótese
  annotate(geom = "text", 
           x = 5, # Posição x da anotação
           y = 0.27, # Posição y (um pouco acima da linha)
           label = paste("H0: p =", valor_proporcao_h0), 
           color = "#e41a1c",
           fontface = "bold") +
  
  # Formatar o eixo Y como porcentagem
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  # Definir as cores do destaque
  scale_fill_manual(values = c("Destaque (i4)" = "steelblue", "Outros" = "grey80")) +
  
  labs(title = "Visualização do Teste de Proporção: Motores 'i4'",
       subtitle = paste("Proporção Observada (", round(teste_proporcao$estimate * 100, 1), "%) vs. Hipótese Nula (25%)"),
       x = "Tipo de Motor",
       y = "Proporção no Dataset") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), # Rotaciona texto do eixo X
        legend.position = "none") # Remove a legenda (destaque já é óbvio)

