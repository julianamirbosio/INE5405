# --- 1. Carregar Bibliotecas ---
library(ggplot2)
library(scales) 

## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

# --- 3. Limpeza dos Dados (Potência e Performance) ---

# Limpar 'cavalo_potencia' (removendo vírgulas)
carros$cavalo_potencia <- gsub(",", "", carros$cavalo_potencia)
carros$cavalo_potencia <- as.numeric(carros$cavalo_potencia)

# Limpar 'perfomance' (trocando vírgula por ponto)
carros$perfomance <- gsub(",", ".", carros$perfomance)
carros$perfomance <- as.numeric(carros$perfomance)

# --- 4. Preparar os dados para o teste ---
# Criar um dataframe limpo, removendo NAs *em pares*
carros_corr_limpo <- na.omit(carros[, c("cavalo_potencia", "perfomance")])

# --- 5. Definir a Hipótese ---
# H0: A correlação (rho) é >= 0 (positiva ou nula)
# H1: A correlação (rho) é < 0 (negativa)

# --- 6. Executar o Teste de Correlação de Pearson ---
# Usamos alternative = "less" porque nossa H1 é "menor que" (correlação negativa)
teste_corr <- cor.test(~ cavalo_potencia + perfomance, 
                       data = carros_corr_limpo, 
                       method = "pearson", 
                       alternative = "less")

# --- 7. Exibir os resultados completos do teste (no console) ---
print(teste_corr)

# --- 8. GERAR GRÁFICO DE APOIO (Gráfico de Dispersão) ---

# Pegar o valor 'r' (estimativa da correlação) do teste para o subtítulo
valor_r <- teste_corr$estimate

ggplot(carros_corr_limpo, aes(x = cavalo_potencia, y = perfomance)) +
  
  # Gráfico de dispersão
  geom_point(alpha = 0.4, color = "steelblue") +
  
  # Linha de tendência (regressão linear)
  geom_smooth(method = "lm", col = "#e41a1c", fill = "grey80") +
  
  # A sua análise de HP (Trabalho 1, Fig 4 [cite: 201-209]) mostrou forte assimetria
  # Usar log no eixo X melhora a visualização (como na sua Fig 9 [cite: 371-376])
  scale_x_log10(labels = scales::comma) +
  
  labs(title = "Correlação: Potência (HP) vs. Desempenho (0-100 km/h)",
       subtitle = paste("Correlação de Pearson (r) =", round(valor_r, 3)),
       x = "Cavalos de Potência (HP) - (Eixo logarítmico)",
       y = "Tempo de Aceleração (segundos)") +
  theme_minimal()
