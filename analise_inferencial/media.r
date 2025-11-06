# --- 1. Carregar Bibliotecas ---
library(ggplot2)
library(scales) 

## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

# --- 3. Limpeza da Coluna 'cavalo_potencia' ---
carros$cavalo_potencia <- gsub(",", "", carros$cavalo_potencia)
carros$cavalo_potencia <- as.numeric(carros$cavalo_potencia)

# --- 4. Criar um vetor limpo de cavalos de potência ---
# (Removendo NAs)
# (Conforme sua metodologia de remoção de NAs [cite: 43-44])
hp_limpo <- na.omit(carros$cavalo_potencia)

# --- 5. Executar o Teste T para Média de uma Amostra ---
# H0: mu = 300
# H1: mu != 300
valor_referencia_mu <- 300

teste_media <- t.test(hp_limpo, 
                      mu = valor_referencia_mu, 
                      alternative = "two.sided")

# --- 6. Exibir os resultados do teste no console ---
print(teste_media)

# --- 7. GERAR O GRÁFICO DE APOIO ---
# (Este é o passo novo que você pediu)

# Precisamos dos dados em um data.frame para o ggplot
hp_df <- data.frame(potencia = hp_limpo)

# Pegar a média da amostra que o teste calculou
media_amostra <- teste_media$estimate

# --- NOVO: Criar um data.frame para as linhas e legenda ---
linhas_info <- data.frame(
  Tipo = c("Média Hipotética (mu)", "Média da Amostra (x)"),
  Valor = c(valor_referencia_mu, media_amostra)
)

# Criar o gráfico (Versão Limpa)
ggplot(hp_df, aes(x = potencia)) +
  # Histograma (similar ao do seu Trabalho 1 [cite: 201-209])
  geom_histogram(aes(y = ..density..), binwidth = 25, fill = "steelblue", color = "black", alpha = 0.7) +
  # Linha de densidade
  geom_density(col = "blue", size = 1) +
  
  # --- ALTERADO: Adiciona linhas verticais com base no data.frame 'linhas_info' ---
  # Isso cria a legenda automaticamente
  geom_vline(data = linhas_info, 
             aes(xintercept = Valor, color = Tipo, linetype = Tipo), 
             size = 1.2) +
  
  # --- NOVO: Define cores e tipos de linha personalizados para a legenda ---
  scale_color_manual(values = c("Média Hipotética (mu)" = "tomato", "Média da Amostra (x)" = "darkgreen")) +
  scale_linetype_manual(values = c("Média Hipotética (mu)" = "dotted", "Média da Amostra (x)" = "dashed")) +
  
  # --- NOVO: Limita o zoom do eixo X para ver a área de interesse ---
  # Isso não remove os outliers do cálculo, apenas ajusta a visualização.
  coord_cartesian(xlim = c(0, 800)) +
  
  labs(title = "Visualização do Teste T: Potência dos Carros",
       subtitle = "Distribuição da Potência (HP) vs. Média Hipotética (300 HP)",
       x = "Cavalos de Potência (HP)",
       y = "Densidade",
       color = "Legenda",  # Título da legenda
       linetype = "Legenda") + # Título da legenda
  theme_minimal() +
  theme(legend.position = "top") # Põe a legenda no topo

