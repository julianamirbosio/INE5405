## --- Leitura dos dados ---
carros <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(carros)
names(carros)

## --- Tabela de frequência: variável qualitativa (marca) ---
table(carros$marca)

## --- Gráfico de pizza ---
pie(table(carros$marca),
    main = "Proporção das Marcas de Carros",
    col = rainbow(length(unique(carros$marca))))

# Ordenar por frequência
freq_marcas <- sort(table(carros$marca), decreasing = TRUE)
barplot(freq_marcas,
        las = 2,
        col = "tomato",
        border = "black",
        main = "Distribuição das Marcas (Ordenadas)",
        xlab = "Marcas",
        ylab = "Frequência")

