# Carregar pacotes necessários
library(dplyr)
library(ggplot2)

# Descomentar caso necessário -> Instalação de Pacotes
#install.packages("ggplot2") 
#install.packages("car", repos = "https://vps.fmvz.usp.br/CRAN/", parallel::detectCores()) 
#install.packages("data.table", repos = "https://vps.fmvz.usp.br/CRAN/", parallel::detectCores()) 

# Carregar o dataset (substituir pelo caminho do arquivo correto)
#BRAZIL_CITIES_REV2022 <- read.csv("C:/Users/anasa/Desktop/aprendendo-r/MQAM/MQA2024 – Grupo11 – dataset.csv")

################################################################################

###################  PARTE 01 : ESTATÍSTICA DESCRITIVA  ########################

#################### MEDIDAS DE POSIÇÃO E DISPERSÃO ############################

# Função para calcular a moda
moda <- function(variavel) {
  valores_distintos <- unique(variavel)
  
  valores_distintos[
    which.max(
      tabulate(
        match(variavel, valores_distintos)
      )
    )
  ]
}

# Função para calcular as estatísticas
calcular_estatisticas <- function(dados, variavel) {
  media <- mean(dados[[variavel]], na.rm = TRUE)
  moda_valor <- moda(dados[[variavel]])
  mediana <- median(dados[[variavel]], na.rm = TRUE)
  
  total <- sum(dados[[variavel]], na.rm = TRUE)
  
  variancia <- var(dados[[variavel]], na.rm = TRUE)
  desvio_padrao <- sd(dados[[variavel]], na.rm = TRUE)
  amplitude_total <- range(dados[[variavel]], na.rm = TRUE)
  amplitude_total_valor <- diff(amplitude_total)
  coef_variacao <- (desvio_padrao / mean(dados[[variavel]], na.rm = TRUE)) * 100
  quartis <- quantile(dados[[variavel]], na.rm = TRUE)
  
  return(list(
    Média = media,
    Moda = moda_valor,
    Mediana = mediana,
    Total = total,
    Variância = variancia,
    Desvio_Padrão = desvio_padrao,
    Amplitude_Total = amplitude_total_valor,
    Coeficiente_Variação = coef_variacao,
    Quartis = quartis
  ))
}

# Variável a ser analisada
variavel_analisada <- "IBGE_60."

# Estatísticas descritivas para o dataset inteiro
estatisticas_total <- calcular_estatisticas(BRAZIL_CITIES_REV2022, variavel_analisada)

# Exibição
cat("\nEstatísticas do dataset inteiro:\n")
cat("Média: ", estatisticas_total$Média, "\n")
cat("Moda: ", estatisticas_total$Moda, "\n")
cat("Mediana: ", estatisticas_total$Mediana, "\n")
cat("Total da variável", variavel_analisada, ": ", estatisticas_total$Total, "\n")
cat("Variância: ", estatisticas_total$Variância, "\n")
cat("Desvio Padrão: ", estatisticas_total$Desvio_Padrão, "\n")
cat("Amplitude Total: ", estatisticas_total$Amplitude_Total, "\n")
cat("Coeficiente de Variação: ", estatisticas_total$Coeficiente_Variação, "%\n")
cat("Quartis:\n")
print(estatisticas_total$Quartis)

# Por região
regioes <- unique(BRAZIL_CITIES_REV2022$REGION)
resultados <- list()

for (regiao in regioes) {
  dados_regiao <- BRAZIL_CITIES_REV2022 %>% filter(REGION == regiao)
  
  resultados[[regiao]] <- calcular_estatisticas(dados_regiao, variavel_analisada)
}

# Exibição
for (regiao in regioes) {
  cat("\nEstatísticas da região:", regiao, "\n")
  cat("Média: ", resultados[[regiao]]$Média, "\n")
  cat("Moda: ", resultados[[regiao]]$Moda, "\n")
  cat("Mediana: ", resultados[[regiao]]$Mediana, "\n")
  cat("Total da variável", variavel_analisada, ": ", resultados[[regiao]]$Total, "\n")
  cat("Variância: ", resultados[[regiao]]$Variância, "\n")
  cat("Desvio Padrão: ", resultados[[regiao]]$Desvio_Padrão, "\n")
  cat("Amplitude Total: ", resultados[[regiao]]$Amplitude_Total, "\n")
  cat("Coeficiente de Variação: ", resultados[[regiao]]$Coeficiente_Variação, "%\n")
  cat("Quartis:\n")
  print(resultados[[regiao]]$Quartis)
}

######################## PARTE 02 : TÉCNICA ANOVA ##############################

medidas_estatisticas_imprimir <- function(variavel_quantitativa) {
  sprintf(
    "- Medidas de tendencia central:\nMedia: %f\nMediana: %f\nModa: %f\n\n- Medidas de dispersao:\nDesvio-padrao: %f\n\n",
    mean(variavel_quantitativa),
    median(variavel_quantitativa),
    moda(variavel_quantitativa),
    sd(variavel_quantitativa)
  ) |>
    cat()
}

anova_grupos_testar_pressupostos <- function(variavel_quantitativa, variavel_qualitativa) {
  valor_p_min <- 0.05
  
  for (grupo in unique(variavel_qualitativa)) {
    teste_shapiro_wilk_resultado <- variavel_quantitativa[which(variavel_qualitativa == grupo)] |>
      shapiro.test()
    
    if (teste_shapiro_wilk_resultado[["p.value"]] < valor_p_min) {
      sprintf(
        "(AVISO-Normalidade) A variavel dependente provavelmente nao segue uma distribuicao normal no grupo %s (Shapiro-Wilk: valor-p = %g, W = %f)\n",
        grupo,
        teste_shapiro_wilk_resultado[["p.value"]],
        teste_shapiro_wilk_resultado[["statistic"]]
      ) |>
        cat()
    }
  }
  
  teste_levene_resultado <- car::leveneTest(variavel_quantitativa ~ variavel_qualitativa) |>
    na.omit()
  
  if (teste_levene_resultado[["Pr(>F)"]][[1L]] < valor_p_min) {
    sprintf(
      "(AVISO-Variancia) As variancias dos grupos provavelmente sao diferentes (Levene: valor-p = %g)\n",
      teste_levene_resultado[["Pr(>F)"]]
    ) |>
      cat()
  }
} 

resultado_anova_imprimir <- function(variavel_quantitativa, variavel_qualitativa) {
  anova_resultados <- aov(formula = variavel_quantitativa ~ variavel_qualitativa)
  
  soma_quadrados_total <- summary(anova_resultados)[[1L]][["Sum Sq"]] |>
    sum()
  soma_quadrados_total_graus_de_liberdade <- summary(anova_resultados)[[1L]][["Df"]] |>
    sum()
  quadrados_medios_total <- soma_quadrados_total / soma_quadrados_total_graus_de_liberdade
  
  cat("- Resultado da ANOVA:\n")
  
  anova_grupos_testar_pressupostos(variavel_quantitativa, variavel_qualitativa)
  
  sprintf(
    "Soma de quadrados total: %.4g (%d graus de liberdade)\nQuadrados medios total: %.4g\n",
    soma_quadrados_total,
    soma_quadrados_total_graus_de_liberdade,
    quadrados_medios_total
  ) |>
    cat()
  
  summary(anova_resultados) |>
    print()
}

# Carregar o dataset (substituir pelo caminho do arquivo correto)

main <- function() {
  dataset_caminho_relativo <- "C:/Users/anasa/Desktop/aprendendo-r/MQAM/MQA2024 – Grupo11 – dataset.csv"
  variavel_quantitativa_nome <- "IBGE_60."
  variavel_qualitativa_nome <- "REGION"
  
  dataset <- data.table::fread(file = dataset_caminho_relativo) |>
    unique() |>
    na.omit()
  variavel_quantitativa <- dataset[[variavel_quantitativa_nome]]
  variavel_qualitativa <- dataset[[variavel_qualitativa_nome]] |>
    as.factor()
  
  medidas_estatisticas_imprimir(variavel_quantitativa)
  resultado_anova_imprimir(variavel_quantitativa, variavel_qualitativa)
}

main()

################################################################################

############################## PARTE 03: GRÁFICOS ##############################

# Carregar o dataset (substituir pelo caminho do arquivo correto)
#BRAZIL_CITIES_REV2022 <- data.table::fread("C:/Users/anasa/Desktop/aprendendo-r/MQAM/MQA2024 – Grupo11 – dataset.csv") -> EXEMPLO!
# OBSERVAÇÃO: É NECESSÁRIO SUBSTITUIR O CAMINHO NOS PONTOS:
# "ggsave" e "data.table" !

############################## PERCENTUAIS #####################################

# Calcular o percentual de cada região
percentuais_regioes <- BRAZIL_CITIES_REV2022 %>%
  group_by(REGION) %>%
  summarise(total = sum(get(variavel_analisada), na.rm = TRUE)) %>%
  mutate(percentual = (total / sum(total)) * 100)

print(percentuais_regioes)

grafico_de_pizza <- ggplot(percentuais_regioes, aes(x = "", y = percentual, fill = REGION)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = paste("Percentual da variável IBGE_60+ por Região"),
       fill = "Região") +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.background = element_rect(fill = "white"),  
        panel.background = element_rect(fill = "white")) +
  scale_fill_brewer(palette = "Set3") 

ggsave(filename = "grafico_de_pizza.png", 
       plot = grafico_de_pizza, 
       width = 8, height = 6, dpi = 300, bg = "white")


############################# GRÁFICO DE COLUNAS ###############################

dataset <-
  data.table::fread("BRAZIL_CITIES_REV2022.csv") |>
  unique() |>
  na.omit()

divisao_em_intervalos <- dataset[[variavel_analisada]] |>
  cut(breaks = 3L) |>
  table() |>
  as.data.frame()

colnames(divisao_em_intervalos)[[1L]] <- "Intervalo"

print(divisao_em_intervalos)

grafico_de_colunas <- ggplot2::ggplot(divisao_em_intervalos, ggplot2::aes(x = Intervalo, y = Freq)) +
  ggplot2::geom_col(color = "white", fill = "cornflowerblue") +
  labs(title = paste("Gráfico de colunas do valor da variável IBGE_60+ por intervalo")) 

ggplot2::ggsave(filename = "grafico_de_colunas.png", plot = grafico_de_colunas)

################################ BOXPLOT 02 ####################################

dataset <- data.table::fread("BRAZIL_CITIES_REV2022.csv") |>
  unique() |>
  na.omit()

linhas_selecionadas <- which(dataset[["IBGE_60."]] != 0L)
dados <- data.frame(
  dataset[linhas_selecionadas, "IBGE_60."] |>
    log(),
  dataset[linhas_selecionadas, "REGION"]
) |>
  setNames(c("IBGE_60._log", "Região"))

boxplot <- ggplot2::ggplot(dados) +
  ggplot2::geom_boxplot(ggplot2::aes(x = Região, y = IBGE_60._log, fill = Região)) +
  ggplot2::ylab("IBGE_60+_log") +
  labs(title = paste("Gráfico Boxplot com valores em logarítmo da variável IBGE_60+")) 

ggplot2::ggsave(filename = "boxplot_log.png", plot = boxplot)


############################ HISTOGRAMA ########################################

dataset <- data.table::fread("BRAZIL_CITIES_REV2022.csv") |>
  unique() |>
  na.omit()

dados <- dataset[which(dataset[["IBGE_60."]] > 0L), "IBGE_60."] |>
  log() |>
  as.data.frame() |>
  setNames("IBGE_60._log")

histograma <- ggplot2::ggplot(
  data = dados,
  ggplot2::aes(x = IBGE_60._log)
) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(density)),
    color = "black",
    fill = "cornflowerblue",
    bins = 30L
  ) +
  ggplot2::geom_density(
    color = "darkred"
  ) +
  ggplot2::labs(
    x = "IBGE_60+_log",
    y = "Densidade"
  ) +
  labs(title = paste("Histograma de Densidade da variável IBGE_60+")) 

ggplot2::ggsave(filename = "histograma.png", plot = histograma)

################################################################################


