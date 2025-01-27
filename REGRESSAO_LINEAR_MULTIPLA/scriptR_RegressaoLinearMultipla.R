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

# Alterar para a Variável a ser analisada
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

################################ GRÁFICOS ######################################

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
  labs(title = paste("Percentual da variável por Região"),
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
  data.table::fread("C:/Users/anasa/Desktop/aprendendo-r/MQAM/MQA2024 – Grupo11 – dataset.csv") |>
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
  labs(title = paste("Gráfico de colunas do valor da variável por intervalo")) 

ggplot2::ggsave(filename = "grafico_de_colunas.png", plot = grafico_de_colunas)

################################ BOXPLOT 02 ####################################

dataset <- data.table::fread("C:/Users/anasa/Desktop/aprendendo-r/MQAM/MQA2024 – Grupo11 – dataset.csv") |>
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
  labs(title = paste("Gráfico Boxplot com valores em logarítmo da variável")) 

ggplot2::ggsave(filename = "boxplot_log.png", plot = boxplot)


############################ HISTOGRAMA ########################################

dataset <- data.table::fread("C:/Users/anasa/Desktop/aprendendo-r/MQAM/MQA2024 – Grupo11 – dataset.csv") |>
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
  labs(title = paste("Histograma de Densidade da variável")) 

ggplot2::ggsave(filename = "histograma.png", plot = histograma)

################################################################################

################## PARTE 02 : TÉCNICA DE REGRESSÃO LINEAR ######################


library(magrittr)

dataset_filtrar <- function(arquivo_caminho, variaveis_nomes) {
  dataset <- data.table::fread(file = arquivo_caminho) %>%
    unique() %>% # remove todas as linhas repetidas
    dplyr::select(tidyselect::all_of(variaveis_nomes))
  
  dataset <- dataset %>%
    dplyr::filter(dplyr::across(tidyselect::where(is.numeric), ~ !is.na(.) & . > 0.0)) %>% # remove todas as linhas com valores quantitativos obviamente invalidos para alguma das variaveis
    dplyr::filter(dplyr::across(tidyselect::where(is.character), ~ !is.na(.))) %>% # remove todas as linhas com valores qualitativos obviamente invalidos para para alguma das variaveis
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ log(.))) # fazer a transformacao log das variaveis quantitativas
  # dplyr::rename_with(~ paste0(., "_log"), tidyselect::where(is.numeric)) # renomear todas as variaveis quantitativas com "_log" no final
  
  dataset
}

########################### VARIÁVEL DUMMY #####################################

variaveis_dummy_obter <- function(dataset, variaveis_independentes_nomes) {
  for (variavel_nome in variaveis_independentes_nomes) {
    # se a variavel for qualitativa, substitui-la por variaveis dummy
    if (is.character(dataset[[variavel_nome]][[1L]])) {
      formula <- reformulate(c(0L, variavel_nome))
      variaveis_dummy <- do.call("model.matrix", list(object = formula, data = dataset)) %>%
        as.data.frame() %>%
        dplyr::select(-1L) # excluir uma das variaveis dummy para que a quantidade delas seja a quantidade de categorias - 1
      
      dataset <- dataset %>%
        dplyr::mutate(
          variaveis_dummy
        ) %>% # adicionar as variaveis dummy ao dataset
        dplyr::select(-tidyselect::all_of(variavel_nome)) # remover a variavel qualitativa que originou as variaveis dummy
    }
  }
  
  dataset
}

############################# MÉTODO BACKWARD ##################################

metodo_backward <- function(regressao_linear_modelo, dataset, variavel_dependente_nome, variaveis_independentes_colunas_selecionadas) {
  beta0_coluna <- 1L # indice da coluna que contem o coeficiente beta0
  nivel_de_confianca <- 0.05 # nivel de confianca usado em todos os testes de hipotese
  valores_p_coluna <- 4L # indice da coluna que contem os valores-p
  regressao_linear_resultado <- summary(regressao_linear_modelo)
  valores_p <- coefficients(regressao_linear_resultado)[-beta0_coluna, valores_p_coluna] # valores-p de todos os coeficientes exceto por beta0
  valor_p_max_indice <- which.max(valores_p) # indice do coeficiente diferente de beta0 com maior valor-p
  
  # Se nenhuma variavel independente tiver valor-p maior que o nivel de confianca, retornar o modelo atual
  if (valores_p[valor_p_max_indice] <= nivel_de_confianca) {
    return(regressao_linear_modelo)
  }
  # Em cada iteracao, remover a variavel com maior valor-p desde que ele seja maior que o nivel de confianca
  repeat {
    sprintf(
      "Removendo a variavel \"%s\" (valor-p = %.3g > alfa = %.3g) do modelo\n",
      variaveis_independentes_colunas_selecionadas[valor_p_max_indice],
      valores_p[valor_p_max_indice],
      nivel_de_confianca
    ) %>%
      cat()
    
    variaveis_independentes_colunas_selecionadas <-
      variaveis_independentes_colunas_selecionadas[-valor_p_max_indice]
    
    regressao_linear_modelo <- reformulate(
      termlabels = variaveis_independentes_colunas_selecionadas,
      response = variavel_dependente_nome
    ) %>%
      lm(data = dataset)
    
    regressao_linear_resultado <- summary(regressao_linear_modelo)
    valores_p <- coefficients(regressao_linear_resultado)[-beta0_coluna, valores_p_coluna]
    valor_p_max_indice <- which.max(valores_p)
    
    # Se nenhuma variavel independente tiver valor-p maior que o nivel de confianca, retornar o modelo atual
    if (valores_p[valor_p_max_indice] <= nivel_de_confianca) {
      return(regressao_linear_modelo)
    }
  }
}

############################# EQUAÇÃO LINEAR ###################################

modelo_linear_equacao_imprimir <- function(regressao_linear_resultado, variavel_dependente_nome) {
  coeficientes_beta_coluna <- 1L # indice da coluna que contem os coeficientes beta
  beta0_coluna <- 1L # indice da coluna que contem o coeficiente beta0
  coeficientes_beta_valores <- coefficients(regressao_linear_resultado)[, coeficientes_beta_coluna] # valores dos coeficientes beta
  coeficientes_beta_variavel_nome <- regressao_linear_resultado %>%
    coefficients() %>%
    rownames() # nomes das variaveis independentes
  
  # Imprimir beta0
  sprintf(
    "%s = %.3g",
    variavel_dependente_nome,
    coeficientes_beta_valores[[beta0_coluna]]
  ) %>%
    cat()
  
  # Imprimir o resto da formula do modelo linear
  for (indice in seq(
    from = beta0_coluna + 1L,
    to = length(coefficients(regressao_linear_resultado)[, coeficientes_beta_coluna])
  )
  ) {
    coeficiente_beta_valor_absoluto <- abs(coeficientes_beta_valores[indice])
    variavel_nome <- coeficientes_beta_variavel_nome[indice]
    
    # Imprimir o sinal do coeficiente beta
    if (coeficientes_beta_valores[indice] >= 0.0) {
      cat(" +")
    } else {
      cat(" -")
    }
    
    # Imprimir o valor do coeficiente beta junto ao nome da variavel correspondente
    sprintf(
      " %.3g*%s",
      coeficiente_beta_valor_absoluto,
      variavel_nome
    ) %>%
      cat()
  }
  cat("\n")
}

########################## REGRESSÃO LINEAR SIMPLES ############################

regressao_linear_simples <- function(dataset, variavel_dependente_nome, variavel_independente_nome) {
  # Obter o modelo
  regressao_linear_modelo <- reformulate(
    termlabels = variavel_independente_nome,
    response = variavel_dependente_nome
  ) %>%
    lm(data = dataset)
  regressao_linear_resultado <- summary(regressao_linear_modelo)
  
  cat("=====(INICIO - RLS)=====\n")
  paste0(variavel_dependente_nome, " X ", variavel_independente_nome, "\n") %>%
    cat()
  
  # Imprimir o modelo
  cat("\n- Equacao do modelo: ")
  modelo_linear_equacao_imprimir(regressao_linear_resultado, variavel_dependente_nome)
  
  print(regressao_linear_resultado)
  
  valores_reais_e_valores_ajustados <- data.frame(
    dataset[[variavel_independente_nome]],
    regressao_linear_modelo %>%
      fitted() %>%
      c()
  ) %>%
    setNames(c("Previsor_valor", "Valor_previsto"))
  
  variavel_dependente_e_variavel_independente <- data.frame(
    dataset[[variavel_dependente_nome]],
    dataset[[variavel_independente_nome]]
  ) %>%
    setNames(c("Valor_dependente", "Valor_independente"))
  
  residuos_e_valores_ajustados <- data.frame(
    regressao_linear_modelo %>%
      residuals() %>%
      c(),
    regressao_linear_modelo %>%
      fitted() %>%
      c()
  ) %>%
    setNames(c("Residuo", "Valor_ajustado"))
  
  # Criar um arquivo com a reta da regressao sobreposta a um grafico de dispersao de variavel dependente vs variavel independente (se a variavei independente for quantitativa)
  if (is.numeric(dataset[[variavel_independente_nome]][[1L]])) {
    cat("- Criando um grafico com a reta da regressao sobreposta a um grafico de dispersao de variavel dependente vs variavel independente\n\n")
    
    modelo_linear_grafico <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = variavel_dependente_e_variavel_independente,
        ggplot2::aes(
          x = Valor_independente,
          y = Valor_dependente
        )
      ) +
      ggplot2::geom_line(
        data = valores_reais_e_valores_ajustados,
        ggplot2::aes(
          x = Previsor_valor,
          y = Valor_previsto
        ),
        color = "red"
      ) +
      ggplot2::labs(
        title = paste0(
          variavel_dependente_nome,
          " X ",
          variavel_independente_nome
        ),
        x = variavel_independente_nome,
        y = variavel_dependente_nome
      )
    
    ggplot2::ggsave(
      filename = paste0(
        variavel_dependente_nome,
        "_vs_",
        variavel_independente_nome,
        ".png"
      ),
      plot = modelo_linear_grafico,
      units = "cm",
      dpi = "retina",
      width = 24L,
      height = 10L
    )
  }
  
############################# GRÁFICO RESÍDUOS  ################################
  
  # Criar um arquivo "residuos_vs_valores_ajustados.png" com o grafico de residuos vs valores ajustados do modelo
  cat("- Criando um grafico de residuos vs valores ajustados do modelo (residuos_vs_valores_ajustados.png)\n")
  residuo_vs_fit <- ggplot2::ggplot(
    data = residuos_e_valores_ajustados,
    ggplot2::aes(
      x = Valor_ajustado,
      y = Residuo
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0L, color = "red") +
    ggplot2::labs(
      title = "Resíduos X Valores ajustados",
      x = "Valor ajustado",
      y = "Resíduo"
    )
  
  ggplot2::ggsave(
    filename = paste0(
      variavel_dependente_nome,
      "_vs_",
      variavel_independente_nome,
      "_residuos_vs_valores_ajustados.png"
    ),
    plot = residuo_vs_fit,
    units = "cm",
    dpi = "retina",
    width = 10L,
    height = 10L
  )
  
  # Criar um arquivo "<variavel_dependente_nome>_vs_<variavel_independente_nome>_residuos_qq_plot.png" com o Q-Q plot dos residuos do modelo
  cat("- Criando um Q-Q plot dos residuos do modelo (residuos_qq_plot.png)\n")
  qq_plot <- ggplot2::ggplot(
    data = residuos_e_valores_ajustados,
    ggplot2::aes(sample = Residuo)
  ) +
    ggplot2::geom_qq() +
    ggplot2::geom_qq_line(color = "red") +
    ggplot2::labs(title = "Q-Q plot dos resíduos")
  
  ggplot2::ggsave(
    filename = paste0(
      variavel_dependente_nome,
      "_vs_",
      variavel_independente_nome,
      "_residuos_qq_plot.png"
    ),
    plot = qq_plot,
    units = "cm",
    dpi = "retina",
    width = 10L,
    height = 10L
  )
  
  paste0(variavel_dependente_nome, " X ", variavel_independente_nome, "\n") %>%
    cat()
  cat("=====(FIM - RLS)=====\n\n")
}

########################### REGRESSÃO LINEAR MÚLTIPLA ##########################

regressao_linear_multipla <- function(dataset, variavel_dependente_nome, variaveis_independentes_nomes) {
  dataset <- dplyr::select(
    dataset,
    tidyselect::all_of(variavel_dependente_nome),
    tidyselect::all_of(variaveis_independentes_nomes)
  ) # remover todas as variaveis desnecessarias do dataset
  
  # Fazer a regressao linear para cada variavel independente (variaveis qualitativas serao transformadas em variaveis dummy)
  for (variavel_independente_nome in variaveis_independentes_nomes) {
    regressao_linear_simples(dataset, variavel_dependente_nome, variavel_independente_nome)
  }
  
  dataset <- variaveis_dummy_obter(dataset, variaveis_independentes_nomes) # transformar todas as variaveis qualitativas em variaveis dummy
  
  # Substituir as variaveis independentes qualitativas pelas variaveis dummy criadas anteriormente na lista de nomes das variaveis independentes
  variaveis_independentes_nomes <- colnames(dataset)[colnames(dataset) != variavel_dependente_nome]
  
  # Obter o modelo inicial com todas as variaveis independentes
  regressao_linear_modelo <- reformulate(
    termlabels = variaveis_independentes_nomes,
    response = variavel_dependente_nome
  ) %>%
    lm(data = dataset)
  
  regressao_linear_resultado <- summary(regressao_linear_modelo)
  
  residuos_e_valores_ajustados <- data.frame(
    regressao_linear_modelo %>%
      residuals() %>%
      c(),
    regressao_linear_modelo %>%
      fitted() %>%
      c()
  ) %>%
    setNames(c("Residuo", "Valor_ajustado"))
  
  cat("=====(INICIO - RLM)=====\n\n")
  
########################### GRÁFICO RESÍDUOS INICIAL ###########################
  
  # Criar um arquivo "RLM_inicial_residuos_qq_plot.png" com o Q-Q plot dos residuos do modelo
  cat("- Criando um Q-Q plot dos residuos do modelo (residuos_qq_plot.png)\n")
  qq_plot <- ggplot2::ggplot(
    data = residuos_e_valores_ajustados,
    ggplot2::aes(sample = Residuo)
  ) +
    ggplot2::geom_qq() +
    ggplot2::geom_qq_line(color = "red") +
    ggplot2::labs(title = "Q-Q plot dos resíduos")
  
  ggplot2::ggsave(
    filename = "RLM_inicial_residuos_qq_plot.png",
    plot = qq_plot,
    units = "cm",
    dpi = "retina",
    width = 10L,
    height = 10L
  )
  
  # Criar um arquivo "RLM_inicial_residuos_vs_valores_ajustados.png" com o grafico de residuos vs valores ajustados do modelo
  cat("- Criando um grafico de residuos vs valores ajustados do modelo (residuos_vs_valores_ajustados.png)\n")
  residuo_vs_fit <- ggplot2::ggplot(
    data = residuos_e_valores_ajustados,
    ggplot2::aes(
      x = Valor_ajustado,
      y = Residuo
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0L, color = "red") +
    ggplot2::labs(
      title = "Resíduos X Valores ajustados",
      x = "Valor ajustado",
      y = "Resíduo"
    )
  
  ggplot2::ggsave(
    filename = "RLM_inicial_residuos_vs_valores_ajustados.png",
    plot = residuo_vs_fit,
    units = "cm",
    dpi = "retina",
    width = 10L,
    height = 10L
  )
  
############################## MODELO INICIAL ##################################
  
  # Imprimir o modelo inicial
  cat("#####(Modelo inicial)#####")
  
  cat("\n- Equacao do modelo inicial: ")
  modelo_linear_equacao_imprimir(regressao_linear_resultado, variavel_dependente_nome)
  
  print(regressao_linear_resultado)
  
  # Uso do metodo de backward para melhorar o modelo inicial
  cat("#####(Inicio do metodo backward)#####\n")
  regressao_linear_modelo <- metodo_backward(
    regressao_linear_modelo,
    dataset,
    variavel_dependente_nome,
    variaveis_independentes_nomes
  )
  
  regressao_linear_resultado <- summary(regressao_linear_modelo)
  
  cat("#####(Fim do metodo backward)########\n\n")
  
  residuos_e_valores_ajustados <- data.frame(
    regressao_linear_modelo %>%
      residuals() %>%
      c(),
    regressao_linear_modelo %>%
      fitted() %>%
      c()
  ) %>%
    setNames(c("Residuo", "Valor_ajustado"))

  #################### GRÁFICOS DE ANÁLISE DOS RESÍDUOS FINAL ##################
  
  # Criar um arquivo "RLM_final_residuos_qq_plot.png" com o Q-Q plot dos residuos do modelo
  cat("- Criando um Q-Q plot dos residuos do modelo (residuos_qq_plot.png)\n")
  qq_plot <- ggplot2::ggplot(
    data = residuos_e_valores_ajustados,
    ggplot2::aes(sample = Residuo)
  ) +
    ggplot2::geom_qq() +
    ggplot2::geom_qq_line(color = "red") +
    ggplot2::labs(title = "Q-Q plot dos resíduos")
  
  ggplot2::ggsave(
    filename = "RLM_final_residuos_qq_plot.png",
    plot = qq_plot,
    units = "cm",
    dpi = "retina",
    width = 10L,
    height = 10L
  )
  
  # Criar um arquivo "RLM_final_residuos_vs_valores_ajustados.png" com o grafico de residuos vs valores ajustados do modelo
  cat("- Criando um grafico de residuos vs valores ajustados do modelo (residuos_vs_valores_ajustados.png)\n")
  residuo_vs_fit <- ggplot2::ggplot(
    data = residuos_e_valores_ajustados,
    ggplot2::aes(
      x = Valor_ajustado,
      y = Residuo
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0L, color = "red") +
    ggplot2::labs(
      title = "Resíduos X Valores ajustados",
      x = "Valor ajustado",
      y = "Resíduo"
    )
  
  ggplot2::ggsave(
    filename = "RLM_final_residuos_vs_valores_ajustados.png",
    plot = residuo_vs_fit,
    units = "cm",
    dpi = "retina",
    width = 10L,
    height = 10L
  )
  
############################## MODELO FINAL ####################################
  
  # Imprimir o modelo final
  cat("\n#####(Modelo final)#####")
  
  cat("\n- Equacao do modelo final: ")
  modelo_linear_equacao_imprimir(regressao_linear_resultado, variavel_dependente_nome)
  
  print(regressao_linear_resultado)
  
  valores_reais_e_valores_ajustados <- data.frame(
    dataset[[variavel_independente_nome]],
    regressao_linear_modelo %>%
      fitted() %>%
      c()
  ) %>%
    setNames(c("Previsor_valor", "Valor_previsto"))
  
  variavel_dependente_e_variavel_independente <- data.frame(
    dataset[[variavel_dependente_nome]],
    dataset[[variavel_independente_nome]]
  ) %>%
    setNames(c("Valor_dependente", "Valor_independente"))
  
  residuos_e_valores_ajustados <- data.frame(
    regressao_linear_modelo %>%
      residuals() %>%
      c(),
    regressao_linear_modelo %>%
      fitted() %>%
      c()
  ) %>%
    setNames(c("Residuo", "Valor_ajustado"))
  
  # Criar um arquivo com a reta da regressao sobreposta a um grafico de dispersao de variavel dependente vs variavel independente (se a variavei independente for quantitativa)
  if (is.numeric(dataset[[variavel_independente_nome]][[1L]])) {
    cat("- Criando um grafico com a reta da regressao sobreposta a um grafico de dispersao de variavel dependente vs variavel independente\n\n")
    
    modelo_linear_grafico <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = variavel_dependente_e_variavel_independente,
        ggplot2::aes(
          x = Valor_independente,
          y = Valor_dependente
        )
      ) +
      ggplot2::geom_line(
        data = valores_reais_e_valores_ajustados,
        ggplot2::aes(
          x = Previsor_valor,
          y = Valor_previsto
        ),
        color = "red"
      ) +
      ggplot2::labs(
        title = paste0(
          variavel_dependente_nome,
          " X ",
          variavel_independente_nome
        ),
        x = variavel_independente_nome,
        y = variavel_dependente_nome
      )
    
    ggplot2::ggsave(
      filename = paste0(
        variavel_dependente_nome,
        "_vs_",
        variavel_independente_nome,
        ".png"
      ),
      plot = modelo_linear_grafico,
      units = "cm",
      dpi = "retina",
      width = 24L,
      height = 10L
    )
  }
  
  # Criar um arquivo "<variavel_dependente_nome>_vs_<variavel_independente_nome>_residuos_vs_valores_ajustados.png" com o grafico de residuos vs valores ajustados do modelo
  cat("- Criando um grafico de residuos vs valores ajustados do modelo (residuos_vs_valores_ajustados.png)\n")
  residuo_vs_fit <- ggplot2::ggplot(
    data = residuos_e_valores_ajustados,
    ggplot2::aes(
      x = Valor_ajustado,
      y = Residuo
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0L, color = "red") +
    ggplot2::labs(
      title = "Resíduos X Valores ajustados",
      x = "Valor ajustado",
      y = "Resíduo"
    )
  
  ggplot2::ggsave(
    filename = paste0(
      variavel_dependente_nome,
      "_vs_",
      variavel_independente_nome,
      "_residuos_vs_valores_ajustados.png"
    ),
    plot = residuo_vs_fit,
    units = "cm",
    dpi = "retina",
    width = 10L,
    height = 10L
  )
  
  # Criar um arquivo "<variavel_dependente_nome>_vs_<variavel_independente_nome>_residuos_qq_plot.png" com o Q-Q plot dos residuos do modelo
  cat("- Criando um Q-Q plot dos residuos do modelo (residuos_qq_plot.png)\n")
  qq_plot <- ggplot2::ggplot(
    data = residuos_e_valores_ajustados,
    ggplot2::aes(sample = Residuo)
  ) +
    ggplot2::geom_qq() +
    ggplot2::geom_qq_line(color = "red") +
    ggplot2::labs(title = "Q-Q plot dos resíduos")
  
  ggplot2::ggsave(
    filename = paste0(
      variavel_dependente_nome,
      "_vs_",
      variavel_independente_nome,
      "_residuos_qq_plot.png"
    ),
    plot = qq_plot,
    units = "cm",
    dpi = "retina",
    width = 10L,
    height = 10L
  )
  
  paste0(variavel_dependente_nome, " X ", variavel_independente_nome, "\n") %>%
    cat()
  cat("=====(FIM - RLS)=====\n\n")
}

dataset <- dataset_filtrar(
  "BRAZIL_CITIES_REV2022_WITH_FILTERS_02.csv", # caminho do arquivo
  c("IBGE_60.", "IDHM_Longevidade", "IBGE_60.", "IBGE_DU", "REGION") # nomes das variaveis selecionadas
)

################################ EXEMPLOS ######################################

# exemplo de regressao linear multipla
regressao_linear_multipla(
  dataset, # dataset a ser utilizado
  "IBGE_60.", # nome da variavel dependente
  c("IDHM_Longevidade", "IBGE_60.", "IBGE_DU", "REGION") # nomes das variaveis independentes
)

# # exemplo de regressao linear simples
# regressao_linear_simples(
#     dataset, # dataset a ser utilizado
#     "IDHM_Longevidade", # nome da variavel dependente
#     "IBGE_60." # nome da variavel independente
# )
