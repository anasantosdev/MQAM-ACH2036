# retorna a moda de uma variavel
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

variaveis_dummy_obter <- function(dataset, variaveis_independentes) {
    for (variavel_nome in variaveis_independentes) {
        # se a variavel for qualitativa, substitui-la por variaveis dummy
        if (is.character(dataset[[variavel_nome]][[1L]])) {
            formula <- reformulate(c(0L, variavel_nome))
            variaveis_dummy <- do.call("model.matrix", list(object = formula, data = dataset)) |>
                as.data.frame() |>
                dplyr::select(-1L) # excluir uma das variaveis dummy para que a quantidade delas seja a quantidade de categorias - 1

            dataset <- dataset |>
                dplyr::mutate(
                    variaveis_dummy
                ) |> # adicionar as variaveis dummy ao dataset
                dplyr::select(-tidyselect::all_of(variavel_nome)) # remover a variavel qualitativa que originou as variaveis dummy
        }
    }

    dataset
}

# retorna uma tabela com as medidas de tendencia central, posicao e dispercao de uma variavel
medidas_estatisticas_calcular <- function(dados, variavel) {
    media <- mean(dados[[variavel]], na.rm = TRUE)
    moda_valor <- moda(dados[[variavel]])
    total <- sum(dados[[variavel]], na.rm = TRUE)
    quartis <- quantile(dados[[variavel]], na.rm = TRUE)

    variancia <- var(dados[[variavel]], na.rm = TRUE)
    desvio_padrao <- sd(dados[[variavel]], na.rm = TRUE)
    amplitude_total <- range(dados[[variavel]], na.rm = TRUE)
    amplitude_total_valor <- diff(amplitude_total)
    coef_variacao <- (desvio_padrao / mean(dados[[variavel]], na.rm = TRUE)) * 100L

    matrix(
        c(
            Média = media,
            Moda = moda_valor,
            Mediana = quartis[[3L]], # como o 1o valor de quartis corresponde ao valor minimo, entao a mediana corresponde ao 3o valor de quartis em vez do 2o nesse caso
            Total = total,
            Variância = variancia,
            Desvio_Padrão = desvio_padrao,
            Amplitude_Total = amplitude_total_valor,
            Coeficiente_Variação = coef_variacao,
            min = quartis[[1L]],
            primeiro_quartil = quartis[[2L]],
            terceiro_quartil = quartis[[4L]],
            max = quartis[[5L]]
        ),
        ncol = 12L,
        dimnames = list(c(variavel), c("Média", "Moda", "Mediana", "Total", "Variância", "Desvio Padrão", "Amplitude Total", "Coeficiente de Variação", "min", "primeiro quartil", "terceiro quartil", "max"))
    ) |>
        formatC(digits = 3L, format = "g") |> # remover casas decimais excessivas
        as.table()
}

# salva as medidas de tendencia central, posicao e dispercao de cada variavel em uma tabela
# contida em um arquivo png e gera e salva os graficos (histograma, boxplot e grafico de
# setores) em arquivos png
estatistica_descritiva_realizar <- function(dataset) {
    cat("\n\n=====(INICIO - Estatistica descritiva das variaveis)=====\n\n")
    variaveis_qualitativas <- "RURAL_OU_URBANO" # nomes de todas as variaveis qualitativas
    for (nome in colnames(dataset)) {
        cat("\n\n\t-----(INICIO - Estatistica descritiva da variavel", nome, ")-----\n\n")
        estatisticas <- medidas_estatisticas_calcular(dataset, nome)
        png(
            filename = paste0("estatisticas_", nome, ".png"),
            width = 500L * ncol(estatisticas),
            res = 320L
        )
        gridExtra::grid.table(estatisticas)
        dev.off()

        if (is.element(nome, variaveis_qualitativas)) { # esta variavel eh qualitativa, entao eh gerado um grafico de setores
            dataset_pizza <- data.frame(
                Percentual = c(
                    paste0((mean(dataset[[nome]]) * 100L) |>
                        formatC(digits = 1L, format = "f"), "%"), # remover casas decimais excessivas
                    paste0(((1L - mean(dataset[[nome]])) * 100L) |>
                        formatC(digits = 1L, format = "f"), "%") # remover casas decimais excessivas
                ),
                Categoria = as.factor(c("Rural", "Urbano"))
            ) # dataframe com os dados que serao usados para gerar o grafico de setores
            pizza <- ggplot2::ggplot(dataset_pizza, ggplot2::aes(x = "", y = Percentual, fill = Categoria)) +
                ggplot2::geom_col(color = "black") + # faz o contorno do grafico ficar mais visivel
                ggplot2::geom_text(
                    ggplot2::aes(label = Percentual),
                    position = ggplot2::position_stack(vjust = 0.5)
                ) + # adiciona um texto com as porcentagens dentro de cada fatia
                ggplot2::coord_polar(theta = "y") +
                ggplot2::theme_void() + # remove a cor de fundo do grafico
                ggplot2::labs(title = "Proporção de municípios rurais e urbanos")
            ggplot2::ggsave(
                filename = paste0("pizza_", nome, ".png"),
                plot = pizza
            )
        } else { # esta variavel eh quantitativa, entao sao gerados um histograma e um boxplot
            histograma <- ggplot2::ggplot(data = dataset, ggplot2::aes(x = .data[[nome]])) +
                ggplot2::geom_histogram(
                    ggplot2::aes(y = ggplot2::after_stat(density)),
                    bins = 30L
                ) + # faz um histograma com valores percentuais em vez de absolutos no eixo y
                ggplot2::geom_density(
                    color = "red"
                ) + # adiciona um grafico de densidade
                ggplot2::labs(
                    title = paste0("Histograma de densidade de ", nome),
                    x = nome,
                    y = "Densidade"
                )
            ggplot2::ggsave(
                plot = histograma,
                filename = paste0("histograma_", nome, ".png"),
                dpi = "retina"
            )

            boxplot <- ggplot2::ggplot(data = dataset) +
                ggplot2::geom_boxplot(ggplot2::aes(x = "", y = .data[[nome]])) +
                ggplot2::labs(
                    title = paste0("Boxplot de ", nome),
                    y = nome
                )
            ggplot2::ggsave(
                plot = boxplot,
                filename = paste0("boxplot_", nome, ".png"),
                dpi = "retina"
            )
        }
        cat("\n\n\t-----(FIM - Estatistica descritiva da variavel", nome, ")-----\n\n")
    }
    cat("\n\n=====(FIM - Estatistica descritiva das variaveis)=====\n\n")
}

# remove simbolos que confundem o R dos nomes das variaveis
nomes_remover_simbolos_proibidos <- function(nomes) {
    gsub("_$", "", x = nomes, fixed = TRUE)
}

# imprime apenas uma equacao linear com os coeficientes beta e os nomes das variaveis independentes
equacao_linear_coeficientes_imprimir <- function(coeficientes_beta) {
    coeficientes_beta_valores_coluna <- 1L # indice da coluna que contem os valores dos coeficientes beta
    coeficientes_beta_valores <- coeficientes_beta[, coeficientes_beta_valores_coluna] # valores dos coeficientes beta
    beta0_linha <- 1L # indice da linha que contem o coeficiente beta0
    # Imprimir beta0
    sprintf(
        "%.3f",
        coeficientes_beta_valores[[beta0_linha]]
    ) |>
        cat()

    coeficientes_beta_variavel_nome <- rownames(coeficientes_beta) # nomes das variaveis independentes
    # Imprimir o resto da formula do modelo linear
    for (indice in seq(
        from = beta0_linha + 1L,
        to = length(coeficientes_beta_valores)
    )) {
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
            " %.3f*%s",
            coeficiente_beta_valor_absoluto,
            variavel_nome
        ) |>
            cat()
    }
}

# imprime todas as equacoes do modelo logistico por completo
modelo_logistico_equacao_imprimir <- function(coeficientes_beta, variavel_dependente_nome, evento_nome) {
    cat("\n\n\t-----(INICIO - Equacao logit)-----\n\n")
    # Imprimir beta0
    sprintf(
        "logit(%s) = ",
        variavel_dependente_nome
    ) |>
        cat()
    equacao_linear_coeficientes_imprimir(coeficientes_beta)
    cat("\n\n\t-----(FIM - Equacao logit)-----\n\n")

    cat("\n\n\t-----(INICIO - Equacao probabilidade)-----\n\n")
    # Imprimir "P(evento)"
    sprintf(
        "P(%s) = 1 / (1 + e^-(",
        evento_nome
    ) |>
        cat()
    equacao_linear_coeficientes_imprimir(coeficientes_beta)
    cat("))\n")
    cat("\n\n\t-----(FIM - Equacao probabilidade)-----\n\n")
}

urbano_valor <- 0L # valor escolhido para representar tipologia urbana na variavel dependente
rural_valor <- 1L # valor escolhido para representar tipologia rural na variavel dependente

# transforma a variavel de tipologia do municipio ("RURAL_URBAN") em uma variavel binaria
rural_urban_binaria <- function(rural_urban_vetor) {
    urbano_categorias <- c("Intermediário Adjacente", "Urbano") # categorias predominantementes urbanas
    rural_categorias <- c("Rural Adjacente", "Rural Remoto", "Intermediário Remoto") # categorias predominantementes rurais

    rural_urban_vetor[is.element(rural_urban_vetor, urbano_categorias)] <- urbano_valor # transforma os valores de todos os municipios predominantemente urbanos em 0
    rural_urban_vetor[is.element(rural_urban_vetor, rural_categorias)] <- rural_valor # transforma os valores de todos os municipios predominantemente rurais em 1

    # # debugging
    # sprintf(
    #     "RURAL_URBAN foi transformada em uma variavel binomial:\nUrbano (%% do total): %f\nRural (%% do total): %f\n",
    #     length(which(vetor == urbano_valor)) / length(vetor), # porcentagem de municipios urbanos
    #     length(which(vetor == rural_valor)) / length(vetor) # porcentagem de municipios rurais
    # ) |>
    #     cat()

    as.numeric(rural_urban_vetor)
}

# filtra, renomeia e transforma as variaveis do dataset que serao utilizadas
dataset_filtrar <- function(arquivo_caminho, variaveis_nomes) {
    cat("\n\n=====(INICIO - Filtrando dataset)=====\n\n")

    dataset <- data.table::fread(file = arquivo_caminho) |>
        unique() |> # remove todas as linhas repetidas
        dplyr::select(tidyselect::all_of(variaveis_nomes)) # remove todas as variaveis que nao foram selecionadas
    dataset <- dataset |>
        colnames() |>
        nomes_remover_simbolos_proibidos() |> # remove todos os simbolos que confundem o R dos nomes das variaveis
        setNames(object = dataset) # remove todos os simbolos que confundem o R dos nomes das variaveis
    observacoes_iniciais_quantidade <- nrow(dataset)

    # # debugging
    # dataset |>
    #     head(n = 10L) |>
    #     print()

    # # debugging
    # print(colunas_nomes)

    variavel_dependente <- "RURAL_OU_URBANO" # nome da variavel dependente
    variaveis_percentuais <- c(
        "AREA_PLANTADA_PORCENTAGEM",
        "VAB_AGROPECUARIA_PORCENTAGEM",
        "VAB_INDUSTRIA_PORCENTAGEM",
        "DOMICILIOS_RURAIS_PORCENTAGEM"
    ) # nomes das variaveis cujos valores sao porcentagens
    transformacao_logaritmica_excecoes <- c(variavel_dependente, variaveis_percentuais)

    dataset_final <- dataset |>
        dplyr::mutate(
            dplyr::across(
                "AREA",
                function(string) {
                    gsub(x = string, ".", "", fixed = TRUE) |> # remover os pontos usados como separadores de milhares
                        as.numeric() / # converter as strings para numeros
                        10L # por algum motivo, todos os valores das areas vem incorretamente multiplicados por 10 no dataset original, entao eh necessario dividi-los por 10
                }
            )
        ) |> # converter as areas de strings para numeros
        dplyr::filter(
            dplyr::if_all(
                tidyselect::where(is.numeric),
                function(valor) !is.na(valor) & valor > 0.0
            )
        ) |> # remove todas as linhas com valores quantitativos obviamente invalidos para uma ou mais das variaveis
        dplyr::filter(
            dplyr::if_all(
                tidyselect::where(is.character),
                function(valor) !is.na(valor)
            )
        ) |> # remove todas as linhas com valores qualitativos obviamente invalidos para uma ou mais das variaveis
        dplyr::filter(
            dplyr::if_all(
                "RURAL_URBAN",
                function(valor) valor != "0" & valor != "Sem classificação"
            )
        ) |> # remove todos os valores invalidos de "RURAL_URBAN"
        # transforma RURAL_URBAN em uma variavel binomial
        dplyr::mutate(
            RURAL_URBAN = rural_urban_binaria(RURAL_URBAN)
        ) |>
        # transforma populacao absoluta em densidade demografica
        dplyr::mutate(IBGE_RES_POP = (IBGE_RES_POP * 1000.0) / AREA) |> # os valores do numero de habitantes vem em milhares, entao eh necessario multiplica-los por 1000
        # transforma IBGE_PLANTED_AREA em porcentagem
        dplyr::mutate(IBGE_PLANTED_AREA = (IBGE_PLANTED_AREA / 100L) / # eh necessario dividir por 100 para converter hectares para quilometros quadrados
            AREA) |> # transforma IBGE_PLANTED_AREA em porcentagem
        # transforma GVA_AGROPEC em porcentagem
        dplyr::mutate(GVA_AGROPEC = GVA_AGROPEC / GVA_TOTAL) |>
        # transforma GVA_INDUSTRY em porcentagem
        dplyr::mutate(GVA_INDUSTRY = GVA_INDUSTRY / GVA_TOTAL) |>
        # transforma IBGE_DU_RURAL em porcentagem
        dplyr::mutate(IBGE_DU_RURAL = IBGE_DU_RURAL / IBGE_DU) |>
        dplyr::rename(DENSIDADE_DEMOGRAFICA = IBGE_RES_POP) |> # muda o nome da variavel para refletir as transformacoes realizadas
        dplyr::rename(AREA_PLANTADA_PORCENTAGEM = IBGE_PLANTED_AREA) |> # muda o nome da variavel para refletir as transformacoes realizadas
        dplyr::rename(VAB_AGROPECUARIA_PORCENTAGEM = GVA_AGROPEC) |> # muda o nome da variavel para refletir as transformacoes realizadas
        dplyr::rename(VAB_INDUSTRIA_PORCENTAGEM = GVA_INDUSTRY) |> # muda o nome da variavel para refletir as transformacoes realizadas
        dplyr::rename(DOMICILIOS_RURAIS_PORCENTAGEM = IBGE_DU_RURAL) |> # muda o nome da variavel para um mais legivel
        dplyr::rename(DOMICILIOS_QUANTIDADE_TOTAL = IBGE_DU) |> # muda o nome da variavel para um mais legivel
        dplyr::rename(COLHEITA_VALOR_TOTAL = IBGE_CROP_PRODUCTION) |> # muda o nome da variavel para um mais legivel
        dplyr::rename(VAB_VALOR_TOTAL = GVA_TOTAL) |> # muda o nome da variavel para um mais legivel
        dplyr::rename(RURAL_OU_URBANO = RURAL_URBAN) |> # muda o nome da variavel para um mais legivel
        # faz a transformacao logaritmica de todas as variaveis quantitativas exceto pela variavel dependente e as variaveis expressas em porcentagem
        dplyr::mutate(
            dplyr::across(
                tidyselect::where(is.numeric) & !tidyselect::matches(transformacao_logaritmica_excecoes),
                function(valor) log(valor)
            )
        )

    # # debugging
    # dataset_final |>
    #     head(n = 10L) |>
    #     print()

    # # debugging
    # data.table::fwrite(dataset_final, file = "dataset_final_filtrado_reg_log.csv")

    observacoes_finais_quantidade <- nrow(dataset_final)
    sprintf(
        "%d das %d observacoes iniciais foram removidas devido a presenca de valores estranhos ou invalidos, restando %d observacoes",
        observacoes_iniciais_quantidade - observacoes_finais_quantidade,
        observacoes_iniciais_quantidade,
        observacoes_finais_quantidade
    ) |>
        cat()

    cat("\n\n=====(FIM - Filtrando dataset)=====\n\n")
    dataset_final
}

# retorna um modelo logistico com apenas variaveis independentes de nivel de
# significancia aceitavel partindo de um modelo inicial
metodo_backward <- function(regressao_linear_modelo, dataset, variavel_dependente, variaveis_independentes_colunas_selecionadas) {
    beta0_linha <- 1L # indice da linha que contem o coeficiente beta0
    nivel_de_confianca <- 0.05 # nivel de confianca usado em todos os testes de hipotese (alterar esse valor para 1.00 apra executar o exemplo)
    valores_p_coluna <- 4L # indice da coluna que contem os valores-p
    regressao_linear_resultado <- summary(regressao_linear_modelo)
    valores_p <- coefficients(regressao_linear_resultado)[-beta0_linha, valores_p_coluna] # valores-p de todos os coeficientes exceto por beta0
    valor_p_max_indice <- which.max(valores_p) # indice do coeficiente diferente de beta0 com maior valor-p

    # se nenhuma variavel independente tiver valor-p maior que o nivel de confianca, retornar o modelo atual
    if (valores_p[valor_p_max_indice] <= nivel_de_confianca) {
        return(regressao_linear_modelo)
    }

    # em cada iteracao, remover a variavel com maior valor-p desde que ele seja maior que o nivel de confianca
    repeat {
        sprintf(
            "Removendo a variavel \"%s\" (valor-p = %.3g > alfa = %.3g) do modelo\n",
            variaveis_independentes_colunas_selecionadas[valor_p_max_indice],
            valores_p[valor_p_max_indice],
            nivel_de_confianca
        ) |>
            cat()

        variaveis_independentes_colunas_selecionadas <-
            variaveis_independentes_colunas_selecionadas[-valor_p_max_indice]

        # se todas as variaveis independentes tiverem sido removidas, retornar um modelo sem variaveis independentes
        if (length(variaveis_independentes_colunas_selecionadas) <= 0L) {
            cat("(AVISO) Todas as variaveis independentes foram removidas do modelo\n")

            return(
                glm(variavel_dependente ~ 1L, data = dataset, family = "binomial")
            )
        }

        # refazer a regressao logistica com as variaveis independentes restantes
        regressao_linear_modelo <- reformulate(
            termlabels = variaveis_independentes_colunas_selecionadas,
            response = variavel_dependente
        ) |>
            glm(data = dataset, family = "binomial")

        regressao_linear_resultado <- summary(regressao_linear_modelo)
        valores_p <- coefficients(regressao_linear_resultado)[-beta0_linha, valores_p_coluna]
        valor_p_max_indice <- which.max(valores_p)

        # se nenhuma variavel independente restante tiver valor-p maior que o nivel de confianca, retornar o modelo atual
        if (valores_p[valor_p_max_indice] <= nivel_de_confianca) {
            return(regressao_linear_modelo)
        }
    }
}

# realiza os testes de pressupostos do modelo logistico e ou salva ou imprime
# suas saidas
pressupostos_testes <- function(modelo, dataset, variavel_dependente) {
    cat("\n\n\t\t--(INICIO - Salvando matriz de correlacao das variaveis do modelo)--\n\n")
    png(filename = "matriz_correlacao.png")
    dataset |>
        dplyr::select(!tidyselect::matches(variavel_dependente)) |>
        cor() |>
        corrplot::corrplot(method = "number", type = "upper")
    dev.off()
    cat("\n\n\t\t--(FIM - Salvando matriz de correlacao das variaveis do modelo)--\n\n")

    cat("\n\n\t\t--(INICIO - Fator de inflacao da variancia (VIF))--\n\n")
    car::vif(modelo) |>
        print()
    cat("\n\n\t\t--(FIM - Fator de inflacao da variancia (VIF))--\n\n")

    cat("\n\n\t\t--(INICIO - Salvando grafico dos residuos)--\n\n")
    residuos <- residuals(modelo)
    probabilidades <- predict(modelo, dataset, type = "response")
    png(
        filename = "grafico_residuos.png",
        res = 320L,
        units = "cm",
        width = 12L,
        height = 10L
    )
    arm::binnedplot(x = probabilidades, y = residuos)
    dev.off()
    cat("\n\n\t\t--(FIM - Salvando grafico dos residuos)--\n\n")

    cat("\n\n\t\t--(INICIO - Teste de autocorrelacao)--\n\n")
    cat("\n\n\t\t\tTeste de Ljung-Box:\n")
    residuos |>
        Box.test(type = "Ljung-Box") |>
        print()
    cat("\n\n\t\t--(FIM - Teste de autocorrelacao)--\n\n")

    cat("\n\n\t\t--(INICIO - Grafico de autocorrelacao)--\n\n")
    png(
        filename = "grafico_autocorrelacao_residuos.png",
        res = 320L,
        units = "cm",
        width = 12L,
        height = 10L
    )
    acf(residuos)
    dev.off()
    cat("\n\n\t\t--(FIM - Grafico de autocorrelacao)--\n\n")
}

# imprime as estimativas (na forma de logit e de odds ratio), o nivel de
# significancia e os intervalos de confianca de 95% das estimativas da
# representacao de odds ratios de todos os coeficientes do modelo logistico
modelo_logistico_resultado_imprimir <- function(modelo) {
    modelo_resultado <- summary(modelo)
    print(modelo_resultado, digits = 3L)

    coeficientes_beta_valores <- coefficients(modelo)
    odds_ratios <- coeficientes_beta_valores |>
        exp() |>
        matrix()
    intervalos_de_confianca_de_95_porcento <- modelo |>
        confint.default() |>
        exp() |>
        matrix()
    intervalos_limites_inferiores <- intervalos_de_confianca_de_95_porcento[
        seq_along(coeficientes_beta_valores) # primeira metade do conjunto de limites (limites inferiores)
    ]
    intervalos_limites_superiores <- intervalos_de_confianca_de_95_porcento[
        seq(
            from = length(coeficientes_beta_valores) + 1L,
            to = length(coeficientes_beta_valores) * 2L
        ) # segunda metade do conjunto de limites (limites superiores)
    ]

    # # debugging
    # print(odds_ratios)
    # print(intervalo_limite_inferior)
    # print(intervalo_limite_superior)

    modelo_resultado_coeficientes <- coefficients(modelo_resultado)
    tabela <- as.table(matrix(
        c(
            odds_ratios, # 1a coluna
            intervalos_limites_inferiores, # 2a coluna
            intervalos_limites_superiores # 3a coluna
        ) |> formatC(digits = 3L, format = "g"), # remover casas decimais excessivas
        ncol = 3L,
        dimnames = list(
            Coeficiente = rownames(modelo_resultado_coeficientes),
            "Odds ratio" = c(
                "Odds ratio",
                "Limite inferior",
                "Limite superior"
            )
        )
    ))
    print(tabela)
}

# realiza uma regressao logistica entre as variaveis dependente e independentes
# usando os dados contidos no dataset fornecido
regressao_logistica <- function(dataset, variavel_dependente, variaveis_independentes) {
    cat("\n\n=====(INICIO - Regressao logistica)=====\n\n")
    modelo_inicial <- reformulate(
        response = variavel_dependente,
        termlabels = variaveis_independentes
    ) |>
        glm(data = dataset, family = "binomial")

    cat("\n\n\t-----(INICIO - Saida da regressao logistica)-----\n\n")

    cat("\n\n\t\t--(INICIO - Modelo inicial)--\n\n")
    modelo_logistico_resultado_imprimir(modelo_inicial)
    cat("\n\n\t\t--(FIM - Modelo inicial)--\n\n")

    cat("\n\n\t\t--(INICIO - Metodo backward)--\n\n")
    modelo_final <- metodo_backward(modelo_inicial, dataset, variavel_dependente, variaveis_independentes)

    variaveis_independentes_finais <- modelo_final |>
        summary() |>
        coefficients() |>
        rownames() |>
        purrr::discard(function(x) x == "(Intercept)")
    dataset_final <- dplyr::select(
        dataset,
        tidyselect::all_of(c(
            variavel_dependente,
            variaveis_independentes_finais
        ))
    )
    cat("\n\n\t\t--(FIM - Metodo backward)--\n\n")

    cat("\n\n\t\t--(INICIO - Modelo final)--\n\n")
    modelo_logistico_resultado_imprimir(modelo_final)
    cat("\n\n\t\t--(FIM - Modelo final)--\n\n")

    cat("\n\n\t-----(FIM - Saida da regressao logistica)-----\n\n")

    cat("\n\n\t-----(INICIO - Medidas de avaliacao do modelo)-----\n\n")

    cat("\n\n\t\t--(INICIO - log-likelihood e pseudo-r^2)--\n\n")
    log_likelihood_modelo <- logLik(modelo_final) # log-likelihood do modelo final obtido
    log_likelihood_nulo <- logLik(update(modelo_final, . ~ 1L)) # log-likelihood de um modelo sem qualquer variavel independente
    pseudo_r_quadrado_modelo_logistico <-
        (-2L * log_likelihood_nulo - (-2L * log_likelihood_modelo)) /
            (-2L * log_likelihood_nulo) # calcula o R^2 a partir dos valores de log-likelihood
    sprintf(
        "log_likelihood_nulo: %.3f\nlog_likelihood_modelo: %.3f\nr_quadrado_modelo_logistico: %.3f",
        log_likelihood_nulo,
        log_likelihood_modelo,
        pseudo_r_quadrado_modelo_logistico
    ) |>
        cat()
    cat("\n\n\t\t--(FIM - log-likelihood e pseudo-r^2)--\n\n")

    cat("\n\n\t\t--(INICIO - Matriz de confusao)--\n\n")
    probabilidades <- predict(modelo_final, dataset_final, type = "response") # conjunto das probabilidades de um municipio ser rural de acordo com o modelo
    ponto_de_corte <- 0.5 # foi escolhido um classification cutoff de 50%
    previsoes <- as.factor(ifelse(
        probabilidades > ponto_de_corte,
        "Rural",
        "Urbano"
    )) # conjunto de previsoes do modelo
    matriz_de_confusao <- caret::confusionMatrix(
        data = as.factor(ifelse(
            dataset_final[[variavel_dependente]] == rural_valor,
            "Rural",
            "Urbano"
        )), # transforma os valores 0 e 1 da variavel dependente nas categorias que eles representam para deixar a saida mais legivel
        reference = previsoes
    )
    tabela_de_confusao_incompleta <- matriz_de_confusao[["table"]] # tabela de confusao sem as quantidades totais
    observado_rural <- tabela_de_confusao_incompleta[, 1L] # 1a coluna da tabela incompleta
    observado_urbano <- tabela_de_confusao_incompleta[, 2L] # 2a coluna da tabela incompleta
    predito_rural <- tabela_de_confusao_incompleta[1L, ] # 1a linha da tabela incompleta
    predito_urbano <- tabela_de_confusao_incompleta[2L, ] # 2a linha da tabela incompleta
    tabela_de_confusao_completa <- as.table(matrix(
        c(
            observado_rural, sum(observado_rural), # 1a coluna
            observado_urbano, sum(observado_urbano), # 2a coluna
            sum(predito_rural), sum(predito_urbano), sum(predito_rural, predito_urbano) # 3a coluna
        ),
        nrow = 3L,
        ncol = 3L,
        dimnames = list(
            Predito = c("Rural", "Urbano", "Total"),
            Observado = c("Rural", "Urbano", "Total")
        )
    )) # tabela de confusao com as quantidades totais
    cat("+++++++++++++++++++++++++++\n")
    print(tabela_de_confusao_completa)
    cat("+++++++++++++++++++++++++++\n\n")
    print(matriz_de_confusao)
    cat("\n\n\t\t--(FIM - Matriz de confusao)--\n\n")

    cat("\n\n\t\t--(INICIO - Testes de qualidade do modelo)--\n\n")
    performance::performance_hosmer(model = modelo_final, n_bins = 10L) |> # teste de Hosmer e Lemeshow
        print(digits = 3L)

    aod::wald.test(
        b = coefficients(modelo_final),
        Sigma = vcov(modelo_final),
        Terms = seq(
            from = 2L,
            to = length(coefficients(modelo_final))
        ) # inclue todas as variaveis independentes (todos os coeficientes exceto o intercepto)
    ) |> # teste de Wald
        print(digits = 3L)
    cat("\n\n\t\t--(FIM - Testes de qualidade do modelo)--\n\n")

    cat("\n\n\t-----(FIM - Medidas de avaliacao do modelo)-----\n\n")

    cat("\n\n\t-----(INICIO - Testes dos pressupostos)-----\n\n")
    pressupostos_testes(modelo_final, dataset_final, variavel_dependente)
    cat("\n\n\t-----(FIM - Testes dos pressupostos)-----\n\n")

    modelo_logistico_equacao_imprimir(
        modelo_final |>
            summary() |>
            coefficients(),
        variavel_dependente,
        "Rural"
    ) # imprime as equacoes do modelo final no terminal
    cat("\n\n=====(FIM - Regressao logistica)=====\n\n")
}

# realiza um exemplo de aplicacao simples e ilustrativa de uma regressao logistica
regressao_logistica_exemplo <- function() {
    cat("\n\n=====(INICIO - Exemplo de regressao logistica)=====\n\n")
    set.seed(123) # garantir a reprodutibilidade do exemplo
    numero_de_observacoes <- 100 # numero de observacoes do exemplo

    experiencia <- round(runif(numero_de_observacoes, 0, 40), 0) # experiencia profissional em anos
    certificacao <- sample(c("Sim", "Não"), numero_de_observacoes, replace = TRUE) # possui ou nao certificacao
    escolaridade <- sample(c("Fundamental", "Médio", "Superior"), numero_de_observacoes, replace = TRUE) # nivel de escolaridade
    idade <- round(runif(numero_de_observacoes, 18, 65), 0) # idade em anos
    contratado <- sample(c(0, 1), numero_de_observacoes, replace = TRUE) # foi contratado ou nao

    dataset <- data.frame(
        Experiencia = experiencia,
        Certificacao = certificacao,
        Escolaridade = escolaridade,
        Idade = idade,
        Contratado = contratado
    ) |>
        variaveis_dummy_obter(c("Experiencia", "Certificacao", "Escolaridade", "Idade"))

    variavel_dependente <- "Contratado"
    variaveis_independentes <- dataset |>
        colnames() |>
        purrr::discard(function(x) x == variavel_dependente)

    modelo <- reformulate(
        response = variavel_dependente,
        termlabels = variaveis_independentes
    ) |>
        glm(data = dataset, family = "binomial")

    modelo_logistico_resultado_imprimir(modelo) # imprime os resultados do modelo de exemplo no terminal
    modelo_logistico_equacao_imprimir(
        modelo |>
            summary() |>
            coefficients(),
        variavel_dependente,
        "Contratado"
    ) # imprime as equacoes do modelo de exemplo no terminal

    cat("\n\n=====(FIM - Exemplo de regressao logistica)=====\n\n")
}

# ponto de entrada do script de regressao logistica
main <- function() {
    regressao_logistica_exemplo() # comentar esta linha apos a primeira execucao

    pacotes_utilizados <- c("ggplot2", "corrplot", "arm", "data.table", "dplyr", "gridExtra", "tidyselect", "caret", "performance", "aod", "purrr") # nomes de todos os pacotes utilizados no script
    install.packages(setdiff(pacotes_utilizados, rownames(installed.packages()))) # instala todos os pacotes que sao necessarios, mas nao estao instalados

    dataset_caminho <- "BRAZIL_CITIES_REV2022 - BRAZIL_CITIES_REV2022.CSV" # esse dataset eh o original do kaggle, e nao o alterado

    variavel_dependente_nome <- "RURAL_URBAN"
    variaveis_independentes_nomes <- c(
        "AREA",
        "GVA_TOTAL",
        "GVA_AGROPEC",
        "GVA_INDUSTRY",
        "IBGE_CROP_PRODUCTION_$",
        "IBGE_DU_RURAL",
        "IBGE_DU",
        "IBGE_PLANTED_AREA",
        "IBGE_RES_POP"
    )

    dataset <- dataset_filtrar(
        dataset_caminho,
        c(variavel_dependente_nome, variaveis_independentes_nomes) # nomes das variaveis selecionadas
    )
    estatistica_descritiva_realizar(dataset) # eh bom comentar esta linha apos a primeira execucao para reduzir o tempo de execucao do script se nenhum dos valores das variaveis tiver sido alterado desde a ultima execucao

    variavel_dependente_nome_final_indice <- 1L # o nome da variavel dependente eh o primeiro elemento da lista de nomes finais
    variavel_dependente_nome_final <- colnames(dataset)[[variavel_dependente_nome_final_indice]]
    variaveis_independentes_nomes_finais <- colnames(dataset)[colnames(dataset) != variavel_dependente_nome_final] # atualizar os nomes das variaveis independentes apos a filtragem

    regressao_logistica(dataset, variavel_dependente_nome_final, variaveis_independentes_nomes_finais)
}

main()
