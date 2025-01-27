##################  PARTE 01 : ESTATÍSTICA DESCRITIVA  ########################


# Usa os nomes originais das variaveis como chaves e os nomes novos como valores
nomes_novos_hashmap <- new.env(hash = TRUE)

# Adiciona as variaveis do dataset Brazillian Cities
nomes_novos_hashmap[["IBGE_POP"]] <- "Populacao_Total"
nomes_novos_hashmap[["IBGE_1"]] <- "Populacao_1"
nomes_novos_hashmap[["IBGE_1-4"]] <- "Populacao_1_a_4"
nomes_novos_hashmap[["IBGE_5-9"]] <- "Populacao_5_a_9"
nomes_novos_hashmap[["IBGE_10-14"]] <- "Populacao_10_a_14"
nomes_novos_hashmap[["IBGE_15-59"]] <- "Populacao_15_a_59"
nomes_novos_hashmap[["IBGE_60+"]] <- "Populacao_60_mais"
nomes_novos_hashmap[["IDHM"]] <- "IDH"
nomes_novos_hashmap[["IDHM_Renda"]] <- "IDH_Renda"
nomes_novos_hashmap[["IDHM_Longevidade"]] <- "IDH_Longevidade"
nomes_novos_hashmap[["IDHM_Educacao"]] <- "IDH_Educacao"
nomes_novos_hashmap[["COMP_TOT"]] <- "Empresas"
nomes_novos_hashmap[["GDP"]] <- "PIB"
nomes_novos_hashmap[["GDP_CAPITA"]] <- "PIB_per_Capita"
nomes_novos_hashmap[["GVA_TOTAL"]] <- "VAB_Total"
nomes_novos_hashmap[["GVA_PUBLIC"]] <- "VAB_Servicos_Publicos"
nomes_novos_hashmap[["GVA_SERVICES"]] <- "VAB_Servicos"
nomes_novos_hashmap[["GVA_AGROPEC"]] <- "VAB_Agropecuaria"
nomes_novos_hashmap[["GVA_INDUSTRY"]] <- "VAB_Industria"
nomes_novos_hashmap[["IBGE_DU"]] <- "Unidades_Domesticas_Total"
nomes_novos_hashmap[["IBGE_DU_URBAN"]] <- "Unidades_Domesticas_Urbanas"
nomes_novos_hashmap[["IBGE_DU_RURAL"]] <- "Unidades_Domesticas_Rurais"

# Adiciona as variaveis do dataset Lifestyle_and_Wellbeing_Data
nomes_novos_hashmap[["FRUITS_VEGGIES"]] <- "Frutas_e_Vegetais_Consumidos"
nomes_novos_hashmap[["DAILY_STRESS"]] <- "Estresse_Diario"
nomes_novos_hashmap[["PLACES_VISITED"]] <- "Lugares_Novos_Visitados"
nomes_novos_hashmap[["CORE_CIRCLE"]] <- "Pessoas_Proximas"
nomes_novos_hashmap[["SUPPORTING_OTHERS"]] <- "Pessoas_que_Ajuda"
nomes_novos_hashmap[["SOCIAL_NETWORK"]] <- "Pessoas_com_quem_Interage"
nomes_novos_hashmap[["ACHIEVEMENT"]] <- "Conquistas_das_quais_se_Orgulha"
nomes_novos_hashmap[["DONATION"]] <- "Doacoes_Feitas"
nomes_novos_hashmap[["TODO_COMPLETED"]] <- "Nivel_de_Finalizacao_de_Tarefas"
nomes_novos_hashmap[["FLOW"]] <- "Horas_em_Estado_de_Fluxo"
nomes_novos_hashmap[["DAILY_STEPS"]] <- "Passos_Andados"
nomes_novos_hashmap[["LIVE_VISION"]] <- "Anos_Futuros_com_Visao_Clara"
nomes_novos_hashmap[["SLEEP_HOURS"]] <- "Horas_de_Sono"
nomes_novos_hashmap[["LOST_VACATION"]] <- "Dias_de_Ferias_Perdidos"
nomes_novos_hashmap[["DAILY_SHOUTING"]] <- "Frequencia_com_que_Grita"
nomes_novos_hashmap[["PERSONAL_AWARDS"]] <- "Reconhecimentos_Recebidos"
nomes_novos_hashmap[["TIME_FOR_PASSION"]] <- "Horas_Fazendo_Algo_com_Paixao"
nomes_novos_hashmap[["WEEKLY_MEDITATION"]] <- "Oportunidades_de_Reflexao"

executar_exemplo <- FALSE # Trocar o valor para TRUE para executar o exemplo de analise fatorial

# Retorna a moda de uma variavel
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

# Retorna uma tabela com as medidas de tendencia central, posicao e dispercao de uma variavel
medidas_estatisticas_calcular <- function(dados, variavel) {
    media <- mean(dados[[variavel]], na.rm = TRUE)
    moda_valor <- moda(dados[[variavel]])
    total <- sum(dados[[variavel]], na.rm = TRUE)
    quartis <- quantile(dados[[variavel]], na.rm = TRUE)

    variancia <- var(dados[[variavel]], na.rm = TRUE)
    desvio_padrao <- sd(dados[[variavel]], na.rm = TRUE)
    amplitude_total <- range(dados[[variavel]], na.rm = TRUE)
    amplitude_total_valor <- diff(amplitude_total)
    coeficiente_de_variacao <- (desvio_padrao / mean(dados[[variavel]], na.rm = TRUE)) * 100L

    matrix(
        c(
            media = media,
            moda = moda_valor,
            mediana = quartis[[3L]], # Como o 1o valor de quartis corresponde ao valor minimo, entao a mediana corresponde ao 3o valor de quartis em vez do 2o nesse caso
            total = total,
            variancia = variancia,
            desvio_padrao = desvio_padrao,
            amplitude_total = amplitude_total_valor,
            coeficiente_variacao = coeficiente_de_variacao,
            min = quartis[[1L]],
            primeiro_quartil = quartis[[2L]],
            segundo_quartil = quartis[[3L]],
            terceiro_quartil = quartis[[4L]],
            max = quartis[[5L]]
        ),
        ncol = 13L,
        dimnames = list(c(variavel), c("Média", "Moda", "Mediana", "Total", "Variância", "Desvio Padrão", "Amplitude Total", "Coeficiente de Variação", "Valor Mínimo", "1º Quartil", "2º Quartil", "3º Quartil", "Valor Máximo"))
    ) |>
        formatC(digits = 3L, format = "g") |> # Remover casas decimais excessivas
        as.table()
}

# Converte uma string que usa pontos como separadores de milhar para um numero
string_para_numero <- function(string) {
    string |>
        gsub(x = _, ".", "", fixed = TRUE) |> # remover os pontos usados como separadores de milhares
        as.numeric() # converter as strings para numeros
}

# Renomeia os nomes das colunas do dataframe fornecido para nomes mais amigaveis e
# retorna um dataframe com os mesmos dados, mas com colunas renomeadas
colunas_renomear <- function(dataset) {
    nomes <- colnames(dataset)
    hashmap_chaves <- names(nomes_novos_hashmap)

    for (i in seq_along(nomes)) {
        nome <- nomes[i]
        if (is.element(nome, hashmap_chaves)) {
            nomes[i] <- nomes_novos_hashmap[[nome]]
        }
    }

    setNames(dataset, nomes)
}

# Salva as medidas de tendencia central, posicao e dispercao de cada variavel em uma tabela
# contida em um arquivo png e gera e salva os graficos (histograma, boxplot e grafico de
# setores) em arquivos png
estatistica_descritiva_realizar <- function(dataset) {
    cat("\n\n\t-----(INICIO - Estatistica descritiva das variaveis)-----\n\n")
    variaveis_qualitativas <- c() # Nomes de todas as variaveis qualitativas
    for (nome in colnames(dataset)) {
        cat("\n\n\t\t--(INICIO - Estatistica descritiva da variavel", nome, ")--\n\n")

        if (typeof(dataset[[nome]]) == "character") {
            dataset[[nome]] <- as.factor(dataset[[nome]])
        }

        estatisticas <- medidas_estatisticas_calcular(dataset, nome)
        png(
            filename = paste0("C:/Users/anasa/Desktop/aprendendo-r/MQAM/estatisticas_", nome, ".png"),
            width = 400L * ncol(estatisticas) + 10L * nchar(nome),
            height = 240L,
            res = 320L
        )
        gridExtra::grid.table(estatisticas)
        dev.off()

        if (is.element(nome, variaveis_qualitativas)) { # Esta variavel eh qualitativa, entao eh gerado um grafico de setores
            dataset_pizza <- data.frame(
                Percentual = c(
                    paste0((mean(dataset[[nome]]) * 100L) |>
                        formatC(digits = 1L, format = "f"), "%"), # Remover casas decimais excessivas
                    paste0(((1L - mean(dataset[[nome]])) * 100L) |>
                        formatC(digits = 1L, format = "f"), "%") # Remover casas decimais excessivas
                ),
                Categoria = as.factor(c("Rural", "Urbano"))
            ) # Dataframe com os dados que serao usados para gerar o grafico de setores
            pizza <- ggplot2::ggplot(dataset_pizza, ggplot2::aes(x = "", y = Percentual, fill = Categoria)) +
                ggplot2::geom_col(color = "black") + # Faz o contorno do grafico ficar mais visivel
                ggplot2::geom_text(
                    ggplot2::aes(label = Percentual),
                    position = ggplot2::position_stack(vjust = 0.5)
                ) + # Adiciona um texto com as porcentagens dentro de cada fatia
                ggplot2::coord_polar(theta = "y") +
                ggplot2::theme_void() + # Remove a cor de fundo do grafico
                ggplot2::labs(title = "Proporção de municípios rurais e urbanos")
            ggplot2::ggsave(
                filename = paste0("C:/Users/anasa/Desktop/aprendendo-r/MQAM/pizza_", nome, ".png"),
                plot = pizza
            )
        } else { # Esta variavel eh quantitativa, entao sao gerados um histograma e um boxplot
            histograma <- ggplot2::ggplot(data = dataset, ggplot2::aes(x = .data[[nome]])) +
                ggplot2::geom_histogram(
                    ggplot2::aes(y = ggplot2::after_stat(density)),
                    bins = 30L
                ) + # Faz um histograma com valores percentuais em vez de absolutos no eixo y
                ggplot2::geom_density(
                    color = "red"
                ) + # Adiciona um grafico de densidade
                ggplot2::labs(
                    title = paste0("Histograma de densidade de ", nome),
                    x = nome,
                    y = "Densidade"
                )
            ggplot2::ggsave(
                plot = histograma,
                filename = paste0("C:/Users/anasa/Desktop/aprendendo-r/MQAM/histograma_", nome, ".png"),
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
                filename = paste0("C:/Users/anasa/Desktop/aprendendo-r/MQAM/boxplot_", nome, ".png"),
                dpi = "retina"
            )
        }
        cat("\n\n\t\t--(FIM - Estatistica descritiva da variavel", nome, ")--\n\n")
    }
    cat("\n\n\t-----(FIM - Estatistica descritiva das variaveis)-----\n\n")
}

# Filtra, renomeia e transforma as variaveis do dataset que serao utilizadas
dataset_filtrar <- function(arquivo_caminho, variaveis) {
    cat("\n\n=====(INICIO - Filtrando dataset)=====\n\n")

    # Dataset original
    dataset <- arquivo_caminho |>
        data.table::fread(file = _) |>
        dplyr::select(tidyselect::all_of(variaveis)) # Remove todas as variaveis que nao serao usadas

    observacoes_iniciais_quantidade <- nrow(dataset)

    # Dataset sem linhas com algum valor nulo, linhas repetidas e com variaveis
    # renomeadas para nomes mais amigaveis
    dataset <- dataset |>
        na.omit() |> # Remove todas as linhas com pelo menos um valor NA
        unique() |> # Remove todas as linhas repetidas
        colunas_renomear()

    # # Debugging - 10 primeiras linhas do dataset
    # dataset |>
    #     head(n = 10L) |>
    #     print()

    # # Debugging - nomes, media e desvio padrao das variaveis
    # for (nome in colnames(dataset)) {
    #     print(nome)
    #     print(mean(dataset[[nome]]))
    #     print(sd(dataset[[nome]]))
    # }

    transformacao_logaritmica_alvos <- c(
        nomes_novos_hashmap[["IBGE_POP"]],
        nomes_novos_hashmap[["IBGE_1"]],
        nomes_novos_hashmap[["IBGE_1-4"]],
        nomes_novos_hashmap[["IBGE_5-9"]],
        nomes_novos_hashmap[["IBGE_10-14"]],
        nomes_novos_hashmap[["IBGE_15-59"]],
        nomes_novos_hashmap[["IBGE_60+"]],
        nomes_novos_hashmap[["IDHM"]],
        nomes_novos_hashmap[["IDHM_Renda"]],
        nomes_novos_hashmap[["IDHM_Longevidade"]],
        nomes_novos_hashmap[["IDHM_Educacao"]],
        nomes_novos_hashmap[["COMP_TOT"]],
        nomes_novos_hashmap[["GDP"]],
        nomes_novos_hashmap[["GDP_CAPITA"]],
        nomes_novos_hashmap[["GVA_TOTAL"]],
        nomes_novos_hashmap[["GVA_PUBLIC"]],
        nomes_novos_hashmap[["GVA_SERVICES"]],
        nomes_novos_hashmap[["GVA_AGROPEC"]],
        nomes_novos_hashmap[["GVA_INDUSTRY"]],
        nomes_novos_hashmap[["IBGE_DU_URBAN"]],
        nomes_novos_hashmap[["IBGE_DU_RURAL"]]
        
    ) # Nomes das variaveis que passarao por uma transformacao logaritmica

    realizar_estatistica_descritiva <- TRUE # Quando for TRUE, realiza a demorada estatistica descritiva
    if (realizar_estatistica_descritiva) {
        transformacao_logaritmica_alvos <- transformacao_logaritmica_alvos[
            !transformacao_logaritmica_alvos %in% c(
                nomes_novos_hashmap[["IDHM"]],
                nomes_novos_hashmap[["IDHM_Renda"]],
                nomes_novos_hashmap[["IDHM_Longevidade"]],
                nomes_novos_hashmap[["IDHM_Educacao"]]
            )
        ]
    }

    if (!executar_exemplo) {
        dataset <- dataset |>
            dplyr::mutate(
                PIB_per_Capita = string_para_numero(PIB_per_Capita) / 1000L # Os valores de GDP_CAPITA vem multiplicado por 1000 por algum motivo, entao eh necessario dividi-los por 1000
            ) |>
            dplyr::mutate(
                dplyr::across(
                    tidyselect::matches("IDH"),
                    function(vetor) {
                        vetor / 10L
                    }
                )
            ) |> # Por algum motivo, todos os valores das colunas relacionadas a IDH vem multiplicados por 10, entao eh necessario dividi-los por 10
            dplyr::filter(
                dplyr::if_all(
                    tidyselect::where(is.numeric),
                    function(vetor) {
                        vetor > 0.0
                    }
                )
            ) # Remove todas as linhas com valores quantitativos zerados para uma ou mais das variaveis
    }
    dataset <- dataset |>
        # dplyr::select(!tidyselect::all_of(c(
        #
        # ))) |> # Remove as variaveis especificadas
        dplyr::mutate(
            dplyr::across(
                tidyselect::where(is.character),
                function(vetor) {
                    string_para_numero(vetor)
                }
            )
        ) |> # Converte strings para numeros
        na.omit() |> # Remove NAs que podem ser introduzidos por string_para_numero()
        dplyr::mutate(
            dplyr::across(
                tidyselect::matches(transformacao_logaritmica_alvos),
                function(vetor) {
                    log(vetor)
                }
            )
        ) # Realiza a transformacao logaritmica das variaveis especificadas

    # # Debugging - 10 primeiras linhas do dataset
    # dataset |>
    #     head(n = 10L) |>
    #     print()

    # # Debugging - 10 linhas com maiores valores de PIB_PER_CAPITA
    # dataset[order(-EMPRESAS_POR_HABITANTE), ] |>
    #     head(x = 10L) |>
    #     print()

    # # Debugging - arquivo csv com o dataset
    # data.table::fwrite(dataset, file = "dataset.csv")

    if (realizar_estatistica_descritiva) {
        # Faz a estatistica descritiva dos dados antes da padronizacao
        estatistica_descritiva_realizar(dataset) # Eh bom comentar esta linha apos a primeira execucao para reduzir o tempo de execucao do script se nenhum dos valores das variaveis tiver sido alterado desde a ultima execucao
    }

    # Dataset com valores padronizados em todas as variaveis quantitativas
    dataset <- dplyr::mutate(
        dataset,
        dplyr::across(
            tidyselect::where(is.numeric),
            function(vetor) {
                scale(vetor) # Padroniza as variaveis usando o metodo Z-score
                # as.vector()
            }
        )
    )

    # # Debugging - 10 primeiras linhas do dataset
    # dataset |>
    #     head(n = 10L) |>
    #     print()

    # # Debugging - nomes, media e desvio padrao das variaveis
    # for (nome in colnames(dataset)) {
    #     print(nome)
    #     if (is.numeric(dataset[[nome]])) {
    #         print(mean(dataset[[nome]]))
    #         print(sd(dataset[[nome]]))
    #     }
    # }

    observacoes_finais_quantidade <- nrow(dataset)

    if (observacoes_iniciais_quantidade - observacoes_finais_quantidade > 0L) {
        sprintf(
            "\n\n[AVISO] %d das %d observacoes iniciais foram removidas devido a presenca de valores estranhos ou invalidos, restando %d observacoes\n\n",
            observacoes_iniciais_quantidade - observacoes_finais_quantidade,
            observacoes_iniciais_quantidade,
            observacoes_finais_quantidade
        ) |>
            cat()
    }

    cat("\n\n=====(FIM - Filtrando dataset)=====\n\n")

    dataset
}

#######################  PARTE 02 : ANÁLISE FATORIAL  ##########################

# Realiza uma analise fatorial usando os dados das variaveis especificadas
# contidos no dataset fornecido
analise_fatorial <- function(dataset, variaveis, fatores_quantidade) {
    paste0("\n\n=====(INICIO - Analise fatorial para as variaveis: ", toString(variaveis), ")=====\n\n") |>
        cat()

    dataset <- dataset |>
        dplyr::select(tidyselect::all_of(variaveis))

    cat("\n\n\t-----(INICIO - Matriz de correlacao)--\n\n")
    png(filename = "matriz_correlacao.png")
    matriz_de_correlacao <- cor(dataset)
    corrplot::corrplot(
        matriz_de_correlacao,
        method = "circle",
        type = "upper",
        order = "AOE"
    )
    dev.off()
    cat("\n\n\t-----(FIM - Matriz de correlacao)--\n\n")

    cat("\n\n\t-----(INICIO - KMO e MSA)--\n\n")
    kmo <- psych::KMO(dataset)
    print(kmo)
    cat("\n\n\t-----(FIM - KMO e MSA)--\n\n")

    cat("\n\n\t-----(INICIO - Teste de Bartlett)--\n\n")
    bartlett <- psych::cortest.bartlett(dataset)
    print(bartlett)
    cat("\n\n\t-----(FIM - Teste de Bartlett)--\n\n")

    cat("\n\n\t-----(INICIO - Metodo do cotovelo)--\n\n")
    # Metodo do cotovelo para determinar a quantidade de fatores
    png(filename = "scree.png")
    psych::scree(dataset, pc = FALSE)
    # psych::fa.parallel(dataset, fa = "fa")
    dev.off()
    cat("\n\n\t-----(FIM - Metodo do cotovelo)--\n\n")

    cat("\n\n\t-----(INICIO - Analise sem rotacao)--\n\n")
    resultado_sem_rotacao <- factanal(dataset, factors = fatores_quantidade, rotation = "none")
    print(resultado_sem_rotacao)

    png(filename = "fatores_sem_rotacao.png", width = "800", height = "480")
    psych::fa.diagram(resultado_sem_rotacao[["loadings"]])
    dev.off()
    cat("\n\n\t-----(FIM - Analise sem rotacao)--\n\n")

    cat("\n\n\t-----(INICIO - Analise com rotacao varimax)--\n\n")
    resultado_com_rotacao <- factanal(dataset, factors = fatores_quantidade, rotation = "varimax")
    print(resultado_com_rotacao)

    png(filename = "fatores_com_rotacao.png", width = "800", height = "480")
    psych::fa.diagram(resultado_com_rotacao[["loadings"]])
    dev.off()
    cat("\n\n\t-----(FIM - Analise com rotacao varimax)--\n\n")

    paste0("\n\n=====(FIM - Analise fatorial para as variaveis: ", toString(variaveis), ")=====\n\n") |>
        cat()
}

# Realiza um exemplo de analise fatorial
exemplo_de_analise_fatorial <- function() {
    cat("\n\n=====(INICIO - Exemplo de analise fatorial)=====\n\n")
    dataset_caminho <- "Wellbeing_and_lifestyle_data_Kaggle.csv"

    # # Debugging - imprime os nomes das variaveis do dataset
    # data.table::fread(dataset_caminho) |>
    #     colnames() |>
    #     print()

    # Nomes originais das variaveis selecionadas
    variaveis_originais <- c(
        "FRUITS_VEGGIES",
        "DAILY_STRESS",
        "PLACES_VISITED",
        "CORE_CIRCLE",
        "SUPPORTING_OTHERS",
        "SOCIAL_NETWORK",
        "ACHIEVEMENT",
        "DONATION",
        "TODO_COMPLETED",
        "FLOW",
        "DAILY_STEPS",
        "LIVE_VISION",
        "SLEEP_HOURS",
        "LOST_VACATION",
        "DAILY_SHOUTING",
        "PERSONAL_AWARDS",
        "TIME_FOR_PASSION",
        "WEEKLY_MEDITATION"
    )

    dataset <- dataset_filtrar(
        dataset_caminho, # Caminho do arquivo contendo o dataset
        variaveis_originais # Nomes das variaveis selecionadas
    )

    variaveis_novas <- c(
        nomes_novos_hashmap[["FRUITS_VEGGIES"]],
        nomes_novos_hashmap[["DAILY_STRESS"]],
        nomes_novos_hashmap[["PLACES_VISITED"]],
        nomes_novos_hashmap[["CORE_CIRCLE"]],
        nomes_novos_hashmap[["SUPPORTING_OTHERS"]],
        nomes_novos_hashmap[["SOCIAL_NETWORK"]],
        nomes_novos_hashmap[["ACHIEVEMENT"]],
        nomes_novos_hashmap[["DONATION"]],
        nomes_novos_hashmap[["TODO_COMPLETED"]],
        nomes_novos_hashmap[["FLOW"]],
        nomes_novos_hashmap[["DAILY_STEPS"]],
        nomes_novos_hashmap[["LIVE_VISION"]],
        nomes_novos_hashmap[["SLEEP_HOURS"]],
        nomes_novos_hashmap[["LOST_VACATION"]],
        nomes_novos_hashmap[["DAILY_SHOUTING"]],
        nomes_novos_hashmap[["PERSONAL_AWARDS"]],
        nomes_novos_hashmap[["TIME_FOR_PASSION"]],
        nomes_novos_hashmap[["WEEKLY_MEDITATION"]]
    )

    # Realiza a analise fatorial
    fatores_quantidade <- 2L
    analise_fatorial(dataset, variaveis_novas, fatores_quantidade)

    # Fatores resultantes da analise com 3 fatores:
    # Fator 1: Interacao social e saude fisica
    # Fator 2: Satisfacao pessoal
    # Fator 3: Estresse

    # Fatores resultantes da analise com 2 fatores:
    # Fator 1: Interacao social e satisfacao pessoal
    # Fator 2: Estresse

    cat("\n\n=====(FIM - Exemplo de analise fatorial)=====\n\n")
}

# Ponto de entrada do script de analise fatorial
main <- function() {
    pacotes_utilizados <- c(
        "gridExtra",
        "ggplot2",
        "data.table",
        "dplyr",
        "tidyselect",
        "psych",
        "corrplot"
    ) # Nomes de todos os pacotes nao pre-instalados utilizados no script

    # Instala todos os pacotes que sao necessarios, mas nao estao instalados
    install.packages(
        setdiff(pacotes_utilizados, rownames(installed.packages()))
        # , repos = "https://vps.fmvz.usp.br/CRAN/" # Descomentar esta linha para especificar um repositorio da CRAN
        # , Ncpus = 12L # Descomentar esta linha para especificar a quantidade de threads da CPU os quais serao utilizados para instalar os pacotes
    )

    if (executar_exemplo) {
        # Executa o exemplo de analise fatorial
        exemplo_de_analise_fatorial()
        return()
    }

    # Substitua esta string pelo caminho correto
    dataset_caminho <- "C:/Users/anasa/Desktop/aprendendo-r/MQAM/BRAZIL_CITIES_REV2022.CSV" # Esse dataset eh o original do kaggle, e nao o alterado

    # # Debugging - imprime os nomes das variaveis do dataset
    # data.table::fread(dataset_caminho) |>
    #     colnames() |>
    #     print()

    # Nomes originais das variaveis selecionadas
    variaveis <- c(
        "GDP", # PIB (2016)
        # "POP_GDP", # Populacao absoluta do municipio (2016)
        "COMP_TOT", # Quantidade de empresas (2016)
        "IDHM", # IDH do municipio (2010)
        "IDHM_Renda",
        "IDHM_Longevidade",
        "IDHM_Educacao",
        "GVA_AGROPEC",
        "GVA_INDUSTRY",
        "GVA_SERVICES",
        "GVA_PUBLIC",
        "GVA_TOTAL",
        "GDP_CAPITA", # Os valores da coluna GDP_CAPITA vem como strings e multiplicados por 1000, entao eh melhor nao inclui-la
        "IBGE_POP", # Populacao absoluta do municipio (2010)
        "IBGE_1",
        "IBGE_1-4",
        "IBGE_5-9",
        "IBGE_10-14",
        "IBGE_15-59",
        "IBGE_60+",
        "IBGE_DU",
        "IBGE_DU_URBAN",
        "IBGE_DU_RURAL"
    )

    dataset <- dataset_filtrar(
        dataset_caminho, # Caminho do arquivo contendo o dataset
        variaveis # Nomes das variaveis selecionadas
    )

    # Nomes das variaveis que serao usadas para formar os fatores (dataset_filtrar() altera os nomes das variaveis)
    analise_fatorial_variaveis <- c(
        nomes_novos_hashmap[["IBGE_POP"]],
        nomes_novos_hashmap[["IBGE_1"]],
        # nomes_novos_hashmap[["IBGE_1-4"]],
        # nomes_novos_hashmap[["IBGE_5-9"]],
        # nomes_novos_hashmap[["IBGE_10-14"]],
        nomes_novos_hashmap[["IBGE_15-59"]],
        nomes_novos_hashmap[["IBGE_60+"]],
        nomes_novos_hashmap[["IDHM"]],
        nomes_novos_hashmap[["IDHM_Renda"]],
        nomes_novos_hashmap[["IDHM_Longevidade"]],
        nomes_novos_hashmap[["IDHM_Educacao"]],
        nomes_novos_hashmap[["COMP_TOT"]],
        nomes_novos_hashmap[["GDP"]],
        nomes_novos_hashmap[["GDP_CAPITA"]],
        nomes_novos_hashmap[["GVA_TOTAL"]],
        nomes_novos_hashmap[["GVA_PUBLIC"]],
        nomes_novos_hashmap[["GVA_SERVICES"]],
        nomes_novos_hashmap[["GVA_AGROPEC"]],
        nomes_novos_hashmap[["GVA_INDUSTRY"]],
        # nomes_novos_hashmap[["IBGE_DU"]],
        nomes_novos_hashmap[["IBGE_DU_URBAN"]],
        nomes_novos_hashmap[["IBGE_DU_RURAL"]]
    )

    # Realiza a analise fatorial
    fatores_quantidade <- 2L
    analise_fatorial(dataset, analise_fatorial_variaveis, fatores_quantidade)
}

main() # Executa o script
