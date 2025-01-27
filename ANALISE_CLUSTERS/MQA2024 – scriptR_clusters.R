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
    coeficiente_de_variacao <- (desvio_padrao / mean(dados[[variavel]], na.rm = TRUE)) * 100L

    matrix(
        c(
            media = media,
            moda = moda_valor,
            mediana = quartis[[3L]], # como o 1o valor de quartis corresponde ao valor minimo, entao a mediana corresponde ao 3o valor de quartis em vez do 2o nesse caso
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
        formatC(digits = 3L, format = "g") |> # remover casas decimais excessivas
        as.table()
}

# salva as medidas de tendencia central, posicao e dispercao de cada variavel em uma tabela
# contida em um arquivo png e gera e salva os graficos (histograma, boxplot e grafico de
# setores) em arquivos png
estatistica_descritiva_realizar <- function(dataset) {
    cat("\n\n\t-----(INICIO - Estatistica descritiva das variaveis)-----\n\n")
    variaveis_qualitativas <- c() # nomes de todas as variaveis qualitativas
    for (nome in colnames(dataset)) {
        cat("\n\n\t\t--(INICIO - Estatistica descritiva da variavel", nome, ")--\n\n")

        if (typeof(dataset[[nome]]) == "character") {
            dataset[[nome]] <- as.factor(dataset[[nome]])
        }

        estatisticas <- medidas_estatisticas_calcular(dataset, nome)
        png(
            filename = paste0("estatisticas_", nome, ".png"),
            width = 400L * ncol(estatisticas) + 10L * nchar(nome),
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
        cat("\n\n\t\t--(FIM - Estatistica descritiva da variavel", nome, ")--\n\n")
    }
    cat("\n\n\t-----(FIM - Estatistica descritiva das variaveis)-----\n\n")
}

# remove simbolos que confundem o R dos nomes das variaveis
nomes_remover_simbolos_proibidos <- function(nomes) {
    gsub("_$", "", x = nomes, fixed = TRUE)
}

# filtra, renomeia e transforma as variaveis do dataset que serao utilizadas
dataset_filtrar <- function(arquivo_caminho, variaveis) {
    cat("\n\n=====(INICIO - Filtrando dataset)=====\n\n")

    # dataset original
    dataset_1 <- arquivo_caminho |>
        data.table::fread(file = _) |>
        dplyr::select(tidyselect::all_of(variaveis)) # remove todas as variaveis que nao serao usadas

    observacoes_iniciais_quantidade <- nrow(dataset_1)

    # dataset sem linhas com algum valor nulo, linhas repetidas e simbolos
    # proibidos nos nomes das variaveis
    dataset_2 <- dataset_1 |>
        na.omit() |> # remove todas as linhas com pelo menos um valor NA
        unique() |> # remove todas as linhas repetidas
        colnames() |>
        nomes_remover_simbolos_proibidos() |> # remove todos os simbolos que confundem o R dos nomes das variaveis
        setNames(object = dataset_1) # remove todos os simbolos que podem confundir o R dos nomes das variaveis

    # # debugging - 10 primeiras linhas do dataset
    # dataset_2 |>
    #     head(n = 10L) |>
    #     print()

    # # debugging - nomes, media e desvio padrao das variaveis
    # for (nome in colnames(dataset_2)) {
    #     print(nome)
    #     print(mean(dataset_2[[nome]]))
    #     print(sd(dataset_2[[nome]]))
    # }

    transformacao_logaritmica_alvos <- c(
        "EMPRESAS_QUANTIDADE",
        "PIB",
        "PIB_PER_CAPITA"
    ) # nomes das variaveis que passarao por uma transformacao logaritmica

    # dataset com variaveis renomeadas para nomes mais amigaveis e transformadas
    dataset_3 <- dataset_2 |>
        dplyr::filter(
            dplyr::if_all(
                tidyselect::where(is.numeric),
                function(vetor) {
                    vetor > 0.0
                }
            )
        ) |> # remove todas as linhas com valores quantitativos zerados para uma ou mais das variaveis
        dplyr::rename(POPULACAO = POP_GDP) |> # renomeia a variavel POP_GDP para POPULACAO
        dplyr::rename(PIB = GDP) |> # renomeia a variavel GDP para PIB
        dplyr::rename(EMPRESAS_QUANTIDADE = COMP_TOT) |> # renomeia a variavel COMP_TOT para EMPRESAS_QUANTIDADE
        dplyr::mutate(PIB_PER_CAPITA = PIB / POPULACAO) |> # cria a variavel PIB_PER_CAPITA
        dplyr::select(!tidyselect::all_of(c( # remove as variaveis especificadas
            "POPULACAO"
            # "IDHM" # O IDH municipal pode ser usado como variavel de validacao
        ))) |>
        dplyr::mutate(
            dplyr::across(
                tidyselect::matches(transformacao_logaritmica_alvos),
                function(vetor) {
                    log(vetor)
                }
            )
        ) # realiza a transformacao logaritmica das variaveis especificadas

    # # debugging - 10 primeiras linhas do dataset
    # dataset_3 |>
    #     head(n = 10L) |>
    #     print()

    # # debugging - 10 linhas com maiores valores de PIB_PER_CAPITA
    # dataset_3[order(-EMPRESAS_POR_HABITANTE), ] |>
    #     head(x = 10L) |>
    #     print()

    # # debugging - arquivo csv com o dataset
    # data.table::fwrite(dataset_3, file = "dataset_3.csv")

    # faz a estatistica descritiva dos dados antes da padronizacao
    estatistica_descritiva_realizar(dataset_3) # eh bom comentar esta linha apos a primeira execucao para reduzir o tempo de execucao do script se nenhum dos valores das variaveis tiver sido alterado desde a ultima execucao

    # dataset com valores padronizados em todas as variaveis quantitativas
    dataset_4 <- dplyr::mutate(
        dataset_3,
        dplyr::across(
            tidyselect::where(is.numeric),
            function(vetor) {
                scale(vetor) # padroniza as variaveis usando o metodo Z-score
            }
        )
    )

    # # debugging - 10 primeiras linhas do dataset
    # dataset_4 |>
    #     head(n = 10L) |>
    #     print()

    # # debugging - nomes, media e desvio padrao das variaveis
    # for (nome in colnames(dataset_4)) {
    #     print(nome)
    #     if (is.numeric(dataset_4[[nome]])) {
    #         print(mean(dataset_4[[nome]]))
    #         print(sd(dataset_4[[nome]]))
    #     }
    # }

    observacoes_finais_quantidade <- nrow(dataset_4)

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
    dataset_4
}

# realiza uma analise de clusters usando os dados das variaveis especificadas
# contidos no dataset fornecido
analise_de_clusters <- function(dataset_1, variaveis) {
    paste0("\n\n=====(INICIO - Analise de clusters para as variaveis: ", toString(variaveis), ")=====\n\n") |>
        cat()

    # dataset com apenas as variaveis usadas para formar os clusters
    dataset_2 <- dataset_1 |>
        dplyr::select(tidyselect::all_of(variaveis)) # apenas as variaveis especificadas serao usadas para a formacao de clusters

    matriz_de_distancias <- dist(dataset_2) # calcula a matriz de distancias

    cat("\n\n\t-----(INICIO - Graficos auxiliares)-----\n\n")
    dataset_2 |>
        factoextra::fviz_nbclust(
            kmeans,
            method = "wss", # metodo elbow
            diss = matriz_de_distancias
        ) |> # gera um grafico que ajuda a escolher o numero ideal de clusters para o k-means
        ggplot2::ggsave(plot = _, filename = "elbow.png", dpi = "retina")

    # dataset_2 |>
    #     factoextra::fviz_nbclust(
    #         kmeans,
    #         method = "silhouette", # metodo silhouette
    #         diss = matriz_de_distancias
    #     ) |> # gera um grafico que ajuda a escolher o numero ideal de clusters para o k-means
    #     ggplot2::ggsave(plot = _, filename = "silhouette.png", dpi = "retina")
    cat("\n\n\t-----(FIM - Graficos auxiliares)-----\n\n")

    cat("\n\n\t-----(INICIO - Clusterizacao hierarquica)-----\n\n")
    clusters_quantidade <- 4L
    hierarquico_metodo <- "ward.D2"
    hierarquico_clusterizacao <- hclust(matriz_de_distancias, method = hierarquico_metodo) # realiza a clusterizacao hierarquica
    dendrograma <- as.dendrogram(hierarquico_clusterizacao) # gera um objeto dendrograma a partir da clusterizacao hierarquica

    # INICIO - dendrograma
    png(paste0("dendrograma_", hierarquico_metodo, ".png"))
    dendrograma |>
        dendextend::color_branches(k = clusters_quantidade, col = 2L:(clusters_quantidade + 1L)) |> # colora os ramos do dendrograma para facilitar a visualizacao dos clusters
        dendextend::set("labels_col", "white") |> # remove o texto incompreensivel e feio da parte de baixo do dendrograma
        plot() # plota o dendrograma (sem os retangulos em volta dos clusters selecionados)

    rect.hclust(
        hierarquico_clusterizacao,
        k = clusters_quantidade,
        border = 2L:(clusters_quantidade + 1L)
    ) # adiciona retangulos em volta dos clusters que serao usados na analise
    dev.off()
    # FIM - dendrograma

    hierarquico_clusters <- hierarquico_clusterizacao |>
        cutree(k = clusters_quantidade) |> # agrupa todas as observacoes em 5 clusters de acordo com a clusterizacao hierarquica
        as.factor()
    factoextra::fviz_cluster(
        list(
            data = dataset_2,
            cluster = hierarquico_clusters
        ),
        geom = "point",
        main = paste0("Clusterização ", hierarquico_metodo)
    ) |>
        ggplot2::ggsave(
            plot = _,
            filename = "hierarquico_clusters.png",
            dpi = "retina",
            units = "cm",
            width = 17L,
            height = 10L
        )
    cat("\n\n\t-----(FIM - Clusterizacao hierarquica)-----\n\n")

    cat("\n\n\t-----(INICIO - Clusterizacao k-means)-----\n\n")
    kmeans_clusterizacao <- kmeans(dataset_2, centers = clusters_quantidade)

    kmeans_clusterizacao |>
        factoextra::fviz_cluster(
            data = dataset_2,
            geom = "point",
            main = "Clusterização k-means"
        ) |>
        ggplot2::ggsave(
            plot = _,
            filename = "kmeans_clusters.png",
            dpi = "retina",
            units = "cm",
            width = 17L,
            height = 10L
        )
    cat("\n\n\t-----(FIM - Clusterizacao k-means)-----\n\n")

    cat("\n\n\t-----(INICIO - Analise dos resultados)-----\n\n")

    cat("\n\n\t\t--(INICIO - Tamanho dos clusters (Clusterizacao hierarquica))--\n\n")
    # imprime uma tabela com a quantidade de observacoes em cada cluster de acordo com o metodo hierarquico
    hierarquico_clusters |>
        table() |>
        print()
    cat("\n\n\t\t--(FIM - Tamanho dos clusters (Clusterizacao hierarquica))--\n\n")

    cat("\n\n\t\t--(INICIO - Tamanho dos clusters (Clusterizacao nao-hierarquica))--\n\n")
    # imprime uma tabela com a quantidade de observacoes em cada cluster de acordo com o metodo k-means
    as.table(matrix(
        c(
            1L:clusters_quantidade,
            kmeans_clusterizacao[["size"]]
        ),
        byrow = TRUE,
        nrow = 2L,
        ncol = clusters_quantidade,
        dimnames = list(c("Cluster", "Tamanho"), NULL)
    )) |>
        print()
    cat("\n\n\t\t--(FIM - Tamanho dos clusters (Clusterizacao nao-hierarquica))--\n\n")

    cat("\n\n\t\t--(INICIO - Centroides dos clusters (Clusterizacao hierarquica))--\n\n")
    hierarquico_centroides <- aggregate(
        dataset_1,
        by = list(hierarquico_clusters),
        FUN = mean
    ) # calcula os centroides dos clusters
    print(hierarquico_centroides, digits = 3L)
    cat("\n\n\t\t--(FIM - Centroides dos clusters (Clusterizacao hierarquica))--\n\n")

    cat("\n\n\t\t--(INICIO - Centroides dos clusters (Clusterizacao nao-hierarquica))--\n\n")
    kmeans_centroides <- aggregate(
        dataset_1,
        by = kmeans_clusterizacao[["cluster"]] |>
            as.factor() |>
            list(),
        FUN = mean
    ) # calcula os centroides dos clusters
    print(kmeans_centroides, digits = 3L)
    cat("\n\n\t\t--(FIM - Centroides dos clusters (Clusterizacao nao-hierarquica))--\n\n")

    print(kmeans_clusterizacao, digits = 3L) # imprime os resultados do k-means

    hierarquico_dataset <- dplyr::mutate(dataset_1, cluster = as.factor(hierarquico_clusters)) # adiciona uma coluna com os clusters de cada municipio de acordo com o metodo hierarquico
    kmeans_dataset <- dplyr::mutate(dataset_1, cluster = as.factor(kmeans_clusterizacao[["cluster"]])) # adiciona uma coluna com os clusters de cada municipio de acordo com o metodo hierarquico

    # gera graficos dos clusters para todos os pares de variaveis usadas na clusterizacao
    cat("\n\n\t\t--(INICIO - Graficos 2D dos clusters)--\n\n")
    if (length(variaveis) > 2L) {
        variaveis_pares <- combn(variaveis, 2L) # todas as combinacoes de variaveis usadas na clusterizacao tomadas de 2 em 2

        for (par_indice in seq_len(ncol(variaveis_pares))) {
            ggpubr::ggscatter(
                hierarquico_dataset,
                x = variaveis_pares[1L, par_indice],
                y = variaveis_pares[2L, par_indice],
                color = "cluster",
                ellipse = TRUE,
                ellipse.type = "convex"
            ) |>
                ggplot2::ggsave(
                    filename = paste0("hierarquico_", variaveis_pares[1L, par_indice], "_vs_", variaveis_pares[2L, par_indice], ".png"),
                    plot = _,
                    dpi = "retina",
                    units = "cm",
                    width = 17L,
                    height = 10L
                )
        }

        for (par_indice in seq_len(ncol(variaveis_pares))) {
            ggpubr::ggscatter(
                kmeans_dataset,
                x = variaveis_pares[1L, par_indice],
                y = variaveis_pares[2L, par_indice],
                color = "cluster",
                ellipse = TRUE,
                ellipse.type = "convex"
            ) |>
                ggplot2::ggsave(
                    filename = paste0("kmeans_", variaveis_pares[1L, par_indice], "_vs_", variaveis_pares[2L, par_indice], ".png"),
                    plot = _,
                    dpi = "retina",
                    units = "cm",
                    width = 17L,
                    height = 10L
                )
        }
    }
    cat("\n\n\t\t--(FIM - Graficos 2D dos clusters)--\n\n")

    # gera boxplots de IDH para cada cluster
    cat("\n\n\t\t--(INICIO - Boxplots de IDH)--\n\n")
    boxplot_idhm_hierarquico <- ggplot2::ggplot(data = hierarquico_dataset) +
        ggplot2::geom_boxplot(ggplot2::aes(x = reorder(cluster, IDHM), y = IDHM, color = cluster)) +
        ggplot2::labs(
            title = "Boxplots de IDH para os diferentes clusters do método hierárquico",
            x = "Cluster",
            y = "IDH"
        )
    ggplot2::ggsave(
        plot = boxplot_idhm_hierarquico,
        filename = "hierarquico_boxplot.png",
        dpi = "retina",
        width = 8L,
        height = 7L
    )

    boxplot_idhm_kmeans <- ggplot2::ggplot(data = kmeans_dataset) +
        ggplot2::geom_boxplot(ggplot2::aes(x = reorder(cluster, IDHM), y = IDHM, color = cluster)) +
        ggplot2::labs(
            title = "Boxplots de IDH para os diferentes clusters do método não-hierárquico",
            x = "Cluster",
            y = "IDH"
        )
    ggplot2::ggsave(
        plot = boxplot_idhm_kmeans,
        filename = "kmeans_boxplot.png",
        dpi = "retina",
        width = 8L,
        height = 7L
    )
    cat("\n\n\t\t--(FIM - Boxplots de IDH)--\n\n")

    cat("\n\n\t\t--(INICIO - ANOVA IDHM ~ cluster (metodo hierarquico))--\n\n")
    hierarquico_anova <- aov(formula = IDHM ~ cluster, data = hierarquico_dataset) |>
        summary()
    print(hierarquico_anova, digits = 3L)
    cat("\n\n\t\t--(FIM - ANOVA IDHM ~ cluster (metodo hierarquico))--\n\n")

    cat("\n\n\t\t--(INICIO - ANOVA IDHM ~ cluster (metodo nao-hierarquico))--\n\n")
    kmeans_anova <- aov(formula = IDHM ~ cluster, data = kmeans_dataset) |>
        summary()
    print(kmeans_anova, digits = 3L)
    cat("\n\n\t\t--(FIM - ANOVA IDHM ~ cluster (metodo nao-hierarquico))--\n\n")

    cat("\n\n\t-----(FIM - Analise dos resultados)-----\n\n")

    paste0("\n\n=====(FIM - Analise de clusters para as variaveis: ", toString(variaveis), ")=====\n\n") |>
        cat()
}

# realiza um exemplo de analise de clusters
exemplo_de_analise_de_clusters <- function() {
    cat("\n\n=====(INICIO - Exemplo de analise de clusters)=====\n\n")
    # dados de exemplo
    dataset_1 <- data.frame(
        Empresa = c("Empresa 1", "Empresa 2", "Empresa 3", "Empresa 4", "Empresa 5", "Empresa 6"),
        Vendas = c(500L, 300L, 800L, 400L, 900L, 700L),
        Empregados = c(50L, 20L, 60L, 30L, 70L, 55L),
        stringsAsFactors = TRUE
    )
    print(dataset_1, digits = 3L)

    cat("\n\n")

    # calcula as medias e desvios padrao das variaveis "Vendas" e "Empregados"
    media_e_desvio_padrao <- as.table(matrix(
        c(
            c(mean(dataset_1[["Vendas"]]), sd(dataset_1[["Vendas"]])),
            c(mean(dataset_1[["Empregados"]]), sd(dataset_1[["Empregados"]]))
        ),
        byrow = TRUE,
        nrow = 2L,
        dimnames = list(
            c("Vendas", "Empregados"),
            c("Média", "Desvio Padrão")
        )
    ))
    print(media_e_desvio_padrao, digits = 3L)

    cat("\n\n")

    # dataset com valores padronizados em todas as variaveis quantitativas
    dataset_2 <- dataset_1 |>
        dplyr::select(!tidyselect::matches("Empresa")) |>
        dplyr::mutate(
            dplyr::across(
                tidyselect::where(is.numeric),
                function(vetor) {
                    scale(vetor) # padroniza as variaveis usando o metodo Z-score
                }
            )
        )
    print(dataset_2, digits = 3L)

    cat("\n\n")

    matriz_de_distancias <- dist(dataset_2) # calcula a matriz de distancias
    print(matriz_de_distancias)

    cat("\n\n")

    dataset_2 |>
        factoextra::fviz_nbclust(
            kmeans,
            method = "wss", # metodo elbow
            diss = matriz_de_distancias,
            k.max = nrow(dataset_2) - 1L
        ) |> # gera um grafico que ajuda a escolher o numero ideal de clusters para o k-means
        ggplot2::ggsave(plot = _, filename = "exemplo_elbow.png", dpi = "retina")

    cat("\n\n")

    hierarquico_metodo <- "ward.D2"
    clusterizacao_hierarquica <- hclust(matriz_de_distancias, method = hierarquico_metodo) # realiza a clusterizacao hierarquica
    dendrograma <- as.dendrogram(clusterizacao_hierarquica) # gera um objeto dendrograma a partir da clusterizacao hierarquica

    # INICIO - dendrograma
    clusters_quantidade <- 2L
    png(paste0("exemplo_dendrograma_", hierarquico_metodo, ".png"))
    dendrograma |>
        dendextend::color_branches(k = clusters_quantidade, col = 2L:(clusters_quantidade + 1L)) |> # colora os ramos do dendrograma para facilitar a visualizacao dos clusters
        dendextend::color_labels(k = clusters_quantidade, col = 2L:(clusters_quantidade + 1L)) |> # remove o texto incompreensivel e feio da parte de baixo do dendrograma
        plot() # plota o dendrograma (sem os retangulos em volta dos clusters selecionados)
    dev.off()
    # FIM - dendrograma

    hierarquico_clusters <- cutree(clusterizacao_hierarquica, k = clusters_quantidade) # agrupa todas as observacoes em 5 clusters de acordo com a clusterizacao hierarquica
    factoextra::fviz_cluster(
        list(
            data = dataset_2,
            cluster = hierarquico_clusters
        ),
        geom = "point",
        main = paste0("Clusterização ", hierarquico_metodo)
    ) |>
        ggplot2::ggsave(
            plot = _,
            filename = "exemplo_hierarquico_clusters.png",
            dpi = "retina"
        )
    cat("\n\n=====(FIM - Exemplo de analise de clusters)=====\n\n")
}

# ponto de entrada do script de analise de clusters
main <- function() {
    pacotes_utilizados <- c(
        "data.table",
        "dplyr",
        "gridExtra",
        "ggplot2",
        "tidyselect",
        "factoextra",
        "dendextend",
        "cluster",
        "ggpubr"
    ) # nomes de todos os pacotes nao pre-instalados utilizados no script

    # instala todos os pacotes que sao necessarios, mas nao estao instalados
    install.packages(
        setdiff(pacotes_utilizados, rownames(installed.packages()))
        # , repos = "https://vps.fmvz.usp.br/CRAN/" # descomentar esta linha para especificar um repositorio da CRAN
        # , Ncpus = 12L # descomentar esta linha para especificar a quantidade de threads da CPU os quais serao utilizados para instalar os pacotes
    )

    # executa o exemplo de analise de clusters
    exemplo_de_analise_de_clusters() # comentar esta linha apos a primeira execucao para reduzir o tempo de execucao do script

    # substitua esta string pelo caminho correto
    dataset_caminho <- "BRAZIL_CITIES_REV2022 - BRAZIL_CITIES_REV2022.CSV" # esse dataset eh o original do kaggle, e nao o alterado

    # nomes originais das variaveis selecionadas
    variaveis <- c(
        "GDP", # PIB (2016)
        "POP_GDP", # populacao absoluta do municipio (2016)
        "COMP_TOT", # quantidade de empresas (2016)
        "IDHM" # IDH do municipio (2010)
    )

    dataset <- dataset_filtrar(
        dataset_caminho, # caminho do arquivo contendo o dataset
        variaveis # nomes das variaveis selecionadas
    )

    # nomes das variaveis que serao usadas para formar os clusters (dataset_filtrar() altera os nomes das variaveis)
    clusters_variaveis <- c(
        "EMPRESAS_QUANTIDADE",
        "PIB",
        "PIB_PER_CAPITA"
    )

    # realiza a analise de clusters
    analise_de_clusters(dataset, clusters_variaveis)
}

main() # executa o script
