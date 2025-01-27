# 📊 Métodos Quantitativos para Análise Multivariada

Este repositório contém os códigos desenvolvidos durante a disciplina **Métodos Quantitativos para Análise Multivariada**, da Escola de Artes, Ciências e Humanidades (EACH) na Universidade de São Paulo (USP). O objetivo do curso foi aplicar técnicas estatísticas e de aprendizado de máquina em dados reais para realizar análises multivariadas. 

## 🗂 Estrutura do Repositório

O repositório está organizado de acordo com os principais tópicos abordados durante a disciplina. Abaixo, você encontra uma breve descrição de cada um:

- **ANOVA (Análise de Variância)** 📊: Teste estatístico para comparar as médias de mais de dois grupos e determinar se existe uma diferença significativa entre eles.
- **Regressão Linear Múltipla** 📉: Modelagem para prever uma variável dependente a partir de múltiplas variáveis independentes.
- **Regressão Logística** 🔢: Técnica de modelagem para problemas de classificação, onde a variável dependente é binária.
- **Análise de Clusters** 🧑‍🤝‍🧑: Algoritmos de agrupamento de dados em clusters com base em características semelhantes.
- **Análise Fatorial** 🔬: Técnica usada para reduzir a dimensionalidade dos dados e identificar fatores subjacentes a múltiplas variáveis observadas.

## 🔗 Dataset
Os dados utilizados para essas análises estão disponíveis no [Kaggle: Brazilian Cities Dataset](https://www.kaggle.com/).  
O conjunto de dados contém informações socioeconômicas e demográficas sobre as cidades brasileiras.

## 💡💻 Como Executar

### Pré-requisitos

Para rodar os códigos deste repositório, você precisará de:
- **R Studio**
- **R** (versão 4.x ou superior)
- **Pacotes R**: Os códigos utilizam pacotes específicos para cada tipo de análise. Para instalar os pacotes necessários, execute o seguinte código no R:

```r
install.packages(c("ggplot2", "dplyr", "caret", "MASS", "factoextra", "cluster"))
