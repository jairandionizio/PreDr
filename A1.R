# 1. Bibliotecas e Preparação

install.packages("remotes")
remotes::install_github("aspeddro/siconfir")

install.packages("tidyverse")
install.packages("readxl")
install.packages("knitr")

suppressPackageStartupMessages({
  library(siconfir)
  library(tidyverse)
  library(readxl)
  library(knitr)
  library(dplyr)
  library(stats)
  library(ggplot2)
})

# Simulação de dados do SICONFI (Ranking de Qualidade 2023) para 497 municípios do RS
# Indicadores: I1 (Consistência), I2 (Prazos), I3 (Intersetorialidade)
set.seed(42)
municipios_rs <- data.frame(
  ibge_code = 4300000 + sample(1:20000, 497),
  municipio = paste0("Município_", 1:497),
  consistencia_contabil = runif(497, 60, 100), # % de acerto no SICONFI
  tempestividade = runif(497, 50, 100),       # Entrega de relatórios no prazo
  transparencia_fiscal = runif(497, 40, 95)
)

# 2. Normalização (Conforme Lógica Central do AMEF)
# O pensamento estatístico exige escala comum para clustering
dados_norm <- municipios_rs %>%
  select(consistencia_contabil, tempestividade, transparencia_fiscal) %>%
  scale()

# 3. Execução do Clustering (K-means)
# Objetivo: Identificar níveis de capacidade institucional [Pseudocódigo AMEF]
set.seed(123)
modelo_km <- kmeans(dados_norm, centers = 3, nstart = 25)

# 4. Integração dos Resultados
municipios_rs$cluster <- factor(modelo_km$cluster,
                                labels = c("Maturidade Alta", "Maturidade Média", "Maturidade Inicial"))

# 5. Visualização da Prova de Conceito
ggplot(municipios_rs, aes(x = consistencia_contabil, y = transparencia_fiscal, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(title = "Clustering AMEF: Maturidade Estatística/Fiscal (RS)",
       subtitle = "Base: Indicadores SICONFI (Simulados)",
       x = "Consistência Contábil (%)",
       y = "Índice de Transparência",
       color = "Nível de Maturidade")

#################################################
# 1. Protótipo Algoritmo AMEF (Maturidade Estatística)
# Este protótipo foca na lógica de Scoring Multicritério e Clustering.
# Bibliotecas necessárias
install.packages("glmnet")
install.packages("stats")
install.packages("dplyr")
library(glmnet)  # Regressão Penalizada (Lasso/Ridge)
library(stats)   # K-means
library(dplyr)

# Exemplo: Dados fictícios representando indicadores do SICONFI e IBGE
dados_entes <- data.frame(
  ente = paste0("Município_", 1:100),
  infra_ti = runif(100, 0, 1),
  transparencia = runif(100, 0, 1),
  qualidade_registro = runif(100, 0, 1),
  uso_planejamento = runif(100, 0, 1),
  proxy_performance = rnorm(100, 5, 2) # Variável alvo para Lasso
)

# 1. Aprendizado de Pesos via Lasso [Logic]
x <- as.matrix(dados_entes[, 2:5])
y <- dados_entes$proxy_performance
modelo_lasso <- cv.glmnet(x, y, alpha = 1)
pesos <- as.numeric(coef(modelo_lasso, s = "lambda.min")[-1])

# 2. Cálculo do Índice de Maturidade Estatística (IME)
dados_entes <- dados_entes %>%
  mutate(IME = as.matrix(select(., 2:5)) %*% pesos)

# 3. Agrupamento de Maturidade (Clusters)
set.seed(123)
clusters <- kmeans(dados_entes$IME, centers = 3)
dados_entes$nivel <- factor(clusters$cluster, labels = c("Iniciante", "Intermediário", "Avançado"))

print(head(dados_entes))

############################################

# 2. Protótipo Algoritmo DVSBDP (Detecção de Viés)
# Focado na quantificação de viés de seletividade comparando fontes administrativas com denominadores censitários.
install.packages("solitude")
install.packages("ggplot2")

library(solitude) # Isolation Forest para anomalias
library(ggplot2)

# Simulação: Comparação entre CAGED (Administrativo) e CENSO (População)
df_bias <- data.frame(
  categoria = rep(c("Jovens", "Adultos", "Idosos"), each = 10),
  contagem_fonte = c(rnorm(10, 100, 10), rnorm(10, 500, 20), rnorm(10, 50, 5)),
  pop_censo = c(rep(150, 10), rep(550, 10), rep(100, 10))
)

# 1. Cálculo da Cobertura e Viés
df_bias <- df_bias %>%
  mutate(taxa_cobertura = contagem_fonte / pop_censo,
         vies = abs(1 - taxa_cobertura))

# 2. Detecção de Anomalias (Isolation Forest) [Logic]
iforest <- isolationForest$new(sample_size = 30)
iforest$fit(df_bias[, c("taxa_cobertura", "vies")])
df_bias$score_anomalia <- iforest$predict(df_bias[, c("taxa_cobertura", "vies")])$anomaly_score

# Visualização de Seletividade
ggplot(df_bias, aes(x=categoria, y=vies, fill=score_anomalia)) +
  geom_boxplot() + theme_minimal() + labs(title="Viés de Seletividade por Categoria")

############################################

# 3. Protótipo Algoritmo GAIP (Geração de Indicadores)
# Implementa a PCA (Redução de Dimensionalidade) e controle de qualidade via Winsorização.
library(DescTools)  # Winsorização
library(dplyr)      # mutate, across, pipe
library(stats)      # PCA

# Reprodutibilidade
set.seed(42)

# Base simulada com outliers
dados_brutos <- data.frame(
  municipio   = 1:50,
  gasto_saude = c(runif(48, 100, 500), 5000, 2),  # Outliers incluídos
  leitos_hab  = runif(50, 1, 10)
)

# --------------------------------------------------
# Função de Winsorização manual
winsorize_manual <- function(x, lower = 0.05, upper = 0.95) {
  q <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

# 1. Tratamento de Outliers
dados_limpos <- dados_brutos %>%
  mutate(across(c(gasto_saude, leitos_hab),
                ~ winsorize_manual(.x, lower = 0.05, upper = 0.95)))
# --------------------------------------------------
# 2. Síntese via PCA
# --------------------------------------------------
pca_res <- prcomp(dados_limpos[, 2:3], scale. = TRUE)

# Diagnóstico: variância explicada pela PC1
cat("=== Variância Explicada ===\n")
print(summary(pca_res))

# Diagnóstico: direção dos loadings
cat("\n=== Loadings da PC1 ===\n")
print(pca_res$rotation[, 1])

# Ajuste de sinal — garante que maiores scores = melhor desempenho
sinal <- sign(mean(pca_res$rotation[, 1]))
dados_limpos$indicador_sintetico <- sinal * pca_res$x[, 1]

# --------------------------------------------------
# 3. Normalização Min-Max (escala 0–100)
# --------------------------------------------------
dados_limpos <- dados_limpos %>%
  mutate(IC_Final = (indicador_sintetico - min(indicador_sintetico)) /
                    (max(indicador_sintetico) - min(indicador_sintetico)) * 100)

# --------------------------------------------------
# Resultado final
# --------------------------------------------------
cat("\n=== Resumo do Indicador Final ===\n")
print(summary(dados_limpos$IC_Final))

################################

# 4. Protótipo Algoritmo ACPPD (Parcerias em Dados)
# Protótipo de Suporte à Decisão usando o método TOPSIS aplicado a cenários de PPP.
# Função simplificada TOPSIS para avaliação de PPPs [Logic]
topsis_ppp <- function(matriz_decisao, pesos) {
  # Normalização
  norm_m <- sweep(matriz_decisao^2, 2, colSums(matriz_decisao^2), "/") %>% sqrt()
  pond_m <- sweep(norm_m, 2, pesos, "*")

  # Soluções Ideais
  ideal_pos <- apply(pond_m, 2, max)
  ideal_neg <- apply(pond_m, 2, min)

  # Distâncias e Score
  d_pos <- sqrt(rowSums(sweep(pond_m, 2, ideal_pos, "-")^2))
  d_neg <- sqrt(rowSums(sweep(pond_m, 2, ideal_neg, "-")^2))

  return(d_neg / (d_pos + d_neg))
}

# Cenários: {Custo, Privacidade, Qualidade, Governança}
cenarios <- matrix(runif(12, 0, 1), nrow=3,
                   dimnames=list(c("Empresa_A", "Empresa_B", "Empresa_C"), NULL))
pesos_criterios <- c(0.2, 0.4, 0.3, 0.1) # Foco em Privacidade conforme LGPD

scores_viabilidade <- topsis_ppp(cenarios, pesos_criterios)
print(sort(scores_viabilidade, decreasing = TRUE))

#####################################

# O código abaixo simula a detecção de viés em municípios gaúchos, 
# comparando a cobertura da fonte administrativa em relação à população
#  real estimada pelo Censo.

# 1. Bibliotecas e Simulação de Ingestão
library(dplyr)
library(ggplot2)

# Simulação de dados para os 497 municípios do RS
set.seed(123)
dados_cobertura <- data.frame(
  cod_ibge = 4300000 + sample(1:20000, 497),
  municipio = paste0("Município_", 1:497),
  pop_pea_ibge = round(runif(497, 2000, 800000)) # Denominador Censitário
)

# Simulando microdados do CAGED com viés de seletividade
# (O CAGED cobre apenas emprego formal, gerando sub-representação natural)
dados_cobertura <- dados_cobertura %>%
  mutate(vagas_caged = round(pop_pea_ibge * runif(497, 0.2, 0.6)),
         # Inserindo anomalias propositais (erros de registro ou cobertura extrema)
         vagas_caged = ifelse(row_number() %in% c(10, 50, 100),
                              vagas_caged * 1.8, vagas_caged))

# 2. Lógica Central: Diagnóstico de Cobertura e Viés
dados_cobertura <- dados_cobertura %>%
  mutate(taxa_cobertura = vagas_caged / pop_pea_ibge,
         vies = abs(1 - taxa_cobertura))

# 3. Classificação de Aptidão da Fonte (Relatório Automatizado)
# Define como 'Inapta' fontes com desvio de cobertura acima de 2 desvios padrão
limiar_alerta <- mean(dados_cobertura$taxa_cobertura) + 2 * sd(dados_cobertura$taxa_cobertura)

dados_cobertura <- dados_cobertura %>%
  mutate(status_fonte = ifelse(taxa_cobertura > limiar_alerta,
                               "Inapta (Viés de Seletividade Alto)", "Apta"))

# 4. Saída Visual: Mapa de Calor de Representatividade
ggplot(dados_cobertura, aes(x = pop_pea_ibge, y = taxa_cobertura, color = status_fonte)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = mean(dados_cobertura$taxa_cobertura), linetype="dashed") +
  theme_minimal() +
  labs(title = "DVSBDP: Teste de Cobertura Populacional (CAGED vs IBGE PEA)",
       subtitle = "Quantificação de Viés e Seletividade por Município",
       x = "População PEA (IBGE)",
       y = "Taxa de Cobertura (Registros CAGED / PEA)",
       color = "Diagnóstico da Fonte")

############################################
Exemplo de Estrutura "Vibe Coding" em R Shiny
install.packages("shiny")
install.packages("shinydashboard")
library(shiny)
library(shinydashboard)

# UI: Definindo o 'vibe' do sistema de suporte à decisão
ui <- dashboardPage(
  dashboardHeader(title = "Plataforma de Inteligência em Dados Públicos"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Maturidade (AMEF)", tabName = "amef"),
      menuItem("Auditoria (DVSBDP)", tabName = "dvsbdp"),
      menuItem("Indicadores (GAIP)", tabName = "gaip"),
      menuItem("Risco PPP (ACPPD)", tabName = "acppd")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "amef",
              fluidRow(box(plotOutput("mapa_clusters")), box(plotOutput("shap_explanation")))),
      tabItem(tabName = "dvsbdp",
              fluidRow(infoBoxOutput("status_fonte"), box(plotOutput("heatmap_vies"))))
    )
  )
)

# Server: Lógica dos algoritmos integrados
server <- function(input, output) {
  # Aqui entra a chamada para os protótipos R desenvolvidos anteriormente
}

shinyApp(ui, server)
