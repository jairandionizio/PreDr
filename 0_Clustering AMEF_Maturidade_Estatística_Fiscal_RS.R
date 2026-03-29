# 1. Bibliotecas e Preparação----

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

# 2. Normalização (Conforme Lógica Central do AMEF)----
# O pensamento estatístico exige escala comum para clustering
dados_norm <- municipios_rs %>%
  select(consistencia_contabil, tempestividade, transparencia_fiscal) %>%
  scale()

# 3. Execução do Clustering (K-means)----
# Objetivo: Identificar níveis de capacidade institucional [Pseudocódigo AMEF]
set.seed(123)
modelo_km <- kmeans(dados_norm, centers = 3, nstart = 25)

# 4. Integração dos Resultados----
municipios_rs$cluster <- factor(modelo_km$cluster,
                                labels = c("Maturidade Alta", "Maturidade Média", "Maturidade Inicial"))

# 5. Visualização da Prova de Conceito----
ggplot(municipios_rs, aes(x = consistencia_contabil, y = transparencia_fiscal, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(title = "Clustering AMEF: Maturidade Estatística/Fiscal (RS)",
       subtitle = "Base: Indicadores SICONFI (Simulados)",
       x = "Consistência Contábil (%)",
       y = "Índice de Transparência",
       color = "Nível de Maturidade")

