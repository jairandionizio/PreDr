# 3. Protótipo Algoritmo GAIP (Geração de Indicadores)----
# Implementa a PCA (Redução de Dimensionalidade) e controle de qualidade via Winsorização.
install.packages("DescTools")
install.packages("dplyr")
install.packages("stats")
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


# Função de Winsorização manual
winsorize_manual <- function(x, lower = 0.05, upper = 0.95) {
  q <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

# 1. Tratamento de Outliers----
dados_limpos <- dados_brutos %>%
  mutate(across(c(gasto_saude, leitos_hab),
                ~ winsorize_manual(.x, lower = 0.05, upper = 0.95)))

# 2. Síntese via PCA----

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

# 3. Normalização Min-Max (escala 0–100)----

dados_limpos <- dados_limpos %>%
  mutate(IC_Final = (indicador_sintetico - min(indicador_sintetico)) /
                    (max(indicador_sintetico) - min(indicador_sintetico)) * 100)


# Resultado final----

cat("\n=== Resumo do Indicador Final ===\n")
print(summary(dados_limpos$IC_Final))
