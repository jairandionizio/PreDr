# 1. Protótipo Algoritmo AMEF (Maturidade Estatística)----
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

# 1. Aprendizado de Pesos via Lasso [Logic]----
x <- as.matrix(dados_entes[, 2:5])
y <- dados_entes$proxy_performance
modelo_lasso <- cv.glmnet(x, y, alpha = 1)
pesos <- as.numeric(coef(modelo_lasso, s = "lambda.min")[-1])

# 2. Cálculo do Índice de Maturidade Estatística (IME)----
dados_entes <- dados_entes %>%
  mutate(IME = as.matrix(select(., 2:5)) %*% pesos)

# 3. Agrupamento de Maturidade (Clusters)----
set.seed(123)
clusters <- kmeans(dados_entes$IME, centers = 3)
dados_entes$nivel <- factor(clusters$cluster, labels = c("Iniciante", "Intermediário", "Avançado"))

print(head(dados_entes))
