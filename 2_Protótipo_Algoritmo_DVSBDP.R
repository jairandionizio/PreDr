# 2. Protótipo Algoritmo DVSBDP (Detecção de Viés)----
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

# 1. Cálculo da Cobertura e Viés----
df_bias <- df_bias %>%
  mutate(taxa_cobertura = contagem_fonte / pop_censo,
         vies = abs(1 - taxa_cobertura))

# 2. Detecção de Anomalias (Isolation Forest) [Logic]----
iforest <- isolationForest$new(sample_size = 30)
iforest$fit(df_bias[, c("taxa_cobertura", "vies")])
df_bias$score_anomalia <- iforest$predict(df_bias[, c("taxa_cobertura", "vies")])$anomaly_score

# Visualização de Seletividade----
ggplot(df_bias, aes(x=categoria, y=vies, fill=score_anomalia)) +
  geom_boxplot() + 
  theme_minimal() + 
  labs(title="Viés de Seletividade por Categoria")
