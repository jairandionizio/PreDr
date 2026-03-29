# O código abaixo simula a detecção de viés em municípios gaúchos, 
# comparando a cobertura da fonte administrativa em relação à população
#  real estimada pelo Censo.

# 1. Bibliotecas e Simulação de Ingestão----
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

# 2. Lógica Central: Diagnóstico de Cobertura e Viés----
dados_cobertura <- dados_cobertura %>%
  mutate(taxa_cobertura = vagas_caged / pop_pea_ibge,
         vies = abs(1 - taxa_cobertura))

# 3. Classificação de Aptidão da Fonte (Relatório Automatizado)----
# Define como 'Inapta' fontes com desvio de cobertura acima de 2 desvios padrão
limiar_alerta <- mean(dados_cobertura$taxa_cobertura) + 2 * sd(dados_cobertura$taxa_cobertura)

dados_cobertura <- dados_cobertura %>%
  mutate(status_fonte = ifelse(taxa_cobertura > limiar_alerta,
                               "Inapta (Viés de Seletividade Alto)", "Apta"))

# 4. Saída Visual: Mapa de Calor de Representatividade----
ggplot(dados_cobertura, aes(x = pop_pea_ibge, y = taxa_cobertura, color = status_fonte)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = mean(dados_cobertura$taxa_cobertura), linetype="dashed") +
  theme_minimal() +
  labs(title = "DVSBDP: Teste de Cobertura Populacional (CAGED vs IBGE PEA)",
       subtitle = "Quantificação de Viés e Seletividade por Município",
       x = "População PEA (IBGE)",
       y = "Taxa de Cobertura (Registros CAGED / PEA)",
       color = "Diagnóstico da Fonte")
