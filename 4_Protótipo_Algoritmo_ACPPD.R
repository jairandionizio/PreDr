# 4. Protótipo Algoritmo ACPPD (Parcerias em Dados)----
# Protótipo de Suporte à Decisão usando o método TOPSIS aplicado a cenários de PPP.
# Função simplificada TOPSIS para avaliação de PPPs [Logic]----
topsis_ppp <- function(matriz_decisao, pesos) {
  # Normalização----
  norm_m <- sweep(matriz_decisao^2, 2, colSums(matriz_decisao^2), "/") %>% sqrt()
  pond_m <- sweep(norm_m, 2, pesos, "*")

  # Soluções Ideais----
  ideal_pos <- apply(pond_m, 2, max)
  ideal_neg <- apply(pond_m, 2, min)

  # Distâncias e Score----
  d_pos <- sqrt(rowSums(sweep(pond_m, 2, ideal_pos, "-")^2))
  d_neg <- sqrt(rowSums(sweep(pond_m, 2, ideal_neg, "-")^2))

  return(d_neg / (d_pos + d_neg))
}

# Cenários: {Custo, Privacidade, Qualidade, Governança}----
cenarios <- matrix(runif(12, 0, 1), nrow=3,
                   dimnames=list(c("Empresa_A", "Empresa_B", "Empresa_C"), NULL))
pesos_criterios <- c(0.2, 0.4, 0.3, 0.1) # Foco em Privacidade conforme LGPD

scores_viabilidade <- topsis_ppp(cenarios, pesos_criterios)
print(sort(scores_viabilidade, decreasing = TRUE))
