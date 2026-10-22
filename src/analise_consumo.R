
# ============================================================
# PROJETO: ANALISE ESTATISTICA DE CONSUMO DE AGUA
# Linguagem: R
# ============================================================

# 1. Configuracao de diretorios
diretorio_projeto <- getwd()

dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/graficos", recursive = TRUE,
           showWarnings = FALSE)

# 2. Geracao de dados simulados
set.seed(42)

datas <- seq.Date(
  from = as.Date("2026-01-01"),
  by = "day",
  length.out = 90
)

fator_dia <- ifelse(
  as.POSIXlt(datas)$wday %in% c(0, 6),
  1.08,
  1
)

consumo <- round(
  pmax(80, rnorm(90, mean = 430, sd = 55) * fator_dia),
  1
)

# Valores inseridos para demonstrar a deteccao de extremos
consumo[c(19, 54, 78)] <- c(690, 245, 720)

dados <- data.frame(
  data = datas,
  consumo_litros = consumo
)

write.csv(
  dados,
  "data/consumo_agua_diario.csv",
  row.names = FALSE
)

# 3. Carregamento e validacao dos dados
dados <- read.csv(
  "data/consumo_agua_diario.csv",
  stringsAsFactors = FALSE
)

dados$data <- as.Date(dados$data)

if (anyNA(dados$data) ||
    anyNA(dados$consumo_litros) ||
    any(dados$consumo_litros < 0)) {
  stop("Existem dados invalidos, ausentes ou negativos.")
}

if (anyDuplicated(dados$data)) {
  stop("Existem datas duplicadas.")
}

# 4. Estatistica descritiva
x <- dados$consumo_litros

q1 <- as.numeric(quantile(x, 0.25))
q3 <- as.numeric(quantile(x, 0.75))
iqr <- IQR(x)

limite_inferior <- q1 - 1.5 * iqr
limite_superior <- q3 + 1.5 * iqr

atipicos <- dados[
  x < limite_inferior | x > limite_superior,
]

estatisticas <- data.frame(
  indicador = c(
    "Numero de dias",
    "Consumo total (litros)",
    "Media diaria (litros)",
    "Mediana (litros)",
    "Desvio-padrao (litros)",
    "Consumo minimo (litros)",
    "Consumo maximo (litros)",
    "Amplitude (litros)",
    "Primeiro quartil",
    "Terceiro quartil",
    "Coeficiente de variacao (%)",
    "Quantidade de dias atipicos"
  ),
  valor = c(
    nrow(dados),
    sum(x),
    mean(x),
    median(x),
    sd(x),
    min(x),
    max(x),
    max(x) - min(x),
    q1,
    q3,
    sd(x) / mean(x) * 100,
    nrow(atipicos)
  )
)

estatisticas$valor <- round(estatisticas$valor, 2)

write.csv(
  estatisticas,
  "outputs/resumo_estatistico.csv",
  row.names = FALSE
)

write.csv(
  atipicos,
  "outputs/dias_atipicos.csv",
  row.names = FALSE
)

# 5. Resumo mensal
dados$mes <- format(dados$data, "%Y-%m")

resumo_mensal <- aggregate(
  consumo_litros ~ mes,
  data = dados,
  FUN = mean
)

names(resumo_mensal)[2] <- "media_diaria_litros"

resumo_mensal$media_diaria_litros <-
  round(resumo_mensal$media_diaria_litros, 2)

write.csv(
  resumo_mensal,
  "outputs/resumo_mensal.csv",
  row.names = FALSE
)

# 6. Grafico de consumo diario
png(
  "outputs/graficos/consumo_diario.png",
  width = 1200,
  height = 650
)

plot(
  dados$data,
  dados$consumo_litros,
  type = "o",
  pch = 16,
  main = "Consumo diario de agua",
  xlab = "Data",
  ylab = "Consumo (litros)"
)

points(
  atipicos$data,
  atipicos$consumo_litros,
  pch = 8,
  cex = 1.5
)

dev.off()

# 7. Histograma
png(
  "outputs/graficos/distribuicao_consumo.png",
  width = 900,
  height = 650
)

hist(
  x,
  breaks = 12,
  main = "Distribuicao do consumo diario",
  xlab = "Consumo (litros)",
  ylab = "Frequencia"
)

dev.off()

# 8. Boxplot
png(
  "outputs/graficos/boxplot_consumo.png",
  width = 900,
  height = 650
)

boxplot(
  x,
  main = "Boxplot do consumo de agua",
  ylab = "Consumo (litros)"
)

dev.off()

# 9. Media diaria por mes
png(
  "outputs/graficos/media_mensal.png",
  width = 900,
  height = 650
)

barplot(
  resumo_mensal$media_diaria_litros,
  names.arg = resumo_mensal$mes,
  main = "Media diaria de consumo por mes",
  xlab = "Mes",
  ylab = "Media (litros/dia)"
)

dev.off()

# 10. Relatorio textual
relatorio <- c(
  "RELATORIO: ANALISE ESTATISTICA DE CONSUMO DE AGUA",
  "=================================================",
  "",
  "Os dados utilizados sao simulados para fins educacionais.",
  paste("Periodo:", min(dados$data), "a", max(dados$data)),
  paste("Dias analisados:", nrow(dados)),
  paste("Consumo total:", round(sum(x), 2), "litros"),
  paste("Media diaria:", round(mean(x), 2), "litros"),
  paste("Mediana:", round(median(x), 2), "litros"),
  paste("Desvio-padrao:", round(sd(x), 2), "litros"),
  paste("Minimo:", min(x), "litros"),
  paste("Maximo:", max(x), "litros"),
  paste("Dias atipicos:", nrow(atipicos)),
  "",
  "METODOLOGIA",
  "Os valores atipicos foram identificados pelo metodo IQR.",
  "Valores atipicos precisam ser investigados e nao indicam",
  "necessariamente erros de medicao."
)

writeLines(
  relatorio,
  "outputs/relatorio.txt"
)

# 11. Resultado no console
cat("\nANALISE CONCLUIDA!\n")
cat("==================\n")
print(estatisticas)
cat("\nResumo mensal:\n")
print(resumo_mensal)
cat("\nArquivos salvos na pasta outputs.\n")
