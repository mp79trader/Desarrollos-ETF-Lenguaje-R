# EstrategiaETF-Optimizada.R
# Estrategia profesional con filtros de riesgo, reglas de salida, pesos dinámicos y benchmark VOO

cat("\n════════════════════════════════════════════════════════════\n")
cat("  🚀 INICIANDO ESTRATEGIA OPTIMIZADA\n")
cat("════════════════════════════════════════════════════════════\n\n")

# 1) Paquetes -----------------------------------------------------------------
req_pkgs <- c("quantmod", "xts", "zoo", "PerformanceAnalytics", "ggplot2", "openxlsx")
for (p in req_pkgs) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE, repos = "https://cloud.r-project.org/")
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  } else {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}

# 2) Parámetros ----------------------------------------------------------------
PARAM <- list(
  start_date = as.Date("2018-01-01"),
  symbols     = c("EZU", "GLD", "LQD", "VOO", "TLT", "UUP", "VNQ", "JNK", "EEM", "VTV", "GSG", "VEA", "XLK", "XLY", "XLV", "XLE", "ITB"),
  benchmark   = "VOO",  # SP500 ETF
  rf_daily    = 0.02/252,
  roll_vol_n  = 21,      # ventana volatilidad (días)
  dd_stop_n   = 63,      # ventana drawdown para stop
  dd_stop_thr = -0.10,   # stop si DD rolling < -10%
  risk_off_vol_annual_thr = 0.25, # 25% anualizada
  rebalance   = "months", # mensual
  cost        = 0.001,    # 10 bps
  max_w       = 0.25,     # peso máximo por activo
  penalize    = c("XLE", "GSG", "ITB"),
  penalize_factor = 0.5,  # -50% del peso asignado
  outdir      = file.path(getwd(), paste0("Resultados_Optimizada/", format(Sys.time(), "%Y%m%d_%H%M")))
)

if (!dir.exists(PARAM$outdir)) dir.create(PARAM$outdir, recursive = TRUE)

# 3) Utilidades ----------------------------------------------------------------
ann_vol <- function(x) sd(x, na.rm = TRUE) * sqrt(252)
sharpe_ann <- function(x, rf = 0) {
  ex <- x - rf
  m <- mean(ex, na.rm = TRUE); s <- sd(ex, na.rm = TRUE)
  if (is.na(m) || is.na(s) || s == 0) return(NA_real_)
  (m/s) * sqrt(252)
}
max_dd <- function(x) {
  eq <- cumprod(1 + x)
  dd <- (eq/ cummax(eq)) - 1
  min(dd, na.rm = TRUE)
}

merge_xts_safe <- function(lst) {
  m <- do.call(merge, lst)
  na.omit(m)
}

# 4) Descarga de datos ---------------------------------------------------------
cat("📥 Descargando datos...\n")
get_one <- function(sym) {
  tryCatch({
    xt <- suppressWarnings(getSymbols(sym, src = "yahoo", from = PARAM$start_date,
                                      to = Sys.Date(), auto.assign = FALSE, warnings = FALSE))
    ret <- dailyReturn(Ad(xt), type = "log")
    colnames(ret) <- sym
    na.omit(ret)
  }, error = function(e) {
    message(sprintf("✗ Error downloading %s: %s", sym, e$message)); NULL
  })
}

rets_list <- lapply(PARAM$symbols, get_one)
rets_list <- Filter(Negate(is.null), rets_list)
if (length(rets_list) == 0) stop("No se pudo descargar ningún símbolo")
R <- merge_xts_safe(rets_list)

# Benchmark
bench <- get_one(PARAM$benchmark)
if (is.null(bench)) stop("No se pudo descargar benchmark")

# Alinear por fechas comunes entre cartera y benchmark
common_idx <- index(na.omit(merge(R, bench)))
R <- R[common_idx]
bench <- bench[common_idx]
R <- na.omit(R)
bench <- na.omit(bench)

cat(sprintf("✅ Datos listos: %d días, %d activos\n", nrow(R), ncol(R)))

# 5) Filtro de riesgo por activo ----------------------------------------------
cat("🧹 Aplicando filtro de riesgo por activo (Sharpe < 0.3 o DD < -35%)...\n")
metrics <- data.frame(
  Symbol = colnames(R),
  Sharpe = apply(R, 2, sharpe_ann, rf = PARAM$rf_daily),
  MaxDrawdown = apply(R, 2, max_dd),
  stringsAsFactors = FALSE
)
keep <- with(metrics, (Sharpe >= 0.3) & (MaxDrawdown >= -0.35))
if (sum(keep) == 0) stop("Filtro eliminó todos los activos. Ajusta umbrales.")
R_f <- R[, keep]
metrics_f <- metrics[keep, ]

cat(sprintf("✅ Activos seleccionados: %d/%d\n", ncol(R_f), ncol(R)))
write.csv(metrics, file.path(PARAM$outdir, "metricas_originales.csv"), row.names = FALSE)
write.csv(metrics_f, file.path(PARAM$outdir, "metricas_filtradas.csv"), row.names = FALSE)

# 6) Pesos dinámicos con vol-scaling + penalizaciones -------------------------
cat("⚖️ Calculando pesos dinámicos (inversa de volatilidad, límites y penalizaciones)...\n")
vol_rolling <- rollapply(R_f, PARAM$roll_vol_n, ann_vol, by.column = TRUE, align = "right")
vol_rolling <- na.locf(vol_rolling, na.rm = FALSE)
vol_rolling <- na.omit(vol_rolling)

# Inversa de la vol
inv_vol <- 1/vol_rolling
inv_vol[!is.finite(inv_vol)] <- NA
inv_vol <- na.locf(inv_vol, na.rm = FALSE)

# Normalizar a 1
row_sums <- rowSums(inv_vol, na.rm = TRUE)
W_raw <- inv_vol / row_sums
W_raw[is.na(W_raw)] <- 0

# Penalizar sectores pesados
pen_cols <- intersect(colnames(W_raw), PARAM$penalize)
if (length(pen_cols) > 0) {
  W_raw[, pen_cols] <- W_raw[, pen_cols] * PARAM$penalize_factor
  row_sums2 <- rowSums(W_raw, na.rm = TRUE)
  W_raw <- W_raw / row_sums2
}

# Limitar a max_w y renormalizar
cap_row <- function(w) {
  w[w > PARAM$max_w] <- PARAM$max_w
  if (sum(w) == 0) return(w)
  w / sum(w)
}
W_capped <- xts(t(apply(W_raw, 1, cap_row)), order.by = index(W_raw))

# Rebalance mensual
ep <- endpoints(W_capped, on = PARAM$rebalance)
W_m <- W_capped[ep, ]
W_m <- na.omit(W_m)

# 7) Reglas de salida (risk management) ---------------------------------------
cat("🛡️ Aplicando reglas de salida (stop DD y risk-off por volatilidad)...\n")
# Señal stop por DD del portafolio (simulando con W_m)
# Para construir cartera, proyectamos pesos mensuales forward-fill
W_daily <- na.locf(W_m, fromLast = FALSE)
W_daily <- W_daily[index(R_f)]

# Retorno cartera base
ret_base <- rowSums(W_daily * R_f)
ret_base <- xts(ret_base, order.by = index(R_f))

# DD rolling para stop
roll_eq <- cumprod(1 + ret_base)
roll_max <- rollapply(roll_eq, PARAM$dd_stop_n, max, align = "right", fill = NA)
dd_roll <- (roll_eq/roll_max) - 1
stop_signal <- ifelse(dd_roll < PARAM$dd_stop_thr, 0.5, 1) # reducir 50%
stop_signal <- na.locf(stop_signal, na.rm = FALSE)
stop_signal[is.na(stop_signal)] <- 1

# Risk-off por volatilidad del benchmark
vol_bench <- runSD(bench, n = PARAM$roll_vol_n) * sqrt(252)
riskoff_signal <- ifelse(vol_bench > PARAM$risk_off_vol_annual_thr, 0.7, 1) # 30% cash
riskoff_signal <- na.locf(riskoff_signal, na.rm = FALSE)
riskoff_signal[is.na(riskoff_signal)] <- 1

# Señal total de exposición
expo <- pmin(stop_signal, riskoff_signal)
expo <- expo[index(R_f)]

# 8) Retorno cartera con costos ------------------------------------------------
shift_w <- lag(W_daily, 1)
shift_w[is.na(shift_w)] <- 0
turnover <- rowSums(abs(W_daily - shift_w), na.rm = TRUE)
costs <- turnover * PARAM$cost

ret_port <- rowSums(W_daily * R_f) * as.numeric(expo) - costs
ret_port <- xts(ret_port, order.by = index(R_f))
ret_port <- na.omit(ret_port)

# 9) Comparación con benchmark -------------------------------------------------
bench_a <- bench[index(ret_port)]
cmp <- na.omit(merge(ret_port, bench_a))
colnames(cmp) <- c("Strategy", "Benchmark")

kpi <- data.frame(
  Metric = c("CAGR", "Vol (%)", "Sharpe", "MaxDD (%)"),
  Strategy = c(
    (tail(cumprod(1 + cmp$Strategy), 1)^(252/nrow(cmp)) - 1),
    ann_vol(cmp$Strategy) * 100,
    sharpe_ann(cmp$Strategy, rf = PARAM$rf_daily),
    max_dd(cmp$Strategy) * 100
  ),
  Benchmark = c(
    (tail(cumprod(1 + cmp$Benchmark), 1)^(252/nrow(cmp)) - 1),
    ann_vol(cmp$Benchmark) * 100,
    sharpe_ann(cmp$Benchmark, rf = PARAM$rf_daily),
    max_dd(cmp$Benchmark) * 100
  )
)
write.csv(kpi, file.path(PARAM$outdir, "kpis_vs_benchmark.csv"), row.names = FALSE)

# 10) Exportaciones ------------------------------------------------------------
write.csv(data.frame(Date = index(W_m), coredata(W_m)), file.path(PARAM$outdir, "pesos_mensuales.csv"), row.names = FALSE)
write.csv(data.frame(Date = index(cmp), coredata(cmp)), file.path(PARAM$outdir, "comparativo_rets.csv"), row.names = FALSE)

# Excel con openxlsx
wb <- createWorkbook()
addWorksheet(wb, "KPIs")
writeData(wb, "KPIs", kpi)
addWorksheet(wb, "Pesos")
writeData(wb, "Pesos", data.frame(Date = index(W_m), coredata(W_m)))
addWorksheet(wb, "Rets")
writeData(wb, "Rets", data.frame(Date = index(cmp), coredata(cmp)))
saveWorkbook(wb, file.path(PARAM$outdir, "resumen_optimizada.xlsx"), overwrite = TRUE)

# 11) Gráficos comprensibles ---------------------------------------------------
cap <- function(x) max(min(x, 3), -3)

eq <- cumprod(1 + cmp)
eq <- data.frame(Date = index(eq), coredata(eq))

p_eq <- ggplot(eq, aes(Date, Strategy)) +
  geom_line(color = "#2E86AB", size = 1) +
  geom_line(aes(y = Benchmark), color = "#F6A01A", size = 1, alpha = 0.8) +
  labs(title = "Evolución del Capital: Estrategia vs Benchmark",
       x = "Fecha", y = "Crecimiento (base 1)") + theme_minimal(base_size = 12)
ggsave(file.path(PARAM$outdir, "equity_vs_benchmark.png"), p_eq, width = 9, height = 5, dpi = 120)

# Drawdown
DD <- function(r) { eq <- cumprod(1 + r); (eq/cummax(eq)) - 1 }
dd_df <- data.frame(Date = index(cmp),
                    Strategy = DD(cmp$Strategy),
                    Benchmark = DD(cmp$Benchmark))
p_dd <- ggplot(dd_df, aes(Date, Strategy)) +
  geom_line(color = "#C43340", size = 0.9) +
  geom_line(aes(y = Benchmark), color = "#888888", size = 0.8, linetype = "dashed") +
  labs(title = "Drawdown: Estrategia vs Benchmark", x = "Fecha", y = "Drawdown") +
  theme_minimal(base_size = 12)
ggsave(file.path(PARAM$outdir, "drawdown.png"), p_dd, width = 9, height = 5, dpi = 120)

# Barras de retornos mensuales (estrategia)
ret_m <- apply.monthly(cmp$Strategy, Return.cumulative)
ret_m_df <- data.frame(Date = index(ret_m), Ret = coredata(ret_m) * 100)
p_m <- ggplot(ret_m_df, aes(Date, Ret)) +
  geom_col(fill = "#4C78A8") + geom_hline(yintercept = 0, color = "gray40") +
  labs(title = "Retornos Mensuales (%) - Estrategia", x = "Mes", y = "%") +
  theme_minimal(base_size = 12)
ggsave(file.path(PARAM$outdir, "retornos_mensuales.png"), p_m, width = 9, height = 5, dpi = 120)

# Dispersión riesgo vs retorno reciente
last_window <- tail(cbind(R_f), 21)
if (nrow(last_window) >= 10) {
  rr <- data.frame(Symbol = colnames(last_window),
                   Recent_Return = colMeans(last_window) * 252 * 100,
                   Annual_Volatility = apply(R_f, 2, ann_vol) * 100)
  p_sc <- ggplot(rr, aes(Annual_Volatility, Recent_Return, label = Symbol)) +
    geom_point(color = "#54A24B", size = 3) +
    geom_text(vjust = -0.8, size = 3) +
    labs(title = "Riesgo vs Retorno (reciente)", x = "Volatilidad Anual (%)", y = "Retorno Anual (%)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(PARAM$outdir, "riesgo_vs_retorno.png"), p_sc, width = 8, height = 5, dpi = 120)
}

# 12) README amigable ----------------------------------------------------------
md <- c(
  "# Estrategia Optimizada",
  "",
  "## Qué hace (en términos simples)",
  "- Usa activos con buen equilibrio riesgo/retorno (Sharpe >= 0.3 y drawdown >= -35%)",
  "- Pondera más a los activos estables (menos volátiles)",
  "- Reduce riesgo si el drawdown del portafolio empeora o si la volatilidad del mercado sube",
  "- Compara contra el S&P500 (VOO) para validar si vale la pena",
  "",
  "## Cómo ejecutar",
  "1. Abra RStudio",
  "2. Ejecute: source('EstrategiaETF-Optimizada.R')",
  "3. Revise esta carpeta de resultados",
  "",
  "## Archivos importantes",
  "- kpis_vs_benchmark.csv: métricas clave comparadas con VOO",
  "- pesos_mensuales.csv: pesos aplicados por mes",
  "- comparativo_rets.csv: retornos diarios estrategia vs benchmark",
  "- resumen_optimizada.xlsx: todo en Excel",
  "- equity_vs_benchmark.png: curvas de capital",
  "- drawdown.png: caídas desde máximos",
  "- retornos_mensuales.png: barras por mes",
  "- riesgo_vs_retorno.png: dispersión de riesgo/retorno reciente",
  "",
  "## Reglas de riesgo (en humano)",
  "- Si la caída reciente es fuerte, la cartera se achica para proteger capital",
  "- Si el mercado está muy volátil, pasamos una parte a efectivo",
  "",
  "## ¿Cuándo considerarla buena?",
  "- Si vence a VOO en retorno y/o Sharpe a 3–5 años",
  "- Si su drawdown es razonablemente menor que VOO",
  "",
  "## Parámetros (ajustables en el script)",
  paste0("- Umbral Sharpe: ", 0.3),
  paste0("- Umbral Drawdown: ", "-35%"),
  paste0("- Volatilidad benchmark para risk-off: ", 100 * PARAM$risk_off_vol_annual_thr, "%"),
  paste0("- Stop por DD rolling (", PARAM$dd_stop_n, " días): ", 100 * PARAM$dd_stop_thr, "%"),
  paste0("- Rebalance: ", PARAM$rebalance),
  paste0("- Costo por trade: ", 10000 * PARAM$cost, " bps"),
  "",
  "## Nota",
  "- Este modelo es educativo pero robusto: prioriza control de riesgo y transparencia."
)
writeLines(md, con = file.path(PARAM$outdir, "README.md"))

cat("\n🎉 Estrategia optimizada completada. Resultados en: ", PARAM$outdir, "\n", sep = "")
