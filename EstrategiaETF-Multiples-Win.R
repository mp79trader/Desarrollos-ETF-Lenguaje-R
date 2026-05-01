# Mensaje final
if (!exists("resultados_finales") || is.null(resultados_finales)) {
  cat("⚠️ No se generaron resultados. Revisa los errores arriba.\n")
} else {
  cat("🎉 Análisis completado. Variable 'resultados_finales' disponible.\n")
  cat("   Ejecuta: str(resultados_finales) para ver la estructura\n\n")
}

# ═════════════════════════════════════════════════════════════════════════════
#  COMANDOS ÚTILES DESPUÉS DE LA EJECUCIÓN
# ═════════════════════════════════════════════════════════════════════════════

# Descomenta (quita el #) y ejecuta los comandos que necesites:

# Ver lista de estrategias procesadas:
# names(resultados_finales)

# Ver todas las recomendaciones:
# lapply(resultados_finales, function(x) x$recommendations)

# Ver recomendaciones de una estrategia específica:
# print(resultados_finales$DDG$recommendations)
# print(resultados_finales$CEG$recommendations)
# print(resultados_finales$EIR$recommendations)
# print(resultados_finales$DGSE$recommendations)

# Ver calificaciones:
# lapply(resultados_finales, function(x) list(rating = x$rating, score = x$score))

# Exportar recomendaciones a CSV:
# for (strat in names(resultados_finales)) {
#   write.csv(resultados_finales[[strat]]$recommendations, 
#             paste0("recomendaciones_", strat, ".csv"), 
#             row.names = FALSE)
# }

# Ver estructura completa:
# str(resultados_finales)

# ═════════════════════════════════════════════════════════════════════════════
#  PRUEBA RÁPIDA (Si el script completo falla)
# ═════════════════════════════════════════════════════════════════════════════

# Si el análisis completo falla, prueba con UNA sola estrategia:
# 
# prueba_rapida <- function() {
#   cat("\n🧪 PRUEBA RÁPIDA - Solo estrategia DDG\n\n")
#   
#   # Descargar datos
#   data_list <- download_financial_data(
#     c("SPY", "GLD", "BND"), 
#     as.Date("2022-01-01")
#   )
#   
#   # Procesar
#   returns <- do.call(merge.xts, data_list)
#   returns <- na.omit(returns)
#   
#   # Calcular métricas
#   metrics <- calculate_portfolio_metrics(returns)
#   recommendations <- generate_recommendations(returns, metrics)
#   rating <- rate_strategy(metrics)
#   
#   # Mostrar resultados
#   cat("\n📊 RECOMENDACIONES:\n")
#   print(recommendations)
#   cat("\n⭐ CALIFICACIÓN:", rating$rating, "\n\n")
#   
#   return(list(recommendations = recommendations, rating = rating))
# }
# 
# # Ejecuta la prueba rápida:
# # resultado_prueba <- prueba_rapida()# 11. Execute Analysis -----------------------------------------------------------------

# NO EJECUTAR LÍNEA POR LÍNEA - EJECUTAR TODO EL SCRIPT COMPLETO
# En RStudio: Ctrl + Shift + Enter (ejecutar todo el script)
# O: Source > Source (ejecutar archivo completo)

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  🚀 INICIANDO ANÁLISIS AUTOMÁTICO\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Función principal de ejecución
resultados_finales <- tryCatch({
  # Ejecutar análisis
  res <- main()
  
  if (!is.null(res) && length(res) > 0) {
    cat("\n")
    cat("═══════════════════════════════════════════════════════════════\n")
    cat("  ✅ ANÁLISIS COMPLETADO EXITOSAMENTE\n")
    cat("═══════════════════════════════════════════════════════════════\n\n")
    
    cat("📊 CÓMO USAR LOS RESULTADOS:\n")
    cat("─────────────────────────────────────────────────────────────\n")
    cat("• Ver estrategias disponibles:\n")
    cat("  names(resultados_finales)\n\n")
    cat("• Ver recomendaciones de una estrategia:\n")
    cat("  resultados_finales$DDG$recommendations\n\n")
    cat("• Ver calificación:\n")
    cat("  resultados_finales$DDG$rating\n\n")
    cat("• Ver todas las recomendaciones:\n")
    cat("  lapply(resultados_finales, function(x) x$recommendations)\n\n")
    cat("• Gráficos PNG guardados en:\n")
    cat("  ", getwd(), "\n")
    cat("─────────────────────────────────────────────────────────────\n\n")
  }
  
  res
  
}, error = function(e) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  ❌ ERROR DURANTE LA EJECUCIÓN\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  cat("Mensaje de error:", e$message, "\n\n")
  cat("💡 SOLUCIONES:\n")
  cat("1. Verifica tu conexión a internet\n")
  cat("2. Intenta con menos datos:\n")
  cat("   ANALYSIS_PARAMS$start_date <- as.Date('2020-01-01')\n")
  cat("3. Vuelve a ejecutar TODO el script (Ctrl + Shift + Enter)\n\n")
  NULL
})

# Mensaje final
if (is.null(resultados_finales)) {
  cat("⚠️ No se generaron resultados. Revisa los errores arriba.\n")
} else {
  cat("🎉 Análisis completado. Variable 'resultados_finales' disponible.\n")
  cat("   Ejecuta: str(resultados_finales) para ver la estructura\n\n")
}# ============================= Portfolio Analysis in R ============================= #
# Author: Professional Investment Analyst
# Last Update: 2025
# Platform: Windows Compatible

# ┌─────────────────────────────────────────────────────────────────────────────┐
# │  INSTRUCCIONES IMPORTANTES:                                                 │
# │                                                                              │
# │  1. NO ejecutes línea por línea (Enter)                                     │
# │  2. Ejecuta TODO el script completo:                                        │
# │     - RStudio: Presiona Ctrl + Shift + Enter                                │
# │     - O: Click en "Source" → "Source" (arriba derecha)                      │
# │  3. Espera 5-10 minutos (descarga de datos toma tiempo)                     │
# │  4. Los resultados se guardarán en la variable: resultados_finales          │
# │                                                                              │
# │  Si hay errores de conexión, ejecuta primero:                               │
# │  ANALYSIS_PARAMS$start_date <- as.Date("2020-01-01")                        │
# │  Y vuelve a ejecutar todo el script                                         │
# └─────────────────────────────────────────────────────────────────────────────┘

# 1. Package Installation and Loading -------------------------------------------------
cat("📦 Instalando y cargando paquetes necesarios...\n")

# Lista de paquetes requeridos
packages <- c("quantmod", "PerformanceAnalytics", "tidyverse", "ggplot2", 
              "scales", "reshape2", "corrplot", "plotly", "xts", "zoo")

# Función para instalar paquetes faltantes
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Instalando %s...\n", pkg))
    install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

cat("✅ Todos los paquetes cargados correctamente\n\n")

# 2. Global Configuration --------------------------------------------------------------
options(warn = -1, scipen = 999, digits = 4)
Sys.setenv(TZ = "UTC")
set.seed(2025)

# 3. Investment Strategies -------------------------------------------------------------
STRATEGIES <- list(
  DDG = list(
    name = "Defensive Dividend Growth",
    symbols = c("EZU", "GLD", "LQD", "VOO", "TLT")
  ),
  CEG = list(
    name = "Conservative Equity Growth",
    symbols = c("GLD", "UUP", "VNQ", "JNK", "EEM", "VTV")
  ),
  EIR = list(
    name = "Enhanced Income Return",
    symbols = c("GSG", "VEA", "XLK", "UUP", "XLY")
  ),
  DGSE = list(
    name = "Diversified Growth & Sector Exposure",
    symbols = c("VEA", "GLD", "XLK", "XLV", "XLE", "ITB")
  )
)

# 4. Analysis Parameters ---------------------------------------------------------------
ANALYSIS_PARAMS <- list(
  start_date = as.Date("2015-01-01"),  # Reducido para mejor performance
  risk_free_rate = 0.02/252,
  confidence_level = 0.95,
  rolling_window = 252,
  initial_investment = 10000
)

# 5. Download Financial Data -----------------------------------------------------------
download_financial_data <- function(symbols, start_date) {
  cat("📥 Descargando datos financieros...\n")
  data_list <- list()
  failed_symbols <- character()
  
  for (symbol in symbols) {
    attempts <- 0
    success <- FALSE
    
    while (attempts < 3 && !success) {
      tryCatch({
        # Descargar datos desde Yahoo Finance
        temp_data <- getSymbols(
          symbol, 
          src = "yahoo", 
          from = start_date,
          to = Sys.Date(),
          auto.assign = FALSE,
          warnings = FALSE
        )
        
        if (is.null(temp_data) || nrow(temp_data) < 50) {
          stop("Datos insuficientes")
        }
        
        # Extraer precios ajustados
        prices <- Ad(temp_data)
        
        # Calcular retornos diarios
        returns <- dailyReturn(prices, type = "log")
        colnames(returns) <- symbol
        
        # Remover NAs
        returns <- na.omit(returns)
        
        if (nrow(returns) > 0) {
          data_list[[symbol]] <- returns
          cat(sprintf("  ✅ %s | %d días\n", symbol, nrow(returns)))
          success <- TRUE
        } else {
          stop("No hay datos después de limpiar NAs")
        }
        
      }, error = function(e) {
        attempts <<- attempts + 1
        if (attempts < 3) {
          cat(sprintf("  ⏳ Reintento %d/3 para %s...\n", attempts, symbol))
          Sys.sleep(2)
        } else {
          cat(sprintf("  ❌ %s falló\n", symbol))
          failed_symbols <<- c(failed_symbols, symbol)
        }
      })
    }
  }
  
  if (length(data_list) == 0) {
    stop("❌ No se pudo descargar ningún dato. Verifica tu conexión a internet.")
  }
  
  if (length(failed_symbols) > 0) {
    cat(sprintf("\n⚠️ Símbolos fallidos: %s\n", paste(failed_symbols, collapse = ", ")))
  }
  
  cat(sprintf("✅ Total descargado: %d/%d símbolos\n\n", 
              length(data_list), length(symbols)))
  
  return(data_list)
}

# 6. Calculate Portfolio Metrics -------------------------------------------------------
calculate_portfolio_metrics <- function(returns) {
  cat("\n📊 Calculando métricas del portafolio...\n")
  
  # Asegurar que no haya NAs
  returns <- na.omit(returns)
  
  if (nrow(returns) < 50) {
    stop("Datos insuficientes para calcular métricas")
  }
  
  metrics <- list()
  
  # Retornos anuales
  tryCatch({
    metrics$annual_returns <- apply.yearly(returns, Return.cumulative)
  }, error = function(e) {
    metrics$annual_returns <<- NA
  })
  
  # Retornos mensuales
  tryCatch({
    metrics$monthly_returns <- apply.monthly(returns, Return.cumulative)
  }, error = function(e) {
    metrics$monthly_returns <<- NA
  })
  
  # Volatilidad anualizada
  metrics$annual_volatility <- apply(returns, 2, function(x) {
    sd(x, na.rm = TRUE) * sqrt(252)
  })
  
  # Sharpe Ratio
  metrics$sharpe_ratio <- apply(returns, 2, function(x) {
    excess <- x - ANALYSIS_PARAMS$risk_free_rate
    mean(excess, na.rm = TRUE) / sd(excess, na.rm = TRUE) * sqrt(252)
  })
  
  # Sortino Ratio
  metrics$sortino_ratio <- apply(returns, 2, function(x) {
    downside <- x[x < 0]
    if (length(downside) < 2) return(NA)
    mean(x, na.rm = TRUE) / sd(downside, na.rm = TRUE) * sqrt(252)
  })
  
  # Maximum Drawdown
  metrics$max_drawdown <- apply(returns, 2, function(x) {
    cum_ret <- cumprod(1 + x)
    running_max <- cummax(cum_ret)
    drawdown <- (cum_ret - running_max) / running_max
    min(drawdown, na.rm = TRUE)
  })
  
  # VaR al 95%
  metrics$var_95 <- apply(returns, 2, function(x) {
    quantile(x, probs = 0.05, na.rm = TRUE)
  })
  
  # Correlación
  metrics$correlation <- cor(returns, use = "pairwise.complete.obs")
  
  cat("✅ Métricas calculadas correctamente\n")
  return(metrics)
}

# 7. Generate Recommendations ----------------------------------------------------------
generate_recommendations <- function(returns, metrics) {
  cat("💡 Generando recomendaciones de inversión...\n")
  
  # Retornos recientes (último mes)
  recent_returns <- tail(na.omit(returns), 21)
  
  recommendations <- data.frame(
    Symbol = colnames(returns),
    Recent_Return = colMeans(recent_returns, na.rm = TRUE) * 252 * 100,  # Anualizado en %
    Annual_Volatility = metrics$annual_volatility * 100,  # En %
    Sharpe = round(metrics$sharpe_ratio, 2),
    Sortino = round(metrics$sortino_ratio, 2),
    MaxDrawdown = round(metrics$max_drawdown * 100, 2),  # En %
    VaR_95 = round(metrics$var_95 * 100, 2),  # En %
    stringsAsFactors = FALSE
  )
  
  # Generar señales de trading
  recommendations$Signal <- with(recommendations, {
    signal <- character(length(Symbol))
    for (i in seq_along(Symbol)) {
      if (is.na(Recent_Return[i]) || is.na(Sharpe[i])) {
        signal[i] <- "N/A"
      } else if (Recent_Return[i] > 15 && Sharpe[i] > 1) {
        signal[i] <- "🟢 Strong Buy"
      } else if (Recent_Return[i] > 10 && Sharpe[i] > 0.5) {
        signal[i] <- "🔵 Buy"
      } else if (Recent_Return[i] < -10 && MaxDrawdown[i] < -15) {
        signal[i] <- "🔴 Sell"
      } else if (Recent_Return[i] < -5) {
        signal[i] <- "🟡 Reduce"
      } else {
        signal[i] <- "⚪ Hold"
      }
    }
    signal
  })
  
  return(recommendations)
}

# 8. Rate Strategy ---------------------------------------------------------------------
rate_strategy <- function(metrics) {
  score <- 0
  
  # Sharpe Ratio (0-30 puntos)
  mean_sharpe <- mean(metrics$sharpe_ratio, na.rm = TRUE)
  if (!is.na(mean_sharpe)) {
    if (mean_sharpe > 1.5) score <- score + 30
    else if (mean_sharpe > 1.0) score <- score + 20
    else if (mean_sharpe > 0.5) score <- score + 10
  }
  
  # Volatilidad (0-30 puntos)
  mean_vol <- mean(metrics$annual_volatility, na.rm = TRUE)
  if (!is.na(mean_vol)) {
    if (mean_vol < 0.15) score <- score + 30
    else if (mean_vol < 0.20) score <- score + 20
    else if (mean_vol < 0.25) score <- score + 10
  }
  
  # Drawdown (0-20 puntos)
  mean_dd <- mean(metrics$max_drawdown, na.rm = TRUE)
  if (!is.na(mean_dd)) {
    if (mean_dd > -0.15) score <- score + 20
    else if (mean_dd > -0.20) score <- score + 10
  }
  
  # Sortino Ratio (0-20 puntos)
  mean_sortino <- mean(metrics$sortino_ratio, na.rm = TRUE)
  if (!is.na(mean_sortino)) {
    if (mean_sortino > 1.5) score <- score + 20
    else if (mean_sortino > 1.0) score <- score + 10
  }
  
  # Clasificación
  rating <- if (score >= 80) {
    "⭐⭐⭐⭐⭐ Excelente"
  } else if (score >= 60) {
    "⭐⭐⭐⭐ Buena"
  } else if (score >= 40) {
    "⭐⭐⭐ Promedio"
  } else {
    "⭐⭐ Pobre"
  }
  
  return(list(rating = rating, score = score))
}

# 9. Visualization Functions -----------------------------------------------------------
create_performance_chart <- function(returns, strategy_name) {
  cat("📈 Creando gráfico de rendimiento...\n")
  
  tryCatch({
    # Calcular retornos acumulados
    cum_returns <- cumprod(1 + returns)
    
    # Crear dataframe para ggplot
    df <- data.frame(
      Date = index(cum_returns),
      as.data.frame(coredata(cum_returns))
    )
    
    # Convertir a formato largo
    df_long <- df %>%
      pivot_longer(cols = -Date, names_to = "Symbol", values_to = "Value")
    
    # Crear gráfico con ggplot2
    p <- ggplot(df_long, aes(x = Date, y = Value, color = Symbol)) +
      geom_line(size = 1) +
      labs(
        title = paste("Rendimiento Acumulado -", strategy_name),
        subtitle = paste("Periodo:", format(min(df$Date), "%Y-%m-%d"), "a", format(max(df$Date), "%Y-%m-%d")),
        x = "Fecha",
        y = "Valor ($1 inicial)",
        color = "Símbolo"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        legend.position = "bottom"
      ) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_color_brewer(palette = "Set2")
    
    # Convertir a plotly para interactividad
    plotly_chart <- ggplotly(p, tooltip = c("x", "y", "color"))
    
    print(plotly_chart)
    
  }, error = function(e) {
    cat(sprintf("⚠️ Error creando gráfico: %s\n", e$message))
  })
}

create_correlation_plot <- function(correlation_matrix, strategy_name) {
  cat("🔗 Creando matriz de correlación...\n")
  
  tryCatch({
    # Guardar gráfico en PNG
    png_file <- paste0("correlation_", gsub(" ", "_", strategy_name), ".png")
    png(png_file, width = 800, height = 600, res = 100)
    
    corrplot(
      correlation_matrix,
      method = "color",
      type = "upper",
      order = "hclust",
      tl.col = "black",
      tl.srt = 45,
      addCoef.col = "black",
      number.cex = 0.7,
      title = paste("Matriz de Correlación -", strategy_name),
      mar = c(0, 0, 2, 0)
    )
    
    dev.off()
    cat(sprintf("✅ Gráfico guardado: %s\n", png_file))
    
  }, error = function(e) {
    cat(sprintf("⚠️ Error creando correlación: %s\n", e$message))
  })
}

# 10. Main Execution Function ----------------------------------------------------------
main <- function() {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║   ANÁLISIS PROFESIONAL DE PORTAFOLIO - WINDOWS COMPATIBLE     ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")
  
  results_summary <- list()
  
  # Procesar cada estrategia
  for (strategy_id in names(STRATEGIES)) {
    strategy <- STRATEGIES[[strategy_id]]
    
    cat("\n")
    cat("═══════════════════════════════════════════════════════════════════\n")
    cat(sprintf("   ESTRATEGIA: %s (%s)\n", strategy$name, strategy_id))
    cat("═══════════════════════════════════════════════════════════════════\n\n")
    
    tryCatch({
      # 1. Descargar datos
      data_list <- download_financial_data(strategy$symbols, ANALYSIS_PARAMS$start_date)
      
      if (length(data_list) == 0) {
        cat("❌ No hay datos disponibles para esta estrategia\n")
        next
      }
      
      # 2. Fusionar retornos
      returns <- do.call(merge.xts, data_list)
      returns <- na.omit(returns)
      
      if (nrow(returns) < 50) {
        cat("❌ Datos insuficientes después de limpieza\n")
        next
      }
      
      cat(sprintf("\n📊 Datos procesados: %d días de %d activos\n", 
                  nrow(returns), ncol(returns)))
      
      # 3. Calcular métricas
      metrics <- calculate_portfolio_metrics(returns)
      
      # 4. Generar recomendaciones
      recommendations <- generate_recommendations(returns, metrics)
      
      # 5. Calificar estrategia
      rating_result <- rate_strategy(metrics)
      
      # 6. Mostrar resultados
      cat("\n┌─────────────────────────────────────────────────────────────┐\n")
      cat("│                    RECOMENDACIONES                          │\n")
      cat("└─────────────────────────────────────────────────────────────┘\n\n")
      print(recommendations, row.names = FALSE)
      
      cat("\n┌─────────────────────────────────────────────────────────────┐\n")
      cat(sprintf("│  CALIFICACIÓN DE ESTRATEGIA: %-30s │\n", rating_result$rating))
      cat(sprintf("│  Puntuación: %d/100                                      │\n", rating_result$score))
      cat("└─────────────────────────────────────────────────────────────┘\n")
      
      # 7. Crear visualizaciones
      cat("\n🎨 Generando visualizaciones...\n")
      create_performance_chart(returns, STRATEGIES[[strategy_id]]$name)
      create_correlation_plot(metrics$correlation, STRATEGIES[[strategy_id]]$name)
      
      # Guardar resultados
      results_summary[[strategy_id]] <- list(
        name = STRATEGIES[[strategy_id]]$name,
        recommendations = recommendations,
        rating = rating_result$rating,
        score = rating_result$score
      )
      
    }, error = function(e) {
      cat(sprintf("\n❌ Error procesando estrategia %s: %s\n", strategy_id, e$message))
    })
  }
  
  # Resumen final
  cat("\n\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║                   RESUMEN DE ANÁLISIS                          ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")
  
  for (strategy_id in names(results_summary)) {
    result <- results_summary[[strategy_id]]
    cat(sprintf("📌 %s: %s (Score: %d/100)\n", 
                strategy_id, result$rating, result$score))
  }
  
  cat("\n✅ Análisis completado exitosamente\n")
  cat(sprintf("📁 Directorio de trabajo: %s\n", getwd()))
  cat("📊 Los gráficos de correlación se han guardado como PNG\n\n")
  
  return(results_summary)
}

# 11. Execute Analysis -----------------------------------------------------------------
# Función wrapper para ejecución segura
ejecutar_todo <- function() {
  cat("\n🚀 Iniciando análisis de portafolio...\n")
  cat("⏱️  Este proceso puede tomar varios minutos...\n\n")
  
  tryCatch({
    # Ejecutar análisis principal
    resultados <- main()
    
    if (is.null(resultados)) {
      cat("\n❌ El análisis no produjo resultados\n")
      cat("💡 Posibles causas:\n")
      cat("   - Problemas de conexión a internet\n")
      cat("   - Símbolos no válidos\n")
      cat("   - Fecha de inicio muy antigua\n")
      return(NULL)
    }
    
    cat("\n🎉 ¡Proceso completado exitosamente!\n")
    cat(sprintf("✅ Se procesaron %d estrategias\n", length(resultados)))
    
    return(resultados)
    
  }, error = function(e) {
    cat(sprintf("\n❌ Error durante la ejecución: %s\n", e$message))
    cat("\n💡 Intenta lo siguiente:\n")
    cat("   1. Verifica tu conexión a internet\n")
    cat("   2. Ejecuta: ANALYSIS_PARAMS$start_date <- as.Date('2020-01-01')\n")
    cat("   3. Vuelve a ejecutar: resultados <- ejecutar_todo()\n")
    return(NULL)
  })
}

# EJECUTAR AUTOMÁTICAMENTE
cat("\n" )
cat("═══════════════════════════════════════════════════════════════\n")
cat("  EJECUTANDO ANÁLISIS AUTOMÁTICO - POR FAVOR ESPERA...\n")
cat("═══════════════════════════════════════════════════════════════\n")

resultados <- ejecutar_todo()

# Fallback: prueba rápida si no hubo resultados
if (is.null(resultados)) {
  cat("\n🧪 Ejecutando prueba rápida con SPY, GLD, BND...\n")
  tryCatch({
    data_list <- download_financial_data(c("SPY", "GLD", "BND"), as.Date("2020-01-01"))
    returns <- do.call(merge.xts, data_list)
    returns <- na.omit(returns)
    metrics <- calculate_portfolio_metrics(returns)
    recommendations <- generate_recommendations(returns, metrics)
    rating <- rate_strategy(metrics)
    resultados <- list(
      QuickTest = list(
        name = "Quick Test",
        recommendations = recommendations,
        rating = rating$rating,
        score = rating$score
      )
    )
    cat("✅ Prueba rápida completada.\n")
  }, error = function(e) {
    cat("❌ Prueba rápida falló: ", e$message, "\n", sep = "")
  })
}

# Mostrar información adicional si fue exitoso
if (!is.null(resultados) && length(resultados) > 0) {
  cat("\n📊 CÓMO USAR LOS RESULTADOS:\n")
  cat("─────────────────────────────────────────────────────────────\n")
  cat("• Ver estructura: str(resultados)\n")
  cat("• Ver estrategias: names(resultados)\n")
  cat("• Ver recomendaciones: resultados$DDG$recommendations\n")
  cat("• Gráficos guardados en: ", getwd(), "\n")
  cat("─────────────────────────────────────────────────────────────\n\n")
  
  # Exportar recomendaciones a CSV automáticamente
  tryCatch({
    for (strat in names(resultados)) {
      if (!is.null(resultados[[strat]]$recommendations)) {
        out <- paste0("recomendaciones_", strat, ".csv")
        write.csv(resultados[[strat]]$recommendations, out, row.names = FALSE)
      }
    }
    cat("✅ Recomendaciones exportadas a CSV en el directorio de trabajo.\n")
  }, error = function(e) {
    cat("⚠️ Error exportando CSV: ", e$message, "\n", sep = "")
  })
}

