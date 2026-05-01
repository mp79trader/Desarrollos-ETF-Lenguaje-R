# ============================= ESTRATEGIA CUANTITATIVA PARA ETFs ============================= #
# ------------------------------------- Versión 4.1 ------------------------------------------- #

# 1. Instalación y Carga de Paquetes ----------------------------------------------------------
if (!require("pacman", quietly = TRUE)) install.packages("pacman")

# Instalación robusta de dependencias críticas
if (!require("caret")) install.packages("caret", dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"))
if (!require("ranger")) install.packages("ranger")

# Carga explícita para evitar conflictos
library(lattice)
library(ggplot2)
library(caret)

pacman::p_load(
  quantmod, PerformanceAnalytics, tidyverse, scales, reshape2, corrplot,
  GA, TTR, plotly, DT, tidyquant, xts, lubridate, gridExtra, knitr, parallel,
  foreach, doParallel, tseries, PortfolioAnalytics, htmltools,  randomForest, ranger
)

importance <- ranger::importance

# 2. Configuración Global ----------------------------------------------------------------------
library(ggplot2)  # Carga explícita
theme_set(theme_minimal(base_size = 12))

options(warn = -1, scipen = 999, digits = 4)
Sys.setenv(TZ = "UTC")
set.seed(2025)
options(tidyverse.quiet = TRUE)
options(ranger.num.threads = parallel::detectCores())  # Usar todos los núcleos

# 3. Parámetros y Estrategias ------------------------------------------------------------------
STRATEGIES <- list(
  DIV_GROWTH = list(
    name = "Dividendo y Crecimiento",
    symbols = c("VTI", "VIG", "VNQ", "BND", "GLD"),
    rebalance_days = 90
  ),
  CONS_GROWTH = list(
    name = "Conservadora",
    symbols = c("SPY", "IEF", "VCIT", "VIG", "XLK"),
    rebalance_days = 60
  ),
  HEDGE_NEG_CORR = list(
    name = "Cobertura con Correlación Negativa",
    symbols = c("SPY", "VIXY", "TLT", "GLD", "BND"),
    rebalance_days = 30
  )
)

PARAMS <- list(
  fecha_inicio = as.Date("2018-01-01"),
  tasa_libre_riesgo = 0.02 / 252,
  costo_transaccion = 0.001,
  ventana_optimizacion = 252 * 3,
  simulaciones_mc = 2000,
  horizonte_proyeccion = 252,
  min_obs_ml = 150,
  n_arboles = 500,
  n_cores = detectCores() - 1,
  umbral_volatilidad = 0.30,
  max_allocation = 0.35
)

# 4. Función de Descarga de Datos con Reintentos -----------------------------------------------
descargar_datos <- function(simbolos, fecha_inicio) {
  datos <- list()
  
  for (s in simbolos) {
    intentos <- 0
    success <- FALSE
    
    while (intentos < 3 && !success) {
      tryCatch({
        tmp <- tq_get(
          s, 
          from = fecha_inicio, 
          to = Sys.Date(), 
          get = "stock.prices",
          complete_cases = TRUE
        )
        
        if (nrow(tmp) < 50) stop("Datos insuficientes")
        
        retornos <- tmp %>% 
          tq_transmute(
            select = adjusted,
            mutate_fun = periodReturn,
            period = "daily",
            type = "log",
            col_rename = "retornos"
          )
        
        datos[[s]] <- xts(retornos$retornos, order.by = retornos$date)
        message(sprintf("✅ %s | Descargado (%d días)", s, nrow(retornos)))
        success <- TRUE
      }, error = function(e) {
        intentos <<- intentos + 1
        message(sprintf("⏳ Reintento %d/3 para %s: %s", intentos, s, e$message))
        Sys.sleep(2)
      })
    }
    
    if (!success) warning(sprintf("❌ Fallo descarga %s después de 3 intentos", s))
  }
  
  merged_data <- tryCatch({
    na.omit(do.call(merge, datos))
  }, error = function(e) {
    stop("❌ Error al fusionar datos: ", e$message)
  })
  
  if (nrow(merged_data) < 100) stop("❌ Datos insuficientes después de la fusión")
  
  return(merged_data)
}

# 5. Optimización de Parámetros con Algoritmo Genético Paralelo --------------------------------
optimizar_estrategia <- function(retornos) {
  cl <- makeCluster(PARAMS$n_cores)
  registerDoParallel(cl)
  
  calcular_aptitud <- function(periodos) {
    tryCatch({
      periodos <- as.integer(periodos)
      if (any(periodos < 20 | periodos > 200)) return(-1e5)
      
      sharpe_values <- foreach(a = colnames(retornos), .combine = 'c') %dopar% {
        precio <- cumprod(1 + retornos[, a])
        ma_fast <- TTR::SMA(precio, n = periodos[1])
        ma_slow <- TTR::SMA(precio, n = periodos[2])
        senal <- ifelse(ma_fast > ma_slow, 1, 0)
        ret_neto <- lag(senal) * retornos[, a] - abs(diff(senal)) * PARAMS$costo_transaccion
        PerformanceAnalytics::SharpeRatio.annualized(ret_neto, Rf = PARAMS$tasa_libre_riesgo)[1]
      }
      
      mean(sharpe_values, na.rm = TRUE)
    }, error = function(e) -1e5)
  }
  
  resultado <- ga(
    type = "real-valued",
    fitness = calcular_aptitud,
    lower = c(20, 50),
    upper = c(100, 200),
    popSize = 30,
    maxiter = 100,
    parallel = TRUE,
    seed = 2025,
    monitor = FALSE
  )
  
  stopCluster(cl)
  return(round(resultado@solution))
}

# 6. Modelado de Machine Learning (Versión Mejorada) ------------------------------------------
entrenar_modelos <- function(datos) {
  cl <- makeCluster(PARAMS$n_cores)
  registerDoParallel(cl)
  
  # Exportación explícita de parámetros y paquetes
  clusterExport(cl, c("PARAMS"), envir = environment())
  clusterEvalQ(cl, {
    library(caret)
    library(ranger)
    library(TTR)
    library(dplyr)
    library(tidyr)
  })
  
  modelos <- list()
  
  for (activo in colnames(datos)) {
    tryCatch({
      # Crear matriz de características robusta
      df <- data.frame(
        target = as.numeric(datos[, activo]),
        lag1 = lag(datos[, activo], 1),
        lag2 = lag(datos[, activo], 2),
        ma5 = TTR::SMA(datos[, activo], 5),
        ma20 = TTR::SMA(datos[, activo], 20),
        ma50 = TTR::SMA(datos[, activo], 50),
        rsi = TTR::RSI(datos[, activo]),
        vol10 = TTR::runSD(datos[, activo], n = 10)
      ) %>% 
        tidyr::drop_na() %>%
        dplyr::filter(abs(target) < PARAMS$umbral_volatilidad)
      
      # Validación de datos suficiente
      if (nrow(df) >= PARAMS$min_obs_ml) {
        ctrl <- caret::trainControl(
          method = "timeslice",
          initialWindow = min(500, nrow(df) - 100),  # Asegurar ventana válida
          horizon = 100,
          fixedWindow = TRUE,
          allowParallel = TRUE
        )
        
        # Entrenamiento con parámetros optimizados para ranger
        modelo <- caret::train(
          target ~ .,
          data = df,
          method = "ranger",
          trControl = ctrl,
          num.trees = PARAMS$n_arboles,
          importance = "impurity",
          metric = "RMSE",
          tuneLength = 3  # Reducir tiempo de cómputo
        )
        
        modelos[[activo]] <- modelo
        message(sprintf("🌳 %s | RMSE: %.4f | R²: %.2f | Muestras: %d", 
                        activo, 
                        min(modelo$results$RMSE),
                        max(modelo$results$Rsquared),
                        nrow(df)))
      } else {
        warning(sprintf("⚠️ %s | Datos insuficientes: %d obs < %d requeridas",
                        activo, nrow(df), PARAMS$min_obs_ml))
      }
    }, error = function(e) {
      message(sprintf("❌ Error en %s: %s", activo, e$message))
    })
  }
  
  stopCluster(cl)
  
  # Validación crítica de modelos
  if (length(modelos) == 0) {
    stop("❌ Fallo catastrófico: Todos los modelos fallaron en entrenar")
  } else {
    message(sprintf("\n✅ Entrenamiento completado: %d/%d modelos exitosos",
                    length(modelos), ncol(datos)))
  }
  
  return(modelos)
}

# 7. Sistema de Backtesting (Corrección Clave) ------------------------------------------------
realizar_backtest <- function(retornos, modelos, estrategia) {
  tryCatch({
    # Filtrado de activos con modelos válidos
    activos_validos <- names(modelos)[!sapply(modelos, is.null)]
    
    # Validación crítica de fechas
    if (nrow(retornos) == 0) stop("Retornos vacíos - verificar datos de entrada")
    
    # Generar señales solo para activos con modelos exitosos
    activos_validos <- intersect(names(modelos), colnames(retornos))
    
    for (activo in activos_validos) {
      tryCatch({
        # Verificar que el modelo existe y es válido
        if (is.null(modelos[[activo]])) next
        
        df_pred <- data.frame(
          lag1 = lag(retornos[, activo], 1),
          lag2 = lag(retornos[, activo], 2),
          ma5 = TTR::SMA(retornos[, activo], 5),
          ma20 = TTR::SMA(retornos[, activo], 20),
          ma50 = TTR::SMA(retornos[, activo], 50),
          rsi = TTR::RSI(retornos[, activo]),
          vol10 = TTR::runSD(retornos[, activo], 10)
        ) %>% 
          na.omit()
        
        if (nrow(df_pred) > 0) {
          pred <- predict(modelos[[activo]], newdata = df_pred)
          senal <- ifelse(pred > 0.005, 1, ifelse(pred < -0.005, -1, 0))
          fechas_validas <- index(retornos)[index(retornos) %in% (index(df_pred) + 1)]
          señales[fechas_validas, activo] <- senal[1:length(fechas_validas)]
        }
      }, error = function(e) {
        message(sprintf("⚠️ Error señal %s: %s", activo, e$message))
      })
    }
    
    # Gestión de Riesgo
    volatilidad <- rollapply(retornos, width = 21, FUN = sd, align = "right", fill = NA)
    riesgo <- ifelse(volatilidad > 0.02, 0.5, 1)
    señales <- señales * riesgo
    
    # Rebalanceo periódico
    fechas_rebalanceo <- endpoints(señales, on = "days", k = STRATEGIES[[estrategia]]$rebalance_days)
    señales <- na.locf(señales)[fechas_rebalanceo]
    
    # Calcular retornos
    retornos_netos <- lag(señales) * retornos
    cambios <- abs(diff(señales)) > 0
    costos <- rowSums(cambios, na.rm = TRUE) * PARAMS$costo_transaccion
    retornos_netos <- retornos_netos - xts(costos, index(retornos_netos))
    
    return(na.omit(retornos_netos))
  }, error = function(e) {
    stop("❌ Error en backtesting: ", e$message)
  })
}

# 8. Análisis de Desempeño Profesional ---------------------------------------------------------
analizar_desempeño <- function(retornos, nombre_estrategia) {
  tryCatch({
    # Métricas básicas
    ret_acum <- Return.cumulative(retornos)
    ret_anual <- Return.annualized(retornos)
    volatilidad <- StdDev.annualized(retornos)
    sharpe <- SharpeRatio.annualized(retornos, Rf = PARAMS$tasa_libre_riesgo)
    sortino <- SortinoRatio(retornos)
    max_dd <- maxDrawdown(retornos)
    ulcer <- UlcerIndex(retornos)
    
    # Métricas avanzadas
    var <- VaR(retornos, p = 0.95)
    cvar <- ES(retornos, p = 0.95)
    omega <- OmegaSharpeRatio(retornos)
    capture_ratio <- CaptureRatio(retornos, retornos[,1])
    
    # Estadísticas de operaciones
    operaciones <- table(sign(retornos))
    hit_rate <- operaciones[["1"]] / sum(operaciones)
    payoff_ratio <- mean(retornos[retornos > 0]) / abs(mean(retornos[retornos < 0]))
    
    # Devolver lista estructurada
    list(
      Estrategia = nombre_estrategia,
      Retorno_Acumulado = as.numeric(ret_acum),
      Retorno_Anualizado = as.numeric(ret_anual),
      Volatilidad = as.numeric(volatilidad),
      Sharpe = as.numeric(sharpe),
      Sortino = as.numeric(sortino),
      Max_Drawdown = as.numeric(max_dd),
      Ulcer_Index = as.numeric(ulcer),
      VaR_95 = as.numeric(var),
      CVaR_95 = as.numeric(cvar),
      Omega_Ratio = as.numeric(omega),
      Capture_Ratio = as.numeric(capture_ratio),
      Hit_Rate = hit_rate,
      Payoff_Ratio = payoff_ratio
    )
  }, error = function(e) {
    warning("⚠️ Error en análisis de desempeño: ", e$message)
    return(NULL)
  })
}

# 9. Visualización Interactiva y Reportes ------------------------------------------------------
generar_reportes <- function(resultados, carpeta) {
  dir.create(carpeta, showWarnings = FALSE)
  
  # 1. Reporte de Métricas
  metricas <- lapply(names(resultados), function(e) {
    analizar_desempeño(rowSums(resultados[[e]]$backtest), STRATEGIES[[e]]$name)
  }) %>% bind_rows()
  
  write_csv(metricas, file.path(carpeta, "metricas_desempeño.csv"))
  
  # 2. Gráficos Interactivos
  plots <- list()
  for (e in names(resultados)) {
    ret_acum <- cumprod(1 + rowSums(resultados[[e]]$backtest)) - 1
    dd <- PerformanceAnalytics::Drawdowns(rowSums(resultados[[e]]$backtest))
    
    p1 <- ggplot(fortify(ret_acum), aes(x = Index, y = ret_acum)) +
      geom_line(color = "#1f77b4") +
      labs(title = paste("Retorno Acumulado -", STRATEGIES[[e]]$name)) +
      theme_tq()
    
    p2 <- ggplot(fortify(dd), aes(x = Index, y = dd)) +
      geom_area(fill = "#d62728", alpha = 0.5) +
      labs(title = "Curva de Drawdown") +
      theme_tq()
    
    p3 <- ggplot(fortify(retornos), aes(x = retornos)) +
      geom_histogram(fill = "#2ca02c", bins = 50) +
      labs(title = "Distribución de Retornos") +
      theme_tq()
    
    plots[[e]] <- grid.arrange(p1, p2, p3, ncol = 1)
    ggsave(file.path(carpeta, paste0("reporte_", e, ".png")), plots[[e]], width = 14, height = 10)
  }
  
  # 3. Reporte HTML Interactivo
  html_report <- tagList(
    tags$h1("Reporte de Estrategias Cuantitativas"),
    tags$hr(),
    lapply(names(resultados), function(e) {
      tagList(
        tags$h2(STRATEGIES[[e]]$name),
        tags$img(src = paste0("reporte_", e, ".png"), width = "100%"),
        tags$pre(paste(capture.output(print(metricas[metricas$Estrategia == STRATEGIES[[e]]$name, ])), collapse = "\n"))
      )
    })
  )
  
  save_html(html_report, file.path(carpeta, "reporte.html"))
  
  # 4. Correlación entre Estrategias
  cor_matrix <- cor(do.call(merge, lapply(resultados, function(x) rowSums(x$backtest))))
  png(file.path(carpeta, "correlacion_estrategias.png"), width = 800, height = 600)
  corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")
  dev.off()
}

# 10. Función Principal de Ejecución (Versión Mejorada) ---------------------------------------
ejecutar_analisis <- function() {
  tryCatch({
    message("🚀 Iniciando análisis cuantitativo...")
    
    # Validación inicial de paquetes
    required_pkgs <- c("caret", "ranger", "TTR", "xts")
    if (!all(sapply(required_pkgs, requireNamespace))) 
      stop("Paquetes críticos no instalados")
    
    # Paso 1: Descargar datos
    simbolos <- unique(unlist(lapply(STRATEGIES, `[[`, "symbols")))
    datos <- descargar_datos(simbolos, PARAMS$fecha_inicio)
    
    # Paso 2: Procesar cada estrategia
    resultados <- list()
    for (e in names(STRATEGIES)) {
      message(sprintf("\n🔍 Procesando estrategia: %s", STRATEGIES[[e]]$name))
      
      # Subconjunto de datos
      datos_estrategia <- datos[, STRATEGIES[[e]]$symbols]
      
      # Optimización
      message("⚙️ Optimizando parámetros...")
      parametros_optimos <- optimizar_estrategia(datos_estrategia)
      message(sprintf("⚙️ Parámetros óptimos: MA%d/%d", 
                      parametros_optimos[1], parametros_optimos[2]))
      
      # Entrenamiento modelos
      message("🤖 Entrenando modelos predictivos...")
      modelos <- entrenar_modelos(datos_estrategia)
      
      # Backtesting
      message("📊 Ejecutando backtest...")
      backtest <- realizar_backtest(datos_estrategia, modelos, e)
      
      resultados[[e]] <- list(
        parametros = parametros_optimos,
        modelos = modelos,
        backtest = backtest
      )
    }
    
    # Generar reportes
    message("\n📈 Generando reportes finales...")
    generar_reportes(resultados, "Resultados_Estrategias")
    
    message(paste("\n🎉 Análisis completado. Resultados en:", 
                  file.path(getwd(), "Resultados_Estrategias")))
    
    return(resultados)
  }, error = function(e) {
    stop("❌ Error fatal en ejecución: ", e$message)
  })
}

# 11. Ejecución Automática ---------------------------------------------------------------------
if (!interactive()) {
  tryCatch({
    resultados <- ejecutar_analisis()
  }, error = function(e) {
    message("❌ El análisis no pudo completarse: ", e$message)
  })
}