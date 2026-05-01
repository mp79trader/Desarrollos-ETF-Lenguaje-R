# ============================= ESTRATEGIA CUANTITATIVA PARA ETFs ============================= #
# ------------------------------------- Versión Windows 1.0 ----------------------------------- #

# 1. Instalación y Carga de Paquetes ----------------------------------------------------------
if (!require("pacman", quietly = TRUE)) install.packages("pacman")

# Instalación robusta para Windows
packages <- c("quantmod", "PerformanceAnalytics", "tidyverse", "scales", "reshape2", 
              "corrplot", "GA", "TTR", "plotly", "DT", "tidyquant", "xts", "lubridate", 
              "gridExtra", "knitr", "parallel", "foreach", "doParallel", "tseries", 
              "randomForest")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Carga explícita
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(TTR)
library(xts)
library(lubridate)
library(parallel)
library(foreach)
library(doParallel)
library(GA)
library(gridExtra)
library(corrplot)
library(htmltools)
library(randomForest)

# 2. Configuración Global ----------------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))
options(warn = -1, scipen = 999, digits = 4)
Sys.setenv(TZ = "UTC")
set.seed(2025)

# Configuración para Windows
n_cores <- max(1, detectCores() - 1)
message(sprintf("💻 Sistema Windows detectado - Usando %d núcleos", n_cores))

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
  fecha_inicio = as.Date("2020-01-01"),  # Reducido para prueba
  tasa_libre_riesgo = 0.02 / 252,
  costo_transaccion = 0.001,
  ventana_optimizacion = 252 * 2,
  min_obs_ml = 100,
  n_arboles = 200,  # Reducido para Windows
  umbral_volatilidad = 0.30,
  max_allocation = 0.35
)

# 4. Función de Descarga de Datos --------------------------------------------------------------
descargar_datos <- function(simbolos, fecha_inicio) {
  datos <- list()
  
  for (s in simbolos) {
    intentos <- 0
    success <- FALSE
    
    while (intentos < 3 && !success) {
      tryCatch({
        tmp <- getSymbols(s, 
                          src = "yahoo",
                          from = fecha_inicio, 
                          to = Sys.Date(),
                          auto.assign = FALSE,
                          warnings = FALSE)
        
        if (nrow(tmp) < 50) stop("Datos insuficientes")
        
        # Extraer columna Adjusted
        adj_col <- grep("Adjusted", colnames(tmp), value = TRUE)
        precios <- Cl(tmp)  # Usar Close si Adjusted falla
        
        retornos <- dailyReturn(precios, type = "log")
        colnames(retornos) <- s
        
        datos[[s]] <- retornos
        message(sprintf("✅ %s | Descargado (%d días)", s, nrow(retornos)))
        success <- TRUE
      }, error = function(e) {
        intentos <<- intentos + 1
        message(sprintf("⏳ Reintento %d/3 para %s", intentos, s))
        Sys.sleep(2)
      })
    }
    
    if (!success) warning(sprintf("❌ Fallo descarga %s", s))
  }
  
  if (length(datos) == 0) stop("❌ No se pudo descargar ningún dato")
  
  merged_data <- na.omit(do.call(merge, datos))
  
  if (nrow(merged_data) < 100) stop("❌ Datos insuficientes después de la fusión")
  
  return(merged_data)
}

# 5. Optimización de Parámetros ----------------------------------------------------------------
optimizar_estrategia <- function(retornos) {
  message("🔧 Optimizando parámetros...")
  
  calcular_aptitud <- function(periodos) {
    tryCatch({
      periodos <- as.integer(periodos)
      if (any(periodos < 20 | periodos > 200)) return(-1e5)
      
      sharpe_values <- sapply(colnames(retornos), function(a) {
        precio <- cumprod(1 + retornos[, a])
        ma_fast <- SMA(as.numeric(precio), n = periodos[1])
        ma_slow <- SMA(as.numeric(precio), n = periodos[2])
        senal <- ifelse(ma_fast > ma_slow, 1, 0)
        senal[is.na(senal)] <- 0
        
        ret_neto <- lag(senal) * retornos[, a] - abs(diff(senal)) * PARAMS$costo_transaccion
        ret_neto <- na.omit(ret_neto)
        
        if (nrow(ret_neto) < 50) return(-1e5)
        
        sharpe <- mean(ret_neto) / sd(ret_neto) * sqrt(252)
        ifelse(is.finite(sharpe), sharpe, -1e5)
      })
      
      mean(sharpe_values, na.rm = TRUE)
    }, error = function(e) -1e5)
  }
  
  # Sin paralelización en Windows para GA
  resultado <- ga(
    type = "real-valued",
    fitness = calcular_aptitud,
    lower = c(20, 50),
    upper = c(100, 200),
    popSize = 20,
    maxiter = 50,
    seed = 2025,
    monitor = FALSE,
    parallel = FALSE  # Desactivado para Windows
  )
  
  return(round(as.numeric(resultado@solution)))
}

# 6. Modelado de Machine Learning --------------------------------------------------------------
entrenar_modelos <- function(datos) {
  message("🤖 Entrenando modelos...")
  modelos <- list()
  
  for (activo in colnames(datos)) {
    tryCatch({
      # Crear características
      df <- data.frame(
        target = as.numeric(datos[, activo]),
        lag1 = as.numeric(lag(datos[, activo], 1)),
        lag2 = as.numeric(lag(datos[, activo], 2)),
        ma5 = as.numeric(SMA(datos[, activo], 5)),
        ma20 = as.numeric(SMA(datos[, activo], 20)),
        rsi = as.numeric(RSI(datos[, activo])),
        vol10 = as.numeric(runSD(datos[, activo], n = 10))
      )
      
      df <- na.omit(df)
      
      # Validación de datos
      if (nrow(df) >= PARAMS$min_obs_ml && sd(df$target) > 0) {
        # Dividir datos
        train_size <- floor(0.8 * nrow(df))
        train_data <- df[1:train_size, ]
        
        # Entrenar Random Forest
        modelo <- randomForest(
          target ~ .,
          data = train_data,
          ntree = PARAMS$n_arboles,
          importance = TRUE,
          mtry = 3
        )
        
        modelos[[activo]] <- modelo
        message(sprintf("🌳 %s | Var Explicada: %.2f%% | Muestras: %d", 
                        activo, 
                        modelo$rsq[length(modelo$rsq)] * 100,
                        nrow(df)))
      } else {
        warning(sprintf("⚠️ %s | Datos insuficientes: %d obs", activo, nrow(df)))
      }
    }, error = function(e) {
      message(sprintf("❌ Error en %s: %s", activo, e$message))
    })
  }
  
  if (length(modelos) == 0) {
    stop("❌ Ningún modelo pudo ser entrenado")
  }
  
  return(modelos)
}

# 7. Sistema de Backtesting --------------------------------------------------------------------
realizar_backtest <- function(retornos, modelos, estrategia) {
  message("📊 Ejecutando backtest...")
  
  tryCatch({
    # Validar que tenemos datos
    if (nrow(retornos) == 0 || ncol(retornos) == 0) {
      stop("Retornos vacíos")
    }
    
    # Inicializar matriz de señales
    señales <- xts(matrix(0.5, nrow = nrow(retornos), ncol = ncol(retornos)),
                   order.by = index(retornos))
    colnames(señales) <- colnames(retornos)
    
    # Generar señales para cada activo con modelo
    for (activo in names(modelos)) {
      tryCatch({
        # Verificar que el activo existe en retornos
        if (!(activo %in% colnames(retornos))) {
          message(sprintf("⚠️ %s no encontrado en retornos", activo))
          next
        }
        
        # Crear características para predicción
        df_pred <- data.frame(
          lag1 = as.numeric(lag(retornos[, activo], 1)),
          lag2 = as.numeric(lag(retornos[, activo], 2)),
          ma5 = as.numeric(SMA(retornos[, activo], 5)),
          ma20 = as.numeric(SMA(retornos[, activo], 20)),
          rsi = as.numeric(RSI(retornos[, activo])),
          vol10 = as.numeric(runSD(retornos[, activo], 10))
        )
        
        # Limpiar NAs
        df_pred_clean <- na.omit(df_pred)
        
        if (nrow(df_pred_clean) > 50) {
          # Hacer predicciones
          pred <- predict(modelos[[activo]], newdata = df_pred_clean)
          
          # Convertir predicciones a señales
          senal <- ifelse(pred > 0.002, 1, 
                          ifelse(pred < -0.002, 0, 0.5))
          
          # Obtener índices válidos
          idx_validos <- which(!is.na(rowSums(df_pred)))
          
          # Asignar señales solo a las fechas válidas
          if (length(senal) == length(idx_validos)) {
            señales[idx_validos, activo] <- senal
            message(sprintf("✅ Señales generadas para %s: %d días", 
                            activo, length(senal)))
          }
        } else {
          message(sprintf("⚠️ %s: datos insuficientes para predicción", activo))
        }
      }, error = function(e) {
        message(sprintf("⚠️ Error generando señales %s: %s", activo, e$message))
      })
    }
    
    # Aplicar gestión de riesgo por volatilidad
    volatilidad <- rollapply(retornos, width = 21, FUN = sd, 
                             align = "right", fill = NA)
    
    # Factor de riesgo: reducir exposición si volatilidad alta
    riesgo <- ifelse(is.na(volatilidad), 1, 
                     ifelse(volatilidad > 0.02, 0.5, 1))
    
    señales <- señales * riesgo
    
    # Calcular retornos del portafolio
    retornos_netos <- señales * retornos
    
    # Aplicar costos de transacción
    cambios <- abs(diff(señales))
    cambios[is.na(cambios)] <- 0
    
    costos <- rowSums(cambios, na.rm = TRUE) * PARAMS$costo_transaccion
    
    # Crear xts de costos con las mismas fechas que retornos_netos
    costos_xts <- xts(costos, order.by = index(retornos_netos))
    
    # Restar costos
    retornos_netos <- retornos_netos - costos_xts
    
    # Limpiar y retornar
    resultado <- na.omit(retornos_netos)
    
    if (nrow(resultado) < 50) {
      warning("⚠️ Pocas observaciones en backtest final")
    }
    
    message(sprintf("✅ Backtest completado: %d días", nrow(resultado)))
    
    return(resultado)
    
  }, error = function(e) {
    stop(sprintf("Error en backtesting: %s", e$message))
  })
}

# 8. Análisis de Desempeño ---------------------------------------------------------------------
analizar_desempeno <- function(retornos, nombre_estrategia) {
  ret_portfolio <- rowSums(retornos, na.rm = TRUE)
  
  list(
    Estrategia = nombre_estrategia,
    Retorno_Acumulado = as.numeric(Return.cumulative(ret_portfolio)),
    Retorno_Anualizado = as.numeric(Return.annualized(ret_portfolio)),
    Volatilidad = as.numeric(StdDev.annualized(ret_portfolio)),
    Sharpe = as.numeric(SharpeRatio.annualized(ret_portfolio, Rf = PARAMS$tasa_libre_riesgo)),
    Max_Drawdown = as.numeric(maxDrawdown(ret_portfolio)),
    VaR_95 = as.numeric(VaR(ret_portfolio, p = 0.95)),
    N_Operaciones = sum(!is.na(ret_portfolio))
  )
}

# 9. Visualización -----------------------------------------------------------------------------
generar_reportes <- function(resultados, carpeta) {
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  
  # Métricas
  metricas <- bind_rows(lapply(names(resultados), function(e) {
    analizar_desempeno(resultados[[e]]$backtest, STRATEGIES[[e]]$name)
  }))
  
  write.csv(metricas, file.path(carpeta, "metricas.csv"), row.names = FALSE)
  
  # Gráficos
  for (e in names(resultados)) {
    ret_acum <- cumprod(1 + rowSums(resultados[[e]]$backtest, na.rm = TRUE))
    
    png(file.path(carpeta, paste0("grafico_", e, ".png")), width = 1200, height = 800)
    plot(ret_acum, main = paste("Curva de Equity -", STRATEGIES[[e]]$name),
         ylab = "Retorno Acumulado", col = "blue", lwd = 2)
    grid()
    dev.off()
  }
  
  message(sprintf("✅ Reportes guardados en: %s", carpeta))
}

# 10. Función Principal -----------------------------------------------------------------------
ejecutar_analisis <- function() {
  message("🚀 Iniciando análisis cuantitativo en Windows...")
  
  # Descargar datos
  simbolos <- unique(unlist(lapply(STRATEGIES, `[[`, "symbols")))
  datos <- descargar_datos(simbolos, PARAMS$fecha_inicio)
  
  resultados <- list()
  
  # Procesar cada estrategia
  for (e in names(STRATEGIES)) {
    message(sprintf("\n🔍 Procesando: %s", STRATEGIES[[e]]$name))
    
    datos_estrategia <- datos[, STRATEGIES[[e]]$symbols]
    
    # Optimización
    parametros <- optimizar_estrategia(datos_estrategia)
    message(sprintf("✅ Parámetros óptimos: MA%d/%d", parametros[1], parametros[2]))
    
    # Entrenamiento
    modelos <- entrenar_modelos(datos_estrategia)
    
    # Backtesting
    backtest <- realizar_backtest(datos_estrategia, modelos, e)
    
    resultados[[e]] <- list(
      parametros = parametros,
      modelos = modelos,
      backtest = backtest
    )
  }
  
  # Generar reportes
  generar_reportes(resultados, "Resultados_Estrategias")
  
  message("\n🎉 Análisis completado exitosamente!")
  return(resultados)
}

# 11. EJECUTAR ---------------------------------------------------------------------------------
# Descomentar la siguiente línea para ejecutar:
resultados <- ejecutar_analisis()
