# Portfolio Analysis in R
# Author: Professional Investment Analyst
# Last Update: 2024

# Load required libraries
suppressMessages({
  library(quantmod)
  library(PerformanceAnalytics)
  library(tidyverse)
  library(ggplot2)
  library(scales)
  library(reshape2)
  library(corrplot)
  library(plotly)  # Cargar plotly para gráficos interactivos
})

# Configuration
options(warn = -1)  # Suppress warnings
Sys.setenv(TZ = "UTC")  # Set timezone to UTC for consistency

# Investment strategies and their symbols
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

# Analysis parameters
ANALYSIS_PARAMS <- list(
  start_date = "2000-01-01",
  risk_free_rate = 0.02/252,  # Daily risk-free rate
  rebalancing_frequency = "monthly",
  confidence_level = 0.95,
  rolling_window = 252,
  initial_investment = 100
)

# Function to download and prepare financial data
download_financial_data <- function(symbols, start_date) {
  data <- list()
  
  for (symbol in symbols) {
    tryCatch({
      data_temp <- getSymbols(symbol, 
                              src = "yahoo", 
                              from = start_date, 
                              auto.assign = FALSE)
      
      # Get adjusted prices and calculate returns
      prices <- Ad(data_temp)
      returns <- na.omit(ROC(prices, type = "discrete"))  # Remove NA values
      data[[symbol]] <- returns
      cat(sprintf("✓ Successfully downloaded data for %s\n", symbol))
      
    }, error = function(e) {
      cat(sprintf("✗ Error downloading %s: %s\n", symbol, e$message))
    })
  }
  
  return(data)
}

# Calculate portfolio metrics
calculate_portfolio_metrics <- function(returns) {
  # Remove any remaining NA values
  returns <- na.omit(returns)
  
  # Basic metrics
  metrics <- list()
  metrics$annual_returns <- apply.yearly(returns, Return.cumulative)
  metrics$monthly_returns <- apply.monthly(returns, Return.cumulative)
  metrics$annual_volatility <- StdDev.annualized(returns)
  
  # Calculate Sharpe ratio
  excess_returns <- returns - ANALYSIS_PARAMS$risk_free_rate
  metrics$sharpe_ratio <- apply(excess_returns, 2, function(x) {
    mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(252)
  })
  
  # Other risk metrics
  metrics$sortino_ratio <- apply(returns, 2, function(x) {
    SortinoRatio(x, MAR = ANALYSIS_PARAMS$risk_free_rate)
  })
  metrics$max_drawdown <- apply(returns, 2, maxDrawdown)
  
  # Calculate VaR with NA handling
  metrics$var_95 <- apply(returns, 2, function(x) {
    quantile(x, probs = 0.05, na.rm = TRUE)
  })
  
  # Calculate correlation
  metrics$correlation <- cor(returns, use = "pairwise.complete.obs")
  
  return(metrics)
}

# Generate investment recommendations
generate_recommendations <- function(returns, metrics) {
  # Remove NA values
  recent_returns <- tail(na.omit(returns), 22)
  
  recommendations <- data.frame(
    Symbol = colnames(returns),
    Recent_Return = colMeans(recent_returns, na.rm = TRUE) * 252,
    Annual_Volatility = apply(recent_returns, 2, sd, na.rm = TRUE) * sqrt(252),
    Sharpe = metrics$sharpe_ratio,
    MaxDrawdown = as.numeric(metrics$max_drawdown),
    stringsAsFactors = FALSE
  )
  
  recommendations$Signal <- with(recommendations, 
                                 case_when(
                                   Recent_Return > 0.15 & Sharpe > 1 ~ "Strong Buy",
                                   Recent_Return > 0.10 & Sharpe > 0.5 ~ "Buy",
                                   Recent_Return < -0.10 & MaxDrawdown < -0.15 ~ "Sell",
                                   Recent_Return < -0.05 ~ "Reduce",
                                   TRUE ~ "Hold"
                                 )
  )
  
  return(recommendations)
}

# Rate the strategy
rate_strategy <- function(metrics) {
  score <- 0
  
  # Use mean Sharpe ratio
  mean_sharpe <- mean(metrics$sharpe_ratio, na.rm = TRUE)
  if (!is.na(mean_sharpe) && mean_sharpe > 1.5) score <- score + 30
  
  # Use mean volatility
  mean_vol <- mean(metrics$annual_volatility, na.rm = TRUE)
  if (!is.na(mean_vol) && mean_vol < 0.2) score <- score + 30
  
  # Use mean drawdown
  mean_dd <- mean(metrics$max_drawdown, na.rm = TRUE)
  if (!is.na(mean_dd) && mean_dd > -0.2) score <- score + 20
  
  # Use mean Sortino ratio
  mean_sortino <- mean(metrics$sortino_ratio, na.rm = TRUE)
  if (!is.na(mean_sortino) && mean_sortino > 1) score <- score + 20
  
  rating <- case_when(
    score >= 80 ~ "Excellent",
    score >= 60 ~ "Good",
    score >= 40 ~ "Average",
    TRUE ~ "Poor"
  )
  
  return(rating)
}

# Main execution
main <- function() {
  # Process each strategy
  for (strategy_name in names(STRATEGIES)) {
    strategy <- STRATEGIES[[strategy_name]]
    
    # Download and prepare data
    data <- download_financial_data(strategy$symbols, ANALYSIS_PARAMS$start_date)
    returns <- do.call(merge.xts, data)
    returns <- na.omit(returns)  # Remove any NA rows
    
    # Calculate metrics
    metrics <- calculate_portfolio_metrics(returns)
    
    # Generate recommendations
    recommendations <- generate_recommendations(returns, metrics)
    
    # Print results
    cat("\n=== Strategy:", strategy_name, "===\n")
    print(recommendations)
    cat("\nStrategy Rating:", rate_strategy(metrics), "\n")
    
    # Generate plots
    tryCatch({
      # Gráfico de rendimiento interactivo con plotly
      performance_plot <- charts.PerformanceSummary(returns,
                                                    main = paste("Strategy Performance:", strategy_name),
                                                    color = "blue", plot = FALSE)
      
      # Convertir el gráfico de rendimiento a formato plotly
      performance_plotly <- ggplotly(performance_plot)
      print(performance_plotly)  # Mostrar en la sección de Plots
      
      # Gráfico de correlación interactivo con plotly
      corrplot_data <- corrplot(metrics$correlation, 
                                method = "color", 
                                type = "upper", 
                                title = paste("Correlation Matrix:", strategy_name), 
                                plot = FALSE)
      
      # Convertir el gráfico de correlación a formato plotly
      corrplotly <- ggplotly(corrplot_data)
      print(corrplotly)  # Mostrar en la sección de Plots
      
    }, error = function(e) {
      cat("Error generando gráficos para la estrategia", strategy_name, ":", e$message, "\n")
    })
  }
  
  cat("\nAnálisis completo. Los gráficos se han mostrado en la sección de Plots.\n")
}

# Run the analysis
main()
