# ExportarResumen.R
# Genera un Excel con una hoja por estrategia a partir de los CSV
# recomendaciones_*.csv y crea un README.md con un resumen.

cat("📦 Preparando entorno de exportación...\n")

# Asegurar paquete openxlsx
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  cat("Instalando openxlsx...\n")
  install.packages("openxlsx", dependencies = TRUE, repos = "https://cloud.r-project.org/")
}

suppressPackageStartupMessages(library(openxlsx))

# Buscar CSVs de recomendaciones
csv_files <- list.files(pattern = "^recomendaciones_.*\\.csv$", ignore.case = TRUE)

if (length(csv_files) == 0) {
  cat("❌ No se encontraron archivos recomendaciones_*.csv en el directorio: ", getwd(), "\n", sep = "")
  quit(status = 1)
}

cat(sprintf("✅ Se encontraron %d archivos de recomendaciones\n", length(csv_files)))

# Leer CSVs en lista
rec_list <- list()
for (f in csv_files) {
  strat <- sub("^recomendaciones_(.*)\\.csv$", "\\1", f, ignore.case = TRUE)
  df <- tryCatch({
    read.csv(f, stringsAsFactors = FALSE)
  }, error = function(e) {
    cat(sprintf("⚠️ Error leyendo %s: %s\n", f, e$message))
    NULL
  })
  if (!is.null(df)) {
    rec_list[[strat]] <- df
  }
}

if (length(rec_list) == 0) {
  cat("❌ No se pudo leer ningún CSV de recomendaciones.\n")
  quit(status = 1)
}

# Crear workbook y hojas por estrategia
wb <- createWorkbook()

# También construir un combinado con columna Estrategia
combined <- NULL

for (nm in names(rec_list)) {
  addWorksheet(wb, nm)
  writeData(wb, nm, rec_list[[nm]])
  if (!is.null(rec_list[[nm]]) && nrow(rec_list[[nm]]) > 0) {
    tmp <- rec_list[[nm]]
    tmp$Estrategia <- nm
    combined <- if (is.null(combined)) tmp else rbind(combined, tmp)
  }
}

if (!is.null(combined)) {
  # Reordenar columnas si existen
  cols <- colnames(combined)
  ord <- unique(c("Estrategia", cols))
  combined <- combined[, ord[ord %in% cols], drop = FALSE]
  addWorksheet(wb, "Resumen")
  writeData(wb, "Resumen", combined)
}

excel_name <- "resumen_recomendaciones.xlsx"
saveWorkbook(wb, excel_name, overwrite = TRUE)
cat(sprintf("✅ Excel generado: %s\n", file.path(getwd(), excel_name)))

# Crear README.md con resumen
md <- c(
  "# Resumen de Recomendaciones",
  sprintf("Generado: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Estrategias y archivos",
  paste0("- ", names(rec_list), ": recomendaciones_", names(rec_list), ".csv"),
  "",
  "## Archivo Excel",
  paste0("- ", excel_name, " (hojas por estrategia y hoja 'Resumen')"),
  "",
  "## Gráficos de correlación",
  "Busca archivos correlation_*.png en este directorio."
)

readme_name <- "README.md"
writeLines(md, con = readme_name)
cat(sprintf("✅ README generado: %s\n", file.path(getwd(), readme_name)))

cat("\n🎉 Exportación completada.\n")
