# ==========================================================================
# SCRIPT DE DEBUG: AUDITORÍA DE TIPOS DE DATOS GEOGRÁFICOS EN LA ENA
# Tesis de Maestría: María Noriega / UTEC
# ==========================================================================

library(tidyverse)
library(haven)

debug_modulos_crudos <- function(anio) {
  cat(paste("\n=======================================================\n"))
  cat(paste(" RUNNING DEBUG FOR ENA YEAR:", anio, "\n"))
  cat(paste("=======================================================\n"))
  
  folder_name <- paste0("data_raw/ENA_", anio, "_Módulos/")
  
  cap100_path  <- paste0(folder_name, if(anio == 2024) "Cap100a_1.sav" else "Cap100_1.sav")
  cap200_path  <- paste0(folder_name, "Cap200ab.sav")
  spatial_path <- paste0(folder_name, if(anio == 2024) "USOSTIERRA.sav" else "Espacial.sav")
  
  # 1. Carga individual sin cruces
  cat("-> Cargando Capítulo 100...\n")
  c100 <- read_sav(cap100_path)
  names(c100) <- toupper(names(c100))
  
  cat("-> Cargando Capítulo 200...\n")
  c200 <- read_sav(cap200_path)
  names(c200) <- toupper(names(c200))
  
  cat("-> Cargando Módulo Espacial/Suelos...\n")
  cspacial <- read_sav(spatial_path)
  names(cspacial) <- toupper(names(cspacial))
  
  # 2. INSPECCIÓN DE TIPOS DE LA VARIABLE 'REGION'
  cat("\n--- DETECCION DE TIPOS ORIGINALES DE LA COLUMNA 'REGION' ---\n")
  
  if("REGION" %in% names(c100)) {
    cat("Cap100$REGION class:", class(c100$REGION), "| Type:", typeof(c100$REGION), "\n")
  } else { cat("AVISO: 'REGION' no existe en Cap100\n") }
  
  if("REGION" %in% names(c200)) {
    cat("Cap200$REGION class:", class(c200$REGION), "| Type:", typeof(c200$REGION), "\n")
  } else { cat("AVISO: 'REGION' no existe en Cap200\n") }
  
  if("REGION" %in% names(cspacial)) {
    cat("Espacial$REGION class:", class(cspacial$REGION), "| Type:", typeof(cspacial$REGION), "\n")
  } else { cat("AVISO: 'REGION' no existe en Módulo Espacial\n") }
  
  # 3. PROBAR PRIMER JOIN (Cap100 + Cap200)
  cat("\n--- Evaluando cruce 1: Cap100 + Cap200 ---\n")
  inter_1 <- intersect(names(c100), names(c200))
  cat("Llaves encontradas para el cruce:", paste(inter_1, collapse = ", "), "\n")
  
  tryCatch({
    join1 <- c100 %>% left_join(c200, by = inter_1)
    cat("¡ÉXITO! Cruce 1 completado sin caídas. Filas resultantes:", nrow(join1), "\n")
  }, error = function(e) {
    cat("ERR_CRITICO en Cruce 1:", e$message, "\n")
  })
  
  # 4. PROBAR SEGUNDO JOIN (Resultado anterior + Espacial)
  if(exists("join1")) {
    cat("\n--- Evaluando cruce 2: (Cap100+Cap200) + Espacial ---\n")
    inter_2 <- intersect(names(join1), names(cspacial))
    cat("Llaves encontradas para el cruce:", paste(inter_2, collapse = ", "), "\n")
    
    tryCatch({
      join2 <- join1 %>% left_join(cspacial, by = inter_2)
      cat("¡ÉXITO! Cruce 2 completado sin caídas. Filas resultantes:", nrow(join2), "\n")
    }, error = function(e) {
      cat("ERR_CRITICO en Cruce 2:", e$message, "\n")
    })
  }
  
  cat("\n=======================================================\n")
}