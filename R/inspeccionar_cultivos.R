# ==========================================================================
# SCRIPT DE AUDITORÍA: INSPECCIÓN DE CULTIVOS COMPLETA (3 PANELES)
# Tesis de Maestría: María Noriega / UTEC
# ==========================================================================

library(tidyverse)

inspeccionar_tres_paneles <- function() {
  cat("\n=======================================================\n")
  cat(" 🕵️‍♂️ AUDITORÍA DE TEXTO: EXTRACCIÓN DE CULTIVOS FINALES\n")
  cat("=======================================================\n")
  
  # Rutas de los tres paneles de salida creados por el pipeline
  ruta_17_19 <- "data/base_para_did_2017_2019.rds"
  ruta_18_22 <- "data/base_para_did_2018_2022.rds"
  ruta_22_24 <- "data/base_para_did_2022_2024.rds"
  
  # --- PANEL 1: 2017 - 2019 ---
  if(file.exists(ruta_17_19)) {
    p17_19 <- readRDS(ruta_17_19)
    cat(paste("1. Panel 2017-2019 cargado con:", nrow(p17_19), "observaciones.\n"))
    cat("   Top 15 cultivos únicos encontrados (Nominal Puro):\n")
    print(p17_19 %>% count(nombre_cultivo, sort = TRUE) %>% head(15))
    cat("\n")
  } else { cat("⚠️ Archivo 2017-2019 no encontrado en /data.\n\n") }
  
  # --- PANEL 2: 2018 - 2022 ---
  if(file.exists(ruta_18_22)) {
    p18_22 <- readRDS(ruta_18_22)
    cat(paste("2. Panel 2018-2022 cargado con:", nrow(p18_22), "observaciones.\n"))
    cat("   Top 15 cultivos únicos encontrados (Nominal Puro):\n")
    print(p18_22 %>% count(nombre_cultivo, sort = TRUE) %>% head(15))
    cat("\n")
  } else { cat("⚠️ Archivo 2018-2022 no encontrado en /data.\n\n") }
  
  # --- PANEL 3: 2022 - 2024 ---
  if(file.exists(ruta_22_24)) {
    p22_24 <- readRDS(ruta_22_24)
    cat(paste("3. Panel 2022-2024 cargado con:", nrow(p22_24), "observaciones.\n"))
    cat("   Top 15 cultivos únicos encontrados (Nominal Puro):\n")
    print(p22_24 %>% count(nombre_cultivo, sort = TRUE) %>% head(15))
    cat("\n")
  } else { cat("⚠️ Archivo 2022-2024 no encontrado en /data.\n\n") }
  
  cat("=======================================================\n")
}

# Ejecutar la inspección directamente
inspeccionar_tres_paneles()