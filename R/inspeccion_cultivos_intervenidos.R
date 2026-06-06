# ==========================================================================
# SCRIPT DE AUDITORÍA ACTUALIZADO: TOP 2 CON COLUMNAS DE INTERVENCIÓN (RAW)
# Tesis de Maestría: María Noriega / UTEC
# ==========================================================================

library(tidyverse)
library(haven)
library(stringr)

exportar_top2_con_ventanas_raw <- function() {
  cat("\n=========================================================================\n")
  cat(" 🕵️‍♂️ GENERANDO TABLA TOP 2 CON COLUMNAS DE INTERVENCIÓN (DATA RAW)\n")
  cat("=========================================================================\n")
  
  if(!dir.exists("data_audit")) dir.create("data_audit")
  
  # Diccionario oficial ajustado por el equipo
  nombres_ubigeos <- c(
    "020108" = "Olleros (Huaraz, Ancash)",
    "040520" = "Majes (Caylloma, Arequipa)",
    "040124" = "Uchumayo (Arequipa, Arequipa)",
    "211207" = "San Juan del Oro (Sandia, Puno)"
  )
  
  # ------------------------------------------------------------------------
  # 1. PROCESAMIENTO: INTERVENCIÓN 2018 (VENTANA 2017 VS 2019)
  # ------------------------------------------------------------------------
  ubigeos_est <- c("020108", "040520")
  cat("实时 [1/2] Extrayendo y rankeando Intervención 2018 (2017 vs 2019)...\n")
  
  raw_2017 <- read_sav("data_raw/ENA_2017_Módulos/Cap200ab.sav") %>% rename_with(toupper) %>%
    mutate(ubigeo = paste0(str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
                           str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
                           str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0")),
           nombre_cultivo = tolower(str_trim(P204_NOM))) %>%
    filter(ubigeo %in% ubigeos_est) %>%
    count(ubigeo, nombre_cultivo, name = "freq_2017_pre")
  
  raw_2019 <- read_sav("data_raw/ENA_2019_Módulos/Cap200ab.sav") %>% rename_with(toupper) %>%
    mutate(ubigeo = paste0(str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
                           str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
                           str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0")),
           nombre_cultivo = tolower(str_trim(P204_NOM))) %>%
    filter(ubigeo %in% ubigeos_est) %>%
    count(ubigeo, nombre_cultivo, name = "freq_2019_post")
  
  # Unificar horizontalmente y filtrar el Top 2 basado en el año base
  panel_est_top2 <- full_join(raw_2017, raw_2019, by = c("ubigeo", "nombre_cultivo")) %>%
    mutate(distrito_nombre = nombres_ubigeos[ubigeo],
           intervencion = "Intervención 2018",
           v_orden = coalesce(freq_2017_pre, 0)) %>%
    group_by(ubigeo) %>%
    slice_max(order_by = v_orden, n = 2, with_ties = FALSE) %>%
    mutate(ranking = row_number()) %>%
    ungroup() %>%
    select(intervencion, ubigeo, distrito_nombre, ranking, nombre_cultivo, freq_año_previo = freq_2017_pre, freq_año_post = freq_2019_post)
  
  # ------------------------------------------------------------------------
  # 2. PROCESAMIENTO: INTERVENCIÓN 2023 (VENTANA 2022 VS 2024)
  # ------------------------------------------------------------------------
  ubigeos_rec <- c("040124", "211207")
  cat("实时 [2/2] Extrayendo y rankeando Intervención 2023 (2022 vs 2024)...\n")
  
  raw_2022 <- read_sav("data_raw/ENA_2022_Módulos/Cap200ab.sav") %>% rename_with(toupper) %>%
    mutate(ubigeo = paste0(str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
                           str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
                           str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0")),
           nombre_cultivo = tolower(str_trim(P204_NOM))) %>%
    filter(ubigeo %in% ubigeos_rec) %>%
    count(ubigeo, nombre_cultivo, name = "freq_2022_pre")
  
  raw_2024 <- read_sav("data_raw/ENA_2024_Módulos/Cap200ab.sav") %>% rename_with(toupper) %>%
    mutate(ubigeo = paste0(str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
                           str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
                           str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0")),
           nombre_cultivo = tolower(str_trim(P204_NOM))) %>%
    filter(ubigeo %in% ubigeos_rec) %>%
    count(ubigeo, nombre_cultivo, name = "freq_2024_post")
  
  # Unificar horizontalmente y filtrar el Top 2 basado en el año base
  panel_rec_top2 <- full_join(raw_2022, raw_2024, by = c("ubigeo", "nombre_cultivo")) %>%
    mutate(distrito_nombre = nombres_ubigeos[ubigeo],
           intervencion = "Intervención 2023",
           v_orden = coalesce(freq_2022_pre, 0)) %>%
    group_by(ubigeo) %>%
    slice_max(order_by = v_orden, n = 2, with_ties = FALSE) %>%
    mutate(ranking = row_number()) %>%
    ungroup() %>%
    select(intervencion, ubigeo, distrito_nombre, ranking, nombre_cultivo, freq_año_previo = freq_2022_pre, freq_año_post = freq_2024_post)
  
  # ------------------------------------------------------------------------
  # 3. CONSOLIDACIÓN Y EXPORTACIÓN MATRICIAL DEFINITIVA
  # ------------------------------------------------------------------------
  tabla_final_intervenciones <- bind_rows(panel_est_top2, panel_rec_top2) %>%
    arrange(intervencion, ubigeo, ranking)
  
  # Guardar como CSV físico en tu MacBook
  write_csv(tabla_final_intervenciones, "data_audit/top2_ventanas_pre_post_raw.csv")
  
  cat("\n🎉 [ÉXITO] Tabla reestructurada por tipo de Intervención en: 'data_audit/top2_ventanas_pre_post_raw.csv'\n\n")
  
  # Mostrar el data frame estructurado en la consola de RStudio de forma impecable
  print(as.data.frame(tabla_final_intervenciones))
}

# Ejecución automática
exportar_top2_con_ventanas_raw()