# ==============================================================================
# Script: 04_b_controles_matching.R
# Objetivo: Extraer variables de control distritales del año PRE (2022) - VERSIÓN FINAL AUDITADA
# ==============================================================================

library(tidyverse)
library(haven)

# 1. CARGAR MÓDULOS DE LA ENA 2022 ---------------------------------------------
cat("Cargando módulos de la ENA 2022...\n")
cap100  <- read_sav("data_raw/ENA_2022_Módulos/Cap100_1.sav") %>% rename_with(tolower)
cap1200 <- read_sav("data_raw/ENA_2022_Módulos/Cap1200a.sav") %>% rename_with(tolower)
cap1100 <- read_sav("data_raw/ENA_2022_Módulos/Cap1100.sav")  %>% rename_with(tolower)
cap900  <- read_sav("data_raw/ENA_2022_Módulos/Cap900.sav")   %>% rename_with(tolower)
cap800  <- read_sav("data_raw/ENA_2022_Módulos/Cap800.sav")   %>% rename_with(tolower)

# 2. LIMPIEZA Y COLAPSADO A NIVEL DISTRITAL (UBIGEO) ---------------------------
cat("Procesando y colapsando variables de control...\n")

# Cap 100: Superficie (ha) usando p104_sup_ha
c_superficie <- cap100 %>%
  mutate(ubigeo = paste0(ccdd, ccpp, ccdi)) %>%
  group_by(ubigeo) %>%
  summarise(control_sup_ha = mean(p104_sup_ha, na.rm = TRUE), .groups = 'drop')

# Cap 1200A: Sistema de riego (CORREGIDO: códigos auditados del 2 al 8)
c_riego <- cap1200 %>%
  mutate(
    ubigeo = paste0(ccdd, ccpp, ccdi),
    tiene_riego = if_else(p1206ac >= 2 & p1206ac <= 8, 1, 0, missing = 0)
  ) %>%
  group_by(ubigeo) %>%
  summarise(control_riego = mean(tiene_riego, na.rm = TRUE), .groups = 'drop')

# Cap 1100: Características del Productor (CORREGIDO: p1102 para Jefe y p1105 >= 6 para Secundaria Completa)
c_productor <- cap1100 %>%
  filter(p1102 == 1) %>% 
  mutate(ubigeo = paste0(ccdd, ccpp, ccdi)) %>%
  group_by(ubigeo) %>%
  summarise(
    control_edad = mean(p1104_a, na.rm = TRUE),
    control_educ = mean(if_else(p1105 >= 6, 1, 0, missing = 0), na.rm = TRUE),
    .groups = 'drop'
  )

# Cap 900: Acceso a Financiamiento (p901 == 1 significa Sí)
c_credito <- cap900 %>%
  mutate(
    ubigeo = paste0(ccdd, ccpp, ccdi),
    solicito_credito = if_else(p901 == 1, 1, 0, missing = 0)
  ) %>%
  group_by(ubigeo) %>%
  summarise(control_credito = mean(solicito_credito, na.rm = TRUE), .groups = 'drop')

# Cap 800: Capital Social (p801 == 1 significa Sí)
c_asociacion <- cap800 %>%
  mutate(
    ubigeo = paste0(ccdd, ccpp, ccdi),
    es_asociado = if_else(p801 == 1, 1, 0, missing = 0)
  ) %>%
  group_by(ubigeo) %>%
  summarise(control_asociacion = mean(es_asociado, na.rm = TRUE), .groups = 'drop')

# 3. CONSOLIDACIÓN DE CONTROLES DISTRITALES ------------------------------------
controles_distritales_2022 <- c_superficie %>%
  left_join(c_riego,       by = "ubigeo") %>%
  left_join(c_productor,   by = "ubigeo") %>%
  left_join(c_credito,     by = "ubigeo") %>%
  left_join(c_asociacion,  by = "ubigeo") %>%
  # Imputación segura por medias generales en caso de distritos vacíos
  mutate(across(starts_with("control_"), ~replace_na(.x, mean(.x, na.rm = TRUE))))

# 4. ACOPLAR A LA BASE PRINCIPAL Y GUARDAR -------------------------------------
cat("Acoplando controles a la base de márgenes comerciales...\n")
base_para_did <- readRDS("data/base_para_did_2022_2024.rds")

base_con_controles <- base_para_did %>%
  left_join(controles_distritales_2022, by = "ubigeo")

saveRDS(base_con_controles, "data/base_para_did_2022_2024_controles.rds")

cat("==============================================================\n")
cat("Éxito total: Base de datos construida y auditada.\n")
cat("Archivo guardado en: data/base_para_did_controles.rds\n")
cat("==============================================================\n")