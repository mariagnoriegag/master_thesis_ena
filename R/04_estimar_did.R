# ==============================================================================
# SCRIPT 04: ESTIMACIÓN DID Y ANÁLISIS DE MECANISMOS DE MERCADO
# Autora: María Noriega
# Proyecto: Master Thesis ENA
# ==============================================================================

library(tidyverse)
library(fixest)
library(here)
library(modelsummary)

estimar_mecanismos_did <- function(panel_analisis, ruta_poblacion_csv = "data_clean/poblacion_distrital.csv") {
  
  # ----------------------------------------------------------------------------
  # 1. INTEGRACIÓN CON POBLACIÓN Y CONSTRUCCIÓN DE VARIABLES DE MERCADO
  # ----------------------------------------------------------------------------
  # Cargar o simular población si no existe el archivo CSV externo
  if(file.exists(here(ruta_poblacion_csv))) {
    poblacion_df <- read_csv(here(ruta_poblacion_csv), show_col_types = FALSE) %>% 
      mutate(
        ubigeo_6dig  = str_pad(as.character(ubigeo_6dig), width = 6, pad = "0"),
        anio_encuesta = as.numeric(anio)
      )
    panel_mkt <- panel_analisis %>% left_join(poblacion_df, by = c("ubigeo_6dig", "anio_encuesta"))
  } else {
    warning("⚠️ No se encontró 'poblacion_distrital.csv'. Se genera proxy de población por UAs.")
    panel_mkt <- panel_analisis %>% mutate(poblacion_total = n_uas_foco * 45) # Proxy demográfico
  }
  
  panel_mkt <- panel_mkt %>%
    mutate(
      # Oferta Agregada (Hectáreas cosechadas del cultivo foco)
      oferta_ha        = sup_cultivo_foco_ha,
      log_oferta_ha    = log(oferta_ha + 1),
      
      # Tamaño del Mercado Proxy (Población en miles)
      poblacion_miles  = poblacion_total / 1000,
      log_poblacion    = log(poblacion_miles + 1),
      
      # OPCIÓN 2: Índice de Presión Demográfica / Oferta
      indice_dem_oferta = ifelse(oferta_ha > 0, poblacion_total / oferta_ha, NA_real_),
      log_indice_mkt    = log(indice_dem_oferta + 1),
      
      # Términos de Interacción Triple (Treat x Post x Mercado)
      treat_post_pob    = tratado_x_post * log_poblacion,
      treat_post_oferta = tratado_x_post * log_oferta_ha,
      treat_post_indice = tratado_x_post * log_indice_mkt
    )
  
  # ----------------------------------------------------------------------------
  # 2. OPCIÓN 1: TAMAÑO DE MERCADO Y OFERTA POR SEPARADO EN LA FÓRMULA
  # ----------------------------------------------------------------------------
  # Precio Chacra = f(Treat x Post, Treat x Post x Pob, Treat x Post x Oferta)
  m1_precio_sep <- feols(
    log_precio_chacra ~ tratado_x_post + treat_post_pob + treat_post_oferta + 
      log_poblacion + log_oferta_ha | ubigeo_6dig + anio_encuesta,
    cluster = ~ubigeo_6dig,
    data = panel_mkt
  )
  
  # Direccionamiento Local (%) = f(Treat x Post, Treat x Post x Pob, Treat x Post x Oferta)
  m1_local_sep <- feols(
    pct_venta_local ~ tratado_x_post + treat_post_pob + treat_post_oferta + 
      log_poblacion + log_oferta_ha | ubigeo_6dig + anio_encuesta,
    cluster = ~ubigeo_6dig,
    data = panel_mkt
  )
  
  # ----------------------------------------------------------------------------
  # 3. OPCIÓN 2: POBLACIÓN / OFERTA PUESTOS COMO ÍNDICE EN LA FÓRMULA
  # ----------------------------------------------------------------------------
  # Precio Chacra = f(Treat x Post, Treat x Post x Índice)
  m2_precio_ind <- feols(
    log_precio_chacra ~ tratado_x_post + treat_post_indice + log_indice_mkt | ubigeo_6dig + anio_encuesta,
    cluster = ~ubigeo_6dig,
    data = panel_mkt
  )
  
  # Direccionamiento Local (%) = f(Treat x Post, Treat x Post x Índice)
  m2_local_ind <- feols(
    pct_venta_local ~ tratado_x_post + treat_post_indice + log_indice_mkt | ubigeo_6dig + anio_encuesta,
    cluster = ~ubigeo_6dig,
    data = panel_mkt
  )
  
  return(list(
    data_mkt          = panel_mkt,
    opcion1_precio    = m1_precio_sep,
    opcion1_local     = m1_local_sep,
    opcion2_precio    = m2_precio_ind,
    opcion2_local     = m2_local_ind
  ))
}