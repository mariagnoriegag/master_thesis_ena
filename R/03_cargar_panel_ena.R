# ==============================================================================
# FUNCION 3: CONSOLIDACIÓN DEL PANEL ENA Y VARIABLES DiD
# Autora: María Noriega
# Proyecto: Master Thesis ENA
# ==============================================================================

library(tidyverse)
library(here)

cargar_panel_ena_did <- function(res_buffer, anio_proyecto, ena_pre_year, ena_post_years, dir_ena_base) {
  
  # ----------------------------------------------------------------------------
  # 1. DEFINICIÓN DE MUESTRA Y AÑOS DEL PANEL
  # ----------------------------------------------------------------------------
  ubigeos_tratados <- res_buffer$ubigeos_tratados_buffer
  ubigeos_control  <- res_buffer$ubigeos_control_buffer
  ubigeos_muestra  <- c(ubigeos_tratados, ubigeos_control)
  
  todos_anios <- c(ena_pre_year, ena_post_years)
  
  # Helper para procesar la ENA de un año específico
  procesar_ena_anio <- function(anio) {
    dir_anio <- file.path(dir_ena_base, paste0("ENA_", anio))
    
    # Cargar Módulo 100 (Finca)
    m100 <- cargar_modulo_ena(dir_anio, "Cap100") %>%
      mutate(
        dep_limpio  = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
        pro_limpio  = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
        dis_limpio  = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
        ubigeo_6dig = paste0(dep_limpio, pro_limpio, dis_limpio)
      ) %>%
      filter(ubigeo_6dig %in% ubigeos_muestra)
    
    if(nrow(m100) == 0) return(NULL)
    
    # Cargar Módulo 200 (Cultivos y Producción)
    m200 <- cargar_modulo_ena(dir_anio, "Cap200") %>%
      select(any_of(c("CONGLOMERADO", "NSEC_EST", "VIVIENDA", "HOGAR", 
                      "P204_NOM", "P217_SUP_HA", "P219_PROD_KG", "P223_VALOR_VENTA")))
    
    # Cargar Módulo 1100 (Características del Productor)
    m1100 <- cargar_modulo_ena(dir_anio, "Cap1100") %>%
      filter(P1102 == 1 | is.na(P1102)) %>%
      select(any_of(c("CONGLOMERADO", "NSEC_EST", "VIVIENDA", "HOGAR", "P1104_A", "P1105")))
    
    # Unir módulos a nivel de productor/finca
    panel_sub <- m100 %>%
      left_join(m1100, by = c("CONGLOMERADO", "NSEC_EST", "VIVIENDA", "HOGAR")) %>%
      left_join(m200, by = c("CONGLOMERADO", "NSEC_EST", "VIVIENDA", "HOGAR")) %>%
      mutate(anio_encuesta = anio)
    
    return(panel_sub)
  }
  
  # ----------------------------------------------------------------------------
  # 2. CONSOLIDACIÓN MULTIANUAL Y CREACIÓN DE VARIABLES DiD
  # ----------------------------------------------------------------------------
  panel_raw <- map_dfr(todos_anios, procesar_ena_anio)
  
  panel_did <- panel_raw %>%
    mutate(
      # Asignación de Tratamiento
      tratado = ifelse(ubigeo_6dig %in% ubigeos_tratados, 1, 0),
      
      # Asignación de Temporalidad Post-Intervención
      post = ifelse(anio_encuesta > anio_proyecto, 1, 0),
      
      # Término de Interacción DiD
      tratado_x_post = tratado * post,
      
      # Outcomes clave
      rendimiento_ha = ifelse(P217_SUP_HA > 0, P219_PROD_KG / P217_SUP_HA, NA_real_),
      ingreso_ventas = replace_na(as.numeric(P223_VALOR_VENTA), 0)
    )
  
  return(panel_did)
}