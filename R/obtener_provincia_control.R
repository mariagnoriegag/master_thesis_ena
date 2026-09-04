# ==============================================================================
# TESIS MAESTRÍA UTEC - MARÍA NORIEGA
# MÓDULO AUTOMATIZADO: MATCHING MACRO MAHALANOBIS Y MODELAMIENTO DiD
# ==============================================================================

library(tidyverse)
library(sf)
library(fixest) # Para regresiones de efectos fijos rápidas y robustas

# ------------------------------------------------------------------------------
# FUNCION 1: CALCULO VECTORIZADO DE DISTANCIA DE MAHALANOBIS MACRO
# ------------------------------------------------------------------------------
obtener_provincia_control <- function(df_provincial, ubigeo_tratado_6dig) {
  
  cod_prov_tratada <- substr(ubigeo_tratado_6dig, 1, 4)
  
  # 1. Asegurar matriz limpia sin NA's
  df_limpia <- df_provincial %>%
    drop_na(educativo, dist_lima, dist_cap_region, pct_top_crop, 
            sup_media, sup_desv, n_mercados, n_productores, pvmto_dep)
  
  # 2. Separar objeto tratado y candidatos a control (excluyendo la misma provincia)
  fila_tratada <- df_limpia %>% filter(cod_prov == cod_prov_tratada)
  candidatos   <- df_limpia %>% filter(cod_prov != cod_prov_tratada, flag_intervencion_historica == 0)
  
  if(nrow(fila_tratada) == 0) stop("⚠️ La provincia tratada no se encuentra en la base de datos o tiene valores NA.")
  
  # 3. Matriz de covariables
  vars_matching <- c("educativo", "dist_lima", "dist_cap_region", "pct_top_crop", 
                     "sup_media", "sup_desv", "n_mercados", "n_productores", "pvmto_dep")
  
  mat_covarianzas <- cov(df_limpia %>% select(all_of(vars_matching)))
  inv_cov_mat <- solve(mat_covarianzas)
  
  vec_tratado <- as.numeric(fila_tratada %>% select(all_of(vars_matching)))
  
  # 4. Cálculo iterativo de Mahalanobis
  candidatos <- candidatos %>%
    rowwise() %>%
    mutate(
      vec_cand = list(c_across(all_of(vars_matching))),
      dist_mahalanobis = sqrt(t(vec_cand - vec_tratado) %*% inv_cov_mat %*% (vec_cand - vec_tratado))
    ) %>%
    ungroup() %>%
    arrange(dist_mahalanobis)
  
  provincia_seleccionada <- candidatos %>% slice(1) %>% pull(cod_prov)
  cat(paste0("🎯 Provincia Tratada: ", cod_prov_tratada, " | Provincia Control Elegida: ", provincia_seleccionada, "\n"))
  
  return(list(provincia_control = provincia_seleccionada, ranking_completo = candidatos))
}

# ------------------------------------------------------------------------------
# FUNCION 2: SELECCIÓN DEL DISTRITO FOCAL MICRO Y CREACIÓN DE BUFFERS
# ------------------------------------------------------------------------------
construir_vecindades_estudio <- function(shp_distritos, ubigeo_tratado_6dig, cod_prov_control, radio_km = 15) {
  
  # Transformar a proyectadas UTM 18S (Perú)
  shp_utm <- st_transform(shp_distritos, crs = 32718)
  
  # 1. Objeto Tratado
  distrito_tratado <- shp_utm %>% filter(ubigeo == ubigeo_tratado_6dig)
  centroid_tratado <- st_centroid(distrito_tratado)
  
  # 2. Selección del Distrito Focal en Provincia Control (Criterio: Capital o Cercanía Geométrica)
  distritos_prov_control <- shp_utm %>% filter(substr(ubigeo, 1, 4) == cod_prov_control)
  
  # Por defecto seleccionamos el distrito capital de provincia (termina en '01')
  distrito_focal <- distritos_prov_control %>% filter(substr(ubigeo, 5, 6) == "01")
  if(nrow(distrito_focal) == 0) distrito_focal <- distritos_prov_control %>% slice(1)
  
  centroid_focal <- st_centroid(distrito_focal)
  
  # 3. Trazado de Buffers
  buffer_tratado <- st_buffer(centroid_tratado, dist = radio_km * 1000)
  buffer_control <- st_buffer(centroid_focal, dist = radio_km * 1000)
  
  # 4. Capturar Ubigeos Colindantes
  ubigeos_buffer_tratado <- shp_utm %>% st_filter(buffer_tratado) %>% pull(ubigeo)
  ubigeos_buffer_control <- shp_utm %>% st_filter(buffer_control) %>% pull(ubigeo)
  
  return(list(
    ubigeos_tratados_cluster = ubigeos_buffer_tratado,
    ubigeos_control_cluster = ubigeos_buffer_control,
    ubigeo_focal_centro = distrito_focal %>% pull(ubigeo)
  ))
}

# ------------------------------------------------------------------------------
# FUNCION 3: ESTIMADOR DID VECTORIZADO (OPCIÓN 1 Y OPCIÓN 2)
# ------------------------------------------------------------------------------
estimar_impacto_did <- function(df_panel_ena, var_dep = "log_precio_chacra", opcion_formula = 1) {
  
  # Formulación econométrica dinámica
  if (opcion_formula == 1) {
    # OPCION 1: Heterogeneidad con Interacción Triple
    formula_str <- as.formula(paste0(
      var_dep, " ~ treat * post * poblacion + superficie_cosechada_suma + ",
      "sexo + edad + educacion + asociacion + credito | ubigeo + anio"
    ))
  } else if (opcion_formula == 2) {
    # OPCION 2: Índice de Presión de Mercado (Población / Oferta)
    df_panel_ena <- df_panel_ena %>% 
      mutate(indice_presion_mercado = poblacion / (superficie_cosechada_suma + 0.001))
    
    formula_str <- as.formula(paste0(
      var_dep, " ~ treat * post + indice_presion_mercado + ",
      "sexo + edad + educacion + asociacion + credito | ubigeo + anio"
    ))
  }
  
  # Regresión con Errores Estándar Agrupados (Clustered SE a nivel de Ubigeo)
  modelo <- feols(formula_str, data = df_panel_ena, cluster = ~ubigeo)
  
  return(modelo)
}