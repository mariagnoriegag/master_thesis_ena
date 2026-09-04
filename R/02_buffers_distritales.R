# ==============================================================================
# FUNCION 2: MATCHING MICRO Y COMPARACIÓN DE BUFFERS ESPACIALES
# Autora: María Noriega
# Proyecto: Master Thesis ENA
# ==============================================================================

library(tidyverse)
library(sf)
library(stringr)
library(here)

obtener_distritos_buffer <- function(ubigeo_tratado, cod_prov_control, radio_km = 30, res_macro) {
  
  # ----------------------------------------------------------------------------
  # 1. CARGA DE SHAPEFILE Y EXTRACCIÓN ROBUSTA DE NOMBRES GEOGRÁFICOS
  # ----------------------------------------------------------------------------
  ruta_shp <- here("data_spatial", "Limite Distrital INEI 2025 CPV.shp")
  if (!file.exists(ruta_shp)) stop("⚠️ No se encontró el archivo .shp en 'data_spatial/'.")
  
  mapa_raw <- st_read(ruta_shp, quiet = TRUE)
  
  mapa_prep <- mapa_raw %>% 
    rename_with(~ toupper(.x), .cols = -where(~ inherits(.x, "sfc")))
  
  nombres_cols <- names(mapa_prep)
  
  # Expresiones regulares ampliadas para capturar NOMBDEP, NOMBPROV, NOMBDIST o DEPARTAMEN
  col_dis <- nombres_cols[str_detect(nombres_cols, "^NOMBDIST$|^NOMBREDI$|^NOMDIS$|^DISTRITO$|^NOM_DIS$")][1]
  col_pro <- nombres_cols[str_detect(nombres_cols, "^NOMBPROV$|^NOMBREPV$|^NOMPR$|^PROVINCIA$|^NOM_PROV$")][1]
  col_dep <- nombres_cols[str_detect(nombres_cols, "^NOMBDEP$|^DEPARTAMEN$|^NOMBREDD$|^NOMBDD$|^DEPARTAMENTO$")][1]
  
  peru_distritos <- mapa_prep %>%
    st_transform(crs = 32718) %>%
    mutate(
      dep_limpio  = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
      pro_limpio  = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
      dis_limpio  = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
      ubigeo_6dig = paste0(dep_limpio, pro_limpio, dis_limpio),
      cod_prov    = paste0(dep_limpio, pro_limpio),
      
      nom_distrito     = if(!is.na(col_dis)) as.character(.data[[col_dis]]) else dis_limpio,
      nom_provincia    = if(!is.na(col_pro)) as.character(.data[[col_pro]]) else pro_limpio,
      nom_departamento = if(!is.na(col_dep)) as.character(.data[[col_dep]]) else dep_limpio,
      
      distrito_nombre_completo = paste0(nom_distrito, " (", nom_provincia, ", ", nom_departamento, ")")
    )
  
  radio_m <- radio_km * 1000
  
  obtener_ubigeos_en_radio <- function(ubigeo_centro) {
    dist_sf <- peru_distritos %>% filter(ubigeo_6dig == ubigeo_centro)
    cent    <- suppressWarnings(st_centroid(dist_sf))
    buf     <- st_buffer(cent, dist = radio_m)
    
    suppressWarnings({
      st_intersection(peru_distritos, buf) %>% 
        st_drop_geometry() %>% 
        pull(ubigeo_6dig) %>% 
        unique()
    })
  }
  
  # ----------------------------------------------------------------------------
  # 2. GENERACIÓN DE BUFFERS Y SELECCIÓN MICRO CON DISTANCIA DE MAHALANOBIS
  # ----------------------------------------------------------------------------
  ubigeos_tr_buffer <- obtener_ubigeos_en_radio(ubigeo_tratado)
  
  distritos_prov_control <- peru_distritos %>% 
    filter(cod_prov == cod_prov_control) %>% 
    st_drop_geometry() %>% 
    pull(ubigeo_6dig) %>% 
    unique()
  
  buffers_candidatos <- map(distritos_prov_control, obtener_ubigeos_en_radio)
  names(buffers_candidatos) <- distritos_prov_control
  
  matriz_prov_df <- res_macro$matriz_prov
  vars_match     <- res_macro$vars_match
  
  agregar_covariables_buffer <- function(vector_ubigeos) {
    provincias_en_buffer <- unique(substr(vector_ubigeos, 1, 4))
    matriz_prov_df %>%
      filter(cod_prov %in% provincias_en_buffer) %>%
      summarise(across(all_of(vars_match), ~ mean(.x, na.rm = TRUE)))
  }
  
  cov_buffer_tratado  <- agregar_covariables_buffer(ubigeos_tr_buffer)
  cov_buffers_control <- map_dfr(buffers_candidatos, agregar_covariables_buffer, .id = "ubigeo_centro_control")
  
  mat_data <- cov_buffers_control %>% select(all_of(vars_match))
  mat_cov  <- cov(mat_data)
  inv_cov  <- tryCatch(solve(mat_cov), error = function(e) solve(mat_cov + diag(1e-5, ncol(mat_data))))
  
  vec_tratado <- as.numeric(cov_buffer_tratado)
  
  ranking_micro <- cov_buffers_control %>%
    rowwise() %>%
    mutate(
      vec_cand = list(c_across(all_of(vars_match))),
      d_mahalanobis_micro = sqrt(t(vec_cand - vec_tratado) %*% inv_cov %*% (vec_cand - vec_tratado)),
      n_distritos = length(buffers_candidatos[[ubigeo_centro_control]])
    ) %>%
    ungroup() %>%
    arrange(d_mahalanobis_micro)
  
  ubigeo_centro_elegido <- ranking_micro %>% slice(1) %>% pull(ubigeo_centro_control)
  ubigeos_ct_buffer     <- buffers_candidatos[[ubigeo_centro_elegido]]
  
  # ----------------------------------------------------------------------------
  # 3. CONSOLIDACIÓN DE RESULTADOS Y NOMBRES GEOGRÁFICOS
  # ----------------------------------------------------------------------------
  df_nombres <- peru_distritos %>% 
    st_drop_geometry() %>% 
    select(ubigeo_6dig, distrito_nombre_completo) %>% 
    distinct()
  
  nom_foco_tratado <- df_nombres %>% filter(ubigeo_6dig == ubigeo_tratado) %>% pull(distrito_nombre_completo)
  nom_foco_control <- df_nombres %>% filter(ubigeo_6dig == ubigeo_centro_elegido) %>% pull(distrito_nombre_completo)
  
  df_tratados_nom <- tibble(ubigeo_6dig = ubigeos_tr_buffer, Grupo = "TRATADO / DERRAME (30 km)") %>% 
    left_join(df_nombres, by = "ubigeo_6dig")
  
  df_control_nom  <- tibble(ubigeo_6dig = ubigeos_ct_buffer, Grupo = "CONTROL GEMELO (30 km)") %>% 
    left_join(df_nombres, by = "ubigeo_6dig")
  
  tabla_comparativa_distritos <- bind_rows(df_tratados_nom, df_control_nom) %>% 
    arrange(Grupo, ubigeo_6dig)
  
  return(list(
    ubigeo_foco                 = ubigeo_tratado,
    distrito_foco_nom           = if(length(nom_foco_tratado)>0) nom_foco_tratado else ubigeo_tratado,
    n_distritos_tratados        = length(ubigeos_tr_buffer),
    
    distrito_control_focal      = ubigeo_centro_elegido,
    distrito_control_nom        = if(length(nom_foco_control)>0) nom_foco_control else ubigeo_centro_elegido,
    n_distritos_control         = length(ubigeos_ct_buffer),
    
    ubigeos_tratados_buffer     = ubigeos_tr_buffer,
    ubigeos_control_buffer      = ubigeos_ct_buffer,
    tabla_comparativa_distritos = tabla_comparativa_distritos,
    ranking_micro_buffers       = ranking_micro %>% 
      left_join(df_nombres, by = c("ubigeo_centro_control" = "ubigeo_6dig")) %>% 
      select(distrito_nombre_completo, d_mahalanobis_micro, n_distritos, everything())
  ))
}