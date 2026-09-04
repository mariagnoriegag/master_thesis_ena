# ==============================================================================
# FUNCION 1: MATCHING MACRO PROVINCIAL (DISTANCIA DE MAHALANOBIS COMPLETA)
# Autora: María Noriega
# Proyecto: Master Thesis ENA
# ==============================================================================

library(tidyverse)
library(haven)
library(sf)
library(stringr)
library(here)

# ------------------------------------------------------------------------------
# HELPER: CARGA Y UNIFICACIÓN AUTOMÁTICA DE MÓDULOS FRAGMENTADOS DE LA ENA
# ------------------------------------------------------------------------------
cargar_modulo_ena <- function(directorio, prefijo_cap) {
  patron <- paste0("^", prefijo_cap, ".*\\.[sS][aA][vV]$")
  archivos <- list.files(directorio, pattern = patron, full.names = TRUE)
  
  if (length(archivos) == 0) {
    stop(paste("⚠️ No se encontraron archivos para el módulo", prefijo_cap, "en:", directorio))
  }
  
  df_unificado <- archivos %>% 
    map_dfr(~ read_sav(.x) %>% rename_with(toupper))
  
  return(df_unificado)
}

# ------------------------------------------------------------------------------
# FUNCIÓN PRINCIPAL
# ------------------------------------------------------------------------------
obtener_provincia_control <- function(ubigeo_tratado, anio_proyecto, cultivo_foco, ena_pre_year) {
  
  cod_prov_tratada <- substr(ubigeo_tratado, 1, 4)
  
  # ----------------------------------------------------------------------------
  # 1. CARGA Y AGREGACIÓN DE FUENTES EXTERNAS
  # ----------------------------------------------------------------------------
  # A. Pavimentación Vecinal por Departamento
  df_pvmto  <- read_csv(here("data_external", "CCDD - PORCENTAJE PAVIMENTACION VECINAL.csv"), show_col_types = FALSE) %>% 
    rename_with(tolower) %>% 
    mutate(cod_dep = str_pad(as.character(as.numeric(cod_dd)), width = 2, pad = "0"))
  
  col_pvmto_target <- paste0("pct_pav_vecinal_", ena_pre_year)
  
  agg_pvmto <- df_pvmto %>%
    mutate(
      pct_pavimentacion_vecinal = if(col_pvmto_target %in% names(.)) .[[col_pvmto_target]] else .[[3]]
    ) %>%
    mutate(pct_pavimentacion_vecinal = as.numeric(pct_pavimentacion_vecinal)) %>%
    select(cod_dep, pct_pavimentacion_vecinal)
  
  # B. Intervenciones Ubigeo
  df_interv <- read_csv(here("data_external", "BD_INTERVENCIONES_UBIGEO_2018_2024.csv"), show_col_types = FALSE) %>% 
    rename_with(tolower) %>% 
    mutate(
      ubigeo   = str_pad(as.character(as.numeric(ubigeo)), width = 6, pad = "0"),
      cod_prov = substr(ubigeo, 1, 4)
    )
  
  # C. Mercados Preexistentes
  df_mercados <- read_csv(here("data_external", "UBIGEO - N MERCADOS PREEXISTENTES.csv"), show_col_types = FALSE) %>% 
    rename_with(tolower) %>% 
    mutate(ubigeo = str_pad(as.character(as.numeric(ubigeo)), width = 6, pad = "0"))
  
  col_mercado_target <- paste0("n_mercados_", ena_pre_year)
  
  agg_mercados <- df_mercados %>%
    mutate(
      cod_prov = substr(ubigeo, 1, 4),
      n_mercados_val = if(col_mercado_target %in% names(.)) .[[col_mercado_target]] else .[[2]]
    ) %>%
    mutate(n_mercados_val = as.numeric(str_replace_all(as.character(n_mercados_val), ",", ""))) %>%
    mutate(n_mercados_val = replace_na(n_mercados_val, 0)) %>%
    group_by(cod_prov) %>%
    summarise(n_mercados = sum(n_mercados_val, na.rm = TRUE), .groups = 'drop')
  
  # D. Agricultores por Tipo de Personería
  df_agric <- read_csv(here("data_external", "UBIGEO -  N AGRICULTORES.csv"), show_col_types = FALSE) %>% 
    rename_with(tolower) %>% 
    mutate(ubigeo = str_pad(as.character(as.numeric(ubigeo)), width = 6, pad = "0"))
  
  agg_agric <- df_agric %>%
    mutate(cod_prov = substr(ubigeo, 1, 4)) %>%
    mutate(across(-c(ubigeo, cod_prov), ~ as.numeric(str_replace_all(as.character(.), ",", "")))) %>%
    mutate(across(-c(ubigeo, cod_prov), ~ replace_na(., 0))) %>%
    rowwise() %>%
    mutate(total_dist_agric = sum(c_across(-c(ubigeo, cod_prov)))) %>%
    ungroup() %>%
    group_by(cod_prov) %>%
    summarise(n_productores = sum(total_dist_agric, na.rm = TRUE), .groups = 'drop')
  
  # E. Mapa Distrital INEI (Límites Distritales 2025)
  ruta_shp <- here("data_spatial", "Limite Distrital INEI 2025 CPV.shp")
  mapa_raw <- st_read(ruta_shp, quiet = TRUE) %>% rename_with(toupper)
  
  mapa_peru <- mapa_raw %>%
    st_transform(crs = 32718) %>%
    mutate(
      ubigeo   = paste0(str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
                        str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
                        str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0")),
      cod_prov = substr(ubigeo, 1, 4),
      cod_dep  = substr(ubigeo, 1, 2)
    )
  
  # ----------------------------------------------------------------------------
  # 2. PROCESAMIENTO DE MÓDULOS ENA (NIVEL PROVINCIAL)
  # ----------------------------------------------------------------------------
  dir_ena_pre <- here("data_raw", paste0("ENA_", ena_pre_year, "_Módulos"))
  
  # A. Cap100: Superficie Finca (sup_media, sup_desv)
  m100_prov <- cargar_modulo_ena(dir_ena_pre, "Cap100") %>%
    mutate(
      dep_limpio = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
      pro_limpio = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
      cod_prov   = paste0(dep_limpio, pro_limpio),
      sup_total  = as.numeric(P104_SUP_HA)
    ) %>%
    group_by(cod_prov) %>%
    summarise(
      sup_media = mean(sup_total, na.rm = TRUE),
      sup_desv  = sd(sup_total, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(sup_desv = ifelse(is.na(sup_desv), 0, sup_desv))
  
  # B. Cap200ab: Cultivos (% top crop, % cosechada/sembrada)
  m200_prov <- cargar_modulo_ena(dir_ena_pre, "Cap200") %>%
    mutate(
      dep_limpio     = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
      pro_limpio     = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
      dis_limpio     = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
      ubigeo         = paste0(dep_limpio, pro_limpio, dis_limpio),
      cod_prov       = paste0(dep_limpio, pro_limpio),
      nombre_cultivo = tolower(str_trim(P204_NOM)),
      es_foco        = ifelse(nombre_cultivo == tolower(str_trim(cultivo_foco)), 1, 0),
      sup_sembrada   = as.numeric(P210_SUP_1) + (as.numeric(P210_SUP_2) / 100),
      sup_cosechada  = as.numeric(P217_SUP_HA)
    ) %>%
    group_by(cod_prov) %>%
    summarise(
      pct_top_crop = mean(es_foco, na.rm = TRUE) * 100,
      pct_sup_cosechada_sembrada = (sum(sup_cosechada, na.rm = TRUE) / (sum(sup_sembrada, na.rm = TRUE) + 0.001)) * 100,
      .groups = 'drop'
    )
  
  # C. Cap1100: Controles Sociodemográficos (educ, sexo, edad)
  m1100_raw <- cargar_modulo_ena(dir_ena_pre, "Cap1100") %>%
    select(-any_of("REGION")) %>%
    mutate(
      dep_limpio = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
      pro_limpio = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
      cod_prov   = paste0(dep_limpio, pro_limpio)
    )
  
  # Detectar dinámicamente la columna de edad
  col_edad <- names(m1100_raw)[str_detect(names(m1100_raw), "^P1104_A$|^P1104A$|^P1104$|^EDAD$")][1]
  
  # Si existe la columna P1102, priorizar informante principal; de lo contrario, usar todo el modulo
  if("P1102" %in% names(m1100_raw)) {
    m1100_filtrado <- m1100_raw %>% filter(P1102 == 1 | is.na(P1102))
    # Fallback: Si el filtro vacía alguna provincia, regresar a los datos sin filtrar
    if(nrow(m1100_filtrado) < 50) m1100_filtrado <- m1100_raw
  } else {
    m1100_filtrado <- m1100_raw
  }
  
  m1100_prov <- m1100_filtrado %>%
    mutate(edad_val = as.numeric(.data[[col_edad]])) %>%
    group_by(cod_prov) %>%
    summarise(
      control_educ = mean(as.numeric(P1105), na.rm = TRUE),
      control_sexo = (sum(P1103 == 2, na.rm = TRUE) / sum(P1103 %in% c(1, 2), na.rm = TRUE)) * 100,
      control_edad = mean(edad_val, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Media nacional de respaldo para imputar provincias sin registros válidos
  media_nac_edad <- mean(m1100_prov$control_edad, na.rm = TRUE)
  media_nac_educ <- mean(m1100_prov$control_educ, na.rm = TRUE)
  
  m1100_prov <- m1100_prov %>%
    mutate(
      control_edad = ifelse(is.na(control_edad) | is.nan(control_edad), media_nac_edad, control_edad),
      control_educ = ifelse(is.na(control_educ) | is.nan(control_educ), media_nac_educ, control_educ)
    )
  
  # ----------------------------------------------------------------------------
  # 3. GEOMETRÍA: CENTROIDES, DISTANCIA ENTRE PROVINCIAS Y DISTANCIAS A LIMA/CAPITAL
  # ----------------------------------------------------------------------------
  st_geometry(mapa_peru) <- "geometry"
  
  prov_centroides <- mapa_peru %>%
    group_by(cod_prov, cod_dep) %>%
    summarise(geometry = st_union(geometry), .groups = 'drop') %>%
    st_centroid()
  
  cent_lima    <- prov_centroides %>% filter(cod_prov == "1501") %>% st_geometry()
  cent_tratado <- prov_centroides %>% filter(cod_prov == cod_prov_tratada) %>% st_geometry()
  cod_dep_tratado <- substr(ubigeo_tratado, 1, 2)
  
  distancias_prov <- prov_centroides %>%
    rowwise() %>%
    mutate(
      # Distancia en km desde cada provincia candidata hacia la provincia tratada
      dist_prov_tratada = as.numeric(st_distance(geometry, cent_tratado) / 1000),
      dist_lima         = as.numeric(st_distance(geometry, cent_lima) / 1000),
      dist_cap_region   = as.numeric(st_distance(
        geometry, 
        st_centroid(mapa_peru %>% filter(ubigeo == paste0(cod_dep, "0101")) %>% slice(1))
      ) / 1000)
    ) %>%
    st_drop_geometry()
  
  # ----------------------------------------------------------------------------
  # 4. CONSOLIDACIÓN DE MATRIZ Y MATCHING (OTRO DEPARTAMENTO)
  # ----------------------------------------------------------------------------
  prov_intervenidas <- df_interv %>% 
    filter(anio == anio_proyecto) %>% 
    pull(cod_prov) %>% 
    unique()
  
  matriz_prov <- distancias_prov %>%
    left_join(m1100_prov, by = "cod_prov") %>%
    left_join(m200_prov, by = "cod_prov") %>%
    left_join(m100_prov, by = "cod_prov") %>%
    left_join(agg_mercados, by = "cod_prov") %>%
    left_join(agg_agric, by = "cod_prov") %>%
    left_join(agg_pvmto, by = "cod_dep") %>%
    mutate(
      flag_intervencion = ifelse(cod_prov %in% prov_intervenidas, 1, 0),
      n_mercados = ifelse(is.na(n_mercados), 0, n_mercados),
      n_productores = ifelse(is.na(n_productores), 0, n_productores),
      pct_pavimentacion_vecinal = ifelse(is.na(pct_pavimentacion_vecinal), 0, pct_pavimentacion_vecinal)
    ) %>%
    drop_na()
  
  vars_match <- c("control_educ", "pct_top_crop", "sup_media", "sup_desv", 
                  "pct_sup_cosechada_sembrada", "dist_lima", "dist_cap_region", 
                  "n_mercados", "n_productores", "pct_pavimentacion_vecinal")
  
  mat_cov <- cov(matriz_prov %>% select(all_of(vars_match)))
  inv_cov <- solve(mat_cov)
  
  vec_tratado <- as.numeric(matriz_prov %>% filter(cod_prov == cod_prov_tratada) %>% select(all_of(vars_match)))
  
  # Filtro: Excluir misma provincia, provincias intervenidas Y MISMO DEPARTAMENTO
  candidatas <- matriz_prov %>%
    filter(cod_prov != cod_prov_tratada, 
           cod_dep != cod_dep_tratado,  # <--- Garantiza otro departamento
           flag_intervencion == 0) %>%
    rowwise() %>%
    mutate(
      vec_cand = list(c_across(all_of(vars_match))),
      d_mahalanobis = sqrt(t(vec_cand - vec_tratado) %*% inv_cov %*% (vec_cand - vec_tratado))
    ) %>%
    ungroup() %>%
    arrange(d_mahalanobis)
  
  prov_control_elegida <- candidatas %>% slice(1) %>% pull(cod_prov)
  
  return(list(
    cod_prov_control = prov_control_elegida,
    ranking_top5     = candidatas %>% 
      select(cod_prov, d_mahalanobis, dist_prov_tratada, control_educ, control_edad, 
             pct_top_crop, sup_media, sup_desv, pct_sup_cosechada_sembrada, 
             n_mercados, n_productores, pct_pavimentacion_vecinal, dist_lima, dist_cap_region) %>% 
      head(5),
    matriz_prov      = matriz_prov,    # <--- Agregamos este elemento
    vars_match       = vars_match      # <--- Agregamos la lista de variables usadas
  ))
}
