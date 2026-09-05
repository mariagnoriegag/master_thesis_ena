# ==============================================================================
# FUNCION 3: CONSOLIDACIÓN Y EXPANSIÓN MUESTRAL DEL PANEL ENA (2017-2025)
# Autora: María Noriega
# Proyecto: Master Thesis ENA
# ==============================================================================

library(tidyverse)
library(here)
library(haven)
library(stringr)

cargar_panel_ena_did <- function(res_buffer, anio_proyecto, ena_pre_year, ena_post_years, dir_ena_base, cultivo_foco = "PAPA BLANCA") {
  
  ubigeos_tratados <- res_buffer$ubigeos_tratados_buffer
  ubigeos_control  <- res_buffer$ubigeos_control_buffer
  ubigeos_muestra  <- c(ubigeos_tratados, ubigeos_control)
  
  todos_anios <- c(ena_pre_year, ena_post_years)
  
  cargar_sav_directo <- function(ruta_carpeta, patron_archivo) {
    archivos <- list.files(ruta_carpeta, full.names = TRUE, ignore.case = TRUE)
    match_file <- archivos[str_detect(basename(archivos), regex(patron_archivo, ignore_case = TRUE))][1]
    if(is.na(match_file)) return(NULL)
    read_sav(match_file) %>% zap_labels()
  }
  
  procesar_ena_anio <- function(anio) {
    carpetas <- list.dirs(dir_ena_base, recursive = FALSE)
    dir_anio <- carpetas[str_detect(carpetas, as.character(anio))][1]
    
    if(is.na(dir_anio) || !dir.exists(dir_anio)) return(NULL)
    
    # --------------------------------------------------------------------------
    # 1. Cap100 (Estructura Geográfica y Factor de Expansión)
    # --------------------------------------------------------------------------
    m100_raw <- cargar_sav_directo(dir_anio, "^Cap100.*_1\\.sav$")
    if(is.null(m100_raw)) return(NULL)
    
    m100_prep <- m100_raw %>%
      rename_with(toupper) %>% 
      mutate(
        dep_limpio  = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
        pro_limpio  = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
        dis_limpio  = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
        ubigeo_6dig = paste0(dep_limpio, pro_limpio, dis_limpio),
        factor_exp  = coalesce(if("FACTOR" %in% names(.)) as.numeric(FACTOR) else 1, 1)
      ) %>%
      filter(ubigeo_6dig %in% ubigeos_muestra)
    
    if(nrow(m100_prep) == 0) return(NULL)
    
    nombres_cols <- names(m100_prep)
    col_dis <- nombres_cols[str_detect(nombres_cols, "^NOMBDIST$|^NOMBREDI$|^NOMDIS$|^DISTRITO$|^NOM_DIS$")][1]
    col_pro <- nombres_cols[str_detect(nombres_cols, "^NOMBPROV$|^NOMBREPV$|^NOMPR$|^PROVINCIA$|^NOM_PROV$")][1]
    col_dep <- nombres_cols[str_detect(nombres_cols, "^NOMBDEP$|^DEPARTAMEN$|^NOMBREDD$|^NOMBDD$|^DEPARTAMENTO$")][1]
    
    nombres_geo <- m100_prep %>% 
      mutate(
        nombdist = if(!is.na(col_dis)) as.character(.data[[col_dis]]) else dis_limpio,
        nombprov = if(!is.na(col_pro)) as.character(.data[[col_pro]]) else pro_limpio,
        nombdep  = if(!is.na(col_dep)) as.character(.data[[col_dep]]) else dep_limpio
      ) %>% 
      group_by(ubigeo_6dig) %>% 
      summarise(
        nombdep  = first(nombdep),
        nombprov = first(nombprov),
        nombdist = first(nombdist),
        .groups  = "drop"
      )
    
    # --------------------------------------------------------------------------
    # 2. Cap200 (Filtrado de Cultivo Foco, Prorrateo y Expansión por FACTOR)
    # --------------------------------------------------------------------------
    m200_raw <- cargar_sav_directo(dir_anio, "^Cap200a?b?\\.sav$")
    
    m200_agg <- if(!is.null(m200_raw)) {
      
      m200_prep <- m200_raw %>%
        rename_with(toupper) %>% 
        mutate(
          dep_limpio  = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
          pro_limpio  = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
          dis_limpio  = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
          ubigeo_6dig = paste0(dep_limpio, pro_limpio, dis_limpio)
        ) %>% 
        filter(ubigeo_6dig %in% ubigeos_muestra)
      
      cols_join <- intersect(names(m200_prep), names(m100_prep))
      cols_join <- setdiff(cols_join, c("dep_limpio", "pro_limpio", "dis_limpio", "ubigeo_6dig"))
      
      if(length(cols_join) > 0) {
        m200_prep <- m200_prep %>% 
          left_join(m100_prep %>% select(all_of(cols_join), factor_exp), by = cols_join)
      } else {
        m200_prep <- m200_prep %>% mutate(factor_exp = 1)
      }
      
      m200_prep <- m200_prep %>% 
        mutate(
          factor_exp = replace_na(factor_exp, 1),
          nombre_cultivo = if("P204_NOM" %in% names(.)) toupper(trimws(as.character(P204_NOM))) else "DESCONOCIDO",
          
          P217_SUP_HA   = coalesce(if("P217_SUP_HA" %in% names(.)) as.numeric(P217_SUP_HA) else NA_real_, 0),
          P219_EQUIV_KG = coalesce(if("P219_EQUIV_KG" %in% names(.)) as.numeric(P219_EQUIV_KG) else NA_real_, 0),
          
          p_ent = if("P220_1_PREC_1" %in% names(.)) as.numeric(P220_1_PREC_1) else 0,
          p_dec = if("P220_1_PREC_2" %in% names(.)) as.numeric(P220_1_PREC_2) else 0,
          ingreso_ventas_soles = replace_na(p_ent, 0) + (replace_na(p_dec, 0) / 100),
          
          c_ent = if("P220_1_CANT_1" %in% names(.)) as.numeric(P220_1_CANT_1) else 0,
          c_dec = if("P220_1_CANT_2" %in% names(.)) as.numeric(P220_1_CANT_2) else 0,
          cant_vendida_kg = replace_na(c_ent, 0) + (replace_na(c_dec, 0) / 100),
          
          precio_directo = if("P220_1_PRE_KG" %in% names(.)) as.numeric(P220_1_PRE_KG) else NA_real_,
          
          vende_dentro_chacra = if("P221_1" %in% names(.)) ifelse(P221_1 == 1, 1, 0) else 0,
          vende_fuera_chacra  = if("P221_2" %in% names(.)) ifelse(P221_2 == 1, 1, 0) else 0,
          
          d_local = if("P223_1" %in% names(.)) ifelse(P223_1 == 1, 1, 0) else 0,
          d_reg   = if("P223_2" %in% names(.)) ifelse(P223_2 == 1, 1, 0) else 0,
          d_ext   = if("P223_3" %in% names(.)) ifelse(P223_3 == 1, 1, 0) else 0,
          d_agro  = if("P223_4" %in% names(.)) ifelse(P223_4 == 1, 1, 0) else 0,
          d_lima  = if("P223_5" %in% names(.)) ifelse(P223_5 == 1, 1, 0) else 0,
          d_ns    = if("P223_6" %in% names(.)) ifelse(P223_6 == 1, 1, 0) else 0
        )
      
      cadena_foco_limpia <- toupper(trimws(cultivo_foco))
      
      m200_prep <- m200_prep %>%
        mutate(
          es_cultivo_foco = ifelse(str_detect(nombre_cultivo, regex(cadena_foco_limpia, ignore_case = TRUE)), 1, 0),
          
          n_destinos_marcados = d_local + d_reg + d_ext + d_agro + d_lima + d_ns,
          factor_prorrateo    = ifelse(es_cultivo_foco == 1 & n_destinos_marcados > 0, 1 / n_destinos_marcados, 0),
          
          kg_mkt_local = cant_vendida_kg * factor_prorrateo * d_local,
          kg_mkt_reg   = cant_vendida_kg * factor_prorrateo * d_reg,
          kg_mkt_lima  = cant_vendida_kg * factor_prorrateo * d_lima,
          kg_mkt_otros = cant_vendida_kg * factor_prorrateo * (d_ext + d_agro + d_ns),
          
          soles_mkt_local = ingreso_ventas_soles * factor_prorrateo * d_local,
          soles_mkt_reg   = ingreso_ventas_soles * factor_prorrateo * d_reg,
          soles_mkt_lima  = ingreso_ventas_soles * factor_prorrateo * d_lima,
          soles_mkt_otros = ingreso_ventas_soles * factor_prorrateo * (d_ext + d_agro + d_ns),
          
          precio_unitario_foco = case_when(
            es_cultivo_foco == 1 & !is.na(precio_directo) & precio_directo >= 0.50 & precio_directo <= 6.00 ~ precio_directo,
            es_cultivo_foco == 1 & cant_vendida_kg > 0 & ingreso_ventas_soles > 0 & (ingreso_ventas_soles / cant_vendida_kg) >= 0.50 & (ingreso_ventas_soles / cant_vendida_kg) <= 6.00 ~ ingreso_ventas_soles / cant_vendida_kg,
            TRUE ~ NA_real_
          )
        )
      
      m200_prep %>%
        group_by(ubigeo_6dig) %>%
        summarise(
          cultivos_cosechados_list = paste(unique(na.omit(nombre_cultivo)), collapse = ", "),
          foco_especifico_nom      = paste(unique(na.omit(nombre_cultivo[es_cultivo_foco == 1])), collapse = ", "),
          
          # Totales Agregados EXTENSIVOS (Sí multiplican por factor_exp)
          sup_total_general_ha  = sum(P217_SUP_HA * factor_exp, na.rm = TRUE),
          ingreso_total_general = sum(ingreso_ventas_soles * factor_exp, na.rm = TRUE),
          
          sup_cultivo_foco_ha   = sum((P217_SUP_HA * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          prod_cultivo_foco_kg  = sum((P219_EQUIV_KG * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          ingreso_cultivo_foco  = sum((ingreso_ventas_soles * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          cant_vend_foco_kg     = sum((cant_vendida_kg * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          
          n_venta_dentro_chacra = round(sum((vende_dentro_chacra * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE)),
          n_venta_fuera_chacra  = round(sum((vende_fuera_chacra * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE)),
          
          kg_mkt_local_dist    = sum((kg_mkt_local * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          kg_mkt_reg_dist      = sum((kg_mkt_reg * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          kg_mkt_lima_dist     = sum((kg_mkt_lima * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          kg_mkt_otros_dist    = sum((kg_mkt_otros * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          
          soles_mkt_local_dist = sum((soles_mkt_local * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          soles_mkt_reg_dist   = sum((soles_mkt_reg * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          soles_mkt_lima_dist  = sum((soles_mkt_lima * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          soles_mkt_otros_dist = sum((soles_mkt_otros * factor_exp)[es_cultivo_foco == 1], na.rm = TRUE),
          
          # Variable INTENSIVA Unitarias (NO multiplica por factor_exp)
          precio_foco_kg       = suppressWarnings(median(precio_unitario_foco[!is.na(precio_unitario_foco)], na.rm = TRUE)),
          
          tiene_cultivo_foco   = max(es_cultivo_foco, na.rm = TRUE),
          
          # RENOMBRADO A UNIDADES AGROPECUARIAS (UAs) EXPANDIDAS
          n_uas_foco           = round(sum(factor_exp[es_cultivo_foco == 1], na.rm = TRUE)),
          .groups = "drop"
        )
    } else tibble()
    
    if(nrow(m200_agg) > 0) {
      m200_agg %>% 
        left_join(nombres_geo, by = "ubigeo_6dig") %>% 
        mutate(anio_encuesta = anio)
    } else tibble()
  }
  
  panel_raw <- map_dfr(todos_anios, procesar_ena_anio)
  
  if(nrow(panel_raw) == 0) {
    stop("⚠️ No se encontraron observaciones para los UBIGEOs seleccionados.")
  }
  
  # ----------------------------------------------------------------------------
  # CONSOLIDACIÓN, IMPUTACIÓN REGIONAL Y LOGARITMO EN ESCALA NATURAL
  # ----------------------------------------------------------------------------
  panel_did <- panel_raw %>%
    filter(tiene_cultivo_foco == 1 & sup_cultivo_foco_ha > 0) %>%
    group_by(nombdep, anio_encuesta) %>% 
    mutate(
      mediana_dep_precio = suppressWarnings(median(precio_foco_kg[!is.na(precio_foco_kg) & !is.nan(precio_foco_kg)], na.rm = TRUE))
    ) %>% 
    ungroup() %>% 
    mutate(
      tratado             = ifelse(ubigeo_6dig %in% ubigeos_tratados, 1, 0),
      post                = ifelse(anio_encuesta > anio_proyecto, 1, 0),
      tratado_x_post      = tratado * post,
      
      precio_foco_kg      = case_when(
        !is.na(precio_foco_kg) & !is.nan(precio_foco_kg) & precio_foco_kg >= 0.80 ~ precio_foco_kg,
        !is.na(mediana_dep_precio) & !is.nan(mediana_dep_precio) ~ mediana_dep_precio,
        TRUE ~ 1.20
      ),
      
      log_precio_chacra   = log(precio_foco_kg),
      pct_venta_local     = ifelse(cant_vend_foco_kg > 0, (kg_mkt_local_dist / cant_vend_foco_kg) * 100, 0),
      
      rendimiento_foco_ha = ifelse(sup_cultivo_foco_ha > 0, prod_cultivo_foco_kg / sup_cultivo_foco_ha, NA_real_),
      rendimiento_foco_ha = ifelse(!is.na(rendimiento_foco_ha) & rendimiento_foco_ha > 35000, 35000, rendimiento_foco_ha)
    ) %>% 
    select(
      ubigeo_6dig, nombdep, nombprov, nombdist, anio_encuesta, tratado, post, tratado_x_post,
      cultivos_cosechados_list, foco_especifico_nom,
      sup_total_general_ha, ingreso_total_general, sup_cultivo_foco_ha, prod_cultivo_foco_kg,
      ingreso_cultivo_foco, cant_vend_foco_kg, precio_foco_kg, log_precio_chacra, pct_venta_local,
      
      n_venta_dentro_chacra, n_venta_fuera_chacra,
      kg_mkt_local_dist, kg_mkt_reg_dist, kg_mkt_lima_dist, kg_mkt_otros_dist,
      soles_mkt_local_dist, soles_mkt_reg_dist, soles_mkt_lima_dist, soles_mkt_otros_dist,
      
      rendimiento_foco_ha, tiene_cultivo_foco, n_uas_foco
    )
  
  return(panel_did)
}