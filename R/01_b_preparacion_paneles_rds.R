# ==========================================================================
# COMPONENTE 01 DEFINITIVO: PIPELINE SIN FILTRO DE CULTIVOS PARA AUDITORÍA (DiD)
# Tesis de Maestría: María Noriega / Asesor: PhD. José Larco (UTEC)
# ==========================================================================

library(tidyverse)
library(haven)
library(stringr)

# Función interna de limpieza y homologación de texto para nombres de regiones
limpiar_string <- function(texto) {
  if (is.null(texto)) return(texto)
  texto <- as.character(texto)
  texto <- toupper(texto)
  texto <- iconv(texto, to = "ASCII//TRANSLIT")
  texto <- stringr::str_trim(texto)
  return(texto)
}

#' Función maestra para cargar, colapsar por UBIGEO y deflactar los 5 módulos de la ENA
procesar_modulos_crudos <- function(anio) {
  cat(paste("\n--- Procesando año:", anio, "---\n"))
  folder_name <- paste0("data_raw/ENA_", anio, "_Módulos/")
  
  f_cap100  <- paste0(folder_name, if(anio == 2024) "Cap100a_1.sav" else "Cap100_1.sav")
  f_cap200  <- paste0(folder_name, "Cap200ab.sav")
  f_cap800  <- paste0(folder_name, "Cap800.sav")
  f_cap900  <- paste0(folder_name, "Cap900.sav")
  f_cap1100 <- paste0(folder_name, "Cap1100.sav")
  
  cat("  -> Cargando archivos directamente...\n")
  m100  <- read_sav(as.character(f_cap100))  %>% rename_with(toupper)
  m200  <- read_sav(as.character(f_cap200))  %>% rename_with(toupper)
  m800  <- read_sav(as.character(f_cap800))  %>% rename_with(toupper)
  m900  <- read_sav(as.character(f_cap900))  %>% rename_with(toupper)
  m1100 <- read_sav(as.character(f_cap1100)) %>% rename_with(toupper)
  
  m100  <- m100  %>% select(-any_of("REGION"))
  m200  <- m200  %>% select(-any_of("REGION"))
  m800  <- m800  %>% select(-any_of("REGION"))
  m900  <- m900  %>% select(-any_of("REGION"))
  m1100 <- m1100 %>% select(-any_of("REGION"))
  
  # ------------------------------------------------------------------------
  # 1. PARCHE MAESTRO DE PRECIOS Y CANTIDADES EN M200ab
  # ------------------------------------------------------------------------
  nombres_m200 <- names(m200)
  
  if ("P220_1_CANT_1" %in% nombres_m200) {
    m200 <- m200 %>% mutate(cant_venta_raw_ent = as.numeric(P220_1_CANT_1))
  } else { m200 <- m200 %>% mutate(cant_venta_raw_ent = NA_real_) }
  
  if ("P220_1_CANT_2" %in% nombres_m200) {
    m200 <- m200 %>% mutate(cant_venta_raw_dec = as.numeric(P220_1_CANT_2))
  } else { m200 <- m200 %>% mutate(cant_venta_raw_dec = NA_real_) }
  
  if ("P220_1_PREC_1" %in% nombres_m200 & "P220_1_PREC_2" %in% nombres_m200) {
    cat("     [Parche] Reconstruyendo precios enteros/decimales en M200...\n")
    m200 <- m200 %>%
      mutate(
        entero  = coalesce(as.numeric(P220_1_PREC_1), 0),
        decimal = coalesce(as.numeric(P220_1_PREC_2), 0),
        precio_chacra_raw = entero + (decimal / 100)
      ) %>% select(-entero, -decimal)
  } else if ("P220_1_PRE_KG" %in% nombres_m200) {
    m200 <- m200 %>% mutate(precio_chacra_raw = as.numeric(P220_1_PRE_KG))
  } else if ("P220_1" %in% nombres_m200) {
    m200 <- m200 %>% mutate(precio_chacra_raw = as.numeric(P220_1))
  } else { m200 <- m200 %>% mutate(precio_chacra_raw = as.numeric(P220)) }
  
  # Construcción del Ubigeo directo en Módulo 200ab (SIN FILTRAR CULTIVOS)
  m200_preparado <- m200 %>%
    mutate(
      dep_limpio = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
      pro_limpio = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
      dis_limpio = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
      ubigeo     = paste0(dep_limpio, pro_limpio, dis_limpio),
      nombre_cultivo = tolower(str_trim(P204_NOM))
    )
  
  # Colapsar Módulo 200ab agrupando por Ubigeo y Cultivo
  m200_colapsado <- m200_preparado %>%
    group_by(ubigeo, nombre_cultivo) %>%
    summarise(
      p_chacra_dist = mean(precio_chacra_raw[P221_1 == 1], na.rm = TRUE),
      p_fuera_dist  = mean(precio_chacra_raw[P221_2 == 1], na.rm = TRUE),
      cant_venta_kg_ent_tot = sum(cant_venta_raw_ent, na.rm = TRUE),
      cant_venta_kg_dec_tot = sum(cant_venta_raw_dec, na.rm = TRUE),
      precio_valor_tot_acum = sum(P220_1_VAL, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # ------------------------------------------------------------------------
  # 2. COLAPSAMIENTO INDEPENDIENTE DE CONTROLES TERRITORIALES (SOLO UBIGEO)
  # ------------------------------------------------------------------------
  v_factor <- if_else("FACTOR_PRODUCTOR" %in% names(m100), "FACTOR_PRODUCTOR", "FACTOR")
  
  m100_colapsado <- m100 %>%
    rename(DEPARTAMENTO_CAPTURA = matches("^DEPARTAMENTO$|^NOMBREDD$|^REG_DES$") %>% head(1),
           factor_expansion = all_of(v_factor)) %>%
    mutate(
      dep_limpio = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
      pro_limpio = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
      dis_limpio = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
      ubigeo     = paste0(dep_limpio, pro_limpio, dis_limpio),
      DEPARTAMENTO = limpiar_string(DEPARTAMENTO_CAPTURA)
    ) %>%
    group_by(ubigeo, DEPARTAMENTO) %>%
    summarise(
      control_sup_ha   = mean(P104_SUP_HA, na.rm = TRUE),
      factor_expansion = sum(factor_expansion, na.rm = TRUE),
      .groups = 'drop'
    )
  
  m1100_colapsado <- m1100 %>%
    filter(P1102 == 1) %>%
    mutate(
      dep_limpio = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
      pro_limpio = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
      dis_limpio = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
      ubigeo     = paste0(dep_limpio, pro_limpio, dis_limpio)
    ) %>%
    group_by(ubigeo) %>%
    summarise(
      # FÓRMULA DE MARÍA: (Total de Mujeres (2) / Total de Hombres y Mujeres (1 y 2)) * 100
      control_sexo = (sum(P1103 == 2, na.rm = TRUE) / sum(P1103 %in% c(1, 2), na.rm = TRUE)) * 100,
      control_edad = mean(P1104_A, na.rm = TRUE),
      control_educ = mean(P1105, na.rm = TRUE),
      .groups = 'drop'
    )
  
  m800_colapsado <- m800 %>%
    mutate(
      dep_limpio = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
      pro_limpio = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
      dis_limpio = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
      ubigeo     = paste0(dep_limpio, pro_limpio, dis_limpio),
      dummy_asoc = if_else(P801 == 1, 1, 0)
    ) %>%
    group_by(ubigeo) %>%
    summarise(control_asociacion = mean(dummy_asoc, na.rm = TRUE), .groups = 'drop')
  
  m900_colapsado <- m900 %>%
    mutate(
      dep_limpio = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
      pro_limpio = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
      dis_limpio = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
      ubigeo     = paste0(dep_limpio, pro_limpio, dis_limpio),
      dummy_cred = if_else(P901 == 1, 1, 0)
    ) %>%
    group_by(ubigeo) %>%
    summarise(control_credito = mean(dummy_cred, na.rm = TRUE), .groups = 'drop')
  
  # ------------------------------------------------------------------------
  # 3. ACOPLAMIENTO INTEGRAL EXCLUSIVO POR UBIGEO
  # ------------------------------------------------------------------------
  cat("  -> Unificando modulos colapsados bajo la llave maestra UBIGEO...\n")
  
  base_unificada_distrital <- m200_colapsado %>%
    inner_join(m100_colapsado,  by = "ubigeo") %>%
    left_join(m1100_colapsado, by = "ubigeo") %>%
    left_join(m800_colapsado,  by = "ubigeo") %>%
    left_join(m900_colapsado,  by = "ubigeo") %>%
    mutate(
      anio = anio,
      CATEGORIA_IPC = case_when(
        nombre_cultivo %in% c("papa blanca", "cebolla") ~ "Hortalizas, legumbres y tubérculos",
        nombre_cultivo %in% c("naranjo", "limon acido") ~ "Frutas",
        nombre_cultivo %in% c("quinua", "maiz amarillo duro") ~ "Pan y cereales",
        TRUE ~ "Pan y cereales"
      )
    )
  
  # ------------------------------------------------------------------------
  # 4. ACOPLAMIENTO DE IPC Y DEFLACIÓN
  # ------------------------------------------------------------------------
  ipc_hist_temp <- read_csv("data_external/ipc_departamento_2017_2018_2019.csv", show_col_types = FALSE) %>%
    mutate(DEPARTAMENTO = limpiar_string(DEPARTAMENTO)) %>%
    pivot_longer(cols = starts_with("IPC_"), names_to = "anio_str", values_to = "ipc_val") %>%
    mutate(anio_num = as.numeric(gsub("IPC_", "", anio_str))) %>%
    filter(anio_num == anio) %>% select(DEPARTAMENTO, ipc_deflactor = ipc_val)
  
  ipc_rec_temp <- read_csv("data_external/ipc_especifico_departamento_2022_2024.csv", show_col_types = FALSE) %>%
    rename(DEPARTAMENTO_TEMP = 1) %>% mutate(DEPARTAMENTO = limpiar_string(DEPARTAMENTO_TEMP)) %>%
    filter(ANIO == anio) %>% select(DEPARTAMENTO, CATEGORIA, ipc_deflactor = IPC_ANUAL)
  
  if (anio <= 2019) {
    base_unificada_distrital <- base_unificada_distrital %>% left_join(ipc_hist_temp, by = "DEPARTAMENTO")
  } else {
    base_unificada_distrital <- base_unificada_distrital %>% left_join(ipc_rec_temp, by = c("DEPARTAMENTO", "CATEGORIA_IPC" = "CATEGORIA"))
  }
  
  base_deflactada <- base_unificada_distrital %>%
    mutate(
      p_chacra_real = (p_chacra_dist / ipc_deflactor) * 100,
      p_fuera_real  = (p_fuera_dist  / ipc_deflactor) * 100
    ) %>%
    mutate(across(matches("_COD$|_UM$|_UM_COD$|^P219_UM_COD$"), as.character))
  
  return(base_deflactada)
}

generar_paneles_finales_rds <- function() {
  cat("\nIniciando compilación pesada de paneles longitudinales...\n")
  vial_mtc <- read_csv("data_external/mtc_infraestructura_vial_2017_2018_2022.csv", show_col_types = FALSE) %>%
    mutate(DEPARTAMENTO = limpiar_string(DEPARTAMENTO)) %>% select(-any_of(c("REGION", "COD_DD")))
  
  d17 <- procesar_modulos_crudos(2017)
  d18 <- procesar_modulos_crudos(2018)
  d19 <- procesar_modulos_crudos(2019)
  d22 <- procesar_modulos_crudos(2022)
  d24 <- procesar_modulos_crudos(2024)
  
  # --- CONSTRUCCIÓN DEL PANEL LONGITUDINAL DISTRITAL DID ---
  colapsar_márgenes_distrito <- function(df_pre, df_post, anio_pre, anio_post, col_vial_pre, col_vial_post) {
    label_cohorte <- paste0("[COHORTE ", anio_pre, "-", anio_post, "]")
    cat(paste0("\n📈 ", label_cohorte, " GENERANDO MÁRGENES FINALES:\n"))
    
    panel_raw <- bind_rows(df_pre, df_post) %>% 
      mutate(
        post = if_else(anio == anio_post, 1, 0),
        codigo_provincia = substr(ubigeo, 1, 4)
      )
    cat(paste0("  [Paso 1] Registros distritales combinados cargados: ", nrow(panel_raw), "\n"))
    
    vial_sel  <- vial_mtc %>% select(DEPARTAMENTO, v_pre = !!sym(col_vial_pre), v_post = !!sym(col_vial_post))
    
    # Precios de respaldo provinciales para mitigar NaN muestrales locales
    precios_provinciales <- panel_raw %>%
      group_by(anio, codigo_provincia, nombre_cultivo) %>%
      summarise(
        p_chacra_prov = mean(p_chacra_real, na.rm = TRUE),
        p_fuera_prov  = mean(p_fuera_real, na.rm = TRUE),
        .groups = 'drop'
      )
    
    base_margen <- panel_raw %>%
      left_join(vial_sel, by = "DEPARTAMENTO") %>%
      mutate(pct_transporte_pavimentado = if_else(post == 1, v_post, v_pre)) %>%
      left_join(precios_provinciales, by = c("anio", "codigo_provincia", "nombre_cultivo")) %>%
      mutate(
        p_chacra_final = coalesce(p_chacra_real, p_chacra_prov),
        p_fuera_final  = coalesce(p_fuera_real, p_fuera_prov)
      ) %>%
      mutate(
        margen_comercial  = p_fuera_final - p_chacra_final,
        log_precio_chacra = log(p_chacra_final)
      ) %>%
      filter(!is.nan(margen_comercial) & !is.na(margen_comercial) & !is.infinite(log_precio_chacra))
    
    cat(paste0("  [Paso 2] 🎉 OBSERVACIONES FINALES LOGRADAS PARA EL PANEL DiD: ", nrow(base_margen), "\n"))
    return(base_margen)
  }
  
  saveRDS(colapsar_márgenes_distrito(d17, d19, 2017, 2019, "PCT_PAV_VECINAL_2017", "PCT_PAV_VECINAL_2018"), "data/base_para_did_2017_2019.rds")
  saveRDS(colapsar_márgenes_distrito(d18, d22, 2018, 2022, "PCT_PAV_VECINAL_2018", "PCT_PAV_VECINAL_2022"), "data/base_para_did_2018_2022.rds")
  saveRDS(colapsar_márgenes_distrito(d22, d24, 2022, 2024, "PCT_PAV_VECINAL_2022", "PCT_PAV_VECINAL_2022"), "data/base_para_did_2022_2024.rds")
  
  cat("\n[ÉXITO MÁXIMO] Los tres paneles longitudinales por UBIGEO se han guardado limpiamente.\n")
}