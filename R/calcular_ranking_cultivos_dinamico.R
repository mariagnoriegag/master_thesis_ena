# ==========================================================================
# COMPONENTE ESPACIAL MACRO: AUTOMATIZACIÓN FILTRADA (EXCLUYENDO FORRAJES)
# Tesis de Maestría: María Noriega / UTEC
# ==========================================================================

library(tidyverse)
library(haven)
library(stringr)
library(sf)

analizar_cultivos_por_intervenciones_csv <- function(radio_km = 5, tipo_proyecto_filtro = NULL) {
  cat("\n=========================================================================\n")
  cat(paste("🌍 INICIANDO PIPELINE GEOGRÁFICO CON RADIO DE:", radio_km, "KM\n"))
  cat("🎯 COHORTES EVALUADAS: 2018, 2023, 2024\n")
  cat("🚫 FILTRO CRÍTICO: Excluyendo forrajes y pastos del análisis comercial\n")
  if(!is.null(tipo_proyecto_filtro)) cat(paste("🎯 FILTRANDO POR TIPO DE PROYECTO:", tipo_proyecto_filtro, "\n"))
  cat("=========================================================================\n")
  
  # 1. CARGAR BASE DE INTERVENCIONES (RUTA CORREGIDA A DATA_EXTERNAL)
  ruta_intervenciones <- "data_external/BD_INTERVENCIONES_UBIGEO_2018_2024.csv"
  if (!file.exists(ruta_intervenciones)) {
    stop("⚠️ No se encontró el archivo de intervenciones en 'data_external/BD_INTERVENCIONES_UBIGEO_2018_2024.csv'.")
  }
  
  cat("  [Paso 1] Leyendo registro de intervenciones desde data_external y filtrando cohortes...\n")
  df_intervenciones <- read_csv(ruta_intervenciones, show_col_types = FALSE) %>% 
    rename_with(toupper) %>%
    mutate(UBIGEO = str_pad(as.character(as.numeric(UBIGEO)), width = 6, pad = "0"))
  
  # Filtro de cohortes explícito solicitado
  df_intervenciones <- df_intervenciones %>% filter(ANIO %in% c(2018, 2023, 2024))
  
  if (!is.null(tipo_proyecto_filtro)) {
    df_intervenciones <- df_intervenciones %>% filter(TIPO_PROYECTO == tipo_proyecto_filtro)
  }
  
  if (nrow(df_intervenciones) == 0) {
    stop("⚠️ No quedan registros después de aplicar los filtros de Cohorte y TIPO_PROYECTO.")
  }
  
  intervenciones_foco <- df_intervenciones %>%
    select(UBIGEO, ANIO) %>%
    distinct() %>%
    arrange(ANIO, UBIGEO)
  
  # 2. CARGAR MAPA DIGITAL DEL INEI (Límites Distritales 2025)
  ruta_shp <- "data_spatial/Limite Distrital INEI 2025 CPV.shp"
  if (!file.exists(ruta_shp)) {
    stop("⚠️ No se encontró el archivo .shp en 'data_spatial/'.")
  }
  
  cat("  [Paso 2] Cargando Shapefile distrital de INEI...\n")
  mapa_raw <- st_read(ruta_shp, quiet = TRUE) %>% rename_with(toupper)
  nombres_mapa <- names(mapa_raw)
  
  idx_dist <- matches("DIST|NOMDIS|DISTRITO|NOM_DIS", vars = nombres_mapa) %>% head(1)
  idx_prov <- matches("PROV|NOMPR|PROVINCIA|NOM_PROV", vars = nombres_mapa) %>% head(1)
  idx_dep  <- matches("DEP|NOMBDD|DEPARTAMENTO|NOMBREDD|DEPA|NOMB_DEP", vars = nombres_mapa) %>% head(1)
  
  col_dist_name <- if(length(idx_dist) > 0) nombres_mapa[idx_dist] else "CCDI"
  col_prov_name <- if(length(idx_prov) > 0) nombres_mapa[idx_prov] else "CCPP"
  col_dep_name  <- if(length(idx_dep) > 0)  nombres_mapa[idx_dep]  else "CCDD"
  
  peru_distritos <- mapa_raw %>%
    st_transform(crs = 32718) %>%
    mutate(
      ubigeo_6dig = paste0(str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
                           str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
                           str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0")),
      distrito_unificado = paste0(.data[[col_dist_name]], " (", .data[[col_prov_name]], ", ", .data[[col_dep_name]], ")")
    )
  
  # 3. CONSTRUCCIÓN DE BUFFERS
  cat("  [Paso 3] Calculando centroides y buffers espaciales para las cohortes filtradas...\n")
  puntos_intervencion <- peru_distritos %>% filter(ubigeo_6dig %in% intervenciones_foco$UBIGEO)
  
  if(nrow(puntos_intervencion) == 0) {
    stop("⚠️ Ninguno de los ubigeos de las cohortes seleccionadas coincide con el Shapefile.")
  }
  
  suppressWarnings({ puntos_intervencion <- puntos_intervencion %>% st_centroid() })
  
  radio_metros <- radio_km * 1000
  buffers_viales <- st_buffer(puntos_intervencion, dist = radio_metros)
  
  suppressWarnings({
    interseccion_espacial <- st_intersection(peru_distritos, buffers_viales) %>%
      st_drop_geometry() %>%
      select(ubigeo_vecino = ubigeo_6dig, distrito_vecino_completo = distrito_unificado, 
             ubigeo_intervencion = ubigeo_6dig.1, distrito_intervencion_completo = distrito_unificado.1) %>%
      distinct()
  })
  
  # 4. BARRIDO CRÓNICO CON FILTRO ANTI-FORRAJES EXPLICITO
  cat("  [Paso 4] Escaneando módulos de la ENA aplicando exclusión de forrajes...\n")
  
  años_intervencion_filtrados <- c(2018, 2023, 2024) %>% intersect(unique(intervenciones_foco$ANIO))
  panel_completo_lista <- list()
  
  for (anio_int in años_intervencion_filtrados) {
    anio_pre <- anio_int - 1
    anio_post <- anio_int + 1
    
    ubigeos_del_anio <- intervenciones_foco %>% filter(ANIO == anio_int) %>% pull(UBIGEO)
    cat(paste0("   -> Procesando Cohorte ", anio_int, " (Pre: ", anio_pre, " | Post: ", anio_post, ")...\n"))
    
    vecinos_validos <- interseccion_espacial %>% 
      filter(ubigeo_intervencion %in% ubigeos_del_anio) %>% 
      pull(ubigeo_vecino) %>% unique()
    
    leer_ena_seguro <- function(anio_ena) {
      ruta_sav <- paste0("data_raw/ENA_", anio_ena, "_Módulos/Cap200ab.sav")
      if(!file.exists(ruta_sav)) {
        cat(paste0("      ⚠️ Archivo ausente: ", ruta_sav, ". Saltando cohorte.\n"))
        return(NULL)
      }
      read_sav(ruta_sav) %>% rename_with(toupper) %>%
        mutate(ubigeo = paste0(str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
                               str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
                               str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0")),
               nombre_cultivo = tolower(str_trim(P204_NOM))) %>%
        filter(ubigeo %in% vecinos_validos) %>%
        
        # 🚫 FILTRO DE EXCLUSIÓN TOTAL: Forrajes, Permanentes, Frutales Leñosos y Agroexportación
        filter(!str_detect(nombre_cultivo, paste0("alfalfa|forrajera|forrajero|pasto|rye grass|chala|forraje|",
                                                  "palto|palta|mango|platano|banano|limon|naranjo|esparrago|",
                                                  "pacae|manzano|guayabo|chirimoyo|papaya|maracuya|arandano|",
                                                  "cafe|cacao|vid|uva|olivo|alcachofa|caña|piña|",
                                                  "guanabano|granado|lucumo|higuera|melocotonero|durazno|",
                                                  "tamarindo|zapote|ciruelo|pecano|pakar|maranon"))) %>%
        
        count(ubigeo, nombre_cultivo, name = "freq")
    }
    
    m_pre  <- leer_ena_seguro(anio_pre)
    m_post <- leer_ena_seguro(anio_post)
    
    if(is.null(m_pre) || is.null(m_post)) next
    
    panel_anio <- full_join(m_pre, m_post, by = c("ubigeo", "nombre_cultivo"), suffix = c("_previo", "_post")) %>%
      left_join(interseccion_espacial, by = c("ubigeo" = "ubigeo_vecino")) %>%
      filter(ubigeo_intervencion %in% ubigeos_del_anio) %>%
      mutate(intervencion = paste("Intervención", anio_int))
    
    panel_completo_lista[[as.character(anio_int)]] <- panel_anio
  }
  
  if (length(panel_completo_lista) == 0) {
    stop("⚠️ No se pudo procesar ninguna de las cohortes elegidas.")
  }
  
  base_unificada_completa <- bind_rows(panel_completo_lista)
  
  # 5. CONSOLIDACIÓN MACRO TOTAL
  cat("  [Paso 5] Consolidando matrices macro y ordenando texto largo al final...\n")
  
  lista_distritos_texto <- interseccion_espacial %>%
    group_by(ubigeo_intervencion) %>%
    summarise(
      num_distritos_capturados = n_distinct(ubigeo_vecino),
      distritos_vecinos        = paste(unique(distrito_vecino_completo), collapse = " || "), 
      .groups = 'drop'
    )
  
  matriz_consolidada_macro <- base_unificada_completa %>%
    group_by(intervencion, ubigeo_intervencion, distrito_intervencion_completo, nombre_cultivo) %>%
    summarise(
      sum_freq_previo = sum(freq_previo, na.rm = TRUE),
      sum_freq_post   = sum(freq_post, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    left_join(lista_distritos_texto, by = "ubigeo_intervencion") %>%
    mutate(v_orden = coalesce(sum_freq_previo, 0)) %>%
    group_by(intervencion, ubigeo_intervencion) %>%
    slice_max(order_by = v_orden, n = 5, with_ties = FALSE) %>%
    mutate(ranking = row_number()) %>%
    ungroup() %>%
    mutate(radio_establecido_km = as.numeric(radio_km)) %>%
    
    select(
      intervencion,
      radio_establecido_km,
      ubigeo_intervencion,
      distrito_intervencion_completo,
      ranking,
      nombre_cultivo,
      total_freq_año_previo = sum_freq_previo,
      total_freq_año_post   = sum_freq_post,
      num_distritos_capturados,     
      distritos_vecinos             
    ) %>%
    arrange(intervencion, ubigeo_intervencion, ranking)
  
  sufijo_filtro <- if(!is.null(tipo_proyecto_filtro)) paste0("_", str_replace_all(tolower(tipo_proyecto_filtro), " ", "_")) else ""
  if(!dir.exists("data_spatial_audit")) dir.create("data_spatial_audit")
  ruta_macro_csv <- paste0("data_spatial_audit/top5_neto_comercial_macro_", radio_km, "km", sufijo_filtro, ".csv")
  write_csv(matriz_consolidada_macro, ruta_macro_csv)
  
  cat(paste0("🎉 [ÉXITO] Reporte neto comercial (sin forrajes) guardado en: '", ruta_macro_csv, "'\n\n"))
}