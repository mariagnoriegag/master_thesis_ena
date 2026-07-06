# ==========================================================================
# COMPONENTE ESPACIAL MACRO: OPTIMIZACIÓN VISUAL DE COLUMNAS EJECUTIVAS
# Tesis de Maestría: María Noriega / UTEC
# ==========================================================================

library(tidyverse)
library(haven)
library(stringr)
library(sf)

analizar_cultivos_por_radio_vial <- function(radio_km = 5) {
  cat("\n=========================================================================\n")
  cat(paste("🌍 INICIANDO ANÁLISIS GEOGRÁFICO INTENSIVO CON RADIO DE:", radio_km, "KM\n"))
  cat("=========================================================================\n")
  
  # 1. CARGAR MAPA DIGITAL DEL INEI (Límites Distritales 2025)
  ruta_shp <- "data_spatial/Limite Distrital INEI 2025 CPV.shp"
  if (!file.exists(ruta_shp)) {
    stop("⚠️ No se encontró el archivo .shp en la ruta especificada.")
  }
  
  cat("  [Paso 1] Cargando Shapefile distrital de INEI y adaptando columnas...\n")
  
  # Cargamos la data original y normalizamos los nombres de las columnas a mayúsculas
  mapa_raw <- st_read(ruta_shp, quiet = TRUE) %>% rename_with(toupper)
  nombres_mapa <- names(mapa_raw)
  
  # Buscador flexible avanzado de columnas de atributos texto
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
  
  # 2. DEFINIR LAS COORDENADAS GEOGRÁFICAS DE LAS INTERVENCIONES
  cat("  [Paso 2] Indexando centroides de los distritos foco de intervención...\n")
  suppressWarnings({
    puntos_intervencion <- peru_distritos %>%
      filter(ubigeo_6dig %in% c("020108", "040520", "040124", "211207")) %>%
      st_centroid()
    
    st_geometry(puntos_intervencion) <- st_geometry(puntos_intervencion)
    puntos_intervencion <- puntos_intervencion %>% 
      select(ubigeo_intervencion = ubigeo_6dig, distrito_intervencion_completo = distrito_unificado)
  })
  
  # 3. CONSTRUCCIÓN DE LA FÓRMULA DINÁMICA DEL RADIO (BUFFER)
  cat(paste0("  [Paso 3] Calculando anillo geométrico (Buffer) a ", radio_km, " km...\n"))
  radio_metros <- radio_km * 1000
  buffers_viales <- st_buffer(puntos_intervencion, dist = radio_metros)
  
  # 4. INTERSECCIÓN ESPAlCIAL: Capturar qué distritos vecinos caen en el radio de la intervención
  cat("  [Paso 4] Realizando cruce espacial de vecindades geográficas...\n")
  suppressWarnings({
    interseccion_espacial <- st_intersection(peru_distritos, buffers_viales) %>%
      st_drop_geometry() %>%
      select(ubigeo_vecino = ubigeo_6dig, distrito_vecino_completo = distrito_unificado, 
             ubigeo_intervencion, distrito_intervencion_completo) %>%
      distinct()
  })
  
  # Etiquetas de las intervenciones temporales
  nombres_intervenciones <- c(
    "020108" = "Intervención 2018",
    "040520" = "Intervención 2018",
    "040124" = "Intervención 2023",
    "211207" = "Intervención 2023"
  )
  
  # ------------------------------------------------------------------------
  # 5. BARRIDO CRÓNICO: EXTRACCIÓN DE BASE DE DATOS DE LA ENA
  # ------------------------------------------------------------------------
  cat("  [Paso 5] Escaneando bases de datos de la ENA por entorno de radio...\n")
  
  procesar_anio_radio <- function(anio, ubigeos_foco, sufijo) {
    ruta_sav <- paste0("data_raw/ENA_", anio, "_Módulos/Cap200ab.sav")
    
    vecinos_validos <- interseccion_espacial %>% 
      filter(ubigeo_intervencion %in% ubigeos_foco) %>% 
      pull(ubigeo_vecino)
    
    df_raw <- read_sav(ruta_sav) %>% rename_with(toupper) %>%
      mutate(ubigeo = paste0(str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
                             str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
                             str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0")),
             nombre_cultivo = tolower(str_trim(P204_NOM))) %>%
      filter(ubigeo %in% vecinos_validos) %>%
      count(ubigeo, nombre_cultivo, name = paste0("freq_", anio, sufijo))
    
    return(df_raw)
  }
  
  m2017 <- procesar_anio_radio(2017, c("020108", "040520"), "_pre")
  m2019 <- procesar_anio_radio(2019, c("020108", "040520"), "_post")
  
  panel_v18 <- full_join(m2017, m2019, by = c("ubigeo", "nombre_cultivo")) %>%
    left_join(interseccion_espacial, by = c("ubigeo" = "ubigeo_vecino")) %>%
    filter(ubigeo_intervencion %in% c("020108", "040520")) %>%
    mutate(intervencion = nombres_intervenciones[ubigeo_intervencion]) %>%
    rename(freq_previo = 3, freq_post = 4)
  
  m2022 <- procesar_anio_radio(2022, c("040124", "211207"), "_pre")
  m2024 <- procesar_anio_radio(2024, c("040124", "211207"), "_post")
  
  panel_v23 <- full_join(m2022, m2024, by = c("ubigeo", "nombre_cultivo")) %>%
    left_join(interseccion_espacial, by = c("ubigeo" = "ubigeo_vecino")) %>%
    filter(ubigeo_intervencion %in% c("040124", "211207")) %>%
    mutate(intervencion = nombres_intervenciones[ubigeo_intervencion]) %>%
    rename(freq_previo = 3, freq_post = 4)
  
  base_unificada_completa <- bind_rows(panel_v18, panel_v23)
  
  # ------------------------------------------------------------------------
  # 6. ARCHIVO 1: CONSERVAR REPORTE DESAGREGADO (TOP 5 POR VECINO)
  # ------------------------------------------------------------------------
  cat("  [Paso 6] Guardando Reporte 01: Estructura Desagregada por Vecinos...\n")
  
  matriz_desagregada_top5 <- base_unificada_completa %>%
    mutate(v_orden = coalesce(freq_previo, 0)) %>%
    group_by(intervencion, ubigeo) %>%
    slice_max(order_by = v_orden, n = 5, with_ties = FALSE) %>%
    mutate(ranking = row_number()) %>%
    ungroup() %>%
    mutate(radio_establecido_km = as.numeric(radio_km)) %>%
    select(intervencion, radio_establecido_km, ubigeo_intervencion, distrito_intervencion_completo,
           ubigeo_vecino = ubigeo, distrito_vecino_completo, ranking, nombre_cultivo, 
           freq_año_previo = freq_previo, freq_año_post = freq_post) %>%
    arrange(intervencion, ubigeo_intervencion, ubigeo_vecino, ranking)
  
  if(!dir.exists("data_spatial_audit")) dir.create("data_spatial_audit")
  write_csv(matriz_desagregada_top5, paste0("data_spatial_audit/top5_desagregado_vecinos_", radio_km, "km.csv"))
  
  # ------------------------------------------------------------------------
  # 7. ARCHIVO 2: REPORTE MACRO CON REORDENAMIENTO ESTRUCTURAL DE COLUMNAS
  # ------------------------------------------------------------------------
  cat("  [Paso 7] Generando Reporte 02: Consolidación Macro con reordenamiento visual...\n")
  
  # Extracción de la vecindad agregada
  lista_distritos_texto <- interseccion_espacial %>%
    group_by(ubigeo_intervencion) %>%
    summarise(
      num_distritos_capturados = n_distinct(ubigeo_vecino),
      distritos_vecinos        = paste(unique(distrito_vecino_completo), collapse = " || "), 
      .groups = 'drop'
    )
  
  # Realizar la suma agregada por cultivo y recalcular el ranking macro
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
    
    # ¡REORDENADO!: Mandamos num_distritos_capturados y distritos_vecinos exactamente al final
    select(
      intervencion,
      radio_establecido_km,
      ubigeo_intervencion,
      distrito_intervencion_completo,
      ranking,
      nombre_cultivo,
      total_freq_año_previo = sum_freq_previo,
      total_freq_año_post   = sum_freq_post,
      num_distritos_capturados,     # Antepenúltima columna
      distritos_vecinos             # Última columna de texto largo
    ) %>%
    arrange(intervencion, ubigeo_intervencion, ranking)
  
  # Exportación del reporte macro consolidado ordenado
  ruta_macro_csv <- paste0("data_spatial_audit/top5_consolidado_macro_cluster_", radio_km, "km.csv")
  write_csv(matriz_consolidada_macro, ruta_macro_csv)
  
  cat(paste0("🎉 [ÉXITO DOBLE] Reporte Macro optimizado visualmente en: '", ruta_macro_csv, "'\n\n"))
  
  # Mostrar vista previa en la consola respetando el nuevo orden secuencial
  cat("📊 VISTA PREVIA DEL NUEVO DISEÑO ESTRUCTURAL (FINALES DE TABLA):\n")
  print(as.data.frame(matriz_consolidada_macro %>% select(intervencion, nombre_cultivo, total_freq_año_previo, num_distritos_capturados) %>% head(6)))
}