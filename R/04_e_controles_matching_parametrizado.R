# ==========================================================================
# MOTOR ECONOMÉTRICO PARAMETRIZADO: PSM MULTI-COHORTE Y SPILLOVERS GEOGRÁFICOS
# Tesis de Maestría: María Noriega / Asesor: PhD. José Larco (UTEC)
# ==========================================================================

library(tidyverse)
library(sf)
library(MatchIt)
library(lmtest)
library(sandwich)
library(cobalt)

#' Pipeline General de Balanceo, Filtrado y Estimación de Robustez Espacial
#' @param path_base_base Path del .rds de la línea de base con covariables.
#' @param path_base_panel Path del .rds con el panel longitudinal completo de la cohorte.
#' @param path_vial Path del CSV conteniendo los datos de pavimentación de María.
#' @param anio_base Año correspondiente a la línea de base (e.g., 2017, 2018, 2022).
#' @param col_vial_anio Nombre exacto de la columna de pavimentación para ese año base.
#' @param umbral_venta Porcentaje mínimo de venta para el Filtro Paso 0 (por defecto 50%).
#' @param radio_km Radio radial de expansión para la asignación de spillovers de tratamiento.
#' @param submuestra Tipo de filtro de heterogeneidad ("Todos", "Mejora", "Nuevo Mercado", "Mayorista/Mixto", "Minorista").
#' @param var_dependiente Nombre de la variable Y a estimar.
ejecutar_pipeline_robustez <- function(path_base_base, path_base_panel, path_vial, 
                                       anio_base, col_vial_anio, umbral_venta = 50, 
                                       radio_km = 20, submuestra = "Todos", 
                                       var_dependiente = "log_precio_chacra") {
  
  # 1. Cargar mapa político e inducir proyección métrica UTM Zona 18S
  mapa <- st_read("../data_spatial/Limite Distrital INEI 2025 CPV.shp", quiet = TRUE) %>% 
    st_transform(32718) %>% 
    rename(ubigeo = UBIGEO)
  
  # 2. Cargar microdatos de línea de base y fusionar controles externos (MTC y Fricción de Escala)
  vial_maria <- read_csv(path_vial, quiet = TRUE) %>% 
    select(DEPARTAMENTO, !!sym(col_vial_anio))
  
  base_base_raw <- readRDS(path_base_base) %>% 
    filter(anio == anio_base) %>% 
    mutate(DEPARTAMENTO = iconv(toupper(DEPARTAMENTO), to = "ASCII//TRANSLIT")) %>% 
    left_join(vial_maria, by = "DEPARTAMENTO")
  
  # 3. Aplicación Estricta del Filtro Paso 0 (Muestra orientada al mercado)
  base_base_filtrada <- base_base_raw %>% 
    filter(pct_venta_producto >= umbral_venta)
  
  # 4. Clasificación de Heterogeneidades en la línea de base
  if (submuestra == "Mejora") {
    base_base_filtrada <- base_base_filtrada %>% filter(tipo_proyecto == "Mejora")
  } else if (submuestra == "Nuevo Mercado") {
    base_base_filtrada <- base_base_filtrada %>% filter(tipo_proyecto == "Nuevo Mercado")
  } else if (submuestra == "Mayorista/Mixto") {
    base_base_filtrada <- base_base_filtrada %>% filter(tipo_mercado == "Mayorista/Mixto")
  } else if (submuestra == "Minorista") {
    base_base_filtrada <- base_base_filtrada %>% filter(tipo_mercado == "Minorista")
  }
  
  # Asegurar filas mínimas para el matching
  if (nrow(base_base_filtrada) < 15 || sum(base_base_filtrada$es_tratado == 1) < 3) {
    return(list(error = TRUE, mensaje = "Muestra insuficiente para esta celda."))
  }
  
  # 5. Algoritmo de Spillovers Geográficos basado en la Nueva Estrategia Radial
  u_mercados <- base_base_filtrada %>% filter(es_tratado == 1) %>% pull(ubigeo) %>% unique()
  c_todos <- st_centroid(mapa %>% filter(ubigeo %in% base_base_filtrada$ubigeo))
  c_mercados <- st_centroid(mapa %>% filter(ubigeo %in% u_mercados))
  
  if (nrow(c_mercados) == 0) return(list(error = TRUE, mensaje = "No se hallaron centroides tratados."))
  
  m_dist <- st_distance(c_todos, c_mercados)
  en_radio <- apply(m_dist, 1, function(x) any(x <= (radio_km * 1000)))
  u_influencia <- c_todos$ubigeo[en_radio]
  
  # Reasignar el vector espacial de tratamiento en base al radio de fricción comercial
  df_matching <- base_base_filtrada %>% 
    mutate(es_tratado = if_else(ubigeo %in% u_influencia, 1, 0)) %>% 
    filter(!is.na(control_sup_ha) & !is.na(control_edad) & !is.na(control_educ))
  
  # 6. Algoritmo de Emparejamiento por Puntaje de Propensión (PSM) - Caliper Estricto
  formula_psm <- as.formula(paste("es_tratado ~ control_sup_ha + control_edad + control_educ + control_credito +", col_vial_anio))
  
  psm_obj <- matchit(formula_psm, data = df_matching, method = "nearest", caliper = 0.05, replace = FALSE)
  m_data <- match.data(psm_obj)
  u_elegidos <- m_data$ubigeo
  
  # 7. Construcción del Panel de Regresión y Deflación de Precios de Venta
  panel_filtrado <- readRDS(path_base_panel) %>% 
    filter(ubigeo %in% u_elegidos) %>% 
    mutate(
      es_tratado = if_else(ubigeo %in% u_influencia, 1, 0),
      log_precio_chacra = log(precio_chacra_defactado) # Y3 transformado por IPC constante
    )
  
  # 8. Estimación del Estimador Diferencias en Diferencias (DiD) Clusterizado por Ubigeo
  formula_did <- as.formula(paste(var_dependiente, "~ es_tratado * post + control_sup_ha + control_edad"))
  modelo_did <- lm(formula_did, data = panel_filtrado, weights = weights)
  resultados_robustes <- coeftest(modelo_did, vcov = vcovCL(modelo_did, cluster = ~ubigeo))
  
  return(list(
    error = FALSE,
    psm_output = psm_obj,
    matched_data = m_data,
    reg_results = resultados_robustes,
    n_base = nrow(m_data)
  ))
}