# ==============================================================================
# SCRIPT 05_c: FUNCIÓN COMPLETA (RADIO + MATCHING + REGRESIÓN GLOBAL + GRÁFICO)
# ==============================================================================

library(lmtest)
library(sandwich)
library(tidyverse)
library(sf)
library(MatchIt)
library(ggplot2)

analisis_sensibilidad_nueva_estrategia <- function(radio_km) {
  # 1. Cargar mapa distrital
  mapa <- st_read("data_spatial/Limite Distrital INEI 2025 CPV.shp", quiet=TRUE) %>% 
    st_transform(32718) %>% rename(ubigeo=UBIGEO)
  
  # 2. Leer la base con controles del año base (2022)
  base_2022 <- readRDS("data/base_para_did_2022_2024_controles.rds") %>% 
    filter(anio == 2022) %>% distinct(ubigeo, .keep_all=TRUE)
  
  # 3. Definir tratamiento espacial primero según el radio de prueba
  u_mercados <- base_2022 %>% filter(es_tratado == 1) %>% pull(ubigeo)
  c_todos <- st_centroid(mapa)
  c_mercados <- c_todos %>% filter(ubigeo %in% u_mercados)
  
  m_dist <- st_distance(c_todos, c_mercados)
  en_radio <- apply(m_dist, 1, function(x) any(x <= (radio_km * 1000)))
  u_influencia <- c_todos$ubigeo[en_radio]
  
  # 4. Asignar tratamiento en la base de matching
  df_matching <- base_2022 %>% 
    mutate(es_tratado = if_else(ubigeo %in% u_influencia, 1, 0)) %>% 
    filter(!is.na(control_sup_ha) & !is.na(control_riego) & !is.na(control_edad) & 
             !is.na(control_educ) & !is.na(control_credito) & !is.na(control_asociacion))
  
  # 5. Hacer el Matching segundo sobre este nuevo radio geográfico
  psm <- matchit(es_tratado ~ control_sup_ha + control_riego + control_edad + 
                   control_educ + control_credito + control_asociacion,
                 data = df_matching, method = "nearest", caliper = 0.05, replace = FALSE)
  
  m_data <- match.data(psm)
  u_elegidos <- m_data$ubigeo
  
  # 6. Construir panel longitudinal (2022-2024) filtrado por el Matching
  df_regresion <- readRDS("data/base_para_did_2022_2024.rds") %>% 
    filter(ubigeo %in% u_elegidos) %>% 
    left_join(m_data %>% select(ubigeo, starts_with("control_")), by = "ubigeo") %>% 
    mutate(es_tratado = if_else(ubigeo %in% u_influencia, 1, 0))
  
  # 7. Estimar modelo de Diferencias en Diferencias con errores clúster por Ubigeo
  mod <- lm(margen_comercial ~ es_tratado*post + control_sup_ha + control_riego + 
              control_edad + control_educ + control_credito + control_asociacion, data = df_regresion)
  
  res <- coeftest(mod, vcov = vcovCL(mod, cluster = ~ubigeo))
  
  # IMPRESIÓN DEL MODELO GLOBAL COMPLETITO
  cat(paste("\n====================================================================\n"))
  cat(paste(" NUEVA ESTRATEGIA -> MODELO DE REGRESIÓN GLOBAL PARA RADIO =", radio_km, "KM\n"))
  cat(paste(" Tamaño de muestra efectivo en línea base (Matching):", nrow(m_data), "distritos.\n"))
  cat(paste("====================================================================\n"))
  print(res) # Muestra todas las filas y variables de control sin recortar
  
  # 8. GENERAR EL GRÁFICO DE TENDENCIAS PARALELAS DINÁMICO
  data_grafico <- df_regresion %>%
    group_by(post, es_tratado) %>%
    summarise(
      media_margen = mean(margen_comercial, na.rm = TRUE),
      error_estandar = sd(margen_comercial, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      Periodo = factor(post, levels = c(0, 1), labels = c("Línea de Base\n(2022)", "Post-Intervención\n(2024)")),
      Grupo = factor(es_tratado, levels = c(1, 0), labels = c("Tratamiento Espacial (Radio)", "Grupo de Control Gemelo (PSM)"))
    )
  
  grafico_did <- ggplot(data_grafico, aes(x = Periodo, y = media_margen, group = Grupo, color = Grupo)) +
    geom_line(aes(linetype = Grupo), size = 1.2) +
    geom_point(size = 4, shape = 21, fill = "white", stroke = 2) +
    geom_errorbar(aes(ymin = media_margen - error_estandar, ymax = media_margen + error_estandar), width = 0.08, size = 0.8, alpha = 0.7) +
    scale_color_manual(values = c("#1f77b4", "#7f7f7f")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    labs(
      title = paste("Supuesto de Tendencias Paralelas (Radio", radio_km, "km)"),
      subtitle = "Evolución del Margen Comercial Promedio por Condición de Distrito",
      x = "Periodo de Evaluación", y = "Margen Comercial Promedio (Soles/Kg)",
      color = "Condición:", linetype = "Condición:"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      axis.line = element_line(color = "gray40")
    )
  
  # Mostrar el gráfico en el panel derecho de RStudio
  print(grafico_did)
}