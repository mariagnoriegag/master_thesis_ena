# ==============================================================================
# SCRIPT 05: GENERACIÓN DE GRÁFICOS DID (TENDENCIAS PARALELAS E IMPACTO)
# Autora: María Noriega
# Proyecto: Master Thesis ENA
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(here)

graficar_tendencias_did <- function(panel_mkt, anio_proyecto = 2018) {
  
  # Estilo temático académico para la tesis
  tema_tesis <- theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "grey30"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title       = element_text(face = "bold")
    )
  
  # Preparar datos promedio agregados por año y grupo de tratamiento
  panel_grafico <- panel_mkt %>%
    mutate(
      Grupo = ifelse(tratado == 1, "Tratamiento (Con Proyecto)", "Control (Sin Proyecto)")
    ) %>%
    group_by(anio_encuesta, Grupo) %>%
    summarise(
      precio_prom      = mean(precio_foco_kg, na.rm = TRUE),
      log_precio_prom  = mean(log_precio_chacra, na.rm = TRUE),
      pct_local_prom   = mean(pct_venta_local, na.rm = TRUE),
      indice_mkt_prom  = mean(indice_dem_oferta, na.rm = TRUE),
      
      se_precio        = sd(log_precio_chacra, na.rm = TRUE) / sqrt(n()),
      se_local         = sd(pct_venta_local, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # ----------------------------------------------------------------------------
  # GRÁFICO 1: TENDENCIAS EN PRECIO CHACRA (LOG PRECIO)
  # ----------------------------------------------------------------------------
  g1_precio <- ggplot(panel_grafico, aes(x = anio_encuesta, y = log_precio_prom, color = Grupo, group = Grupo)) +
    geom_vline(xintercept = anio_proyecto, linetype = "dashed", color = "red", alpha = 0.8, linewidth = 0.9) +
    geom_ribbon(aes(ymin = log_precio_prom - 1.96 * se_precio, ymax = log_precio_prom + 1.96 * se_precio, fill = Grupo), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3.5) +
    annotate("text", x = anio_proyecto, y = max(panel_grafico$log_precio_prom, na.rm = TRUE), 
             label = paste(" Año Proyecto (", anio_proyecto, ")", sep = ""), 
             color = "red", hjust = -0.1, fontface = "bold", size = 3.8) +
    scale_color_manual(values = c("Tratamiento (Con Proyecto)" = "#1f77b4", "Control (Sin Proyecto)" = "#ff7f0e")) +
    scale_fill_manual(values = c("Tratamiento (Con Proyecto)" = "#1f77b4", "Control (Sin Proyecto)" = "#ff7f0e")) +
    scale_x_continuous(breaks = unique(panel_grafico$anio_encuesta)) +
    labs(
      title    = "Efecto DiD sobre el Precio de Venta en Chacra",
      subtitle = "Evolución del log(Precio Chacra) del Cultivo Foco (Opción 1)",
      x        = "Año de Encuesta ENA",
      y        = "log(Precio Chacra en S/ por Kg)"
    ) +
    tema_tesis
  
  # ----------------------------------------------------------------------------
  # GRÁFICO 2: TENDENCIAS EN DIRECCIONAMIENTO LOCAL (% VENTA LOCAL)
  # ----------------------------------------------------------------------------
  g2_local <- ggplot(panel_grafico, aes(x = anio_encuesta, y = pct_local_prom, color = Grupo, group = Grupo)) +
    geom_vline(xintercept = anio_proyecto, linetype = "dashed", color = "red", alpha = 0.8, linewidth = 0.9) +
    geom_ribbon(aes(ymin = pct_local_prom - 1.96 * se_local, ymax = pct_local_prom + 1.96 * se_local, fill = Grupo), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3.5) +
    annotate("text", x = anio_proyecto, y = max(panel_grafico$pct_local_prom, na.rm = TRUE), 
             label = paste(" Año Proyecto (", anio_proyecto, ")", sep = ""), 
             color = "red", hjust = -0.1, fontface = "bold", size = 3.8) +
    scale_color_manual(values = c("Tratamiento (Con Proyecto)" = "#2ca02c", "Control (Sin Proyecto)" = "#d62728")) +
    scale_fill_manual(values = c("Tratamiento (Con Proyecto)" = "#2ca02c", "Control (Sin Proyecto)" = "#d62728")) +
    scale_x_continuous(breaks = unique(panel_grafico$anio_encuesta)) +
    labs(
      title    = "Efecto DiD sobre el Direccionamiento Comercial Local",
      subtitle = "Evolución del Porcentaje de Venta Comercializada Localmente (Opción 1)",
      x        = "Año de Encuesta ENA",
      y        = "% Venta en Mercado Local"
    ) +
    tema_tesis
  
  # ----------------------------------------------------------------------------
  # GRÁFICO 3: ÍNDICE INTEGRADO DE PRESIÓN DE DEMANDA (OPCIÓN 2)
  # ----------------------------------------------------------------------------
  g3_indice <- ggplot(panel_grafico, aes(x = anio_encuesta, y = log(indice_mkt_prom + 1), color = Grupo, group = Grupo)) +
    geom_vline(xintercept = anio_proyecto, linetype = "dashed", color = "red", alpha = 0.8, linewidth = 0.9) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3.5) +
    annotate("text", x = anio_proyecto, y = max(log(panel_grafico$indice_mkt_prom + 1), na.rm = TRUE), 
             label = paste(" Año Proyecto (", anio_proyecto, ")", sep = ""), 
             color = "red", hjust = -0.1, fontface = "bold", size = 3.8) +
    scale_color_manual(values = c("Tratamiento (Con Proyecto)" = "#9467bd", "Control (Sin Proyecto)" = "#8c564b")) +
    scale_x_continuous(breaks = unique(panel_grafico$anio_encuesta)) +
    labs(
      title    = "Evolución del Índice de Presión Demográfica (Opción 2)",
      subtitle = "Relación entre Tamaño de Población y Oferta Cosechada (Población / Ha)",
      x        = "Año de Encuesta ENA",
      y        = "log(Población / Oferta Ha)"
    ) +
    tema_tesis
  
  return(list(
    grafico_precio = g1_precio,
    grafico_local  = g2_local,
    grafico_indice = g3_indice
  ))
}