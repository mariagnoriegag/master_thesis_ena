# ==============================================================================
# Script: 06_grafico_did.R
# Objetivo: Graficar las tendencias del DiD (Impacto en Margen Comercial)
# ==============================================================================

library(tidyverse)

# 1. CARGAR DATOS ------------------------------------------------------------
base_para_did <- readRDS("data/base_para_did_2022_2024.rds") %>%
  filter(margen_comercial > 0)

# 2. CALCULAR LOS PROMEDIOS PREDICTOS POR EL MODELO ----------------------------
# Usamos los valores exactos que salieron de tu regresión para armar las líneas
data_grafico <- tribble(
  ~Periodo, ~Grupo,       ~Margen,
  "2022 (Pre)",  "Control",    0.402583, # Intercepto
  "2022 (Pre)",  "Tratamiento", 0.402583 + 0.048364, # Intercepto + es_tratado
  "2024 (Post)", "Control",    0.402583 + 0.101286, # Intercepto + post
  "2024 (Post)", "Tratamiento", 0.402583 + 0.048364 + 0.101286 - 0.260647 # Total interactuado
) %>%
  # Forzamos el orden de los años en el eje X
  mutate(Periodo = factor(Periodo, levels = c("2022 (Pre)", "2024 (Post)")))

# 3. GENERAR EL GRÁFICO CON GGPLOT2 --------------------------------------------
grafico_did <- ggplot(data_grafico, aes(x = Periodo, y = Margen, group = Grupo, color = Grupo)) +
  geom_line(aes(linetype = Grupo), size = 1.2) +
  geom_point(size = 4) +
  # Etiquetas de datos para que los números se vean en el gráfico
  geom_text(aes(label = round(Margen, 3)), vjust = -1.2, size = 4.5, show.legend = FALSE) +
  # Paleta de colores formal para tesis (Azul para Tratamiento, Gris para Control)
  scale_color_manual(values = c("Control" = "#7f8c8d", "Tratamiento" = "#2c3e50")) +
  scale_linetype_manual(values = c("Control" = "dashed", "Tratamiento" = "solid")) +
  # Ajustes de escalas y títulos
  scale_y_continuous(limits = c(0.2, 0.6)) +
  labs(
    title = "Efecto de los Mercados de Abastos sobre el Margen Comercial",
    subtitle = "Modelo de Diferencias en Diferencias (Soles reales por kg)",
    x = "Periodo de Evaluación",
    y = "Margen Comercial Promedio (Flete Proxy)",
    caption = "Fuente: Elaboración propia basada en datos de la ENA (2022-2024) e IPC-INEI."
  ) +
  # Tema visual limpio/académico
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black", face = "bold")
  )

# 4. VISUALIZAR Y GUARDAR ------------------------------------------------------
print(grafico_did)

# Guardamos el gráfico automáticamente en tu carpeta 'output'
ggsave("output/grafico_impacto_did.png", plot = grafico_did, width = 8, height = 6, dpi = 300)
cat("Éxito: Gráfico guardado en 'output/grafico_impacto_did.png'\n")