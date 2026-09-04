# ==============================================================================
# FUNCION 4: REGRESIONES DE DIFERENCIAS EN DIFERENCIAS (DiD)
# ==============================================================================

library(tidyverse)
library(fixest)

estimar_did_caso <- function(df_panel, var_dep = "log_precio_chacra", opcion = 2) {
  
  if (opcion == 1) {
    # Opción 1: Término de Interacción Triple (Heterogeneidad por Población)
    fmla <- as.formula(paste0(var_dep, " ~ treat * post * poblacion | ubigeo + anio"))
  } else if (opcion == 2) {
    # Opción 2: Índice de Presión de Mercado (Demanda / Oferta)
    df_panel <- df_panel %>% mutate(ratio_presion = poblacion / (as.numeric(P204_SUP) + 0.001))
    fmla <- as.formula(paste0(var_dep, " ~ treat * post + ratio_presion | ubigeo + anio"))
  }
  
  modelo <- feols(fmla, data = df_panel, cluster = ~ubigeo)
  return(modelo)
}