library(tidyverse)
library(haven)

importar_y_limpiar_ena <- function(ruta_carpeta, anio_encuesta) {
  
  message(paste("Procesando ENA año:", anio_encuesta))
  
  # 1. Identificar archivos (pattern flexible)
  file_cap100  <- list.files(ruta_carpeta, pattern = "Cap100", full.names = TRUE)
  file_cap200  <- list.files(ruta_carpeta, pattern = "Cap200ab", full.names = TRUE)
  file_cap1100 <- list.files(ruta_carpeta, pattern = "Cap1100", full.names = TRUE)
  file_cap900  <- list.files(ruta_carpeta, pattern = "Cap900", full.names = TRUE)
  
  # 2. Procesar Módulo 100 (Características UA)
  df_ua <- read_sav(file_cap100, col_select = c(CODIGO, CCDD, CCPP, CCDI, P104_SUP_ha, FACTOR_PRODUCTOR)) %>%
    mutate(ubigeo = paste0(CCDD, CCPP, CCDI))
  
  # 3. Procesar Módulo 200 (Producción) - Notar cambio en variable de riego P121
  df_prod <- read_sav(file_cap200, col_select = c(CODIGO, P204_COD, P204_NOM, P220_1_PRE_KG, P220_1_VAL, P223_1, P121))
  
  # 4. Procesar Módulo 1100 (Jefe de Hogar)
  df_jefe <- read_sav(file_cap1100, col_select = c(CODIGO, P1102, P1103, P1104_A, P1105)) %>%
    filter(P1102 == 1) # Solo el productor/a principal
  
  # 5. Procesar Módulo 900 (Crédito para el Matching)
  df_finan <- read_sav(file_cap900, col_select = c(CODIGO, P901))
  
  # 6. Unión Final (Inner Join por CODIGO)
  ena_consolidada <- df_ua %>%
    inner_join(df_prod, by = "CODIGO") %>%
    inner_join(df_jefe, by = "CODIGO") %>%
    inner_join(df_finan, by = "CODIGO") %>%
    mutate(anio_ena = anio_encuesta)
  
  return(ena_consolidada)
}