# ==========================================================================
# SCRIPT DE DEBUG: AUDITORÍA EN VIVO DEL ACOPLAMIENTO MULTIMODULAR (2017)
# Tesis de Maestría: María Noriega / UTEC
# ==========================================================================

library(tidyverse)
library(haven)

debug_joins_2017 <- function() {
  cat("\n=======================================================\n")
  cat(" 🕵️‍♂️ AUDITORÍA DE PASOS: UNIÓN MULTIMODULAR ENA 2017\n")
  cat("=======================================================\n")
  
  folder_name <- "data_raw/ENA_2017_Módulos/"
  
  m100  <- read_sav(paste0(folder_name, "Cap100_1.sav"))  %>% rename_with(toupper)
  m200  <- read_sav(paste0(folder_name, "Cap200ab.sav"))  %>% rename_with(toupper)
  
  cat(paste("1. Dimensiones iniciales de Módulo 200 (Producción):", nrow(m200), "filas y", ncol(m200), "columnas.\n"))
  v_precio_inicial <- if_else("P220_1_PRE_KG" %in% names(m200), "P220_1_PRE_KG", "P220_1")
  cat(paste("   ¿Existe la variable de precio", v_precio_inicial, "en m200 original?:", v_precio_inicial %in% names(m200), "\n\n"))
  
  # Preparar m100_limpio tal cual el script maestro
  m100_limpio <- m100 %>%
    rename(
      DEPARTAMENTO_CAPTURA = matches("^DEPARTAMENTO$|^NOMBREDD$|^REG_DES$") %>% head(1),
      control_sup_ha = P104_SUP_HA,
      factor_expansion = FACTOR
    ) %>%
    mutate(
      dep_limpio = str_pad(as.character(as.numeric(CCDD)), width = 2, pad = "0"),
      pro_limpio = str_pad(as.character(as.numeric(CCPP)), width = 2, pad = "0"),
      dis_limpio = str_pad(as.character(as.numeric(CCDI)), width = 2, pad = "0"),
      ubigeo     = paste0(dep_limpio, pro_limpio, dis_limpio)
    ) %>%
    select(any_of(c("ANIO", "CONGLOMERADO", "VIVIENDA", "HOGAR", "NSELUA", "UA", "CODIGO", "CCDD", "CCPP", "CCDI")), 
           ubigeo, control_sup_ha, factor_expansion)
  
  # Ver cuáles son las llaves que R va a usar para el intersect
  llaves_intersec <- intersect(names(m200), names(m100_limpio))
  cat("2. Llaves compuestas detectadas para el join entre m200 y m100:\n")
  print(llaves_intersec)
  cat("\n")
  
  # Realizar solo el primer inner_join que causa el problema
  test_join <- m200 %>% inner_join(m100_limpio, by = llaves_intersec)
  
  cat(paste("3. Dimensiones de la tabla DESPUÉS del inner_join:", nrow(test_join), "filas.\n"))
  cat(paste("   ¿Sigue existiendo la variable", v_precio_inicial, "después de la unión?:", v_precio_inicial %in% names(test_join), "\n"))
  
  if(!(v_precio_inicial %in% names(test_join))) {
    cat("\n⚠️ ¡ALERTA! La columna desapareció en el join. Columnas disponibles con prefijo P220 ahora:\n")
    print(names(test_join)[str_detect(names(test_join), "^P220")])
  }
  cat("=======================================================\n")
}