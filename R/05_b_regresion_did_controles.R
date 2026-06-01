# ==============================================================================
# Script: 05_b_regresion_did_controles.R
# Objetivo: Estimación del Modelo DiD incluyendo Variables de Control (Covariables X)
# ==============================================================================

library(tidyverse)
library(lmtest)
library(sandwich)

# 1. CARGAR DATOS CON CONTROLES INTEGRADOS -------------------------------------
# Cargamos la base generada por tu script de controles anteriores
base_final <- readRDS("data/base_para_did_2022_2024_controles.rds") %>%
  filter(margen_comercial > 0)

# 2. MODELO DiD CON CONTROLES (FLETE PROXY) ------------------------------------
# Agregamos las variables X a nivel distrital del periodo PRE (2022) 
# para aislar características de la unidad, productor, crédito y asociación.

modelo_did_controles <- lm(
  margen_comercial ~ es_tratado + post + es_tratado * post + 
    control_sup_ha + 
    control_riego + 
    control_edad + 
    control_educ + 
    control_credito + 
    control_asociacion, 
  data = base_final
)

# Errores Estándares Robustos clústerizados por UBIGEO
coeftest_controles <- coeftest(
  modelo_did_controles, 
  vcov = vcovCL(modelo_did_controles, cluster = ~ubigeo)
)

cat("==================================================================\n")
cat("   MODELO 1 CON CONTROLES: DIFERENCIAS EN DIFERENCIAS ROBUSTO     \n")
cat("==================================================================\n")
print(coeftest_controles)


# 3. MODELO DiD HETEROGÉNEO CON CONTROLES --------------------------------------
# Evaluamos Mercado Nuevo vs Mejora manteniendo el blindaje de los controles X.

modelo_hetero_controles <- lm(
  margen_comercial ~ tipo_proyecto + post + tipo_proyecto * post + 
    control_sup_ha + 
    control_riego + 
    control_edad + 
    control_educ + 
    control_credito + 
    control_asociacion, 
  data = base_final
)

coeftest_hetero_controles <- coeftest(
  modelo_hetero_controles, 
  vcov = vcovCL(modelo_hetero_controles, cluster = ~ubigeo)
)

cat("\n==================================================================\n")
cat(" MODELO 2 CON CONTROLES: HETEROGENEIDAD (MERCADO VS MEJORA) ROBUSTO\n")
cat("==================================================================\n")
print(coeftest_hetero_controles)