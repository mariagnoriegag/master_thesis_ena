# ==============================================================================
# Script: 05_regresion_did.R
# Objetivo: Estimación del modelo de Diferencias en Diferencias (DiD)
# ==============================================================================

library(tidyverse)
library(lmtest)
library(sandwich)

# 1. CARGAR DATOS EN PANEL DISTRITAL-PRODUCTO ----------------------------------
base_para_did <- readRDS("data/base_para_did_2022_2024.rds") %>%
  # Nos quedamos con los márgenes válidos para el flete proxy
  filter(margen_comercial > 0)

# 2. ESTIMACIÓN DEL MODELO DiD BASE (LINEAL) ----------------------------------
# Fórmula clásica: Margen = b0 + b1*(es_tratado) + b2*(post) + b3*(es_tratado * post)
# El coeficiente b3 (la interacción) es el impacto neto del mercado.

modelo_did_base <- lm(margen_comercial ~ es_tratado + post + es_tratado * post, 
                      data = base_para_did)

# Aplicamos Errores Estándares Robustos clústerizados por UBIGEO
coeftest_base <- coeftest(modelo_did_base, 
                          vcov = vcovCL(modelo_did_base, cluster = ~ubigeo))

cat("==================================================================\n")
cat("      MODELO 1: DIFERENCIAS EN DIFERENCIAS BASE (FLETE PROXY)     \n")
cat("==================================================================\n")
print(coeftest_base)


# 3. ESTIMACIÓN CONSIDERANDO HETEROGENEIDAD (TIPO DE PROYECTO) ----------------
# Aquí evaluamos si hay diferencia entre un "Nuevo Mercado" y una "Mejora"
# con respecto al grupo que se quedó "Sin Intervención"

modelo_did_hetero <- lm(margen_comercial ~ tipo_proyecto + post + tipo_proyecto * post, 
                        data = base_para_did)

coeftest_hetero <- coeftest(modelo_did_hetero, 
                            vcov = vcovCL(modelo_did_hetero, cluster = ~ubigeo))

cat("\n==================================================================\n")
cat("  MODELO 2: HETEROGENEIDAD POR TIPO DE PROYECTO (MERCADO VS MEJORA) \n")
cat("==================================================================\n")
print(coeftest_hetero)