# ==============================================================================
# Script: 04_calculo_margen.R
# Objetivo: Calcular el flete proxy (Margen Comercial) por distrito-producto
# ==============================================================================

library(tidyverse)

# 1. CARGAR DATOS DEFLACTADOS --------------------------------------------------
base_deflactada <- readRDS("data/base_deflactada_2022_2024.rds")

# 2. CALCULAR PRECIOS PROMEDIO POR PUNTO DE VENTA ------------------------------
# Nota: p221_1 == 1 significa venta "Dentro de la chacra"
#       p221_2 == 1 significa venta "Fuera de la chacra"

base_margen <- base_deflactada %>%
  group_by(anio, ubigeo, p204_nom) %>%
  summarise(
    # Precio promedio real recibido en la chacra (P_chacra)
    p_chacra = mean(precio_real[p221_1 == 1], na.rm = TRUE),
    
    # Precio promedio real recibido fuera de la chacra (P_fuera)
    p_fuera  = mean(precio_real[p221_2 == 1], na.rm = TRUE),
    
    # Arrastramos las variables del modelo DiD (constantes por distrito)
    post = max(post, na.rm = TRUE),
    es_tratado = max(es_tratado, na.rm = TRUE),
    tipo_proyecto = first(tipo_proyecto),
    .groups = 'drop'
  ) %>%
  mutate(
    # NUESTRA VARIABLE DEPENDIENTE: El flete proxy o margen
    margen_comercial = p_fuera - p_chacra
  ) %>%
  # Limpieza: Eliminamos distritos donde no se pudo calcular el margen
  # (por ejemplo, si todos los agricultores de ese distrito vendieron solo en chacra)
  filter(!is.na(margen_comercial))

# 3. GUARDAR BASE FINAL PARA EL MODELO -----------------------------------------
saveRDS(base_margen, "data/base_para_did_2022_2024.rds")
cat("Éxito: Base con márgenes calculados guardada en 'data/base_para_did.rds'\n\n")

# 4. PRIMER VISTAZO AL IMPACTO (ANÁLISIS DESCRIPTIVO) --------------------------
cat("--- COHERENCIA ECONÓMICA DEL MARGEN ---\n")
# El margen debería ser positivo en la mayoría de los casos (P_fuera > P_chacra)
print(table(base_margen$margen_comercial > 0, dnn = "¿Margen Positivo?"))

cat("\n--- EVOLUCIÓN DEL MARGEN COMERCIAL PROMEDIO ---\n")
descriptivos <- base_margen %>%
  # Filtramos márgenes lógicos (mayores a cero) para no distorsionar el promedio
  filter(margen_comercial > 0) %>% 
  group_by(post, es_tratado) %>%
  summarise(
    n_distritos_producto = n(),
    margen_medio = mean(margen_comercial),
    .groups = 'drop'
  )

print(descriptivos)