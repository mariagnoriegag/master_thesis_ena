# ==============================================================================
# NUEVA ESTRATEGIA SCRIPT 04_c: ENFOQUE ESPACIAL PRIMERO + MATCHING SEGUNDO
# Objetivo: Definir tratamiento por radio geográfico y luego buscar gemelos estadísticos.
# ==============================================================================

library(sf)
library(tidyverse)
library(MatchIt)

cat("=== Paso 1: Cargando base de datos ENA Completa y Mapa Distrital ===\n")

# 1. Cargar el mapa distrital oficial
mapa_distrital <- st_read("data_spatial/Limite Distrital INEI 2025 CPV.shp", quiet = TRUE) %>% 
  st_transform(crs = 32718) %>% 
  rename(ubigeo = UBIGEO) %>% 
  select(ubigeo, geometry)

# 2. Cargar la base que tiene los controles calculados para todos los distritos (Línea base 2022)
base_ena_2022 <- readRDS("data/base_para_did_2022_2024_controles.rds") %>% 
  filter(anio == 2022) %>% 
  distinct(ubigeo, .keep_all = TRUE)

cat("=== Paso 2: [PRIMERO] Definir Tratamiento Expandido por Radio Espacial ===\n")

# PARÁMETRO DE ENTRADA (Puedes cambiarlo a 10, 20, etc.)
RADIO_KM <- 10  
radio_metros <- RADIO_KM * 1000

# Identificar distritos con infraestructura física original
ubigeos_mercados_puros <- base_ena_2022 %>% 
  filter(es_tratado == 1) %>% 
  pull(ubigeo)

centroides_todos <- st_centroid(mapa_distrital)
centroides_mercados <- centroides_todos %>% filter(ubigeo %in% ubigeos_mercados_puros)

# Calcular matriz de distancias
matriz_distancias <- st_distance(centroides_todos, centroides_mercados)
dentro_del_radio <- apply(matriz_distancias, 1, function(fila) any(fila <= radio_metros))
ubigeos_bajo_influencia <- centroides_todos$ubigeo[dentro_del_radio]

# REASIGNAR TRATAMIENTO: Toda la cuenca geográfica de 10km se considera tratada
base_antes_del_matching <- base_ena_2022 %>% 
  mutate(
    es_tratado_original = es_tratado,
    es_tratado = if_else(ubigeo %in% ubigeos_bajo_influencia, 1, 0)
  )

cat(paste("-> Unidades bajo tratamiento geográfico en el radio:", length(ubigeos_bajo_influencia), "\n"))

cat("=== Paso 3: [SEGUNDO] Ejecutar Matching sobre el Tratamiento Espacial ===\n")

# Filtrar valores perdidos antes del matching
base_clean_matching <- base_antes_del_matching %>% 
  filter(!is.na(control_sup_ha) & !is.na(control_riego) & !is.na(control_edad) & 
           !is.na(control_educ) & !is.na(control_credito) & !is.na(control_asociacion))

# El algoritmo busca controles idénticos para todo el bloque de distritos del radio
modelo_psm_spatial <- matchit(
  es_tratado ~ control_sup_ha + control_riego + control_edad + 
    control_educ + control_credito + control_asociacion,
  data = base_clean_matching,
  method = "nearest", 
  caliper = 0.05,     
  replace = FALSE
)

# Resumen del nuevo emparejamiento masivo
print(summary(modelo_psm_spatial))

# Extraer los Ubigeos seleccionados (Nuevos Tratados en Radio + Nuevos Controles Gemelos)
datos_emparejados_2022 <- match.data(modelo_psm_spatial)
ubigeos_seleccionados_final <- datos_emparejados_2022$ubigeo

cat("=== Paso 4: Acoplar al panel completo longitudinal y guardar ===\n")

base_panel_original <- readRDS("data/base_para_did_2022_2024.rds")

base_final_spatial <- base_panel_original %>% 
  filter(ubigeo %in% ubigeos_seleccionados_final) %>% 
  # Inyectar variables de control limpias
  left_join(
    datos_emparejados_2022 %>% select(ubigeo, starts_with("control_")), 
    by = "ubigeo"
  ) %>% 
  mutate(
    # Heredar la estructura del radio geográfico de tratamiento
    es_tratado = if_else(ubigeo %in% ubigeos_bajo_influencia, 1, 0)
  )

saveRDS(base_final_spatial, "data/base_para_did_2022_2024_spatial_spillovers.rds")
cat("=== Script 04_c finalizado con éxito bajo la nueva estrategia ===\n")