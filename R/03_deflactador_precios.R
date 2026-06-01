# ==============================================================================
# Script: 03_deflactador_precios.R
# Objetivo: Deflactar precios nominales de la ENA 2022 y 2024 usando IPC regional
# ==============================================================================

library(tidyverse)

# 1. CARGAR DATOS ------------------------------------------------------------
base_final_did <- readRDS("data/base_final_did_2022_2024.rds")
ipc_data <- read_csv("data_external/ipc_especifico_2022_2024.csv")

# 2. DICCIONARIO DE CULTIVOS -------------------------------------------------
# Aseguramos que la categoría coincida EXACTAMENTE con el texto de tu IPC
diccionario_cultivos <- tribble(
  ~p204_nom,            ~categoria,
  "PLATANO",            "Frutas",
  "MANGO",              "Frutas",
  "PALTO",              "Frutas",
  "PAPA BLANCA",        "Hortalizas, legumbres y tubérculos",
  "MAIZ CHOCLO",        "Hortalizas, legumbres y tubérculos",
  "ALFALFA",            "Hortalizas, legumbres y tubérculos",
  "TOMATE",             "Hortalizas, legumbres y tubérculos",
  "CEBOLLA",            "Hortalizas, legumbres y tubérculos",
  "ZANAHORIA",          "Hortalizas, legumbres y tubérculos",
  "MAIZ CHALA",         "Hortalizas, legumbres y tubérculos",
  "MAIZ AMARILLO DURO", "Pan y cereales",
  "ARROZ CASCARA",      "Pan y cereales",
  "MAIZ AMILACEO",      "Pan y cereales"
)

# 3. PROCESAMIENTO Y DEFLACTACIÓN --------------------------------------------

# Limpiamos primero la base de IPC para que tenga 'cod_dd' en minúsculas
ipc_limpio <- ipc_data %>%
  rename_with(tolower) %>%
  mutate(
    cod_dd = str_pad(as.character(cod_dd), 2, pad = "0"),
    categoria = str_trim(categoria)
  )

# Ahora procesamos la base principal
base_deflactada <- base_final_did %>%
  rename_with(tolower) %>% 
  mutate(
    # Extraemos el departamento del UBIGEO (los 2 primeros dígitos)
    cod_dd = substr(ubigeo, 1, 2),
    p204_nom = toupper(str_trim(p204_nom))
  ) %>%
  # Unión 1: Asignar categoría al producto
  left_join(diccionario_cultivos, by = "p204_nom") %>%
  # Unión 2: Traer el índice IPC
  left_join(ipc_limpio, by = c("cod_dd", "anio", "categoria")) %>%
  mutate(
    # Deflactamos usando la columna 'indice_anual' (o el nombre que tenga en tu csv)
    # Si tu columna de IPC se llama distinto, cámbiala aquí:
    precio_real = (p220_1_pre_kg / ipc_anual) * 100 
  ) %>%
  # Quitamos los que no tienen precio o no se pudieron deflactar
  filter(!is.na(precio_real))

# 4. GUARDAR Y VERIFICAR -----------------------------------------------------
saveRDS(base_deflactada, "data/base_deflactada_2022_2024.rds")

cat("--- RESULTADOS DE LA DEFLACTACIÓN ---\n")
base_deflactada %>%
  group_by(anio) %>%
  summarise(
    registros = n(),
    precio_nom_medio = mean(p220_1_pre_kg, na.rm = TRUE),
    precio_real_medio = mean(precio_real, na.rm = TRUE)
  ) %>%
  print()

# CONTROL DE CALIDAD (VERIFICACIÓN) -----------------------------------------
cat("--- VERIFICACIÓN DE PRECIOS MEDIOS ---\n")
control <- base_deflactada %>%
  group_by(anio, categoria) %>%
  summarise(
    n = n(),
    precio_nom = mean(p220_1_pre_kg, na.rm = TRUE),
    precio_real = mean(precio_real, na.rm = TRUE),
    .groups = 'drop'
  )

print(control)

# Verificación en consola
cat("--- VALIDACIÓN FINAL ---\n")
print(head(base_deflactada %>% select(anio, ubigeo, cod_dd, p204_nom, p220_1_pre_kg, precio_real)))

cat("\nÉxito: Base deflactada guardada en data/base_deflactada.rds\n")