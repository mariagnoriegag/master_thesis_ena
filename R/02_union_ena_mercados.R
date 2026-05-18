library(tidyverse)
library(haven)

# 1. Cargar y filtrar mercados: Solo intervenciones del 2023
mercados_2023 <- read_csv("data_external/anio_codd_mercado_mejoras.csv", 
                          col_types = cols(UBIGEO = col_character())) %>%
  filter(ANIO == 2023) %>% # <--- CRUCIAL: Solo lo que se hizo justo antes del 2024
  select(UBIGEO, TIPO_PROYECTO) %>%
  distinct(UBIGEO, .keep_all = TRUE) %>%
  mutate(es_tratado = 1)

# 2. Cargar y preparar ENA 2022 (Línea de Base)
ena_2022 <- read_sav("data_raw/ENA_2022_Módulos/Cap200ab.sav") %>% 
  rename_with(tolower) %>%
  mutate(
    anio = 2022, 
    post = 0, # Antes de la intervención
    UBIGEO = paste0(ccdd, ccpp, ccdi)
  ) %>%
  select(anio, post, UBIGEO, p204_nom, p220_1_pre_kg, p221_1, p221_2)

# 3. Cargar y preparar ENA 2024 (Seguimiento)
ena_2024 <- read_sav("data_raw/ENA_2024_Módulos/Cap200ab.sav") %>% 
  rename_with(tolower) %>%
  mutate(
    anio = 2024, 
    post = 1, # Después de la intervención
    UBIGEO = paste0(ccdd, ccpp, ccdi)
  ) %>%
  select(anio, post, UBIGEO, p204_nom, p220_1_pre_kg, p221_1, p221_2)

# 4. Unión de bases y asignación de Tratamiento
base_final_did <- bind_rows(ena_2022, ena_2024) %>%
  left_join(mercados_2023, by = "UBIGEO") %>%
  mutate(
    es_tratado = replace_na(es_tratado, 0),
    TIPO_PROYECTO = replace_na(TIPO_PROYECTO, "Sin Intervención")
  )

# 5. Guardar la base procesada en la carpeta 'data'
saveRDS(base_final_did, "data/base_final_did_2022_2024.rds")

# --- Verificación de la nueva muestra ---
cat("Muestra filtrada por intervención 2023:\n")
print(table(base_final_did$anio, base_final_did$TIPO_PROYECTO))
