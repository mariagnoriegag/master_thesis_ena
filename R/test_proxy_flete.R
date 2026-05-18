library(tidyverse)
library(haven)

# 1. Cargar el módulo de producción (Cap200ab) de 2022
# Ajusta la ruta a donde tengas tu archivo .sav
df_test <- read_sav("data/ENA_2022_Módulos/Cap200ab.sav")

# 2. Resumen rápido: ¿Cuántos venden dentro vs fuera?
# P221_1 = Dentro de chacra, P221_2 = Fuera de chacra
diagnostico <- df_test %>%
  summarise(
    total_casos = n(),
    venden_dentro = sum(P221_1 == 1, na.rm = TRUE),
    venden_fuera  = sum(P221_2 == 1, na.rm = TRUE),
    ambos = sum(P221_1 == 1 & P221_2 == 1, na.rm = TRUE)
  )

print("--- Resumen de Puntos de Venta ---")
print(diagnostico)

# 3. Prueba de Fuego: ¿Hay diferencia de precios real?
# Calculamos el precio promedio por grupo
precios_comp <- df_test %>%
  filter(!is.na(P220_1_PRE_KG)) %>%
  mutate(punto_venta = case_when(
    P221_1 == 1 ~ "En Chacra",
    P221_2 == 1 ~ "Fuera de Chacra",
    TRUE ~ "Otro"
  )) %>%
  group_by(punto_venta) %>%
  summarise(
    n = n(),
    precio_promedio = mean(P220_1_PRE_KG, na.rm = TRUE),
    desviacion = sd(P220_1_PRE_KG, na.rm = TRUE)
  )

print("--- Comparación de Precios ---")
print(precios_comp)

# 4. Agrupamos por cultivo y punto de venta
diagnostico_cultivo <- df_test %>%
  filter(!is.na(P220_1_PRE_KG)) %>%
  # Limpiamos los nombres de los puntos de venta para evitar espacios
  mutate(punto_venta = case_when(
    P221_1 == 1 ~ "Chacra",
    P221_2 == 1 ~ "Fuera"
  )) %>%
  group_by(P204_NOM, punto_venta) %>%
  summarise(
    n = n(),
    precio_medio = mean(P220_1_PRE_KG, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Pasamos a formato ancho (una fila por cultivo)
  pivot_wider(names_from = punto_venta, values_from = c(n, precio_medio)) %>%
  # Calculamos el flete estimado usando nombres limpios
  mutate(flete_estimado = precio_medio_Fuera - precio_medio_Chacra) %>%
  # Filtramos solo cultivos con data suficiente en ambos lados (>50 casos)
  filter(n_Chacra > 50 & n_Fuera > 50) %>%
  arrange(desc(n_Chacra)) # Ordenamos por los más comunes

print(diagnostico_cultivo)


# 5. Intentamos calcular el margen por UBIGEO y CULTIVO

# Normalizamos nombres y creamos la variable ubigeo
df_test_geo <- df_test %>% 
  rename_with(tolower) %>%
  # Creamos el ubigeo pegando los códigos de departamento, provincia y distrito
  mutate(ubigeo_6 = paste0(ccdd, ccpp, ccdi))

# Ejecutamos el test de disponibilidad
test_disponibilidad <- df_test_geo %>%
  filter(!is.na(p220_1_pre_kg)) %>%
  mutate(punto_venta = case_when(
    p221_1 == 1 ~ "Chacra",
    p221_2 == 1 ~ "Fuera"
  )) %>%
  # Agrupamos por nuestra nueva variable ubigeo_6 y el cultivo
  group_by(ubigeo_6, p204_nom, punto_venta) %>%
  summarise(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from = punto_venta, values_from = n) %>%
  mutate(tiene_pareja = !is.na(Chacra) & !is.na(Fuera))

# Resumen de resultados
resumen_geo <- test_disponibilidad %>%
  summarise(
    total_combinaciones = n(),
    con_ambos_puntos = sum(tiene_pareja),
    porcentaje_exito = (con_ambos_puntos / total_combinaciones) * 100
  )

print("--- Disponibilidad usando CCDD + CCPP + CCDI ---")
print(resumen_geo)

# Ver qué cultivos tienen más éxito por Ubigeo
distritos_top <- test_disponibilidad %>%
  filter(tiene_pareja) %>%
  count(ubigeo_6, sort = TRUE)

print("--- Ubigeos con más cultivos comparables ---")
print(head(distritos_top, 10))

# Unir tu test con la base de mercados (asumiendo que tu csv tiene la columna 'ubigeo')
base_final_analisis <- test_disponibilidad %>%
  mutate(es_tratado = if_else(ubigeo_6 %in% lista_mercados$ubigeo, 1, 0))

# Ver cuántos de los distritos "con pareja" son realmente distritos con mercados nuevos
table(base_final_analisis$es_tratado, base_final_analisis$tiene_pareja)
