library(tidyverse)

# Estructurar la matriz de covariables según tu especificación exacta
matriz_covariables <- tibble(
  id = 1:16,
  nivel_agregacion = c(
    rep("1. Por distrito y cultivo", 5),
    rep("2. Por distrito", 9),
    rep("3. Por departamento", 2)
  ),
  variable_tecnica = c(
    "control_sexo", "control_edad", "control_educ", "control_asociacion", "control_cred",
    "poblacion_distrital", "n_mercados_preexistentes", "n_agricultores_proxy", 
    "pct_mercado_mayorista_mixto", "superficie_cosechada_suma", "pct_superficie_cosechada", 
    "var_superficie_cosechada", "superficie_km2", "altitud_distrital",
    "pct_pavimentacion_vecinal", "vab_agropecuario_real"
  ),
  definicion_operacional = c(
    "% de Unidades Agropecuarias (UAs) dirigidas por mujeres jefas de hogar (0-100%).",
    "Logaritmo natural de la edad promedio del productor principal del cultivo.",
    "% de productores jefes con educación secundaria completa o nivel superior (0-100%).",
    "% de agricultores del cultivo agremiados en cooperativas o asociaciones (0-100%).",
    "% de productores que solicitaron y obtuvieron financiamiento agrícola formal (0-100%).",
    "Número total de habitantes proyectado por distrito para los años base (2017, 2018, 2022).",
    "Conteo físico de mercados minoristas, de abastos o ferias locales operando en el distrito (2017, 2018, 2022).",
    "Número absoluto de productores agrícolas registrados en el territorio (Valor Fijo).",
    "% de mercados en el distrito o provincia que califican como Mayoristas o Mixtos (2017, 2018, 2022).",
    "Suma agregada en hectáreas de la superficie cosechada total del distrito (2017, 2018, 2022).",
    "% de la superficie geográfica útil dedicada a la actividad agrícola o cosechada (2017, 2018, 2022).",
    "Varianza o desviación estándar del tamaño de las parcelas agrícolas del distrito (2017, 2018, 2022).",
    "Extensión territorial física del distrito medida en kilómetros cuadrados (Constante).",
    "Elevación media en metros sobre el nivel del mar (m.s.n.m.) del centroide distrital.",
    "Variable de Tratamiento Continuo: % de km de la red vial vecinal pavimentada en la región.",
    "Valor Agregado Bruto (VAB) de la actividad Agricultura a precios constantes de 2007."
  ),
  fuente_origen = c(
    "ENA - Módulo 1100 (P1103 filtrado por jefe P1102 == 1).",
    "ENA - Módulo 1100 (P1104_A filtrado por jefe P1102 == 1).",
    "ENA - Módulo 1100 (P1105 >= 6 filtrado por jefe P1102 == 1).",
    "ENA - Módulo 800 (P801 == 1).",
    "ENA - Módulo 900 (P901 == 1).",
    "INEI - Estado de la Población Peruana (Compendio de Proyecciones por Distrito).",
    "INEI - Censo Nacional de Mercados de Abasto (CENAMA): Directorio de Mercados.",
    "INEI - IV Censo Nacional Agropecuario (CENAGRO): Cuadros por Distrito.",
    "INEI - CENAMA / MINAGRI (SISAP): Clasificación por tipo de establecimiento.",
    "MINAGRI - SIEA (Estadística Agraria Mensual por Distrito/Provincia).",
    "MINAGRI - SIEA / INEI Límite distrital.",
    "INEI - IV CENAGRO / SIEA (Distribución de frecuencia del tamaño de fincas).",
    "INEI / IGN (Diccionario Geográfico Nacional y Límite Distrital shapefile).",
    "INEI / MINAM (Diccionario de Ubigeos con variables ecológicas fijas).",
    "MTC - Oficina de Planeamiento y Presupuesto (Anuarios de la Red Vial).",
    "INEI - Cuentas Nacionales / Departamentales (Series del PBI por Actividad)."
  ),
  justificacion_econometrica = c(
    "Controla sesgos de género en el acceso a cadenas de comercialización y fletes.",
    "Captura la experiencia biológica y el ciclo de vida del agricultor en las decisiones de venta.",
    "Controla el nivel de capital humano y la asimetría de información frente a intermediarios.",
    "Mide el poder de negociación colectiva y economías de escala para reducir el flete.",
    "Controla la restricción de liquidez para trasladar la cosecha fuera de la chacra.",
    "Captura el tamaño de la demanda de consumo local y las dimensiones del mercado interno.",
    "Mide la infraestructura comercial local preexistente y la oferta de puntos de venta internos.",
    "Proxy indispensable de la densidad y presión de oferta de productores compitiendo en la zona.",
    "Captura la capacidad del clúster para consolidar grandes volúmenes de carga pesada.",
    "Mide el volumen bruto y la escala de la frontera agrícola activa en el ubigeo.",
    "Mide la vocación, intensidad y especialización del uso de la tierra en el ubigeo.",
    "Controla el sesgo de escala agraria; distingue zonas homogéneas de minifundio de latifundios.",
    "Controla la dispersión geográfica y la distancia interna promedio para salir al eje vial.",
    "Controla los pisos ecológicos determinantes del tipo de cultivo y dificultad topográfica.",
    "Mide la intensidad y exposición del territorio al choque de inversión en obras públicas.",
    "Controla el tamaño económico del sector y choques agregados climáticos regionales (ej: El Niño)."
  )
)

# Crear directorio si no existe y exportar el archivo físico CSV
if(!dir.exists("data_metadata")) dir.create("data_metadata")
write_csv(matriz_covariables, "data_metadata/matriz_covariables.csv")

cat("🎉 ¡Archivo 'data_metadata/matriz_covariables.csv' generado con éxito!\n")
