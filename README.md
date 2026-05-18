# Impacto de Mercados de Abastos en la Producción Agropecuaria (ENA)
**Autora:** María Noriega

Este proyecto de investigación utiliza un diseño cuasi-experimental de **Diferencias en Diferencias Espaciales (Spatial DiD)** combinado dinámicamente con un algoritmo de **Propensity Score Matching (PSM)**. El objetivo es evaluar el impacto causal de la creación y mejora de infraestructura de mercados de abastos sobre los márgenes comerciales, los precios en chacra y los canales de comercialización de la agricultura familiar en el Perú.

---

## 🚀 Ruta Metodológica

El diseño empírico se ejecuta de manera iterativa a través del espacio, el tiempo y múltiples especificaciones de variables de resultado:

1. **Agregación Distrital y Armonización Panel:** Consolidación de los microdatos a nivel de productor de la Encuesta Nacional Agropecuaria (ENA) hacia promedios distritales agregados por código de Ubigeo (6 dígitos).
2. **Definición de Tratamiento Espacial Dinámico:** Trazado de buffers geográficos radiales específicos (0 km, 10 km, 20 km, 30 km, 40 km y 100 km) utilizando sistemas de información geográfica (GIS) a partir de los centroides de los distritos intervenidos.
3. **Propensity Score Matching (PSM) Condicional:** Emparejamiento por vecino más cercano durante el periodo de línea de base, utilizando un banco de control de más de 600 distritos agrícolas para equilibrar covariables estructurales (superficie agraria, riego, edad, educación, acceso a crédito y asociatividad).
4. **Estimación de Modelos DiD Globales y Heterogéneos:** Evaluación del impacto neto mediante modelos lineales con errores estándar robustos clusterizados por Ubigeo, controlando el supuesto de tendencias paralelas.
5. **Evaluación de Robustez Multicohorte:** Validación cruzada de la persistencia temporal mediante tres cohortes de intervención pública:
   * **Cohorte 1:** Periodo 2017 - 2019 (Intervención: Año 2018)
   * **Cohorte 2:** Periodo 2018 - 2022 (Intervención: Año 2019)
   * **Cohorte 3:** Periodo 2022 - 2024 (Intervención: Año 2023)

---

## 📁 Estructura del Proyecto

* **`data/`**: Contiene las bases de datos intermedias y los paneles finales comprimidos (`.rds`). 
  * *Nota: Los microdatos brutos de la ENA provienen de los repositorios oficiales del INEI y se omiten del control de versiones por motivos de privacidad.*
* **`data_spatial/`**: Repositorio de la cartografía vectorial oficial (Shapefiles del Límite Distrital del INEI).
* **`R/`**: Scripts modulares ordenados secuencialmente:
  * `01_limpieza.R`: Importación, limpieza de datos atípicos y etiquetado de la ENA.
  * `02_matching.R`: Estimación inicial y balance de covariables pre-intervención.
  * `03_did.R`: Modelos lineales base de Diferencias en Diferencias.
  * `04_d_robustez_variables.R`: Script dinámico de calibración espacial y PSM cíclico.
  * `05_d_regresion_robustez.R`: Funciones automatizadas para análisis de sensibilidad geográfica y gráficos de tendencias paralelas.
* **`docs/`**: Diccionarios de variables, fichas técnicas del INEI y notas de campo metodológicas.
* **`graphs/`**: Salidas visuales en alta definición de las validaciones de tendencias paralelas para cada radio evaluado.
* **`output/`**: Tablas de coeficientes globales recopiladas y resúmenes de heterogeneidad por tipo de proyecto.

---

## 🛠️ Requisitos e Instalación

Para asegurar la reproducibilidad de este entorno cuasi-experimental, es necesario contar con una instalación activa de R y ejecutar la instalación de las siguientes librerías de soporte espacial y econométrico:

```R
install.packages(c(
  "tidyverse",    # Manipulación de datos y visualización (ggplot2)
  "sf",           # Procesamiento de objetos de geometría espacial (Simple Features)
  "MatchIt",      # Algoritmos de Propensity Score Matching
  "lmtest",       # Pruebas de hipótesis e inferencia para modelos lineales
  "sandwich",     # Estimación de matrices de varianza-covarianza robustas (Cluster-Robust SE)
  "haven",        # Lectura de microdatos en formatos nativos (SPSS/Stata)
  "modelsummary"  # Exportación de tablas de regresión con calidad de publicación
))
