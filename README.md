# Impacto de Mercados de Abastos en la Producción Agropecuaria (ENA)

Este proyecto de investigación utiliza un diseño cuasi-experimental de **Diferencias en Diferencias (DiD)** combinado con **Propensity Score Matching (PSM)** para evaluar cómo la creación y mejora de mercados de abastos afecta el precio en chacra y la comercialización de productos agrícolas en el Perú.

## 🚀 Ruta Metodológica

1.  **Limpieza y Armonización:** Unión de la base de inversiones en mercados con los microdatos de la ENA mediante el código de Ubigeo (6 dígitos).
2.  **Matching (PSM):** Identificación de "distritos gemelos" que no recibieron la intervención para servir como grupo de control.
3.  **Análisis DiD:** Estimación del efecto del tratamiento en cultivos seleccionados (Arroz, Maíz, Papa, Frutas).

## 📂 Estructura del Proyecto

-   `data/`: Contiene los microdatos de la ENA y la base de mercados (`anio_codd_mercado_mejoras.csv`). *Nota: Los datos de la ENA no se sincronizan con GitHub por privacidad.*
-   `R/`: Scripts modulares (`01_limpieza.R`, `02_matching.R`, `03_did.R`).
-   `docs/`: Diccionarios de variables del INEI y protocolos metodológicos.
-   `output/`: Gráficos de tendencias paralelas y tablas de resultados.

## 🛠️ Requisitos

Es necesario tener instalado R y las siguientes librerías: \`\`\` r install.packages(c("tidyverse", "haven", "MatchIt", "fixest", "yaml", "modelsummary"))
