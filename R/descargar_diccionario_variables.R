# ==========================================================================
# SCRIPT DE METADATOS: GENERACIÓN Y EXPORTACIÓN DEL DATA CODEBOOK (Tesis)
# Tesis de Maestría: María Noriega / UTEC
# ==========================================================================

library(tidyverse)

generar_csv_diccionario_variables <- function() {
  cat("\n📋 Construyendo matriz estructurada del diccionario de variables...\n")
  
  # Estructurar la tabla con los metadatos exactos del pipeline final
  diccionario_data <- tibble(
    Bloque = c(
      rep("Identificadores Estructurales", 4),
      rep("Variables Dependientes e Insumos", 5),
      rep("Variable de Tratamiento (MTC)", 1),
      rep("Variables de Control Biométricas", 3),
      rep("Variables de Control Financieras/Sociales", 3),
      rep("Variables de Entorno e Inflación", 2)
    ),
    Variable = c(
      "ubigeo", "departamento", "nombre_cultivo", "anio",
      "margen_comercial", "log_precio_chacra", "p_chacra_real", "p_fuera_real", "p_chacra_final / p_fuera_final",
      "pct_transporte_pavimentado",
      "control_sexo", "control_edad", "control_educ",
      "control_asociacion", "control_credito", "c_superficie",
      "CATEGORIA_IPC", "ipc_deflactor"
    ),
    Tipo_Dato = c(
      "Character (6 dígitos)", "Character", "Character", "Numeric (Entero)",
      "Numeric (Continuo)", "Numeric (Continuo)", "Numeric (Continuo)", "Numeric (Continuo)", "Numeric (Continuo)",
      "Numeric (Porcentual)",
      "Numeric (Porcentual)", "Numeric (Continuo)", "Numeric (Porcentual)",
      "Numeric (Porcentual)", "Numeric (Porcentual)", "Numeric (Continuo)",
      "Character", "Numeric (Continuo)"
    ),
    Escala_Medida = c(
      "Código Político Distrital", "Nombre de Región", "Nombre del Producto", "Años cronológicos (2017-2024)",
      "Soles reales por kilogramo", "Valor logarítmico natural", "Soles constantes por kilogramo", "Soles constantes por kilogramo", "Soles reales (Imputados)",
      "Escala de 0% a 100%",
      "Escala de 0% a 100%", "Valor logarítmico natural", "Escala de 0% a 100%",
      "Escala de 0% a 100%", "Escala de 0% a 100%", "Hectáreas promedio físicas",
      "Grupos alimenticios INEI", "Índice base 100 regional"
    ),
    Descripcion_Metodologica = c(
      "Llave maestra de agregación territorial distrital. Construida con paste0(CCDD, CCPP, CCDI).",
      "Nombre de la región política capturado originalmente en el Capítulo 100 y homologado a ASCII.",
      "Identificador nominal del tipo de planta alimentaria transaccionada por el productor en el Módulo 200ab.",
      "Dimensión temporal estructural que identifica la procedencia cronológica de la muestra del INEI.",
      "Variable dependiente principal (Flete Proxy). Diferencia neta entre el valor de venta externo y el intra-chacra.",
      "Logaritmo natural del precio real a pie de parcela. Mide elasticidades de transmisión de precios directas.",
      "Precio promedio bruto nominal en chacra deflactado por el IPC regional específico del rubro.",
      "Precio promedio bruto nominal en mercado local o feria deflactado por el IPC regional específico.",
      "Precios reales corregidos mediante imputación provincial (coalesce) para mitigar la pérdida muestral por NaN.",
      "Variable del nivel de tratamiento de infraestructura regional. Porcentaje de vías vecinales pavimentadas (MTC).",
      "Tasa de representatividad y liderazgo femenino. Porcentaje de Unidades Agropecuarias dirigidas por mujeres jefas.",
      "Logaritmo natural de la edad biológica promedio del productor principal (Jefe de hogar filter(P1102 == 1)).",
      "Tasa de capital humano distrital. Porcentaje de jefes de hogar con educación secundaria completa o superior (P1105 >= 6).",
      "Tasa de asociatividad agraria local. Porcentaje de agricultores pertenecientes a cooperativas o asociaciones (Cap. 800).",
      "Tasa de inclusión financiera formal. Porcentaje de agricultores con créditos agrícolas aprobados (Cap. 900).",
      "Escala física predial promedio. Hectáreas totales de la UA estimadas del Módulo 100 para controlar sesgo de minifundio.",
      "Rubro alimentario de la canasta familiar ('Frutas', 'Pan y cereales', 'Hortalizas...') usado para emparejar la inflación.",
      "Índice de Precios al Consumidor específico por departamento y tipo de alimento publicado formalmente por el INEI."
    )
  )
  
  # Asegurar existencia de la carpeta de salida
  if(!dir.exists("data_metadata")) dir.create("data_metadata")
  
  # Exportación física a la ruta de metadatos
  ruta_csv <- "data_metadata/codebook_variables_tesis.csv"
  write_csv(diccionario_data, ruta_csv)
  
  cat(paste0("\n🎉 [ÉXITO] Tabla de variables generada y descargada en: '", ruta_csv, "'\n"))
  cat("   Puedes abrir el archivo directamente con Excel para auditarlo o cargarlo en Quarto.\n\n")
  
  # Mostrar vista previa tabular limpia en la consola de RStudio
  print(diccionario_data %>% select(Variable, Tipo_Dato, Escala_Medida) %>% head(10))
}

# Ejecutar la exportación automática
generar_csv_diccionario_variables()
