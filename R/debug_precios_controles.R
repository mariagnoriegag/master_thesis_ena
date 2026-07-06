# ==========================================================================
# SCRIPT DE DEBUG: EXTACTOR MULTIMODULAR INTEGRAL DE METADATOS (SAV)
# Tesis de Maestría: María Noriega / Asesor: PhD. José Larco (UTEC)
# ==========================================================================

library(tidyverse)
library(haven)

# Función auxiliar para escanear y reportar metadatos de múltiples candidatas
inspeccionar_bloque_variables <- function(df, lista_candidatas, concepto, modulo_nombre) {
  cat(paste0("\n🔍 Concepto: ", concepto, "\n"))
  nombres_actuales_upper <- toupper(names(df))
  hallada <- FALSE
  
  for (var in lista_candidatas) {
    var_upper <- toupper(var)
    if (var_upper %in% nombres_actuales_upper) {
      hallada <- TRUE
      columna <- df[[var_upper]]
      
      # Extraer la etiqueta de la pregunta
      descripcion <- attr(columna, "label")
      if (is.null(descripcion)) descripcion <- "Sin descripción textual en los metadatos de SPSS."
      
      cat(paste0("  [+] HITT -> ", var_upper, ": ", descripcion, " (", modulo_nombre, ")\n"))
      
      # Extraer las claves de respuesta
      etiquetas_valores <- attr(columna, "labels")
      if (!is.null(etiquetas_valores)) {
        cat("      Diccionario de respuestas en SPSS:\n")
        for (i in seq_along(etiquetas_valores)) {
          cat(paste0("        - ", etiquetas_valores[i], " = ", names(etiquetas_valores)[i], "\n"))
        }
      }
    }
  }
  if (!hallada) {
    cat(paste0("  [-] ❌ NINGUNA de las variables candidatas existe en el ", modulo_nombre, " para este año.\n"))
  }
}

debug_significado_multimodulo_completo <- function(anio) {
  cat(paste("\n========================================================================\n"))
  cat(paste(" 📖 RADIOGRAFÍA INTEGRAL DE METADATOS POR MÓDULO - ENA AÑO:", anio, "\n"))
  cat(paste("========================================================================\n"))
  
  folder_name <- paste0("data_raw/ENA_", anio, "_Módulos/")
  
  f_cap100  <- paste0(folder_name, if(anio == 2024) "Cap100a_1.sav" else "Cap100_1.sav")
  f_cap200  <- paste0(folder_name, "Cap200ab.sav")
  f_cap800  <- paste0(folder_name, "Cap800.sav")     
  f_cap900  <- paste0(folder_name, "Cap900.sav")     
  f_cap1100 <- paste0(folder_name, "Cap1100.sav")   
  
  # ------------------------------------------------------------------------
  # MÓDULO 100: Características de la UA, Ubigeo y Superficie
  # ------------------------------------------------------------------------
  cat("\n========================================================================\n")
  cat("🌱 [MÓDULO 100: CARACTERÍSTICAS DE LA UNIDAD AGROPECUARIA & UBICACIÓN]")
  cat("\n========================================================================\n")
  if (file.exists(f_cap100)) {
    m100 <- read_sav(f_cap100, n_max = 5) 
    names(m100) <- toupper(names(m100))
    
    inspeccionar_bloque_variables(m100, c("P104_SUP_HA", "SUPERFICIE", "P201_SUP", "P201A_SUP", "CONTROL_SUP_HA"), "SUPERFICIE AGRÍCOLA / HECTÁREAS", "Cap 100")
    inspeccionar_bloque_variables(m100, c("FACTOR_PRODUCTOR", "FACTOR"), "FACTOR DE EXPANSIÓN", "Cap 100")
    inspeccionar_bloque_variables(m100, c("CCDD", "NOMBREDD", "DEPARTAMENTO"), "IDENTIFICADOR DEPARTAMENTO", "Cap 100")
    inspeccionar_bloque_variables(m100, c("CCPP", "CCPV"), "IDENTIFICADOR PROVINCIA", "Cap 100")
    inspeccionar_bloque_variables(m100, c("CCDI"), "IDENTIFICADOR DISTRITO", "Cap 100")
  } else { cat("  ❌ Archivo de Módulo 100 no encontrado.\n") }
  
  # ------------------------------------------------------------------------
  # MÓDULO 200: Producción, Precios, Puntos de Venta y Riego
  # ------------------------------------------------------------------------
  cat("\n========================================================================\n")
  cat("💵 [MÓDULO 200: PRODUCCIÓN, PUNTOS DE VENTA Y LOGÍSTICA DE COMERCIALIZACIÓN]")
  cat("\n========================================================================\n")
  if (file.exists(f_cap200)) {
    m200 <- read_sav(f_cap200, n_max = 5)
    names(m200) <- toupper(names(m200))
    
    inspeccionar_bloque_variables(m200, c("P204_NOM", "NOM_CULTIVO", "CULTIVO", "P218"), "NOMBRE DEL CULTIVO", "Cap 200")
    inspeccionar_bloque_variables(m200, c("P220_1_PRE_KG", "P220_1_VAL", "P220_1", "P220", "P220_1_V", "P220_1_P"), "PRECIO RECIBIDO EN CHACRA (VENTA)", "Cap 200")
    inspeccionar_bloque_variables(m200, c("P224_1", "P224", "P224_1_PRE_KG"), "PRECIO RECIBIDO FUERA DE CHACRA (MERCADO)", "Cap 200")
    inspeccionar_bloque_variables(m200, c("P221_1"), "INDICADOR: ¿VENDIÓ DENTRO DE CHACRA?", "Cap 200")
    inspeccionar_bloque_variables(m200, c("P221_2"), "INDICADOR: ¿VENDIÓ FUERA DE CHACRA?", "Cap 200")
    inspeccionar_bloque_variables(m200, c("P223_1"), "DESTINO DE LA PRODUCCIÓN / OTROS CANALES", "Cap 200")
    inspeccionar_bloque_variables(m200, c("PCT_VENTA_PRODUCTO", "P219_PCT"), "PORCENTAJE DESTINADO A LA VENTA", "Cap 200")
    inspeccionar_bloque_variables(m200, c("P121"), "INDICADOR DE RIEGO DE LA PARCELA", "Cap 200")
  } else { cat("  ❌ Archivo de Módulo 200 no encontrado.\n") }
  
  # ------------------------------------------------------------------------
  # MÓDULO 800: Organizaciones y Asociación
  # ------------------------------------------------------------------------
  cat("\n========================================================================\n")
  cat("🤝 [MÓDULO 800: PARTICIPACIÓN EN ORGANIZACIONES Y ASOCIATIVIDAD]")
  cat("\n========================================================================\n")
  if (file.exists(f_cap800)) {
    m800 <- read_sav(f_cap800, n_max = 5)
    names(m800) <- toupper(names(m800))
    
    inspeccionar_bloque_variables(m800, c("P801", "P229A", "ASOCIACION", "CONTROL_ASOCIACION"), "PERTENENCIA A ASOCIACIÓN DE PRODUCTORES", "Cap 800")
  } else { cat("  ❌ Archivo de Módulo 800 no encontrado. Verifica si se llama Cap800.sav\n") }
  
  # ------------------------------------------------------------------------
  # MÓDULO 900: Financiamiento y Crédito
  # ------------------------------------------------------------------------
  cat("\n========================================================================\n")
  cat("🏦 [MÓDULO 900: FINANCIAMIENTO, SOLICITUD Y ACCESO A CRÉDITO]")
  cat("\n========================================================================\n")
  if (file.exists(f_cap900)) {
    m900 <- read_sav(f_cap900, n_max = 5)
    names(m900) <- toupper(names(m900))
    
    inspeccionar_bloque_variables(m900, c("P901", "P228_1", "CREDITO", "CONTROL_CREDITO"), "SOLICITUD / ACCESO A CRÉDITO FORMAL", "Cap 900")
  } else { cat("  ❌ Archivo de Módulo 900 no encontrado. Verifica si se llama Cap900.sav\n") }
  
  # ------------------------------------------------------------------------
  # MÓDULO 1100: Características del Productor / Jefe de Hogar
  # ------------------------------------------------------------------------
  cat("\n========================================================================\n")
  cat("👤 [MÓDULO 1100: CARACTERÍSTICAS DEMOGRÁFICAS Y SOCIALES DEL PRODUCTOR]")
  cat("\n========================================================================\n")
  if (file.exists(f_cap1100)) {
    m1100 <- read_sav(f_cap1100, n_max = 5)
    names(m1100) <- toupper(names(m1100))
    
    inspeccionar_bloque_variables(m1100, c("P1102"), "FILTRO: CONDICIÓN DE PRODUCTOR PRINCIPAL", "Cap 1100")
    inspeccionar_bloque_variables(m1100, c("P1103", "EDAD", "CONTROL_EDAD"), "EDAD DEL PRODUCTOR", "Cap 1100")
    inspeccionar_bloque_variables(m1100, c("P1104_A", "P1104A", "EDUCACION", "CONTROL_EDUC"), "NIVEL DE EDUCACIÓN / INSTRUCCIÓN", "Cap 1100")
    inspeccionar_bloque_variables(m1100, c("P1105"), "CONDICIÓN ALFABETA / IDIOMA O COVARIABLE SOCIAL", "Cap 1100")
  } else { cat("  ❌ Archivo de Módulo 1100 no encontrado. Verifica si se llama Cap1100.sav\n") }
  
  cat("\n========================================================================\n")
}