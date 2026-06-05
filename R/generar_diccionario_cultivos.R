# ==========================================================================
# SCRIPT CORREGIDO: EXTRACCIÓN AUTOMÁTICA DEL UNIVERSO DE CULTIVOS ENA (2017-2024)
# Tesis de Maestría: María Noriega / UTEC
# ==========================================================================

library(tidyverse)
library(haven)
library(stringr)

generar_csv_cultivos_maestro <- function() {
  cat("\n🔍 Escaneando carpetas de la ENA para extraer todos los cultivos...\n")
  anios <- c(2017, 2018, 2019, 2022, 2024)
  lista_anual <- list()
  
  for (anio in anios) {
    ruta_cap200 <- paste0("data_raw/ENA_", anio, "_Módulos/Cap200ab.sav")
    
    if (file.exists(ruta_cap200)) {
      cat(paste("  -> Leyendo año:", anio, "\n"))
      # Cargar solo la columna de nombres de cultivos para máxima velocidad
      m200_temp <- read_sav(ruta_cap200, col_select = any_of(c("P204_NOM", "p204_nom"))) %>% 
        rename_with(toupper)
      
      # Limpiar strings al vuelo y contar frecuencias
      cultivos_anio <- m200_temp %>%
        mutate(nombre_cultivo = tolower(str_trim(P204_NOM))) %>%
        filter(!is.na(nombre_cultivo) & nombre_cultivo != "") %>%
        count(nombre_cultivo, name = paste0("freq_", anio))
      
      lista_anual[[as.character(anio)]] <- cultivos_anio
    } else {
      cat(paste("  ⚠️ Archivo Cap200ab.sav no encontrado para el año:", anio, "\n"))
    }
  }
  
  cat("\n🔄 Consolidando matriz única de cultivos...\n")
  # Unir todas las tablas anuales mediante un full_join por nombre_cultivo
  cultivos_universo <- lista_anual %>% 
    reduce(full_join, by = "nombre_cultivo") %>%
    arrange(nombre_cultivo)
  
  # CORREGIDO: Usamos el mismo nombre 'cultivos_diccionario' consistentemente
  # MUTACIÓN CON CLASIFICACIÓN ECONOMETRICA DEL UNIVERSO DE CULTIVOS ENA
  cultivos_diccionario <- cultivos_universo %>%
    mutate(
      CATEGORIA_IPC = case_when(
        # --- Grupo 1: PAN Y CEREALES (Granos, Harinas y Tubérculos Andinos / Derivados) ---
        nombre_cultivo %in% c(
          "quinua", "maiz amarillo duro", "maiz amilaceo", "maiz choclo", "maiz morado",
          "trigo", "arroz cascara", "kiwi", "kiwicha", "cañihua", "kanyu", "chia",
          "cebada grano", "centeno grano", "avena grano", "maca"
        ) ~ "Pan y cereales",
        
        # --- Grupo 2: HORTALIZAS, LEGUMBRES Y TUBÉRCULOS (Verduras, Menestras secas/verdes y Raíces) ---
        nombre_cultivo %in% c(
          "cebolla", "cebolla china", "cebollin", "poro", "papa blanca", "papa amarilla", 
          "papa amarga", "papa color", "papa huayro", "papa nativa", "camote", "yuca", 
          "olluco", "oca", "mashua", "yacon", "arracacha", "sachapapa", "pituca", "uncucha",
          "ajo", "apio", "acelga", "espinaca", "lechuga", "brocoli", "coliflor", "col", 
          "sacha col", "pak choy", "betarraga", "zanahoria", "nabo", "rabano", "esparrago",
          "pepinillo", "caigua", "calabaza", "calabaza mate", "zapallo", "zapallo italiano", 
          "zambumba", "holantao", "ajenjo", "borraja", "berro", "albahaca", "culantro", 
          "sachaculantro", "huacatay", "chincho", "muña", "oregano", "perejil", "romero", 
          "salvia", "tomillo", "menta", "manzanilla", "toronjil", "hierba buena", "hierba luisa", 
          "cedron", "panisara", "panizara", "santa maria", "valeriana", "hinojo", "hiperico",
          "llanten", "cola de caballo", "palillo", "kion", "pimienta", "canela", "achiote", 
          "aji", "jalapena", "jalapeña", "paprika", "rocoto", "pimiento", "piquillo", "vainita",
          "alcachofa", "alcaparra", "dale dale", "holantao",
          # Todas las variedades de frijoles, habas, arvejas y pallares (secos y verdes)
          "arveja grano seco", "arveja grano verde", "arvejon grano seco", "arvejon grano verde",
          "garbanzo grano seco", "garbanzo grano verde", "lenteja grano seco", "lenteja grano verde",
          "pallar grano seco", "pallar grano verde", "tarhui grano seco", "tarhui grano verde",
          "vicia grano seco", "vicia grano verde", "zarandaja grano seco", "zarandaja grano verde",
          "haba grano seco", "haba grano verde", "nuña grano seco", "nuña grano verde", "pajuro",
          "frijol alubia grano seco", "frijol alubia grano verde", "frijol ashpa grano seco", 
          "frijol ashpa grano verde", "frijol aston grano seco", "frijol aston grano verde", 
          "frijol bayo grano seco", "frijol bayo grano verde", "frijol blanco grano seco", 
          "frijol blanco grano verde", "frijol caballero grano seco", "frijol caballero grano verde", 
          "frijol cambio noventa grano seco", "frijol cambio noventa grano verde", "frijol canario grano seco", 
          "frijol canario grano verde", "frijol caupi grano seco", "frijol caupi grano verde", 
          "frijol chaucha grano seco", "frijol chaucha grano verde", "frijol chiclayo verdura grano seco", 
          "frijol chiclayo verdura grano verde", "frijol chonteño", "frijol de palo grano seco", 
          "frijol de palo grano verde", "frijol guinda grano seco", "frijol guinda grano verde", 
          "frijol habitas grano seco", "frijol habitas grano verde", "frijol huasca grano seco", 
          "frijol huasca grano verde", "frijol huasca poroto grano seco", "frijol huasca poroto grano verde", 
          "frijol huevo de paloma grano seco", "frijol huevo de paloma grano verde", "frijol jacinto grano seco", 
          "frijol jacinto grano verde", "frijol lantreja grano seco", "frijol lantreja grano verde", 
          "frijol laran grano verde", "frijol leche grano seco", "frijol leche grano verde", 
          "frijol loctao grano seco", "frijol loctao grano verde", "frijol manteca grano seco", 
          "frijol negro grano seco", "frijol negro grano verde", "frijol pajatino grano seco", 
          "frijol pajatino grano verde", "frijol paloma grano seco", "frijol paloma grano verde", 
          "frijol panamito grano seco", "frijol panamito grano verde", "frijol pinto grano seco", 
          "frijol pinto grano verde", "frijol pucallpino grano seco", "frijol pucallpino grano verde", 
          "frijol rayado grano seco", "frijol rayado grano verde", "frijol red kidney grano seco", 
          "frijol red kidney grano verde", "frijol regional grano seco", "frijol regional grano verde", 
          "frijol rociño grano seco", "frijol rociño grano verde", "frijol rundo grano seco", 
          "frijol rundo grano verde", "frijol san jacinto grano seco", "frijol san jacinto grano verde", 
          "frijol sangre de toro grano seco", "frijol sangre de toro grano verde", "frijol shinguito grano seco", 
          "frijol shinguito grano verde", "frijol toda la vida grano seco", "frijol toda la vida grano verde", 
          "frijol ucayalino grano seco", "frijol ucayalino grano verde"
        ) ~ "Hortalizas, legumbres y tubérculos",
        
        # --- Grupo 3: FRUTAS (Nativas, Exóticas, Cítricos y Amazónicas) ---
        nombre_cultivo %in% c(
          "naranjo", "limon acido", "limon dulce", "limon mandarina", "limon rugoso", "lima",
          "platano", "mandarina", "mango", "manzano", "maracuya", "melocotonero", "melon", 
          "membrillero", "sandia", "papaya", "papayuela", "fresa", "frambuesa", "zarzamora",
          "arandano", "granadilla", "granado", "guanabano", "guayabo", "lucumo", "pacae",
          "caimito", "camu camu", "aguaje", "aguaymanto", "tumbo", "tuna fruta", "capuli",
          "caqui", "carambola", "chalarina", "chayote", "chirimbache", "chirimoyo", "chope",
          "cidra", "cirolero", "ciruela agria", "ciruela del fraile", "copoazu", "damasco",
          "guindo", "higuera", "huasai", "huito", "humari", "indano", "lanche", "mamey",
          "marañon", "nispero", "pepino fruta", "pijuayo fruta", "pitahaya", "pitanga",
          "pomarrosa", "pomelo", "purush", "quito quito", "sanky", "sauco", "sachatomate",
          "tamarindo", "tangelo", "tangerina", "taperiba", "ungurahui", "uvilla", "uvos",
          "zapote", "anona", "araza", "arbol del pan", "aspa", "atemoya", "pechiche"
        ) ~ "Frutas",
        
        # --- Grupo 4: CAFÉ, TÉ Y CACAO (Estimulantes y Bebidas Calientes) ---
        nombre_cultivo %in% c("cafe pergamino", "te", "cacao") ~ "Café, té y cacao",
        
        # --- Grupo 5: ACEITES Y GRASAS (Oleaginosas y Frutos Secos de Alimento) ---
        nombre_cultivo %in% c(
          "ajonjoli", "olivo", "mani para fruta", "mani para aceite", "almendro", 
          "castaña", "macadamia", "pecano", "walnut", "nogal", "sacha inchi"
        ) ~ "Aceites y grasas",
        
        # --- Grupo 6: NO ALIMENTARIO / EXCLUIR (Pastos, Flores, Fibras y Drogas) ---
        TRUE ~ "No alimentario / Excluir"
      ),
      
      # Conservar tu variable Dummy de control para la Tesis de tus 6 cultivos Core
      ES_CULTIVO_CORE_TESIS = if_else(nombre_cultivo %in% c("papa blanca", "maiz amarillo duro", "quinua", "cebolla", "naranjo", "limon acido"), 1, 0)
    ) %>%
    arrange(desc(ES_CULTIVO_CORE_TESIS), CATEGORIA_IPC, nombre_cultivo)
  
  # Asegurar que exista la carpeta de destino
  if(!dir.exists("data_external")) dir.create("data_external")
  
  # Exportar a CSV para descarga y edición
  ruta_salida <- "data_external/diccionario_cultivos_ipc.csv"
  write_csv(cultivos_diccionario, ruta_salida)
  
  cat(paste0("\n🎉 [ÉXITO] Archivo maestro descargado en: '", ruta_salida, "'\n"))
  cat(paste("   Se encontraron en total", nrow(cultivos_diccionario), "cultivos únicos en el histórico estructural.\n"))
}

# Ejecutar la descarga
generar_csv_cultivos_maestro()