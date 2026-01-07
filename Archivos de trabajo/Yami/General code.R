
# PRUEBA PARA 1 ARCHIVO ####

df <- read_excel(
  "R:/Proyecto Ciclos Santa Fe/DB_actual_desde_02-09-20/IPEC/Comercio exterior/Importaciones por aduanas de la provincia/2018/importaciones-0118.xlsx",
  sheet = "Nomenclador",
  range = "A8:C10000",
  col_names = FALSE
)

indices <- which(grepl("(?i)^por aduana", df[[1]]) | grepl("(?i)^total aduanas", df[[1]]))
indices <- c(indices, nrow(df) + 1)

aduanas <- list()

for (i in seq_along(indices[-length(indices)])) {
  inicio <- indices[i]
  fin <- indices[i + 1] - 1
  nombre <- df[[1]][inicio]
  
  bloque <- df[(inicio + 1):fin, ] %>%
    filter(!is.na(...1)) %>%
    filter(!grepl("(?i)total", ...1))
  
  names(bloque) <- c("ncm", "cif_dolar", "peso_kg")
  
  bloque <- bloque %>%
    mutate(
      ncm = str_trim(as.character(ncm)),
      ncm = ifelse(str_detect(ncm, "^[0-9]{1,7}$"),
                   str_pad(ncm, width = 8, pad = "0"),
                   ncm),
      cif_dolar = as.numeric(str_replace_all(cif_dolar, "-", "0")),
      peso_kg   = as.numeric(str_replace_all(peso_kg, "-", "0"))
    ) %>%
    # mantener solo filas con NCM de 8 dígitos
    filter(str_detect(ncm, "^[0-9]{8}$"))
  
  aduanas[[nombre]] <- bloque
}

names(aduanas)

prueba <- sapply(aduanas, function(x) sum(x[[2]], na.rm = TRUE))[1] -  sum(sapply(aduanas, function(x) sum(x[[2]], na.rm = TRUE))[2:6])

prueba == 0



# PAQUETES ####

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)

options(scipen = 999)


# PERIODOS RECTIFICADOS ####

## CODIGO PARA TODO EL DIRECTORIO ####

### IMPORTACION POR NOMENCLADOR ####

# Carpeta base
base_dir <- "R:/Proyecto Ciclos Santa Fe/DB_actual_desde_02-09-20/IPEC/Comercio exterior/Importaciones por aduanas de la provincia"

# Años a procesar
anios <- c("2019", "2020", "2021", "2022", "2023", "2024", "2025")

# Crear lista vacía general
importaciones_ncm_rectificado <- list()

for (anio in anios) {
  
  # Buscar archivos Excel dentro del año
  # archivos <- list.files(file.path(base_dir, anio), 
  #                        pattern = "\\.xlsx$",
  #                        full.names = TRUE)
  archivos <- list.files(
    file.path(base_dir, anio),
    pattern = "^Importaciones-[0-9]{4}\\.xlsx$",
    full.names = TRUE
  )
  
  # Lista para cada año
  lista_anio <- list()
  
  for (archivo in archivos) {
    
    # Nombre corto del archivo, sin extensión (por ej. "importaciones_ncm_rectificado-0823")
    nombre_archivo <- tools::file_path_sans_ext(basename(archivo))
    
    # Leer base
    df <- read_excel(
      archivo,
      sheet = "Nomenclador",
      range = "A8:C10000",
      col_names = FALSE
    )
    
    # Identificar encabezados
    indices <- which(grepl("(?i)^por aduana", df[[1]]) | grepl("(?i)^total aduanas", df[[1]]))
    indices <- c(indices, nrow(df) + 1)
    
    # Crear lista para las aduanas de este archivo
    aduanas <- list()
    
    for (i in seq_along(indices[-length(indices)])) {
      inicio <- indices[i]
      fin <- indices[i + 1] - 1
      nombre <- df[[1]][inicio]
      
      bloque <- df[(inicio + 1):fin, ] %>%
        filter(!is.na(...1)) %>%
        filter(!grepl("(?i)total", ...1))
      
      names(bloque) <- c("ncm", "cif_dolar", "peso_kg")
      
      bloque <- bloque %>%
        mutate(
          ncm = str_trim(as.character(ncm)),
          ncm = ifelse(str_detect(ncm, "^[0-9]{1,7}$"),
                       str_pad(ncm, width = 8, pad = "0"),
                       ncm),
          cif_dolar = as.numeric(str_replace_all(cif_dolar, "-", "0")),
          peso_kg   = as.numeric(str_replace_all(peso_kg, "-", "0"))
        ) %>%
        filter(str_detect(ncm, "^[0-9]{8}$"))
      
      # Crear variable de periodo (por ejemplo "2022-08" a partir de "Importaciones-0823")
      mes_archivo <- str_extract(nombre_archivo, "[0-9]{2}(?=[0-9]{2}$)")   # 08
      anio_archivo <- str_extract(nombre_archivo, "[0-9]{2}$")              # 23
      
      # Ajustar el año de referencia (uno menos)
      anio_referencia <- paste0("20", as.numeric(anio_archivo) - 1)
      periodo_ref <- paste0(anio_referencia, "-", mes_archivo)
      
      bloque$periodo <- periodo_ref
      
      aduanas[[nombre]] <- bloque
    }
    
    # Guardar lista de aduanas dentro del período
    lista_anio[[nombre_archivo]] <- aduanas
  }
  
  # Guardar lista del año completo
  importaciones_ncm_rectificado[[as.character(as.numeric(anio) - 1)]] <- lista_anio
}

#### VERIFICACION ####

# Crear una tabla de resultados vacía
validaciones_ncm_rectificado <- data.frame(
  anio = character(),
  periodo = character(),
  diferencia = numeric(),
  coincide = logical(),
  stringsAsFactors = FALSE
)

# Bucle por año y período
for (anio in names(importaciones_ncm_rectificado)) {
  for (periodo in names(importaciones_ncm_rectificado[[anio]])) {
    
    aduanas <- importaciones_ncm_rectificado[[anio]][[periodo]]
    
    # Calcular totales como hacías antes
    totales <- sapply(aduanas, function(x) sum(x[[2]], na.rm = TRUE))
    
    # Asegurar que haya al menos 6 aduanas (por consistencia)
    if (length(totales) >= 6) {
      diferencia <- totales[1] - sum(totales[2:6])
    } else {
      diferencia <- NA
    }
    
    # Guardar resultado
    validaciones_ncm_rectificado <- rbind(
      validaciones_ncm_rectificado,
      data.frame(
        anio = anio,
        periodo = periodo,
        diferencia = diferencia,
        coincide = abs(diferencia) < 1,  # tolerancia numérica
        stringsAsFactors = FALSE
      )
    )
  }
}

# Mostrar resumen
validaciones_ncm_rectificado

## IMPORTACIONES POR DESTINO ####

# Carpeta base
  base_dir <- "R:/Proyecto Ciclos Santa Fe/DB_actual_desde_02-09-20/IPEC/Comercio exterior/Importaciones por aduanas de la provincia"

# Años a procesar
  anios <- c("2019", "2020", "2021", "2022", "2023", "2024", "2025")

# Crear lista vacía general
importaciones_origen_rectificado <- list()

for (anio in anios) {
  
  # Buscar archivos Excel dentro del año
  # archivos <- list.files(file.path(base_dir, anio), 
  #                        pattern = "\\.xlsx$",
  #                        full.names = TRUE)
  archivos <- list.files(
    file.path(base_dir, anio),
    pattern = "^Importaciones-[0-9]{4}\\.xlsx$",
    full.names = TRUE
  )
  
  # Lista para cada año
  lista_anio <- list()
  
  for (archivo in archivos) {
    
    # Nombre corto del archivo, sin extensión (por ej. "importaciones_origen_rectificado-0823")
    nombre_archivo <- tools::file_path_sans_ext(basename(archivo))
    
    # Leer base
    df <- read_excel(
      archivo,
      sheet = "Origen",
      range = "A8:C10000",
      col_names = FALSE
    )
    
    # Identificar encabezados de bloque
    indices <- which(grepl("(?i)^por aduana", df[[1]]) | grepl("(?i)^total aduanas", df[[1]]))
    indices <- c(indices, nrow(df) + 1)
    
    # Crear lista para las aduanas de este archivo
    aduanas <- list()
    
    # Recorrer bloques de aduanas
    for (i in seq_along(indices[-length(indices)])) {
      inicio <- indices[i]
      fin <- indices[i + 1] - 1
      nombre <- df[[1]][inicio]
      
      bloque <- df[(inicio + 1):fin, ] %>%
        filter(!is.na(...1)) %>%
        filter(!grepl("(?i)total", ...1))
      
      # Renombrar columnas
      names(bloque) <- c("pais", "cif_dolar", "peso_kg")
      
      # Limpieza y conversión
      bloque <- bloque %>%
        mutate(
          pais = str_trim(as.character(pais)),
          cif_dolar = as.numeric(str_replace_all(cif_dolar, "-", "0")),
          peso_kg   = as.numeric(str_replace_all(peso_kg, "-", "0"))
        ) %>%
        # eliminar filas vacías o no válidas
        filter(!is.na(pais) & !pais %in% c("", "NA")) %>%
        # eliminar filas tipo notas
        filter(!str_detect(tolower(pais), "dato|fuente|///")) %>%
        # eliminar filas posteriores a "Resto países"
        {
          if (any(str_detect(.$pais, "(?i)^resto pais"))) {
            slice_head(., n = which(str_detect(.$pais, "(?i)^resto pais"))[1])
          } else {
            .
          }
        }
      
      # Crear variable de periodo (por ejemplo "2022-08" a partir de "Importaciones-0823")
      mes_archivo <- str_extract(nombre_archivo, "[0-9]{2}(?=[0-9]{2}$)")   # 08
      anio_archivo <- str_extract(nombre_archivo, "[0-9]{2}$")              # 23
      
      # Ajustar el año de referencia (uno menos)
      anio_referencia <- paste0("20", as.numeric(anio_archivo) - 1)
      periodo_ref <- paste0(anio_referencia, "-", mes_archivo)
      
      bloque$periodo <- periodo_ref
      
      # Guardar bloque en la lista de aduanas
      aduanas[[nombre]] <- bloque
    }
    
    # Guardar lista de aduanas dentro del período
    lista_anio[[nombre_archivo]] <- aduanas
  }
  
  # Guardar lista del año completo
  importaciones_origen_rectificado[[as.character(as.numeric(anio) - 1)]] <- lista_anio
}

### VERIFICACION ####

# Crear una tabla de resultados vacía
validaciones_origen_rectificado <- data.frame(
  anio = character(),
  periodo = character(),
  diferencia = numeric(),
  coincide = logical(),
  stringsAsFactors = FALSE
)

# Bucle por año y período
for (anio in names(importaciones_origen_rectificado)) {
  for (periodo in names(importaciones_origen_rectificado[[anio]])) {
    
    aduanas <- importaciones_origen_rectificado[[anio]][[periodo]]
    
    # Calcular totales como hacías antes
    totales <- sapply(aduanas, function(x) sum(x[[2]], na.rm = TRUE))
    
    # Asegurar que haya al menos 6 aduanas (por consistencia)
    if (length(totales) >= 6) {
      diferencia <- totales[1] - sum(totales[2:6])
    } else {
      diferencia <- NA
    }
    
    # Guardar resultado
    validaciones_origen_rectificado <- rbind(
      validaciones_origen_rectificado,
      data.frame(
        anio = anio,
        periodo = periodo,
        diferencia = diferencia,
        coincide = abs(diferencia) < 1,  # tolerancia numérica
        stringsAsFactors = FALSE
      )
    )
  }
}

# Mostrar resumen
validaciones_origen_rectificado


# PERIODOS NO RECTIFICADOS ####

## CODIGO PARA TODO EL DIRECTORIO ####

### IMPORTACIONES POR NOMENCLADOR ####

# Carpeta base
base_dir <- "R:/Proyecto Ciclos Santa Fe/DB_actual_desde_02-09-20/IPEC/Comercio exterior/Importaciones por aduanas de la provincia"

# Años a procesar
anios <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025")  # Podés agregar los demás después

# Crear lista vacía general
importaciones_ncm_no_rectificado <- list()

for (anio in anios) {
  
  # Traer solo archivos tipo "Importaciones-xxxx.xlsx"
  archivos <- list.files(
    file.path(base_dir, anio),
    pattern = "^Importaciones-[0-9]{4}\\.xlsx$",
    full.names = TRUE
  )
  
  # Lista para cada año
  lista_anio <- list()
  
  for (archivo in archivos) {
    
    nombre_archivo <- tools::file_path_sans_ext(basename(archivo))
    
    # === LECTURA ESPECIAL POR AÑO ===
    if (anio == "2018") {
      df <- read_excel(
        archivo,
        sheet = "Nomenclador",
        range = "A8:E20000",
        col_names = FALSE
      ) %>% 
        select(1, 2, 3)   # <--- columnas especiales 2018
    } else {
      df <- read_excel(
        archivo,
        sheet = "Nomenclador",
        range = "A8:E20000",
        col_names = FALSE
      ) %>% 
        select(1, 4, 5)   # columnas estándar
    }
    
    # Identificar encabezados
    indices <- which(grepl("(?i)^por aduana", df[[1]]) | grepl("(?i)^total aduanas", df[[1]]))
    indices <- c(indices, nrow(df) + 1)
    
    # Crear lista para las aduanas del archivo
    aduanas <- list()
    
    for (i in seq_along(indices[-length(indices)])) {
      
      inicio <- indices[i]
      fin    <- indices[i + 1] - 1
      nombre <- df[[1]][inicio]
      
      bloque <- df[(inicio + 1):fin, ] %>%
        filter(!is.na(...1)) %>%
        filter(!grepl("(?i)total", ...1))
      
      names(bloque) <- c("ncm", "cif_dolar", "peso_kg")
      
      bloque <- bloque %>%
        mutate(
          ncm = str_trim(as.character(ncm)),
          ncm = ifelse(str_detect(ncm, "^[0-9]{1,7}$"),
                       str_pad(ncm, width = 8, pad = "0"),
                       ncm),
          cif_dolar = as.numeric(str_replace_all(cif_dolar, "-", "0")),
          peso_kg   = as.numeric(str_replace_all(peso_kg, "-", "0"))
        ) %>%
        filter(str_detect(ncm, "^[0-9]{8}$"))
      
      # Obtener período a partir del nombre
      mes_archivo  <- str_extract(nombre_archivo, "[0-9]{2}(?=[0-9]{2}$)")
      anio_archivo <- str_extract(nombre_archivo, "[0-9]{2}$")
      
      anio_referencia <- paste0("20", as.numeric(anio_archivo))
      periodo_ref <- paste0(anio_referencia, "-", mes_archivo)
      
      bloque$periodo <- periodo_ref
      
      aduanas[[nombre]] <- bloque
    }
    
    lista_anio[[nombre_archivo]] <- aduanas
  }
  
  importaciones_ncm_no_rectificado[[anio]] <- lista_anio
}


#### VERIFICACION ####

# Crear una tabla de resultados vacía
validaciones_ncm_no_rectificado <- data.frame(
  anio = character(),
  periodo = character(),
  diferencia = numeric(),
  coincide = logical(),
  stringsAsFactors = FALSE
)

# Bucle por año y período
for (anio in names(importaciones_ncm_no_rectificado)) {
  for (periodo in names(importaciones_ncm_no_rectificado[[anio]])) {
    
    aduanas <- importaciones_ncm_no_rectificado[[anio]][[periodo]]
    
    # Calcular totales como hacías antes
    totales <- sapply(aduanas, function(x) sum(x[[2]], na.rm = TRUE))
    
    # Asegurar que haya al menos 6 aduanas (por consistencia)
    if (length(totales) >= 6) {
      diferencia <- totales[1] - sum(totales[2:6])
    } else {
      diferencia <- NA
    }
    
    # Guardar resultado
    validaciones_ncm_no_rectificado <- rbind(
      validaciones_ncm_no_rectificado,
      data.frame(
        anio = anio,
        periodo = periodo,
        diferencia = diferencia,
        coincide = abs(diferencia) < 1,  # tolerancia numérica
        stringsAsFactors = FALSE
      )
    )
  }
}

# Mostrar resumen
validaciones_ncm_no_rectificado

## IMPROTACIONES POR DESTINO ####

# Carpeta base
base_dir <- "R:/Proyecto Ciclos Santa Fe/DB_actual_desde_02-09-20/IPEC/Comercio exterior/Importaciones por aduanas de la provincia"
  

# Años a procesar
anios <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025")

# Crear lista vacía general
importaciones_origen_no_rectificado <- list()

for (anio in anios) {
  
  archivos <- list.files(
    file.path(base_dir, anio),
    pattern = "^Importaciones-[0-9]{4}\\.xlsx$",
    full.names = TRUE
  )
  
  lista_anio <- list()
  
  for (archivo in archivos) {
    
    nombre_archivo <- tools::file_path_sans_ext(basename(archivo))
    
    # --- Selección de columnas depende del año ---
    if (anio == "2018") {
      cols_to_select <- c(1, 2, 3)
    } else {
      cols_to_select <- c(1, 4, 5)
    }
    # ------------------------------------------------
    
    df <- read_excel(
      archivo,
      sheet = "Origen",
      range = "A8:E10000",
      col_names = FALSE
    ) %>% 
      select(all_of(cols_to_select))
    
    # Identificar encabezados
    indices <- which(grepl("(?i)^por aduana", df[[1]]) | 
                       grepl("(?i)^total aduanas", df[[1]]))
    indices <- c(indices, nrow(df) + 1)
    
    aduanas <- list()
    
    for (i in seq_along(indices[-length(indices)])) {
      inicio <- indices[i]
      fin <- indices[i + 1] - 1
      nombre <- df[[1]][inicio]
      
      bloque <- df[(inicio + 1):fin, ] %>%
        filter(!is.na(...1)) %>%
        filter(!grepl("(?i)total", ...1))
      
      # Renombra según columnas (siempre país / cif / peso)
      names(bloque) <- c("pais", "cif_dolar", "peso_kg")
      
      bloque <- bloque %>%
        mutate(
          pais = str_trim(as.character(pais)),
          cif_dolar = as.numeric(str_replace_all(cif_dolar, "-", "0")),
          peso_kg = as.numeric(str_replace_all(peso_kg, "-", "0"))
        ) %>%
        filter(!is.na(pais) & pais != "") %>%
        filter(!str_detect(tolower(pais), "dato|fuente|///")) %>%
        {
          if (any(str_detect(.$pais, "(?i)^resto pais"))) {
            slice_head(., n = which(str_detect(.$pais, "(?i)^resto pais"))[1])
          } else .
        }
      
      mes_archivo <- str_extract(nombre_archivo, "[0-9]{2}(?=[0-9]{2}$)")
      anio_archivo <- str_extract(nombre_archivo, "[0-9]{2}$")
      anio_referencia <- paste0("20", as.numeric(anio_archivo))
      periodo_ref <- paste0(anio_referencia, "-", mes_archivo)
      
      bloque$periodo <- periodo_ref
      
      aduanas[[nombre]] <- bloque
    }
    
    lista_anio[[nombre_archivo]] <- aduanas
  }
  
  importaciones_origen_no_rectificado[[anio]] <- lista_anio
}

### VERIFICACION ####

# Crear una tabla de resultados vacía
validaciones_origen_no_rectificado <- data.frame(
  anio = character(),
  periodo = character(),
  diferencia = numeric(),
  coincide = logical(),
  stringsAsFactors = FALSE
)

# Bucle por año y período
for (anio in names(importaciones_origen_no_rectificado)) {
  for (periodo in names(importaciones_origen_no_rectificado[[anio]])) {
    
    aduanas <- importaciones_origen_no_rectificado[[anio]][[periodo]]
    
    # Calcular totales como hacías antes
    totales <- sapply(aduanas, function(x) sum(x[[2]], na.rm = TRUE))
    
    # Asegurar que haya al menos 6 aduanas (por consistencia)
    if (length(totales) >= 6) {
      diferencia <- totales[1] - sum(totales[2:6])
    } else {
      diferencia <- NA
    }
    
    # Guardar resultado
    validaciones_origen_no_rectificado <- rbind(
      validaciones_origen_no_rectificado,
      data.frame(
        anio = anio,
        periodo = periodo,
        diferencia = diferencia,
        coincide = abs(diferencia) < 1,  # tolerancia numérica
        stringsAsFactors = FALSE
      )
    )
  }
}

# Mostrar resumen
validaciones_origen_no_rectificado

# OBTENIENDO MENSUALES ####

## LIMPIANDO OBJETOS PREVIOS INNECESARIOS ####

rm(aduanas, bloque, df, lista_anio, anio, anios, archivo, archivos, base_dir, diferencia, fin, i, indices, inicio, nombre, nombre_archivo, periodo, totales, anio_archivo, anio_referencia, mes_archivo, periodo_ref)

## LLEVANDO CADA PERIODO A UNA BASE CONJUNTA ####

library(purrr)

aplanar_importaciones <- function(lista) {
  map_dfr(names(lista), function(anio) {          # nivel año
    map_dfr(names(lista[[anio]]), function(periodo) {  # nivel período
      map_dfr(names(lista[[anio]][[periodo]]), function(aduana) {  # nivel aduana
        df <- lista[[anio]][[periodo]][[aduana]]
        
        # agregar columna tipo_aduana
        df$tipo_aduana <- aduana
        
        # reordenar columnas según formato pedido
        df <- df %>%
          select(tipo_aduana, everything())
      })
    })
  })
}

impo_ncm_rect       <- aplanar_importaciones(importaciones_ncm_rectificado)
impo_ncm_no_rect    <- aplanar_importaciones(importaciones_ncm_no_rectificado)
impo_origen_rect    <- aplanar_importaciones(importaciones_origen_rectificado)
impo_origen_no_rect <- aplanar_importaciones(importaciones_origen_no_rectificado)

### EXPORTANDO BASES ACUMULADAS 
#### 1. FUNCIONES AUXILIARES #### 

crear_tabla_wide <- function(df, id_cols, value_col, divisor = 1, convertir_fecha = TRUE) {
  df %>%
    mutate(
      periodo = if (convertir_fecha) as.Date(paste0(periodo, "-01")) else periodo
    ) %>%
    group_by(across(all_of(c(id_cols, "periodo")))) %>%
    summarise(valor = sum(.data[[value_col]], na.rm = TRUE) / divisor, .groups = "drop") %>%
    pivot_wider(names_from = periodo, values_from = valor)
}

#### 2. TABLAS NO RECTIFICADAS #### 

# NCM - valor
ncm_no_rect_valor <- crear_tabla_wide(
  impo_ncm_no_rect,
  id_cols = c("ncm", "tipo_aduana"),
  value_col = "cif_dolar",
  divisor = 1e6
)

# NCM - peso
ncm_no_rect_peso <- crear_tabla_wide(
  impo_ncm_no_rect,
  id_cols = c("ncm", "tipo_aduana"),
  value_col = "peso_kg",
  divisor = 1000
)

# Origen - valor
origen_no_rect_valor <- crear_tabla_wide(
  impo_origen_no_rect,
  id_cols = c("pais", "tipo_aduana"),
  value_col = "cif_dolar",
  divisor = 1e6
)

# Origen - peso
origen_no_rect_peso <- crear_tabla_wide(
  impo_origen_no_rect,
  id_cols = c("pais", "tipo_aduana"),
  value_col = "peso_kg",
  divisor = 1000
)

#### 3. TABLAS RECTIFICADAS #### 

# NCM - valor
ncm_rect_valor <- crear_tabla_wide(
  impo_ncm_rect,
  id_cols = c("ncm", "tipo_aduana"),
  value_col = "cif_dolar",
  divisor = 1e6
)

# NCM - peso
ncm_rect_peso <- crear_tabla_wide(
  impo_ncm_rect,
  id_cols = c("ncm", "tipo_aduana"),
  value_col = "peso_kg",
  divisor = 1000
)

# Origen - valor
origen_rect_valor <- crear_tabla_wide(
  impo_origen_rect,
  id_cols = c("pais", "tipo_aduana"),
  value_col = "cif_dolar",
  divisor = 1e6
)

# Origen - peso
origen_rect_peso <- crear_tabla_wide(
  impo_origen_rect,
  id_cols = c("pais", "tipo_aduana"),
  value_col = "peso_kg",
  divisor = 1000
)

####  4. EXPORTAR A EXCEL (NO RECTIFICADO) #### 

wb_no_rect <- createWorkbook()

addWorksheet(wb_no_rect, "NCM_valor")
writeData(wb_no_rect, "NCM_valor", ncm_no_rect_valor)

addWorksheet(wb_no_rect, "NCM_peso")
writeData(wb_no_rect, "NCM_peso", ncm_no_rect_peso)

addWorksheet(wb_no_rect, "Origen_valor")
writeData(wb_no_rect, "Origen_valor", origen_no_rect_valor)

addWorksheet(wb_no_rect, "Origen_peso")
writeData(wb_no_rect, "Origen_peso", origen_no_rect_peso)

saveWorkbook(wb_no_rect, "importaciones_no_rect.xlsx", overwrite = TRUE)

#### 5. EXPORTAR A EXCEL (RECTIFICADO) #### 

wb_rect <- createWorkbook()

addWorksheet(wb_rect, "NCM_valor")
writeData(wb_rect, "NCM_valor", ncm_rect_valor)

addWorksheet(wb_rect, "NCM_peso")
writeData(wb_rect, "NCM_peso", ncm_rect_peso)

addWorksheet(wb_rect, "Origen_valor")
writeData(wb_rect, "Origen_valor", origen_rect_valor)

addWorksheet(wb_rect, "Origen_peso")
writeData(wb_rect, "Origen_peso", origen_rect_peso)

saveWorkbook(wb_rect, "importaciones_rect.xlsx", overwrite = TRUE)

## EXPRESANDO EN VALORES MENSUALES ####

library(dplyr)
library(lubridate)

acumulado_a_mensual <- function(df) {
  
  df <- df %>%
    mutate(periodo = as.Date(paste0(periodo, "-01")))
  
  id_var <- if ("ncm" %in% names(df)) "ncm" else "pais"
  
  df %>%
    arrange(tipo_aduana, .data[[id_var]], periodo) %>%
    group_by(tipo_aduana, .data[[id_var]], year = year(periodo)) %>%
    mutate(
      cif_dolar_mensual = if_else(
        is.na(lag(cif_dolar)),                 # primer dato del grupo
        cif_dolar,
        if_else(month(periodo) == 1, cif_dolar, cif_dolar - lag(cif_dolar))
      ),
      peso_kg_mensual = if_else(
        is.na(lag(peso_kg)),
        peso_kg,
        if_else(month(periodo) == 1, peso_kg, peso_kg - lag(peso_kg))
      )
    ) %>%
    ungroup() %>%
    select(-year)
}


impo_ncm_rect_mensual       <- acumulado_a_mensual(impo_ncm_rect)
impo_ncm_no_rect_mensual    <- acumulado_a_mensual(impo_ncm_no_rect)
impo_origen_rect_mensual    <- acumulado_a_mensual(impo_origen_rect)
impo_origen_no_rect_mensual <- acumulado_a_mensual(impo_origen_no_rect)

## VERIFICACIONES ####

##🔹 NCM RECTIFICADO ####

mean(
  abs(
    (impo_ncm_rect %>%
       mutate(periodo = as.Date(paste0(periodo, "-01"))) %>%
       arrange(tipo_aduana, ncm, periodo))$cif_dolar - (impo_ncm_rect_mensual %>%
         arrange(tipo_aduana, ncm, periodo) %>%
         group_by(tipo_aduana, ncm, year = lubridate::year(periodo)) %>%
         mutate(cif_dolar_acum = cumsum(cif_dolar_mensual)) %>%
         ungroup())$cif_dolar_acum
  ) < 1,
  na.rm = TRUE
)

##🔹 NCM NO RECTIFICADO ####

mean(abs((impo_ncm_no_rect %>% mutate(periodo = as.Date(paste0(periodo,"-01"))) %>% arrange(tipo_aduana,ncm,periodo))$cif_dolar - (impo_ncm_no_rect_mensual %>% arrange(tipo_aduana,ncm,periodo) %>% group_by(tipo_aduana,ncm,year=lubridate::year(periodo)) %>% mutate(cif_dolar_acum=cumsum(cif_dolar_mensual)) %>% ungroup())$cif_dolar_acum) < 1, na.rm=TRUE)

##🔹 Origen RECTIFICADO ####

mean(abs((impo_origen_rect %>% mutate(periodo = as.Date(paste0(periodo,"-01"))) %>% arrange(tipo_aduana,pais,periodo))$cif_dolar - (impo_origen_rect_mensual %>% arrange(tipo_aduana,pais,periodo) %>% group_by(tipo_aduana,pais,year=lubridate::year(periodo)) %>% mutate(cif_dolar_acum=cumsum(cif_dolar_mensual)) %>% ungroup())$cif_dolar_acum) < 1, na.rm=TRUE)

##🔹 Origen NO RECTIFICADO ####

mean(abs((impo_origen_no_rect %>% mutate(periodo = as.Date(paste0(periodo,"-01"))) %>% arrange(tipo_aduana,pais,periodo))$cif_dolar - (impo_origen_no_rect_mensual %>% arrange(tipo_aduana,pais,periodo) %>% group_by(tipo_aduana,pais,year=lubridate::year(periodo)) %>% mutate(cif_dolar_acum=cumsum(cif_dolar_mensual)) %>% ungroup())$cif_dolar_acum) < 1, na.rm=TRUE)

## BASES POR ADUANA

library(tidyr)

### VALOR ####

#### NCM ####

impo_ncm_no_rect_resumen_valor <- impo_ncm_no_rect_mensual %>%  # este codigo muestra los valores no rectificados totales por aduana y NCM
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(cif_dolar_mill = sum(cif_dolar_mensual, na.rm = TRUE)/1000000) %>%
  ungroup() %>%
  pivot_wider(names_from = anio, values_from = cif_dolar_mill)

impo_ncm_rect_resumen_valor <- impo_ncm_rect_mensual %>% # este codigo muestra los valores rectificados totales por aduana y ncm
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(cif_dolar_mill = sum(cif_dolar_mensual, na.rm = TRUE)/1000000) %>%
  ungroup() %>%
  pivot_wider(names_from = anio, values_from = cif_dolar_mill)

#### ORIGEN ####

##### FILTRADO ####

# EN ESTE CASO, PARA QUE DE IGUAL AL ARCHIVO DE DICIEMBRE, SE DEBEN FILTRAR LOS PAÍSES QUE APARECEN ALLÍ

paises_dic <- impo_origen_no_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  distinct(tipo_aduana, pais)

impo_origen_no_rect_mensual_filtrado <- impo_origen_no_rect_mensual %>%
  semi_join(paises_dic, by = c("tipo_aduana", "pais"))

impo_origen_no_rect_resumen_valor <- impo_origen_no_rect_mensual_filtrado %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(cif_dolar_mill = sum(cif_dolar_mensual, na.rm = TRUE)/1000000) %>%
  ungroup() %>%
  pivot_wider(names_from = anio, values_from = cif_dolar_mill)

paises_dic <- impo_origen_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  distinct(tipo_aduana, pais)

impo_origen_rect_mensual_filtrado <- impo_origen_rect_mensual %>%
  semi_join(paises_dic, by = c("tipo_aduana", "pais"))

impo_origen_rect_resumen_valor <- impo_origen_rect_mensual_filtrado %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(cif_dolar_mill = sum(cif_dolar_mensual, na.rm = TRUE)/1000000) %>%
  ungroup() %>%
  pivot_wider(names_from = anio, values_from = cif_dolar_mill)


# ESTOS TOTALES PUEDEN DIFERIR RESPECTO A LOS ARCHIVOS DE DICIEMBRE
# MI CONCLUSIÓN, PREVIO EXPLORACIÓN Y COMPARACIÓN DE VALORES, ES QUE, AL SUMAR
# VALORES MENSUALES QUE FUERON RESTADOS ARCHIVO CONTRA ARCHIVO DIFIEREN DE LOS
# TOTALES DEL ARCHIVO 12 DEBIDO A QUE REALIZAN RESCTIFICACIONES EN NOMENCLADORES
# Y PAÍSES.

#### DIFERENCIAS SUMA MENSUAL CONTRA ARCHIVO DICIEMBRE ####

##### NO RECTIFICADO ####

acum_ncm_no_rect_valor <- impo_ncm_no_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(cif_dolar_acum_dic = sum(cif_dolar, na.rm = TRUE)/1e6) %>%
  ungroup()

acum_origen_no_rect_valor <- impo_origen_no_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(cif_dolar_acum_dic = sum(cif_dolar, na.rm = TRUE)/1e6) %>%
  ungroup()


diff_ncm <- impo_ncm_no_rect_resumen_valor %>%
  pivot_longer(-tipo_aduana, names_to = "anio", values_to = "cif_dolar_mensual_sum") %>%
  mutate(anio = as.numeric(anio)) %>%
  left_join(acum_ncm_no_rect_valor, by = c("tipo_aduana", "anio")) %>%
  mutate(
    diferencia = cif_dolar_mensual_sum - cif_dolar_acum_dic,
    diferencia_por = (cif_dolar_mensual_sum / cif_dolar_acum_dic -1)*100
  ) 

diff_ncm$diferencia # ESTE CODIGO DEVUELVE NAS PORQUE ALGUNOS AÑOS PUEDEN NO ESTAR COMPLETOS Y NO HAY ARCHIVO DE ACUMULADO

diff_origen <- impo_origen_no_rect_resumen_valor %>%
  pivot_longer(-tipo_aduana, names_to = "anio", values_to = "cif_dolar_mensual_sum") %>%
  mutate(anio = as.numeric(anio)) %>%
  left_join(acum_origen_no_rect_valor, by = c("tipo_aduana", "anio")) %>%
  mutate(
    diferencia = cif_dolar_mensual_sum - cif_dolar_acum_dic,
    diferencia_por = (cif_dolar_mensual_sum / cif_dolar_acum_dic -1)*100
  ) 

diff_origen$diferencia

rm(acum_ncm_no_rect_valor, acum_origen_no_rect_valor, diff_ncm, diff_origen)

##### RECTIFICADO ####

acum_ncm_rect_valor <- impo_ncm_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(cif_dolar_acum_dic = sum(cif_dolar, na.rm = TRUE)/1e6) %>%
  ungroup()

acum_origen_rect_valor <- impo_origen_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(cif_dolar_acum_dic = sum(cif_dolar, na.rm = TRUE)/1e6) %>%
  ungroup()

diff_ncm <- impo_ncm_rect_resumen_valor %>%
  pivot_longer(-tipo_aduana, names_to = "anio", values_to = "cif_dolar_mensual_sum") %>%
  mutate(anio = as.numeric(anio)) %>%
  left_join(acum_ncm_rect_valor, by = c("tipo_aduana", "anio")) %>%
  mutate(
    diferencia = cif_dolar_mensual_sum - cif_dolar_acum_dic,
    diferencia_por = (cif_dolar_mensual_sum / cif_dolar_acum_dic -1)*100
  ) 

diff_ncm$diferencia

diff_origen <- impo_origen_rect_resumen_valor %>%
  pivot_longer(-tipo_aduana, names_to = "anio", values_to = "cif_dolar_mensual_sum") %>%
  mutate(anio = as.numeric(anio)) %>%
  left_join(acum_origen_rect_valor, by = c("tipo_aduana", "anio")) %>%
  mutate(
    diferencia = cif_dolar_mensual_sum - cif_dolar_acum_dic,
    diferencia_por = (cif_dolar_mensual_sum / cif_dolar_acum_dic -1)*100
  ) 

diff_origen$diferencia

rm(acum_ncm_rect_valor, acum_origen_rect_valor, diff_ncm, diff_origen)

### PESO ####

#### NCM ####

impo_ncm_no_rect_resumen_peso <- impo_ncm_no_rect_mensual %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(peso_ton = sum(peso_kg_mensual, na.rm = TRUE)/1000) %>%
  ungroup() %>%
  pivot_wider(names_from = anio, values_from = peso_ton)

impo_ncm_rect_resumen_peso <- impo_ncm_rect_mensual %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(peso_ton = sum(peso_kg_mensual, na.rm = TRUE)/1000) %>%
  ungroup() %>%
  pivot_wider(names_from = anio, values_from = peso_ton)

#### ORIGEN ####

paises_dic <- impo_origen_no_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  distinct(tipo_aduana, pais)

impo_origen_no_rect_mensual_filtrado <- impo_origen_no_rect_mensual %>%
  semi_join(paises_dic, by = c("tipo_aduana", "pais"))

impo_origen_no_rect_resumen_peso <- impo_origen_no_rect_mensual_filtrado %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(peso_ton = sum(peso_kg_mensual, na.rm = TRUE)/1000) %>%
  ungroup() %>%
  pivot_wider(names_from = anio, values_from = peso_ton)

paises_dic <- impo_origen_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  distinct(tipo_aduana, pais)

impo_origen_rect_mensual_filtrado <- impo_origen_rect_mensual %>%
  semi_join(paises_dic, by = c("tipo_aduana", "pais"))

impo_origen_rect_resumen_peso <- impo_origen_rect_mensual_filtrado %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(peso_ton = sum(peso_kg_mensual, na.rm = TRUE)/1000) %>%
  ungroup() %>%
  pivot_wider(names_from = anio, values_from = peso_ton)

#### DIFERENCIAS SUMA MENSUAL CONTRA ARCHIVO DICIEMBRE ####

##### NO RECTIFICADO ####

acum_ncm_no_rect_peso <- impo_ncm_no_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(peso_ton_acum_dic =sum(peso_kg, na.rm = TRUE)/1000) %>%
  ungroup()

acum_origen_no_rect_peso <- impo_origen_no_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(peso_ton_acum_dic =sum(peso_kg, na.rm = TRUE)/1000) %>%
  ungroup()

diff_ncm <- impo_ncm_no_rect_resumen_peso %>%
  pivot_longer(-tipo_aduana, names_to = "anio", values_to = "peso_ton_mensual_sum") %>%
  mutate(anio = as.numeric(anio)) %>%
  left_join(acum_ncm_no_rect_peso, by = c("tipo_aduana", "anio")) %>%
  mutate(
    diferencia = peso_ton_mensual_sum - peso_ton_acum_dic,
    diferencia_por = (peso_ton_mensual_sum / peso_ton_acum_dic -1)*100
  ) 

diff_ncm$diferencia

diff_origen <- impo_origen_no_rect_resumen_peso %>%
  pivot_longer(-tipo_aduana, names_to = "anio", values_to = "peso_ton_mensual_sum") %>%
  mutate(anio = as.numeric(anio)) %>%
  left_join(acum_origen_no_rect_peso, by = c("tipo_aduana", "anio")) %>%
  mutate(
    diferencia = peso_ton_mensual_sum - peso_ton_acum_dic,
    diferencia_por = (peso_ton_mensual_sum / peso_ton_acum_dic -1)*100
  ) 

diff_origen$diferencia

rm(acum_ncm_no_rect_peso, acum_origen_no_rect_peso, diff_ncm, diff_origen)

##### RECTIFICADO ####

acum_ncm_rect_peso <- impo_ncm_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(peso_ton_acum_dic = sum(peso_kg, na.rm = TRUE)/1000) %>%
  ungroup()

acum_origen_rect_peso <- impo_origen_rect_mensual %>%
  filter(month(periodo) == 12) %>%
  mutate(anio = year(periodo)) %>%
  group_by(tipo_aduana, anio) %>%
  summarise(peso_ton_acum_dic =sum(peso_kg, na.rm = TRUE)/1000) %>%
  ungroup()

diff_ncm <- impo_ncm_rect_resumen_peso %>%
  pivot_longer(-tipo_aduana, names_to = "anio", values_to = "peso_ton_mensual_sum") %>%
  mutate(anio = as.numeric(anio)) %>%
  left_join(acum_ncm_rect_peso, by = c("tipo_aduana", "anio")) %>%
  mutate(
    diferencia = peso_ton_mensual_sum - peso_ton_acum_dic,
    diferencia_por = (peso_ton_mensual_sum / peso_ton_acum_dic -1)*100
  )

diff_ncm$diferencia

diff_origen <- impo_origen_rect_resumen_peso %>%
  pivot_longer(-tipo_aduana, names_to = "anio", values_to = "peso_ton_mensual_sum") %>%
  mutate(anio = as.numeric(anio)) %>%
  left_join(acum_origen_rect_peso, by = c("tipo_aduana", "anio")) %>%
  mutate(
    diferencia = peso_ton_mensual_sum - peso_ton_acum_dic,
    diferencia_por = (peso_ton_mensual_sum / peso_ton_acum_dic -1)*100
  ) 
  

diff_origen$diferencia # acá hay un temita....

rm(acum_ncm_rect_peso, acum_origen_rect_peso, diff_ncm, diff_origen)

# RESUMENES ####

impo_ncm_total_norect <- impo_ncm_no_rect_mensual %>%
  filter(tipo_aduana == "TOTAL ADUANAS") %>%
  group_by(periodo) %>%
  summarise(
    cif_dolar_mensual = sum(cif_dolar_mensual, na.rm = TRUE)/1000000,
    peso_kg_mensual = sum(peso_kg_mensual, na.rm = TRUE)/1000
  ) %>%
  ungroup()

impo_ncm_total_rect <- impo_ncm_rect_mensual %>%
  filter(tipo_aduana == "TOTAL ADUANAS") %>%
  group_by(periodo) %>%
  summarise(
    cif_dolar_mensual = sum(cif_dolar_mensual, na.rm = TRUE)/1000000,
    peso_kg_mensual = sum(peso_kg_mensual, na.rm = TRUE)/1000
  ) %>%
  ungroup()


impo_ncm_total_norect_adu <- impo_ncm_no_rect_mensual %>%
  filter(tipo_aduana != "TOTAL ADUANAS") %>%
  group_by(tipo_aduana, periodo) %>%
  summarise(
    cif_dolar_mensual = sum(cif_dolar_mensual, na.rm = TRUE)/1000000,
    peso_kg_mensual = sum(peso_kg_mensual, na.rm = TRUE)/1000
  ) %>%
  ungroup()

impo_ncm_total_rect_adu <- impo_ncm_rect_mensual %>%
  filter(tipo_aduana != "TOTAL ADUANAS") %>%
  group_by(tipo_aduana, periodo) %>%
  summarise(
    cif_dolar_mensual = sum(cif_dolar_mensual, na.rm = TRUE)/1000000,
    peso_kg_mensual = sum(peso_kg_mensual, na.rm = TRUE)/1000
  ) %>%
  ungroup()

openxlsx::write.xlsx(impo_ncm_total_rect_adu, "rectificado.xlsx")
openxlsx::write.xlsx(impo_ncm_total_norect_adu, "norectificado.xlsx")

# ESTO SE VA ####

# TRABAJANDO DATOS [SEGUIR ACÁ] ####

library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)


## LIMPIANDO ENVIRONMENT ####




## MANIPULANDO DATOS ####

# NOS QUEDAMOS CON LOS DATOS RECTIFICADOS HASTA 2024.06 Y NORECTIFICADO A PARTIR DE ENTONCS HASTA 2025.06

lapply(ls(), function(x) {
  cat("\n---", x, "---\n")
  str(get(x))
})

cuestion, tengo algo así. lo que hice con rectificado, toma columnas a b y c, mientras que no rectificado toma columnas a b c d y e y se queda con la columna a d y e. la idea es que b y c tienen dolar y peso del año pasado, mientras que d y e del período. quiero un codigo 