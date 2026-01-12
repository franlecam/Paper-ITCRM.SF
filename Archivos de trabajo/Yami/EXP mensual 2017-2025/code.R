library(readxl)
library(dplyr)
library(purrr)
library(stringr)

mes_map <- c(
  ene = 1, feb = 2, mar = 3, abr = 4,
  may = 5, jun = 6, jul = 7, ago = 8,
  sep = 9, oct = 10, nov = 11, dic = 12
)

extraer_mes <- function(nombre_hoja) {
  
  meses <- str_extract_all(
    str_to_lower(nombre_hoja),
    "ene|feb|mar|abr|may|jun|jul|ago|sep|oct|nov|dic"
  )[[1]]
  
  mes_map[tail(meses, 1)]
}


ruta_datos <- "C:/Users/SFC/Downloads/EXP mensual 2017-2025"

archivos <- list.files(
  path = ruta_datos,
  pattern = "^EXP_\\d{4}\\.xlsx$",
  full.names = TRUE
)

archivos

extraer_anio <- function(path) {
  as.integer(str_extract(basename(path), "\\d{4}"))
}

read_excel(archivos[1], sheet = 1) |> glimpse()

leer_hoja <- function(path, hoja, anio) {
  
  mes <- extraer_mes(hoja)
  
  read_excel(
    path,
    sheet = hoja,
    skip = 9,
    col_names = FALSE
  ) %>%
    select(1, 2) %>%
    rename(
      destino = ...1,
      exportaciones_acum = ...2
    ) %>%
    filter(
      !is.na(destino),
      destino != ""
    ) %>%
    mutate(
      exportaciones_acum = as.numeric(
        str_replace_all(exportaciones_acum, "[^0-9.-]", "")
      ),
      año = anio,
      mes = mes,
      tipo_destino = if_else(
        destino == toupper(destino),
        "agregado",
        "pais"
      )
    )
}




leer_archivo <- function(path) {
  
  anio <- extraer_anio(path)
  hojas <- excel_sheets(path)
  
  map_dfr(
    hojas,
    ~ leer_hoja(path, .x, anio)
  )
}

base_2017 <- leer_archivo(archivos[1])

glimpse(base_2017)

# Cantidad de países únicos
n_distinct(base_2017$destino)

# Rango de meses
range(base_2017$mes)

# Ver si quedó algún total
base_2017 %>% 
  filter(str_detect(destino, "TOTAL|AMERICA|EUROPA|AFRICA"))

base_2017 <- leer_archivo(archivos[1])

base_2017 %>%
  count(tipo_destino)

base_2017 %>%
  filter(tipo_destino == "agregado") %>%
  distinct(destino)

