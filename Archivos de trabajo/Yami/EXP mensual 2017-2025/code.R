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


ruta_datos <- "C:/Users/SFC/OneDrive/Escritorio/Paper-ITCRM.SF/Archivos de trabajo/Yami/EXP mensual 2017-2025"

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

base_2017_mensual <- base_2017 %>%
  arrange(destino, año, mes) %>%
  group_by(destino, año) %>%
  mutate(
    exportaciones_mensual_raw = case_when(
      mes == min(mes, na.rm = TRUE) ~ exportaciones_acum,
      TRUE ~ exportaciones_acum - lag(exportaciones_acum)
    ),
    correccion_estadistica = exportaciones_mensual_raw < 0,
    exportaciones_mensual = if_else(
      exportaciones_mensual_raw < 0,
      0,
      exportaciones_mensual_raw
    )
  ) %>%
  ungroup()


base_2017_mensual %>%
  filter(exportaciones_mensual < 0)

base_2017_mensual %>%
  group_by(destino, año) %>%
  summarise(
    total_mensual = sum(exportaciones_mensual, na.rm = TRUE),
    dic_acum = max(exportaciones_acum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(diferencia = dic_acum - total_mensual)

base_2017_limpia <- base_2017_mensual %>%
  group_by(destino, año) %>%
  filter(
    any(!is.na(exportaciones_acum)) &
      max(exportaciones_acum, na.rm = TRUE) > 0
  ) %>%
  ungroup()

base_2017_limpia %>%
  filter(str_detect(
    destino,
    regex("dato igual|dato que no|///", ignore_case = TRUE)
  ))

n_distinct(base_2017_limpia$destino)


#### PARA EL RESTO DE LOS ANIOS ####
leer_archivo <- function(path) {
  
  anio <- extraer_anio(path)
  hojas <- excel_sheets(path)
  
  base_anual <- map_dfr(
    hojas,
    ~ leer_hoja(path, .x, anio)
  )
  
  base_anual %>%
    arrange(destino, mes) %>%
    group_by(destino, año) %>%
    mutate(
      exportaciones_mensual_raw = case_when(
        mes == min(mes, na.rm = TRUE) ~ exportaciones_acum,
        TRUE ~ exportaciones_acum - lag(exportaciones_acum)
      ),
      correccion_estadistica = exportaciones_mensual_raw < 0,
      exportaciones_mensual = if_else(
        exportaciones_mensual_raw < 0,
        0,
        exportaciones_mensual_raw
      )
    ) %>%
    ungroup()
}

base_total <- map_dfr(archivos, leer_archivo)

destinos_validos <- base_total %>%
  group_by(destino, año) %>%
  summarise(
    max_acum = max(exportaciones_acum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(max_acum), max_acum > 0)

base_final <- base_total %>%
  semi_join(destinos_validos, by = c("destino", "año"))

range(base_final$mes) #CHECH QUE DEBE DAR 1 12

base_final %>%
  filter(exportaciones_mensual < 0)

n_distinct(base_final$destino)

write.csv(
  base_final,
  "base_exportaciones_mensual_limpia.csv",
  row.names = FALSE
)

#### PIVOT PARA TENER DESTINOS COMO COLUMNAS ####
library(tidyr)

base_final_unica <- base_final %>%
  group_by(destino, año, mes) %>%
  summarise(
    exportaciones_mensual = sum(exportaciones_mensual, na.rm = TRUE),
    .groups = "drop"
  )


base_panel <- base_final_unica %>%
  mutate(fecha = as.Date(paste(año, mes, "01", sep = "-")))

base_pivot_destinos <- base_panel %>%
  select(fecha, destino, exportaciones_mensual) %>%
  pivot_wider(
    names_from  = destino,
    values_from = exportaciones_mensual
  )

dim(base_pivot_destinos)

glimpse(base_pivot_destinos)

base_pivot_destinos <- base_pivot_destinos %>%
  mutate(across(-fecha, ~ replace_na(.x, 0)))

library(openxlsx)

write.xlsx(
  base_pivot_destinos,
  file = "base_mensual_2017-2025.xlsx",
  overwrite = TRUE
)

#### ARMANDO LAS CANASTAS MENSUALES ####
base_paises <- base_final %>%
  filter(tipo_destino == "pais") %>%
  select(destino, año, mes, exportaciones_mensual)

base_paises <- base_paises %>%
  group_by(año, mes) %>%
  mutate(
    total_mes_paises = sum(exportaciones_mensual, na.rm = TRUE)
  ) %>%
  ungroup()

base_paises <- base_paises %>%
  mutate(
    participacion = exportaciones_mensual / total_mes_paises
  )

base_paises %>%
  group_by(año, mes) %>%
  summarise(suma = sum(participacion))

base_pareto_paises <- base_paises %>%
  arrange(año, mes, desc(participacion)) %>%
  group_by(año, mes) %>%
  mutate(
    participacion_acum = cumsum(participacion)
  ) %>%
  ungroup()

paises_80_mes <- base_pareto_paises %>%
  group_by(año, mes) %>%
  filter(
    participacion_acum <= 0.80 |
      lag(participacion_acum, default = 0) < 0.80
  ) %>%
  ungroup()

base_80_participaciones <- paises_80_mes %>%
  select(
    destino,
    año,
    mes,
    exportaciones_mensual,
    participacion,
    participacion_acum
  )

base_80_participaciones %>%
  group_by(año, mes) %>%
  summarise(
    cobertura = sum(participacion),
    n_paises = n()
  )

library(openxlsx)

write.xlsx(
  base_80_participaciones,
  file = "base_paises_80_participaciones_mensual.xlsx",
  overwrite = TRUE
)

base_80_participaciones <- base_80_participaciones %>%
  mutate(
    fecha = as.Date(sprintf("%d-%02d-01", año, mes))
  )

base_pivot_part <- base_80_participaciones %>%
  select(fecha, destino, participacion) %>%
  pivot_wider(
    names_from  = destino,
    values_from = participacion,
    values_fill = 0
  ) %>%
  arrange(fecha)

base_pivot_part %>%
  mutate(suma = rowSums(across(-fecha))) %>%
  summarise(
    min = min(suma),
    max = max(suma)
  )

dim(base_pivot_part)

write.xlsx(
  base_pivot_part,
  file = "participaciones_paises_80_panel.xlsx",
  overwrite = TRUE
)
