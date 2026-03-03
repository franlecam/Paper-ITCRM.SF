library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(openxlsx)
library(tidyr)

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

# #### TRABAJANDO CON UN ANIO ####
# base_2017 <- leer_archivo(archivos[1])
# 
# glimpse(base_2017)
# 
# n_distinct(base_2017$destino) # Cantidad de países únicos
# 
# range(base_2017$mes) # Rango de meses
# 
# 
# base_2017 %>%
#   filter(tipo_destino == "agregado") %>%
#   distinct(destino)
# 
# 
# base_2017_mensual <- base_2017 %>%
#   arrange(destino, año, mes) %>%
#   group_by(destino, año) %>%
#   mutate(
#     exportaciones_mensual_raw = case_when(
#       mes == min(mes, na.rm = TRUE) ~ exportaciones_acum,
#       TRUE ~ exportaciones_acum - lag(exportaciones_acum)
#     ),
#     correccion_estadistica = exportaciones_mensual_raw < 0,
#     exportaciones_mensual = if_else(
#       exportaciones_mensual_raw < 0,
#       0,
#       exportaciones_mensual_raw
#     )
#   ) %>%
#   ungroup()
# 
# 
# base_2017_mensual %>%
#   filter(exportaciones_mensual < 0) #TIENE QUE DAR CERO
# 
# 
# base_2017_mensual %>%
#   group_by(destino, año) %>%
#   summarise(
#     total_mensual = sum(exportaciones_mensual, na.rm = TRUE),
#     dic_acum = max(exportaciones_acum, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(diferencia = dic_acum - total_mensual)
# 
# 
# base_2017_limpia <- base_2017_mensual %>%
#   group_by(destino, año) %>%
#   filter(
#     any(!is.na(exportaciones_acum)) &
#       max(exportaciones_acum, na.rm = TRUE) > 0
#   ) %>%
#   ungroup()
# 
# 
# base_2017_limpia %>%
#   filter(str_detect(
#     destino,
#     regex("dato igual|dato que no|///", ignore_case = TRUE)
#   ))
# 
# n_distinct(base_2017_limpia$destino)
# 
# setdiff(
#   unique(base_2017$destino),
#   unique(base_2017_limpia$destino)
# )  #CATEGORIAS QUE SE ELIMINAN
# 


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

 
base_total <- map_dfr(archivos, leer_archivo)  #LOOP PARA HACERLO CON TODOS LOS ARCHIVOS


destinos_validos <- base_total %>%
  group_by(destino, año) %>%
  summarise(
    max_acum = max(exportaciones_acum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(max_acum), max_acum > 0)


base_final <- base_total %>%
  semi_join(destinos_validos, by = c("destino", "año"))

range(base_final$mes) #CHECK QUE DEBE DAR 1 12

n_distinct(base_final$destino)

#### PIVOT PARA TENER DESTINOS COMO COLUMNAS ####

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


write.xlsx(
  base_pivot_destinos,
  file = "base_mensual_2017-2025.xlsx",
  overwrite = TRUE
)

#### ARMANDO LAS CANASTAS MENSUALES ####
base_paises <- base_final %>%
  filter(tipo_destino == "pais") %>%
  select(destino, año, mes, exportaciones_mensual) %>%
  group_by(año, mes) %>%
  mutate(
    total_mes = sum(exportaciones_mensual, na.rm = TRUE),
    participacion = exportaciones_mensual / total_mes
  ) %>%
  ungroup() %>%
  mutate(
    fecha = as.Date(sprintf("%d-%02d-01", año, mes))
  )

base_paises %>%
  group_by(fecha) %>%
  summarise(suma = sum(participacion, na.rm = TRUE))

base_pivot_part <- base_paises %>%
  select(fecha, destino, participacion) %>%
  group_by(fecha, destino) %>%
  summarise(
    participacion = sum(participacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = destino,
    values_from = participacion,
    values_fill = 0
  ) %>%
  arrange(fecha)

write.xlsx(
  base_pivot_part,
  file = "participaciones_panel_mensual.xlsx",
  overwrite = TRUE
)
