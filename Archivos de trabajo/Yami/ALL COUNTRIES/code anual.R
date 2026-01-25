library(readxl)
library(dplyr)
library(purrr)
library(stringr)

ruta_archivo <- "C:/Users/SFC/OneDrive/Escritorio/Paper-ITCRM.SF/Archivos de trabajo/Yami/ALL COUNTRIES/Exportaciones-con-origen-en-Santa-Fe-segun-destino-2001-2024-1 (1).xlsx"

hojas <- excel_sheets(ruta_archivo)

leer_y_limpiar_hoja <- function(nombre_hoja) {
  
  read_excel(
    path      = ruta_archivo,
    sheet    = nombre_hoja,
    col_names = FALSE
  ) %>% 
    select(1, 2) %>%               # nos quedamos solo con A y B
    rename(
      destino = ...1,
      exportaciones = ...2
    ) %>% 
    slice(-8) %>%                  # elimina la fila del total
    filter(!is.na(destino)) %>%    # elimina filas vacías
    mutate(
      anio = as.integer(nombre_hoja)
    )
}


base_exportaciones <- map_dfr(
  hojas,
  leer_y_limpiar_hoja
)

glimpse(base_exportaciones)
filter(base_exportaciones, str_detect(destino, "Total|TOTAL|total"))
base_exportaciones %>%
  count(anio) %>%
  arrange(anio)

library(dplyr)
library(readr)
library(stringr)

base_exportaciones_limpia <- base_exportaciones %>%
  mutate(
    exportaciones = parse_number(exportaciones)
  )

base_exportaciones_limpia <- base_exportaciones_limpia %>%
  filter(
    !is.na(exportaciones),                        # solo valores numéricos
    !str_detect(destino, regex("total", ignore_case = TRUE)),
    !str_detect(destino, regex("país", ignore_case = TRUE)),
    !str_detect(destino, regex("evolución", ignore_case = TRUE)),
    !destino %in% c("AFRICA", "AMERICA", "ASIA", "EUROPA", "OCEANIA")
  )

base_exportaciones_limpia <- base_exportaciones_limpia %>%
  mutate(
    destino = str_trim(destino),
    destino = str_to_upper(destino)
  )

str(base_exportaciones_limpia)
summary(base_exportaciones_limpia$exportaciones)
any(is.na(base_exportaciones_limpia$exportaciones))

base_participaciones <- base_exportaciones_limpia %>%
  group_by(anio) %>%
  mutate(
    total_anual = sum(exportaciones),
    participacion = exportaciones / total_anual
  ) %>%
  ungroup()

ponderadores <- base_exportaciones_limpia %>%
  group_by(anio) %>%
  mutate(
    total_anual   = sum(exportaciones),
    participacion = exportaciones / total_anual
  ) %>%
  ungroup()

ponderadores %>%
  group_by(anio) %>%
  summarise(
    suma_participaciones = sum(participacion)
  )

ponderadores_final <- ponderadores %>%
  select(anio, destino, participacion)

ponderadores_final %>%
  group_by(anio) %>%
  summarise(suma = sum(participacion))

base_exportaciones_colapsada <- base_exportaciones_limpia %>%
  group_by(anio, destino) %>%
  summarise(
    exportaciones = sum(exportaciones),
    .groups = "drop"
  )

ponderadores <- base_exportaciones_colapsada %>%
  group_by(anio) %>%
  mutate(
    total_anual   = sum(exportaciones),
    participacion = exportaciones / total_anual
  ) %>%
  ungroup()

ponderadores %>%
  group_by(anio) %>%
  summarise(suma = sum(participacion))

library(tidyr)

ponderadores_wide <- ponderadores %>%
  select(anio, destino, participacion) %>%
  pivot_wider(
    names_from  = destino,
    values_from = participacion
  ) %>%
  arrange(anio)

ponderadores_wide <- ponderadores_wide %>%
  mutate(
    across(-anio, ~ tidyr::replace_na(.x, 0))
  )

ponderadores_wide %>%
  mutate(suma = rowSums(across(-anio))) %>%
  select(anio, suma)

library(writexl)

write_xlsx(
  ponderadores_wide,
  "ponderadores_exportaciones_santa_fe_wide_2001_2024.xlsx"
)
