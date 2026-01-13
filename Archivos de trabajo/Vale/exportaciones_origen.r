#LIBRERIAS ####
base::library("openxlsx")
base::library("tidyverse")
base::library("dplyr")
base::library("ggplot2")

#CARGAS ARCHIVO ####
data <-
  openxlsx::read.xlsx(
    "C:/Users/vcorvalan/Desktop/Trabajo/Paper/Paper-ITCRM.SF/Archivos de trabajo/Vale/Datos/Datos_origen_2002_2024.xlsx",
    sheet = "x"
    )

data <- 
  data |> 
  select(CANIO, DESCRIP_PCIA, DESCRIP_PAIS, DOLARES_FOB)

data <-
  data |>
  mutate(
    DESCRIP_PCIA = iconv(DESCRIP_PCIA, from = "UTF-8", to = "ASCII//TRANSLIT"),
    DESCRIP_PAIS = iconv(DESCRIP_PAIS, from = "UTF-8", to = "ASCII//TRANSLIT"),
    
    DESCRIP_PCIA = toupper(DESCRIP_PCIA),
    DESCRIP_PAIS = toupper(DESCRIP_PAIS),
    
    DESCRIP_PCIA = trimws(DESCRIP_PCIA),
    DESCRIP_PAIS = trimws(DESCRIP_PAIS)
  )



#BASE AGREGADA POR DESTINO ####
base_provincial_pais <-
  data |>
  group_by(CANIO, DESCRIP_PCIA, DESCRIP_PAIS) |>
  summarise(
    fob = sum(DOLARES_FOB, na.rm = TRUE),
    .groups = "drop"
  )

base_nacional_pais <-
  data |>
  group_by(CANIO, DESCRIP_PAIS) |>
  summarise(
    fob = sum(DOLARES_FOB, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    DESCRIP_PCIA = "Argentina"
  )

base_total <-
  bind_rows(
    base_provincial_pais,
    base_nacional_pais
  )


base_anual_pct <-
  base_total |>
  group_by(CANIO, DESCRIP_PCIA) |>
  mutate(
    total_fob = sum(fob),
    porcentaje = fob / total_fob * 100
  ) |>
  ungroup()
##TOP SOCIOS COMERCIALES POR PROVINCIA ####

base_top10 <- 
  base_anual_pct %>%
  group_by(DESCRIP_PCIA, CANIO) %>%
  arrange(desc(porcentaje), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  group_by(DESCRIP_PCIA, CANIO, DESCRIP_PAIS) %>%
  summarise(porcentaje = sum(porcentaje), .groups = "drop")


## GRAFICOS ####
base_sf_nat <- base_top10 |> 
  filter(DESCRIP_PCIA %in% c("SANTA FE", "Argentina"))

ggplot(base_sf_nat,
       aes(x = DESCRIP_PCIA, y = porcentaje, fill = DESCRIP_PAIS)) +
  geom_col(width = 0.7) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  labs(
    x = NULL,
    y = "Participación en el total exportado",
    fill = "Destino"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))


grafico_destinos_sf_vs_arg



## TABLAS DE SOCIOS COMERCIALES POR PROVINCIAS####
tabla_provincia <- function(df, provincia) {
  
  tabla <- df %>%
    filter(DESCRIP_PCIA == provincia) %>%
    pivot_wider(
      names_from  = CANIO,
      values_from = porcentaje,
      values_fill = 0
    )
  
  # ordenar por importancia total (solo columnas numéricas)
  tabla %>%
    arrange(
      desc(rowSums(across(where(is.numeric))))
    )
}

provincias <- unique(base_top10$DESCRIP_PCIA)

wb <- createWorkbook()

for (prov in provincias) {
  
  tabla <- tabla_provincia(base_top10, prov)
  
  addWorksheet(wb, prov)
  writeData(wb, sheet = prov, tabla)
}

saveWorkbook(wb, 
             "C:/Users/vcorvalan/Desktop/Trabajo/Paper/Paper-ITCRM.SF/Archivos de trabajo/Vale/share_socios_total.xlsx",
             overwrite = TRUE)
