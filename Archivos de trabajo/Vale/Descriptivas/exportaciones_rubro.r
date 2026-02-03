#LIBRERIAS ####
base::library("openxlsx")
base::library("tidyverse")
base::library("dplyr")
base::library("ggplot2")

# ruta fran C:/Repositorios/Paper-ITCRM.SF/Archivos de trabajo/Vale/Descriptivas/exportaciones.xlsx

# ruta Vale C:/Users/vcorvalan/Desktop/Trabajo/Paper/ITCRM/exportaciones.xlsx

#CARGAS ARCHIVO ####
data <-
  openxlsx::read.xlsx(
    "C:/Repositorios/Paper-ITCRM.SF/Archivos de trabajo/Vale/Descriptivas/exportaciones.xlsx",
    sheet = "x"
    )
#PRUEBAS ####
## BASE_MENSUAL####
base_agregada <- 
  data |> 
  group_by(prov, anio, mes, rubro) |> 
  summarise(
    fob = sum(fob, na.rm = TRUE),
    .groups = "drop"
  )

base_pct <- 
  base_agregada |> 
  group_by(prov, anio, mes) |> 
  mutate(
    total_FOB = sum(fob),
    porcentaje = fob / total_FOB * 100
  ) |> 
  ungroup()
## BASE_ANUAL####
base_agregada_anual <- 
  data |> 
  group_by(prov, anio, rubro) |> 
  summarise(
    fob = sum(fob, na.rm = TRUE),
    .groups = "drop"
  )

base_pct_anual <- 
  base_agregada_anual |> 
  group_by(prov, anio) |> 
  mutate(
    total_FOB = sum(fob),
    porcentaje = fob / total_FOB * 100
  ) |> 
  ungroup()
## GRAFICOS ####
### X POR RUBRO ANUAL SEGUN PROVINCIA####
base_plot <- 
  base_pct_anual |> 
  dplyr::filter(
    !prov %in% c("Extranjero y Plataforma Continental", "Indeterminado")
  )

grafico1 <- 
ggplot(base_plot, aes(x = anio, y = porcentaje, fill = rubro)) +
  geom_col(width = 0.9, color = "grey30", linewidth = 0.2) +
  facet_wrap(~ prov, ncol = 5) +
  labs(
    y = "Porcentaje del total anual exportado",
    x = "Año",
    fill = "Rubro"
  ) +
  scale_x_continuous(
    breaks = sort(unique(base_plot$anio)),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20)
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.spacing.y = unit(1.2, "lines"),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA),
    axis.text.x = element_text(
      angle = 90,
      size = 12,
      vjust = 0.5,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 12
    ),
    legend.text = element_text(
      size = 12
    ),
    axis.title.y = element_text(
      size = 12
      ),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave(
  filename = "grafico_expo_prov.png",
  plot = grafico1,
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300,
  bg = "white"
)

###SANTA FE VS NACIONAL | X POR RUBRO ANUAL####
base_anual <- 
  data |> 
  group_by(prov, anio, rubro) |> 
  summarise(
    fob = sum(fob, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  filter(!(anio == 2025))

base_nacional <- 
  base_anual |> 
  group_by(anio, rubro) |> 
  summarise(
    fob = sum(fob, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(prov = "Argentina")

base_sf_vs_nat <- 
  base_anual |> 
  filter(prov == "Santa Fe") |> 
  bind_rows(base_nacional)

base_sf_vs_nat_pct <- 
  base_sf_vs_nat |> 
  group_by(prov, anio) |> 
  mutate(
    total_fob = sum(fob),
    porcentaje = fob / total_fob * 100
  ) |> 
  ungroup()

grafico2 <-
ggplot(base_sf_vs_nat_pct,
       aes(x = anio, y = porcentaje, fill = rubro)) +
  geom_col(width = 0.9) +
  facet_wrap(~ prov, ncol = 1) +
  labs(
    x = "Año",
    y = "Porcentjae del total exportado",
    fill = "Rubro"
  ) +
  scale_x_continuous(
    breaks = sort(unique(base_sf_vs_nat_pct$anio)),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "bottom",
    
    strip.text = element_text(size = 20, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA),
    
    axis.text.x = element_text(
      angle = 90,
      size = 20,
      hjust = 1
    ),
    
    axis.text.y = element_text(
      size = 20
    ),
    legend.text = element_text(
      size = 20
    ),
    axis.title.y = element_text(
      size = 20
    ),
    
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave(
  filename = "X_rubro_anual_sfe_vs_arg.png",
  plot = grafico2,
  width = 55,
  height = 25,
  units = "cm",
  dpi = 300,
  bg = "white"
)

#### ESTADISTICAS DESCRIPTIVAS ####
descriptivas1 <- 
  base_sf_vs_nat_pct |> 
  group_by(prov, rubro) |> 
  summarise(
    X_promedio = mean(porcentaje),
  )

check1 <- 
  descriptivas1 |> 
  group_by(prov) |> 
  summarise(
    check = sum(X_promedio)
  )
