library(readxl)
Datos <- read_excel("Datos.xlsx", sheet = "rubros_total_arg")
library(dplyr)
library(stringr)
library(stringi)

Datos <- Datos %>%
  mutate(
    pais_clean = DESCRIP_PAIS %>%  #creamos la columna de paises clean
      str_trim() %>% #quita espacios al principio y final
      str_squish() %>% #quita doble espaciado
      str_to_lower()   ) #todo en minusculas

Datos <- Datos %>%
  mutate(
    pais_std = case_when(
      pais_clean %in% c("republica federal de alemania") ~ "Alemania",
      pais_clean %in% c("antillas holandesas (territorio vinculado a paises bajos)") ~ "Antillas Holandesas",
      pais_clean %in% c("azerbaidzhan", "azerbaiyan") ~ "Azerbaiyán",
      pais_clean %in% c("belarus", "bielorus") ~ "Bielorrusia",
      pais_clean %in% c("cambodya", "camboya (ex kampuchea)") ~ "Camboya",
      pais_clean == "camerun" ~ "Camerún",
      pais_clean %in% c("rep. democratica del congo (ex zaire)",
                        "republica democratica del congo") ~ "República Democrática del Congo",
      pais_clean %in% c("corea republicana", "corea, republica de") ~ "Corea del Sur",
      pais_clean %in% c("costa de marfil", "cote d ivoire (costa de marfil)") ~ "Costa de Marfil",
      pais_clean == "emiratos arabes unidos" ~ "Emiratos Árabes Unidos",
      pais_clean == "etiopia" ~ "Etiopía",
      pais_clean == "grenada" ~ "Granada",
      pais_clean %in% c("hong kong (region administrativa especial de china)",
                        "hong kong region administrativa especial de (china)") ~ "Hong Kong",
      pais_clean == "iran" ~ "Irán",
      pais_clean == "japon" ~ "Japón",
      pais_clean == "libano" ~ "Líbano",
      pais_clean == "libia jamahiriya arabe" ~ "Libia",
      pais_clean == "macao region administrativa especial de (china)" ~ "Macao",
      pais_clean == "macedonia (ex republica yugoslava de)" ~ "Macedonia",
      pais_clean == "mali" ~ "Malí",
      pais_clean == "niger" ~ "Níger",
      pais_clean == "nueva zelandia" ~ "Nueva Zelanda",
      pais_clean == "oman" ~ "Omán",
      pais_clean == "paquistan" ~ "Paquistán",
      pais_clean == "puerto rico (estado asociado)" ~ "Puerto Rico",
      pais_clean == "rusia federacion de" ~ "Rusia",
      pais_clean %in% c("saint kitts y nevis - (san cristobal y nevis)",
                        "san cristobal y nevis") ~ "San Cristóbal y Nevis",
      pais_clean == "samoa occidental" ~ "Samoa",
      pais_clean == "taiwan" ~ "Taiwán",
      pais_clean %in% c("tanzania", "tanzania, republica unida de") ~ "Tanzania",
      pais_clean == "tunez" ~ "Túnez",
      pais_clean == "republica de yemen" ~ "Yemen",
      TRUE ~ str_to_title(pais_clean)
    )
  ) #limipiado nombres

Datos <- Datos %>%
  mutate(
    pais_std = case_when(
      
      str_detect(pais_std, regex("corea", ignore_case = TRUE)) ~ "Corea del Sur",
      
      str_detect(pais_std, regex("rusia", ignore_case = TRUE)) ~ "Rusia",
      
      str_detect(pais_std, regex("hong kong", ignore_case = TRUE)) ~ "Hong Kong",
      
      str_detect(pais_std, regex("alemania", ignore_case = TRUE)) ~ "Alemania",
      
      TRUE ~ pais_std
    )
  )

Datos <- Datos %>%
  filter(
    !str_detect(pais_std, "Indeterminado|Simplificadas|Zonamerica|Colón|Zf Parque")
  )

Datos <- Datos %>% 
  mutate(
    pcia_clean = DESCRIP_PCIA %>% #crea la columna de provincias clean, igual que hicimos antes con los paises
      str_trim() %>%
      str_squish() %>%
      str_to_lower()   )

Datos <- Datos %>% #y ahora las estandarizamos igual que antes
  mutate(
    DESCRIP_PCIA = case_when(
      pcia_clean == "buenos aires" ~ "Buenos Aires",
      pcia_clean == "catamarca" ~ "Catamarca",
      pcia_clean == "chaco" ~ "Chaco",
      pcia_clean == "chubut" ~ "Chubut",
      pcia_clean == "ciudad autonoma de buenos aires" ~ "Ciudad Autónoma de Buenos Aires",
      pcia_clean == "cordoba" ~ "Córdoba",
      pcia_clean == "corrientes" ~ "Corrientes",
      pcia_clean == "entre rios" ~ "Entre Ríos",
      pcia_clean == "extranjero" ~ "Extranjero",
      pcia_clean == "formosa" ~ "Formosa",
      pcia_clean == "indeterminado" ~ "Indeterminado",
      pcia_clean == "jujuy" ~ "Jujuy",
      pcia_clean == "la pampa" ~ "La Pampa",
      pcia_clean == "la rioja" ~ "La Rioja",
      pcia_clean == "mendoza" ~ "Mendoza",
      pcia_clean == "misiones" ~ "Misiones",
      pcia_clean == "neuquen" ~ "Neuquén",
      pcia_clean == "plataforma continental" ~ "Plataforma Continental",
      pcia_clean == "rio negro" ~ "Río Negro",
      pcia_clean == "salta" ~ "Salta",
      pcia_clean == "san juan" ~ "San Juan",
      pcia_clean == "san luis" ~ "San Luis",
      pcia_clean == "santa cruz" ~ "Santa Cruz",
      pcia_clean == "santa fe" ~ "Santa Fe",
      pcia_clean == "santiago del estero" ~ "Santiago del Estero",
      pcia_clean == "tierra del fuego" ~ "Tierra del Fuego",
      pcia_clean == "tucuman" ~ "Tucumán",
      TRUE ~ str_to_title(pcia_clean)
    )
  )

Datos <- Datos %>%
  mutate(
    DESCRIP_PCIA = case_when(
      DESCRIP_PCIA %in% c(
        "Ciudad Autónoma De Buenos Aires",
        "Ciudad Autonoma De Buenos Aires",
        "Ciudad Autonoma de Buenos Aires"
      ) ~ "Ciudad Autónoma de Buenos Aires",
      TRUE ~ DESCRIP_PCIA
    )
  )


library(dplyr)
library(ggplot2)
library(forcats)

#Agregamos exportaciones por año, provincia y país
export_por_pais <- Datos %>%
  group_by(CANIO, DESCRIP_PCIA, pais_std) %>%
  summarise(exportaciones = sum(DOLARES_FOB, na.rm = TRUE), .groups = "drop")

export_por_pais <- Datos %>%
  group_by(CANIO, DESCRIP_PCIA, pais_std) %>%
  summarise(exportaciones = sum(DOLARES_FOB, na.rm = TRUE), .groups = "drop")

export_por_pais <- export_por_pais %>% #calcula las participaciones de los paises
  group_by(CANIO, DESCRIP_PCIA) %>%
  mutate(
    total = sum(exportaciones),
    share = exportaciones / total
  ) %>%
  ungroup()

top5_anual <- export_por_pais %>% #selecciona los 5 (o los que querramos) paises con mayor expo cada año para cada provincia
  group_by(CANIO, DESCRIP_PCIA) %>%
  slice_max(exportaciones, n = 5, with_ties = TRUE) %>% #con FALSE hace un corte arbitrario en empates, si ponemos TRUE puede arrojar mas de 5 si hay un empate
  ungroup() %>%
  select(CANIO, DESCRIP_PCIA, pais_std) %>%
  mutate(is_top5 = TRUE)

export_plot <- export_por_pais %>%
  left_join(top5_anual, by = c("CANIO", "DESCRIP_PCIA", "pais_std")) %>%
  mutate(
    pais_plot = ifelse(is.na(is_top5), "Otros", pais_std) #crea la categoria otros para los paises que no estan en el top 5
  ) %>%
  group_by(CANIO, DESCRIP_PCIA, pais_plot) %>%
  summarise(exportaciones = sum(exportaciones), .groups = "drop")

ggplot(export_plot, aes(x = CANIO, y = exportaciones, fill = pais_plot)) +
  geom_col(position = "fill") +
  facet_wrap(~ DESCRIP_PCIA) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Participación de destinos de exportación (Top 5 por año)",
    x = "Año",
    y = "Participación (%)",
    fill = "País"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

unique(export_plot$pais_plot)

paises_clave <- c(
  "Brasil",
  "China",
  "Estados Unidos",
  "Chile",
  "India",
  "Vietnam",
  "Países Bajos",
  "España",
  "Italia"
)

export_plot <- export_plot %>%
  mutate(
    pais_plot2 = case_when(
      pais_plot %in% paises_clave ~ pais_plot,
      pais_plot == "Otros" ~ "Otros",
      TRUE ~ "Otros países"
    )
  )

colores <- c(
  "Brasil" = "#1b9e77",          # verde fuerte (regional líder)
  "China" = "#d95f02",           # naranja (demanda global)
  "Estados Unidos" = "#7570b3",  # violeta
  "Chile" = "#e7298a",           # fucsia
  "India" = "#66a61e",           # verde oliva (distinto de Brasil)
  "Vietnam" = "#e6ab02",         # amarillo mostaza
  "Países Bajos" = "#a6761d",    # marrón (hub logístico)
  "España" = "#1f78b4",          # azul fuerte
  "Italia" = "#b2df8a",          # verde claro (UE)
  
  "Otros países" = "grey70",
  "Otros" = "grey40"
)

ggplot(export_plot, aes(x = CANIO, y = exportaciones, fill = pais_plot2)) +
  geom_col(position = "fill") +
  facet_wrap(~ DESCRIP_PCIA) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Participación de destinos de exportación (Top 5 dinámico)",
    x = "Año",
    y = "Participación (%)",
    fill = "País"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

export_plot <- export_plot %>%
  mutate(CANIO = as.numeric(CANIO))

scale_x_continuous(
  breaks = seq(min(export_plot$CANIO), max(export_plot$CANIO), by = 2)
)

grafico_destinos <- 
  ggplot(export_plot, aes(x = CANIO, y = exportaciones, fill = pais_plot2)) +
  geom_col(position = "fill", width = 0.9, color = "grey30", linewidth = 0.2) +
  facet_wrap(~ DESCRIP_PCIA, ncol = 5) +
  scale_fill_manual(values = colores) +
  scale_x_continuous(
    breaks = sort(unique(export_plot$CANIO)),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    breaks = seq(0, 1, by = 0.2)
  ) +
  labs(
    title = "Participación de destinos de exportación (Top 5 dinámico)",
    x = "Año",
    y = "Participación (%)",
    fill = "País"
  ) +
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
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))


ggsave(
  filename = "grafico_expo_prov.png",
  plot = grafico_destinos,
  width = 35,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)


export_nacional <- Datos %>%
  group_by(CANIO, pais_std) %>%
  summarise(
    exportaciones = sum(DOLARES_FOB, na.rm = TRUE),
    .groups = "drop"
  )

export_nacional <- export_nacional %>%
  group_by(CANIO) %>%
  mutate(
    total = sum(exportaciones),
    share = exportaciones / total
  ) %>%
  ungroup()

top5_nacional <- export_nacional %>%
  group_by(CANIO) %>%
  slice_max(exportaciones, n = 5, with_ties = TRUE) %>%
  ungroup() %>%
  select(CANIO, pais_std) %>%
  mutate(is_top5 = TRUE)

export_plot_nac <- export_nacional %>%
  left_join(top5_nacional, by = c("CANIO", "pais_std")) %>%
  mutate(
    pais_plot = ifelse(is.na(is_top5), "Otros", pais_std)
  ) %>%
  group_by(CANIO, pais_plot) %>%
  summarise(exportaciones = sum(exportaciones), .groups = "drop")

export_plot_nac <- export_plot_nac %>%
  mutate(
    pais_plot2 = case_when(
      pais_plot %in% paises_clave ~ pais_plot,
      pais_plot == "Otros" ~ "Otros",
      TRUE ~ "Otros países"
    )
  )

export_plot_nac <- export_plot_nac %>%
  mutate(CANIO = as.numeric(CANIO))

grafico_destinos_nacional <-
  ggplot(export_plot_nac, aes(x = CANIO, y = exportaciones, fill = pais_plot2)) +
  geom_col(position = "fill", width = 0.9, color = "grey30", linewidth = 0.2) +
  scale_fill_manual(values = colores) +
  scale_x_continuous(
    breaks = seq(min(export_plot_nac$CANIO), max(export_plot_nac$CANIO), by = 2)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    breaks = seq(0, 1, by = 0.2)
  ) +
  labs(
    title = "Destinos de exportación de Argentina",
    subtitle = "Participación en las exportaciones totales",
    x = "Año",
    y = "Participación (%)",
    fill = "Destino"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.text.x = element_text(size = 20, angle = 90),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave(
  "grafico_destinos_argentina.png",
  grafico_destinos_nacional,
  width = 20,
  height = 9,
  dpi = 300
)


export_sf_vs_nat <- Datos %>%
  mutate(
    territorio = case_when(
      DESCRIP_PCIA == "Santa Fe" ~ "Santa Fe",
      TRUE ~ "Argentina"
    )
  )

expo_sf <- Datos %>%
  filter(DESCRIP_PCIA == "Santa Fe") %>%
  group_by(CANIO, pais_std) %>%
  summarise(
    exportaciones = sum(DOLARES_FOB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(territorio = "Santa Fe")

expo_arg <- Datos %>%
  group_by(CANIO, pais_std) %>%
  summarise(
    exportaciones = sum(DOLARES_FOB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(territorio = "Argentina")

expo_sf_nat <- bind_rows(expo_sf, expo_arg)

expo_sf_nat <- expo_sf_nat %>%
  group_by(CANIO, territorio) %>%
  mutate(
    total = sum(exportaciones),
    share = exportaciones / total
  ) %>%
  ungroup()

top5_sf_nat <- expo_sf_nat %>%
  group_by(CANIO, territorio) %>%
  slice_max(exportaciones, n = 5, with_ties = TRUE) %>%
  ungroup() %>%
  select(CANIO, territorio, pais_std) %>%
  mutate(is_top5 = TRUE)

export_plot_sf_nat <- expo_sf_nat %>%
  left_join(top5_sf_nat, by = c("CANIO", "territorio", "pais_std")) %>%
  mutate(
    pais_plot = ifelse(is.na(is_top5), "Otros", pais_std)
  ) %>%
  group_by(CANIO, territorio, pais_plot) %>%
  summarise(exportaciones = sum(exportaciones), .groups = "drop")

export_plot_sf_nat <- export_plot_sf_nat %>%
  mutate(
    pais_plot2 = case_when(
      pais_plot %in% paises_clave ~ pais_plot,
      pais_plot == "Otros" ~ "Otros",
      TRUE ~ "Otros países"
    )
  )

export_plot_sf_nat <- export_plot_sf_nat %>%
  mutate(CANIO = as.numeric(CANIO))

grafico_sf_vs_arg <-
  ggplot(export_plot_sf_nat,
         aes(x = CANIO, y = exportaciones, fill = pais_plot2)) +
  geom_col(position = "fill", width = 0.9) +
  facet_wrap(~ territorio, ncol = 1) +
  scale_fill_manual(values = colores) +
  scale_x_continuous(
    breaks = sort(unique(export_plot_sf_nat$CANIO)),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    breaks = seq(0,1,0.2)
  ) +
  labs(
    x = "Año",
    y = "Participación en las exportaciones",
    fill = "Destino"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 20, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA),
    axis.text.x = element_text(angle = 90, size = 20),
    axis.text.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave(
  filename = "grafico_destinos_sf_vs_arg.png",
  plot = grafico_sf_vs_arg,
  width = 55,
  height = 25,
  units = "cm",
  dpi = 300,
  bg = "white"
)
