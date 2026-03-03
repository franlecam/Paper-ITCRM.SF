library(readxl)
Datos <- read_excel("Datos.xlsx", sheet = "rubros_total_arg")
library(dplyr)
library(stringr)
library(stringi)

Datos <- Datos %>%
  mutate(
    pais_clean = DESCRIP_PAIS %>%
      str_trim() %>%
      str_squish() %>%
      str_to_lower()   )

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
  )


Datos %>%
  group_by(DESCRIP_PCIA, pais_std)

library(dplyr)
library(stringr)
library(stringi)

Datos <- Datos %>%
  mutate(
    pcia_clean = DESCRIP_PCIA %>%
      str_trim() %>%
      str_squish() %>%
      str_to_lower()   )
Datos <- Datos %>%
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



library(dplyr)
library(ggplot2)
library(forcats)

# 1️⃣ Agregamos por año, provincia y país
top5_anios <- Datos %>%
  group_by(CANIO, DESCRIP_PCIA, pais_std) %>%
  summarise(exportaciones = sum(DOLARES_FOB, na.rm = TRUE), .groups = "drop") %>%
  group_by(CANIO, DESCRIP_PCIA) %>%
  mutate(
    total_prov = sum(exportaciones),
    share = exportaciones / total_prov
  ) %>%
  ungroup()

# 2️⃣ Top 5 países por provincia (promedio de participación total)
top5_global <- top5_anios %>%
  group_by(DESCRIP_PCIA, pais_std) %>%
  summarise(avg_share = mean(share), .groups = "drop") %>%
  group_by(DESCRIP_PCIA) %>%
  slice_max(avg_share, n = 5) %>%
  ungroup()

# 3️⃣ Filtramos solo esos países
top5_anios <- top5_anios %>%
  semi_join(top5_global, by = c("DESCRIP_PCIA", "pais_std"))

# 4️⃣ Gráfico
ggplot(top5_anios, aes(x = as.integer(CANIO), y = share, color = pais_std)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ DESCRIP_PCIA, scales = "free_y") +
  labs(
    title = "Participación de los 5 principales destinos de exportación por provincia a lo largo de los años",
    x = "Año",
    y = "Participación",
    color = "País"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


library(dplyr)
library(ggplot2)
library(forcats)

# 1️⃣ Agregamos exportaciones por año, provincia y país
export_por_pais <- Datos %>%
  group_by(CANIO, DESCRIP_PCIA, pais_std) %>%
  summarise(exportaciones = sum(DOLARES_FOB, na.rm = TRUE), .groups = "drop")

# 2️⃣ Calculamos top 5 países por provincia (promedio de participación)
top5_global <- export_por_pais %>%
  group_by(DESCRIP_PCIA, pais_std) %>%
  summarise(total_prov = sum(exportaciones), .groups = "drop") %>%
  group_by(DESCRIP_PCIA) %>%
  slice_max(total_prov, n = 5) %>%
  ungroup() %>%
  select(DESCRIP_PCIA, pais_std)

# 3️⃣ Creamos columna "Otros" para los países fuera del top 5
export_por_pais <- export_por_pais %>%
  left_join(top5_global %>% mutate(is_top5 = TRUE),
            by = c("DESCRIP_PCIA", "pais_std")) %>%
  mutate(
    pais_plot = ifelse(is.na(is_top5), "Otros", pais_std)
  ) %>%
  group_by(CANIO, DESCRIP_PCIA, pais_plot) %>%
  summarise(exportaciones = sum(exportaciones), .groups = "drop")

# 4️⃣ Gráfico de barras apiladas normalizadas (100%)
ggplot(export_por_pais, aes(x = as.factor(CANIO), y = exportaciones, fill = pais_plot)) +
  geom_col(position = "fill") +
  facet_wrap(~ DESCRIP_PCIA, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Participación de los principales destinos de exportación por provincia",
    x = "Año",
    y = "Participación (%)",
    fill = "País"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
