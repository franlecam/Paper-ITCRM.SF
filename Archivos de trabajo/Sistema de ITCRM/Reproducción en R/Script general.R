# SCRIPT PARA SANTA FE ####

library(tidyverse)
library(readxl)

options(digits = 3, scipen = 999)

# ============================================================
# CARGA DE DATOS
# ============================================================

rubros     <- read_excel("Datos.xlsx", sheet = "rubros")
er.cpi_arg <- read_excel("Datos.xlsx", sheet = "Arg")
itcrm_bcra <- read_excel("Datos.xlsx", sheet = "Arg") |>
  select(mes, itcrm_bcra) |>
  mutate(mes = as.Date(mes))

cpi <- read.csv("CPI all.csv")
er  <- read.csv("ER all.csv")

# ============================================================
# MAPEO DE PAÍSES
# ============================================================

rubros_pulido <- rubros %>%
  mutate(
    rubros_std = case_when(
      DESCRIP_PAIS %in% c("República Federal de Alemania",
                          "República Federal De Alemania")       ~ "Alemania",
      DESCRIP_PAIS == "Antillas Holandesas (territorio vinculado a Países Bajos)" ~ "Antillas Holandesas",
      DESCRIP_PAIS %in% c("Azerbaidzhan", "Azerbaiyán")          ~ "Azerbaiyán",
      DESCRIP_PAIS %in% c("Belarús", "Bielorus")                 ~ "Bielorrusia",
      DESCRIP_PAIS %in% c("Cambodya", "Camboya (ex Kampuchea)")  ~ "Camboya",
      DESCRIP_PAIS == "Camerun"                                   ~ "Camerún",
      DESCRIP_PAIS %in% c("Rep. Democrática del Congo (ex Zaire)",
                          "República Democrática Del Congo")      ~ "República Democrática del Congo",
      DESCRIP_PAIS %in% c("Corea Republicana", "Corea, República de") ~ "Corea del Sur",
      DESCRIP_PAIS %in% c("Costa De Marfil",
                          "Côte d' Ivoire (Costa de Marfil)")    ~ "Costa de Marfil",
      DESCRIP_PAIS == "Emiratos Arabes Unidos"                    ~ "Emiratos Árabes Unidos",
      DESCRIP_PAIS == "Etiopia"                                   ~ "Etiopía",
      DESCRIP_PAIS == "Grenada"                                   ~ "Granada",
      DESCRIP_PAIS %in% c("Hong Kong (Región administrativa especial de China)",
                          "Hong Kong Región Administrativa Especial de (China)") ~ "Hong Kong",
      DESCRIP_PAIS == "Iran"                                      ~ "Irán",
      DESCRIP_PAIS == "Japon"                                     ~ "Japón",
      DESCRIP_PAIS == "Libano"                                    ~ "Líbano",
      DESCRIP_PAIS == "Libia Jamahiriya Árabe"                    ~ "Libia",
      DESCRIP_PAIS == "Macao Región Administrativa Especial de (China)" ~ "Macao",
      DESCRIP_PAIS == "Macedonia (ex República Yugoslava de)"     ~ "Macedonia",
      DESCRIP_PAIS == "Mali"                                      ~ "Malí",
      DESCRIP_PAIS == "Niger"                                     ~ "Níger",
      DESCRIP_PAIS == "Nueva Zelandia"                            ~ "Nueva Zelanda",
      DESCRIP_PAIS == "Oman"                                      ~ "Omán",
      DESCRIP_PAIS == "Paquistan"                                 ~ "Paquistán",
      DESCRIP_PAIS == "Puerto Rico (Estado Asociado)"             ~ "Puerto Rico",
      DESCRIP_PAIS == "Rusia Federación de"                       ~ "Rusia",
      DESCRIP_PAIS %in% c("Saint Kitts y Nevis - (San Cristóbal Y Nevis)",
                          "San Cristóbal Y Nevis")                ~ "San Cristóbal y Nevis",
      DESCRIP_PAIS == "Samoa  Occidental"                         ~ "Samoa",
      DESCRIP_PAIS == "Taiwan"                                    ~ "Taiwán",
      DESCRIP_PAIS %in% c("Tanzania", "Tanzania, República Unida de") ~ "Tanzania",
      DESCRIP_PAIS == "Tunez"                                     ~ "Túnez",
      DESCRIP_PAIS == "República de Yemen"                        ~ "Yemen",
      TRUE ~ DESCRIP_PAIS
    )
  )

mapeo_rubros_a_imf <- tribble(
  ~rubros_std,                      ~pais_match,
  "Afganistán",                     "Afghanistan, Islamic Republic of",
  "Albania",                        "Albania",
  "Alemania",                       "Germany",
  "Andorra",                        "Andorra, Principality of",
  "Angola",                         "Angola",
  "Antigua Y Barbuda",              "Antigua and Barbuda",
  "Antillas Holandesas",            "Netherlands Antilles",
  "Arabia Saudita",                 "Saudi Arabia",
  "Argelia",                        "Algeria",
  "Armenia",                        "Armenia, Republic of",
  "Aruba",                          "Aruba, Kingdom of the Netherlands",
  "Australia",                      "Australia",
  "Austria",                        "Austria",
  "Azerbaiyán",                     "Azerbaijan, Republic of",
  "Bahamas",                        "Bahamas, The",
  "Bahrein",                        "Bahrain, Kingdom of",
  "Bangladesh",                     "Bangladesh",
  "Barbados",                       "Barbados",
  "Bielorrusia",                    "Belarus, Republic of",
  "Bélgica",                        "Belgium",
  "Belice",                         "Belize",
  "Benin",                          "Benin",
  "Bolivia",                        "Bolivia",
  "Bosnia y Herzegovina",           "Bosnia and Herzegovina",
  "Botswana",                       "Botswana",
  "Brasil",                         "Brazil",
  "Brunei Darussalam",              "Brunei Darussalam",
  "Bulgaria",                       "Bulgaria",
  "Burkina Faso",                   "Burkina Faso",
  "Burundi",                        "Burundi",
  "Cabo Verde",                     "Cabo Verde",
  "Camboya",                        "Cambodia",
  "Camerún",                        "Cameroon",
  "Canadá",                         "Canada",
  "Chad",                           "Chad",
  "Chile",                          "Chile",
  "China",                          "China, People's Republic of",
  "Chipre",                         "Cyprus",
  "Colombia",                       "Colombia",
  "Comoras",                        "Comoros, Union of the",
  "Congo",                          "Congo, Republic of",
  "República Democrática del Congo","Congo, Democratic Republic of the",
  "Costa de Marfil",                "Côte d'Ivoire",
  "Costa Rica",                     "Costa Rica",
  "Croacia",                        "Croatia, Republic of",
  "Cuba",                           "Cuba",
  "Curazao",                        "Curaçao, Kingdom of the Netherlands",
  "Dinamarca",                      "Denmark",
  "Djibouti",                       "Djibouti",
  "Dominica",                       "Dominica",
  "República Dominicana",           "Dominican Republic",
  "Ecuador",                        "Ecuador",
  "Egipto",                         "Egypt, Arab Republic of",
  "El Salvador",                    "El Salvador",
  "Emiratos Árabes Unidos",         "United Arab Emirates",
  "Eritrea",                        "Eritrea, The State of",
  "Eslovaquia",                     "Slovak Republic",
  "Eslovenia",                      "Slovenia, Republic of",
  "España",                         "Spain",
  "Estados Unidos",                 "United States",
  "Estonia",                        "Estonia, Republic of",
  "Etiopía",                        "Ethiopia, The Federal Democratic Republic of",
  "Fiji",                           "Fiji, Republic of",
  "Filipinas",                      "Philippines",
  "Finlandia",                      "Finland",
  "Francia",                        "France",
  "Gabón",                          "Gabon",
  "Gambia",                         "Gambia, The",
  "Georgia",                        "Georgia",
  "Ghana",                          "Ghana",
  "Granada",                        "Grenada",
  "Grecia",                         "Greece",
  "Guatemala",                      "Guatemala",
  "Guinea",                         "Guinea",
  "Guinea Bissau",                  "Guinea-Bissau",
  "Guinea Ecuatorial",              "Equatorial Guinea, Republic of",
  "Guyana",                         "Guyana",
  "Haití",                          "Haiti",
  "Honduras",                       "Honduras",
  "Hong Kong",                      "Hong Kong Special Administrative Region, People's Republic of China",
  "Hungría",                        "Hungary",
  "India",                          "India",
  "Indonesia",                      "Indonesia",
  "Irán",                           "Iran, Islamic Republic of",
  "Iraq",                           "Iraq",
  "Irlanda",                        "Ireland",
  "Islandia",                       "Iceland",
  "Israel",                         "Israel",
  "Italia",                         "Italy",
  "Jamaica",                        "Jamaica",
  "Japón",                          "Japan",
  "Jordania",                       "Jordan",
  "Kazajstán",                      "Kazakhstan, Republic of",
  "Kenya",                          "Kenya",
  "Corea del Sur",                  "Korea, Republic of",
  "Kuwait",                         "Kuwait",
  "Lesotho",                        "Lesotho, Kingdom of",
  "Letonia",                        "Latvia, Republic of",
  "Líbano",                         "Lebanon",
  "Liberia",                        "Liberia",
  "Libia",                          "Libya",
  "Lituania",                       "Lithuania, Republic of",
  "Luxemburgo",                     "Luxembourg",
  "Macao",                          "Macao Special Administrative Region, People's Republic of China",
  "Macedonia",                      "North Macedonia, Republic of",
  "Madagascar",                     "Madagascar, Republic of",
  "Malasia",                        "Malaysia",
  "Malawi",                         "Malawi",
  "Maldivas",                       "Maldives",
  "Malí",                           "Mali",
  "Malta",                          "Malta",
  "Marruecos",                      "Morocco",
  "Mauricio",                       "Mauritius",
  "Mauritania",                     "Mauritania, Islamic Republic of",
  "México",                         "Mexico",
  "Moldavia, República de",         "Moldova, Republic of",
  "Mongolia",                       "Mongolia",
  "Montenegro",                     "Montenegro",
  "Mozambique",                     "Mozambique, Republic of",
  "Myanmar",                        "Myanmar",
  "Namibia",                        "Namibia",
  "Nepal",                          "Nepal",
  "Nicaragua",                      "Nicaragua",
  "Níger",                          "Niger",
  "Nigeria",                        "Nigeria",
  "Noruega",                        "Norway",
  "Nueva Zelanda",                  "New Zealand",
  "Omán",                           "Oman",
  "Países Bajos",                   "Netherlands, The",
  "Panamá",                         "Panama",
  "Papua  Nueva Guinea",            "Papua New Guinea",
  "Paquistán",                      "Pakistan",
  "Paraguay",                       "Paraguay",
  "Perú",                           "Peru",
  "Polonia",                        "Poland, Republic of",
  "Portugal",                       "Portugal",
  "Puerto Rico",                    "Puerto Rico",
  "Qatar",                          "Qatar",
  "Reino Unido",                    "United Kingdom",
  "República Centroafricana",       "Central African Republic",
  "República Checa",                "Czech Republic",
  "Rumania",                        "Romania",
  "Rusia",                          "Russian Federation",
  "Rwanda",                         "Rwanda",
  "San Cristóbal y Nevis",          "St. Kitts and Nevis",
  "San Vicente Y Las Granadinas",   "St. Vincent and the Grenadines",
  "Santa Lucía",                    "St. Lucia",
  "Santo Tomé y Príncipe",          "São Tomé and Príncipe, Democratic Republic of",
  "Senegal",                        "Senegal",
  "Serbia",                         "Serbia, Republic of",
  "Seychelles",                     "Seychelles",
  "Sierra Leona",                   "Sierra Leone",
  "Singapur",                       "Singapore",
  "Siria",                          "Syrian Arab Republic",
  "Sri Lanka",                      "Sri Lanka",
  "Sudáfrica",                      "South Africa",
  "Sudán",                          "Sudan",
  "SUDAN DEL SUR",                  "South Sudan, Republic of",
  "Suecia",                         "Sweden",
  "Suiza",                          "Switzerland",
  "Suriname",                       "Suriname",
  "Tailandia",                      "Thailand",
  "Taiwán",                         "Taiwan Province of China",
  "Tanzania",                       "Tanzania, United Republic of",
  "Togo",                           "Togo",
  "Trinidad Y Tobago",              "Trinidad and Tobago",
  "Túnez",                          "Tunisia",
  "Turquía",                        "Türkiye, Republic of",
  "Ucrania",                        "Ukraine",
  "Uganda",                         "Uganda",
  "Uruguay",                        "Uruguay",
  "Uzbekistán",                     "Uzbekistan, Republic of",
  "Venezuela",                      "Venezuela, República Bolivariana de",
  "Vietnam",                        "Vietnam",
  "Yemen",                          "Yemen, Republic of",
  "Zambia",                         "Zambia",
  "Zimbabwe",                       "Zimbabwe"
)

euro_members <- c(
  "Austria", "Belgium", "Estonia, Republic of", "Finland", "France",
  "Germany", "Greece", "Ireland", "Italy", "Latvia, Republic of",
  "Lithuania, Republic of", "Luxembourg", "Malta", "Netherlands, The",
  "Portugal", "Slovak Republic", "Slovenia, Republic of", "Spain"
)

pais_master <- mapeo_rubros_a_imf |>
  distinct(rubros_std, pais_match) |>
  mutate(er_source = if_else(pais_match %in% euro_members, "Euro Area (EA)", pais_match))

# ============================================================
# PARSEO LARGO DE CPI Y ER (helper para no repetir)
# ============================================================

parse_long <- function(df, value_col) {
  df |>
    pivot_longer(
      cols      = starts_with("X"),
      names_to  = "mes_raw",
      values_to = value_col
    ) |>
    mutate(
      mes = as.Date(paste0(str_sub(mes_raw, 2, 5), "-",
                           str_sub(mes_raw, 8, 9), "-01"))
    ) |>
    select(-mes_raw)
}

cpi_long <- parse_long(cpi, "cpi") |>
  rename(pais = COUNTRY) |>
  select(mes, pais, cpi)

er_long <- parse_long(er, "er") |>
  rename(er_source = COUNTRY) |>
  select(mes, er_source, er)

# ============================================================
# PANEL CPI–ER
# ============================================================

map_er <- pais_master |> distinct(pais = pais_match, er_source)

panel <- cpi_long |>
  inner_join(
    er_long |> inner_join(map_er, by = "er_source") |> select(mes, pais, er),
    by = c("mes", "pais")
  )

# ============================================================
# PARTICIPACIONES (MOA + MOI), NORMALIZADAS
# ============================================================

participaciones <- rubros_pulido |>
  filter(Rubro %in% c("MOA", "MOI", "PP", "CyE")) |>
  mutate(Año = as.integer(Año)) |>
  left_join(
    pais_master |> select(rubros_std, pais_match),
    by = "rubros_std"
  ) |>
  filter(!is.na(pais_match)) |>
  group_by(Año, pais = pais_match) |>
  summarise(exportaciones = sum(DOLARES_FOB, na.rm = TRUE), .groups = "drop") |>
  group_by(Año) |>
  mutate(participacion = exportaciones / sum(exportaciones)) |>
  ungroup()

# ============================================================
# PANEL COMPLETO: PANEL + PARTICIPACIONES + ARGENTINA
# ============================================================

er_cpi_arg <- er.cpi_arg |>
  rename(cpi_arg = IPC_ARG, er_arg = TC_ARG) |>
  mutate(mes = as.Date(mes)) |>
  select(mes, cpi_arg, er_arg)

panel_full <- panel |>
  mutate(Año = as.integer(format(mes, "%Y"))) |>
  left_join(participaciones, by = c("Año", "pais")) |>
  left_join(er_cpi_arg, by = "mes") |>
  group_by(mes) |>
  mutate(
    participacion_norm = participacion / sum(participacion, na.rm = TRUE)
  ) |>
  ungroup()

# ============================================================
# ITCRM GEOMÉTRICO ENCADENADO (metodología BCRA)
# ============================================================

itcrm_final <- panel_full |>
  arrange(pais, mes) |>
  group_by(pais) |>
  mutate(
    tcr_bilateral = if_else(
      !is.na(er_arg) & !is.na(er) & !is.na(cpi) & !is.na(cpi_arg),
      (er_arg / er) * (cpi / cpi_arg),
      NA_real_
    ),
    disp = as.integer(!is.na(tcr_bilateral) & !is.na(lag(tcr_bilateral)))
  ) |>
  ungroup() |>
  group_by(mes) |>
  mutate(
    suma_disp = sum(participacion_norm * disp, na.rm = TRUE),
    pond_adj  = if_else(disp == 1 & suma_disp > 0,
                        participacion_norm / suma_disp, 0)
  ) |>
  ungroup() |>
  arrange(pais, mes) |>
  group_by(pais) |>
  mutate(
    var_tcr = if_else(
      disp == 1 & lag(disp) == 1,
      (tcr_bilateral / lag(tcr_bilateral)) ^ pond_adj,
      1
    )
  ) |>
  ungroup() |>
  group_by(mes) |>
  summarise(itcrm_factor = prod(var_tcr, na.rm = TRUE), .groups = "drop") |>
  arrange(mes) |>
  mutate(itcrm_nivel = 100 * cumprod(itcrm_factor)) |>
  left_join(itcrm_bcra, by = "mes")

# ============================================================
# CAMBIO DE BASE: DIC-2015 = 100
# ============================================================

base_sfe  <- itcrm_final$itcrm_nivel[itcrm_final$mes == as.Date("2015-12-01")]
base_bcra <- itcrm_final$itcrm_bcra[itcrm_final$mes  == as.Date("2015-12-01")]

itcrm_graf <- itcrm_final |>
  mutate(
    itcrm_nivel = itcrm_nivel / base_sfe  * 100,
    itcrm_bcra  = itcrm_bcra  / base_bcra * 100
  )

# ============================================================
# GRÁFICO
# ============================================================

ggplot(itcrm_graf, aes(x = mes)) +
  geom_line(aes(y = itcrm_nivel, color = "ITCRM SFE"), linewidth = 0.9) +
  geom_line(aes(y = itcrm_bcra,  color = "ITCRM ARG"), linewidth = 0.9) +
  scale_color_manual(values = c("ITCRM SFE" = "steelblue", "ITCRM ARG" = "black")) +
  labs(title = "ITCRM – Comparación final", x = NULL, y = "Índice (dic-2015 = 100)") +
  theme_minimal() +
  theme(legend.title = element_blank())

resultado_santa_fe <- itcrm_graf
# SISTEMA DE TIPO DE CAMBIO PROVINCIAL. ####

library(tidyverse)
library(readxl)

options(digits = 3, scipen = 999)

# ============================================================
# CARGA DE DATOS
# ============================================================

# CAMBIO: ahora levantamos la sheet con todas las provincias
rubros_arg <- read_excel("Datos.xlsx", sheet = "rubros_total_arg") %>%
  rename("Año" = CANIO)

er.cpi_arg <- read_excel("Datos.xlsx", sheet = "Arg")
itcrm_bcra <- read_excel("Datos.xlsx", sheet = "Arg") |>
  select(mes, itcrm_bcra) |>
  mutate(mes = as.Date(mes))

cpi <- read.csv("CPI all.csv")
er  <- read.csv("ER all.csv")

# ============================================================
# MAPEO DE PAÍSES (sin cambios)
# ============================================================

rubros_pulido <- rubros_arg %>%
  mutate(
    rubros_std = case_when(
      DESCRIP_PAIS %in% c("República Federal de Alemania",
                          "República Federal De Alemania")       ~ "Alemania",
      DESCRIP_PAIS == "Antillas Holandesas (territorio vinculado a Países Bajos)" ~ "Antillas Holandesas",
      DESCRIP_PAIS %in% c("Azerbaidzhan", "Azerbaiyán")          ~ "Azerbaiyán",
      DESCRIP_PAIS %in% c("Belarús", "Bielorus")                 ~ "Bielorrusia",
      DESCRIP_PAIS %in% c("Cambodya", "Camboya (ex Kampuchea)")  ~ "Camboya",
      DESCRIP_PAIS == "Camerun"                                   ~ "Camerún",
      DESCRIP_PAIS %in% c("Rep. Democrática del Congo (ex Zaire)",
                          "República Democrática Del Congo")      ~ "República Democrática del Congo",
      DESCRIP_PAIS %in% c("Corea Republicana", "Corea, República de") ~ "Corea del Sur",
      DESCRIP_PAIS %in% c("Costa De Marfil",
                          "Côte d' Ivoire (Costa de Marfil)")    ~ "Costa de Marfil",
      DESCRIP_PAIS == "Emiratos Arabes Unidos"                    ~ "Emiratos Árabes Unidos",
      DESCRIP_PAIS == "Etiopia"                                   ~ "Etiopía",
      DESCRIP_PAIS == "Grenada"                                   ~ "Granada",
      DESCRIP_PAIS %in% c("Hong Kong (Región administrativa especial de China)",
                          "Hong Kong Región Administrativa Especial de (China)") ~ "Hong Kong",
      DESCRIP_PAIS == "Iran"                                      ~ "Irán",
      DESCRIP_PAIS == "Japon"                                     ~ "Japón",
      DESCRIP_PAIS == "Libano"                                    ~ "Líbano",
      DESCRIP_PAIS == "Libia Jamahiriya Árabe"                    ~ "Libia",
      DESCRIP_PAIS == "Macao Región Administrativa Especial de (China)" ~ "Macao",
      DESCRIP_PAIS == "Macedonia (ex República Yugoslava de)"     ~ "Macedonia",
      DESCRIP_PAIS == "Mali"                                      ~ "Malí",
      DESCRIP_PAIS == "Niger"                                     ~ "Níger",
      DESCRIP_PAIS == "Nueva Zelandia"                            ~ "Nueva Zelanda",
      DESCRIP_PAIS == "Oman"                                      ~ "Omán",
      DESCRIP_PAIS == "Paquistan"                                 ~ "Paquistán",
      DESCRIP_PAIS == "Puerto Rico (Estado Asociado)"             ~ "Puerto Rico",
      DESCRIP_PAIS == "Rusia Federación de"                       ~ "Rusia",
      DESCRIP_PAIS %in% c("Saint Kitts y Nevis - (San Cristóbal Y Nevis)",
                          "San Cristóbal Y Nevis")                ~ "San Cristóbal y Nevis",
      DESCRIP_PAIS == "Samoa  Occidental"                         ~ "Samoa",
      DESCRIP_PAIS == "Taiwan"                                    ~ "Taiwán",
      DESCRIP_PAIS %in% c("Tanzania", "Tanzania, República Unida de") ~ "Tanzania",
      DESCRIP_PAIS == "Tunez"                                     ~ "Túnez",
      DESCRIP_PAIS == "República de Yemen"                        ~ "Yemen",
      TRUE ~ DESCRIP_PAIS
    )
  )

mapeo_rubros_a_imf <- tribble(
  ~rubros_std,                      ~pais_match,
  "Afganistán",                     "Afghanistan, Islamic Republic of",
  "Albania",                        "Albania",
  "Alemania",                       "Germany",
  "Andorra",                        "Andorra, Principality of",
  "Angola",                         "Angola",
  "Antigua Y Barbuda",              "Antigua and Barbuda",
  "Antillas Holandesas",            "Netherlands Antilles",
  "Arabia Saudita",                 "Saudi Arabia",
  "Argelia",                        "Algeria",
  "Armenia",                        "Armenia, Republic of",
  "Aruba",                          "Aruba, Kingdom of the Netherlands",
  "Australia",                      "Australia",
  "Austria",                        "Austria",
  "Azerbaiyán",                     "Azerbaijan, Republic of",
  "Bahamas",                        "Bahamas, The",
  "Bahrein",                        "Bahrain, Kingdom of",
  "Bangladesh",                     "Bangladesh",
  "Barbados",                       "Barbados",
  "Bielorrusia",                    "Belarus, Republic of",
  "Bélgica",                        "Belgium",
  "Belice",                         "Belize",
  "Benin",                          "Benin",
  "Bolivia",                        "Bolivia",
  "Bosnia y Herzegovina",           "Bosnia and Herzegovina",
  "Botswana",                       "Botswana",
  "Brasil",                         "Brazil",
  "Brunei Darussalam",              "Brunei Darussalam",
  "Bulgaria",                       "Bulgaria",
  "Burkina Faso",                   "Burkina Faso",
  "Burundi",                        "Burundi",
  "Cabo Verde",                     "Cabo Verde",
  "Camboya",                        "Cambodia",
  "Camerún",                        "Cameroon",
  "Canadá",                         "Canada",
  "Chad",                           "Chad",
  "Chile",                          "Chile",
  "China",                          "China, People's Republic of",
  "Chipre",                         "Cyprus",
  "Colombia",                       "Colombia",
  "Comoras",                        "Comoros, Union of the",
  "Congo",                          "Congo, Republic of",
  "República Democrática del Congo","Congo, Democratic Republic of the",
  "Costa de Marfil",                "Côte d'Ivoire",
  "Costa Rica",                     "Costa Rica",
  "Croacia",                        "Croatia, Republic of",
  "Cuba",                           "Cuba",
  "Curazao",                        "Curaçao, Kingdom of the Netherlands",
  "Dinamarca",                      "Denmark",
  "Djibouti",                       "Djibouti",
  "Dominica",                       "Dominica",
  "República Dominicana",           "Dominican Republic",
  "Ecuador",                        "Ecuador",
  "Egipto",                         "Egypt, Arab Republic of",
  "El Salvador",                    "El Salvador",
  "Emiratos Árabes Unidos",         "United Arab Emirates",
  "Eritrea",                        "Eritrea, The State of",
  "Eslovaquia",                     "Slovak Republic",
  "Eslovenia",                      "Slovenia, Republic of",
  "España",                         "Spain",
  "Estados Unidos",                 "United States",
  "Estonia",                        "Estonia, Republic of",
  "Etiopía",                        "Ethiopia, The Federal Democratic Republic of",
  "Fiji",                           "Fiji, Republic of",
  "Filipinas",                      "Philippines",
  "Finlandia",                      "Finland",
  "Francia",                        "France",
  "Gabón",                          "Gabon",
  "Gambia",                         "Gambia, The",
  "Georgia",                        "Georgia",
  "Ghana",                          "Ghana",
  "Granada",                        "Grenada",
  "Grecia",                         "Greece",
  "Guatemala",                      "Guatemala",
  "Guinea",                         "Guinea",
  "Guinea Bissau",                  "Guinea-Bissau",
  "Guinea Ecuatorial",              "Equatorial Guinea, Republic of",
  "Guyana",                         "Guyana",
  "Haití",                          "Haiti",
  "Honduras",                       "Honduras",
  "Hong Kong",                      "Hong Kong Special Administrative Region, People's Republic of China",
  "Hungría",                        "Hungary",
  "India",                          "India",
  "Indonesia",                      "Indonesia",
  "Irán",                           "Iran, Islamic Republic of",
  "Iraq",                           "Iraq",
  "Irlanda",                        "Ireland",
  "Islandia",                       "Iceland",
  "Israel",                         "Israel",
  "Italia",                         "Italy",
  "Jamaica",                        "Jamaica",
  "Japón",                          "Japan",
  "Jordania",                       "Jordan",
  "Kazajstán",                      "Kazakhstan, Republic of",
  "Kenya",                          "Kenya",
  "Corea del Sur",                  "Korea, Republic of",
  "Kuwait",                         "Kuwait",
  "Lesotho",                        "Lesotho, Kingdom of",
  "Letonia",                        "Latvia, Republic of",
  "Líbano",                         "Lebanon",
  "Liberia",                        "Liberia",
  "Libia",                          "Libya",
  "Lituania",                       "Lithuania, Republic of",
  "Luxemburgo",                     "Luxembourg",
  "Macao",                          "Macao Special Administrative Region, People's Republic of China",
  "Macedonia",                      "North Macedonia, Republic of",
  "Madagascar",                     "Madagascar, Republic of",
  "Malasia",                        "Malaysia",
  "Malawi",                         "Malawi",
  "Maldivas",                       "Maldives",
  "Malí",                           "Mali",
  "Malta",                          "Malta",
  "Marruecos",                      "Morocco",
  "Mauricio",                       "Mauritius",
  "Mauritania",                     "Mauritania, Islamic Republic of",
  "México",                         "Mexico",
  "Moldavia, República de",         "Moldova, Republic of",
  "Mongolia",                       "Mongolia",
  "Montenegro",                     "Montenegro",
  "Mozambique",                     "Mozambique, Republic of",
  "Myanmar",                        "Myanmar",
  "Namibia",                        "Namibia",
  "Nepal",                          "Nepal",
  "Nicaragua",                      "Nicaragua",
  "Níger",                          "Niger",
  "Nigeria",                        "Nigeria",
  "Noruega",                        "Norway",
  "Nueva Zelanda",                  "New Zealand",
  "Omán",                           "Oman",
  "Países Bajos",                   "Netherlands, The",
  "Panamá",                         "Panama",
  "Papua  Nueva Guinea",            "Papua New Guinea",
  "Paquistán",                      "Pakistan",
  "Paraguay",                       "Paraguay",
  "Perú",                           "Peru",
  "Polonia",                        "Poland, Republic of",
  "Portugal",                       "Portugal",
  "Puerto Rico",                    "Puerto Rico",
  "Qatar",                          "Qatar",
  "Reino Unido",                    "United Kingdom",
  "República Centroafricana",       "Central African Republic",
  "República Checa",                "Czech Republic",
  "Rumania",                        "Romania",
  "Rusia",                          "Russian Federation",
  "Rwanda",                         "Rwanda",
  "San Cristóbal y Nevis",          "St. Kitts and Nevis",
  "San Vicente Y Las Granadinas",   "St. Vincent and the Grenadines",
  "Santa Lucía",                    "St. Lucia",
  "Santo Tomé y Príncipe",          "São Tomé and Príncipe, Democratic Republic of",
  "Senegal",                        "Senegal",
  "Serbia",                         "Serbia, Republic of",
  "Seychelles",                     "Seychelles",
  "Sierra Leona",                   "Sierra Leone",
  "Singapur",                       "Singapore",
  "Siria",                          "Syrian Arab Republic",
  "Sri Lanka",                      "Sri Lanka",
  "Sudáfrica",                      "South Africa",
  "Sudán",                          "Sudan",
  "SUDAN DEL SUR",                  "South Sudan, Republic of",
  "Suecia",                         "Sweden",
  "Suiza",                          "Switzerland",
  "Suriname",                       "Suriname",
  "Tailandia",                      "Thailand",
  "Taiwán",                         "Taiwan Province of China",
  "Tanzania",                       "Tanzania, United Republic of",
  "Togo",                           "Togo",
  "Trinidad Y Tobago",              "Trinidad and Tobago",
  "Túnez",                          "Tunisia",
  "Turquía",                        "Türkiye, Republic of",
  "Ucrania",                        "Ukraine",
  "Uganda",                         "Uganda",
  "Uruguay",                        "Uruguay",
  "Uzbekistán",                     "Uzbekistan, Republic of",
  "Venezuela",                      "Venezuela, República Bolivariana de",
  "Vietnam",                        "Vietnam",
  "Yemen",                          "Yemen, Republic of",
  "Zambia",                         "Zambia",
  "Zimbabwe",                       "Zimbabwe"
)

euro_members <- c(
  "Austria", "Belgium", "Estonia, Republic of", "Finland", "France",
  "Germany", "Greece", "Ireland", "Italy", "Latvia, Republic of",
  "Lithuania, Republic of", "Luxembourg", "Malta", "Netherlands, The",
  "Portugal", "Slovak Republic", "Slovenia, Republic of", "Spain"
)

pais_master <- mapeo_rubros_a_imf |>
  distinct(rubros_std, pais_match) |>
  mutate(er_source = if_else(pais_match %in% euro_members, "Euro Area (EA)", pais_match))


# ============================================================
# PARSEO LARGO DE CPI Y ER (sin cambios)
# ============================================================

parse_long <- function(df, value_col) {
  df |>
    pivot_longer(
      cols      = starts_with("X"),
      names_to  = "mes_raw",
      values_to = value_col
    ) |>
    mutate(
      mes = as.Date(paste0(str_sub(mes_raw, 2, 5), "-",
                           str_sub(mes_raw, 8, 9), "-01"))
    ) |>
    select(-mes_raw)
}

cpi_long <- parse_long(cpi, "cpi") |>
  rename(pais = COUNTRY) |>
  select(mes, pais, cpi)

er_long <- parse_long(er, "er") |>
  rename(er_source = COUNTRY) |>
  select(mes, er_source, er)

# ============================================================
# PARTICIPACIONES POR PROVINCIA (MOA + MOI + PP + CyE)
# ============================================================

map_er <- pais_master |> distinct(pais = pais_match, er_source)

panel <- cpi_long |>
  inner_join(
    er_long |> inner_join(map_er, by = "er_source") |> select(mes, pais, er),
    by = c("mes", "pais")
  )

# CAMBIO: group_by incluye DESCRIP_PCIA
participaciones <- rubros_pulido |>
  mutate(
    DESCRIP_PCIA = case_when(
      DESCRIP_PCIA %in% c("BUENOS AIRES",         "Buenos Aires")                    ~ "Buenos Aires",
      DESCRIP_PCIA %in% c("CATAMARCA",            "Catamarca")                       ~ "Catamarca",
      DESCRIP_PCIA %in% c("CHACO",                "Chaco")                           ~ "Chaco",
      DESCRIP_PCIA %in% c("CHUBUT",               "Chubut")                          ~ "Chubut",
      DESCRIP_PCIA %in% c("CIUDAD AUTONOMA DE BUENOS AIRES", "Ciudad Autónoma de Buenos Aires") ~ "Ciudad Autónoma de Buenos Aires",
      DESCRIP_PCIA %in% c("CORDOBA",              "Córdoba")                         ~ "Córdoba",
      DESCRIP_PCIA %in% c("CORRIENTES",           "Corrientes")                      ~ "Corrientes",
      DESCRIP_PCIA %in% c("ENTRE RIOS",           "Entre Ríos")                      ~ "Entre Ríos",
      DESCRIP_PCIA %in% c("EXTRANJERO",           "Extranjero")                      ~ "Extranjero",
      DESCRIP_PCIA %in% c("FORMOSA",              "Formosa")                         ~ "Formosa",
      DESCRIP_PCIA %in% c("INDETERMINADO",        "Indeterminado")                   ~ "Indeterminado",
      DESCRIP_PCIA %in% c("JUJUY",                "Jujuy")                           ~ "Jujuy",
      DESCRIP_PCIA %in% c("LA PAMPA",             "La Pampa")                        ~ "La Pampa",
      DESCRIP_PCIA %in% c("LA RIOJA",             "La Rioja")                        ~ "La Rioja",
      DESCRIP_PCIA %in% c("MENDOZA",              "Mendoza")                         ~ "Mendoza",
      DESCRIP_PCIA %in% c("MISIONES",             "Misiones")                        ~ "Misiones",
      DESCRIP_PCIA %in% c("NEUQUEN",              "Neuquén")                         ~ "Neuquén",
      DESCRIP_PCIA %in% c("PLATAFORMA CONTINENTAL","Plataforma Continental")         ~ "Plataforma Continental",
      DESCRIP_PCIA %in% c("RIO NEGRO",            "Río Negro")                       ~ "Río Negro",
      DESCRIP_PCIA %in% c("SALTA",                "Salta")                           ~ "Salta",
      DESCRIP_PCIA %in% c("SAN JUAN",             "San Juan")                        ~ "San Juan",
      DESCRIP_PCIA %in% c("SAN LUIS",             "San Luis")                        ~ "San Luis",
      DESCRIP_PCIA %in% c("SANTA CRUZ",           "Santa Cruz")                      ~ "Santa Cruz",
      DESCRIP_PCIA %in% c("SANTA FE",             "Santa Fe")                        ~ "Santa Fe",
      DESCRIP_PCIA %in% c("SANTIAGO DEL ESTERO",  "Santiago del Estero")             ~ "Santiago del Estero",
      DESCRIP_PCIA %in% c("TIERRA DEL FUEGO",     "Tierra del Fuego")                ~ "Tierra del Fuego",
      DESCRIP_PCIA %in% c("TUCUMAN",              "Tucumán")                         ~ "Tucumán",
      TRUE ~ DESCRIP_PCIA
    )
  ) |>
  filter(Rubro %in% c("MOA", "MOI")) |> # , "PP", "CyE"
  mutate(Año = as.integer(Año)) |>
  left_join(pais_master |> select(rubros_std, pais_match), by = "rubros_std") |>
  filter(!is.na(pais_match)) |>
  group_by(DESCRIP_PCIA, Año, pais = pais_match) |>
  summarise(exportaciones = sum(DOLARES_FOB, na.rm = TRUE), .groups = "drop") |>
  group_by(DESCRIP_PCIA, Año) |>
  mutate(participacion = exportaciones / sum(exportaciones)) |>
  ungroup()

# ============================================================
# PANEL COMPLETO: ahora DESCRIP_PCIA es una dimensión más
# ============================================================

er_cpi_arg <- er.cpi_arg |>
  rename(cpi_arg = IPC_ARG, er_arg = TC_ARG) |>
  mutate(mes = as.Date(mes)) |>
  select(mes, cpi_arg, er_arg)

panel_full <- panel |>
  mutate(Año = as.integer(format(mes, "%Y"))) |>
  left_join(participaciones, by = c("Año", "pais"),  # CAMBIO: join trae DESCRIP_PCIA
            relationship = "many-to-many") |>
  left_join(er_cpi_arg, by = "mes") |>
  group_by(DESCRIP_PCIA, mes) |>                             # CAMBIO: group_by incluye DESCRIP_PCIA
  mutate(
    participacion_norm = participacion / sum(participacion, na.rm = TRUE)
  ) |>
  ungroup()

# ============================================================
# ITCRM POR PROVINCIA (misma lógica, DESCRIP_PCIA como dimensión)
# ============================================================

panel_full <- panel_full |>
  filter(mes >= as.Date("2002-01-01"))

itcrm_pcias <- panel_full |>
  arrange(DESCRIP_PCIA, pais, mes) |>
  group_by(DESCRIP_PCIA, pais) |>                            # CAMBIO: DESCRIP_PCIA en todos los groups
  mutate(
    tcr_bilateral = if_else(
      !is.na(er_arg) & !is.na(er) & !is.na(cpi) & !is.na(cpi_arg),
      (er_arg / er) * (cpi / cpi_arg),
      NA_real_
    ),
    disp = as.integer(!is.na(tcr_bilateral) & !is.na(lag(tcr_bilateral)))
  ) |>
  ungroup() |>
  group_by(DESCRIP_PCIA, mes) |>                             # CAMBIO: DESCRIP_PCIA en todos los groups
  mutate(
    suma_disp = sum(participacion_norm * disp, na.rm = TRUE),
    pond_adj  = if_else(disp == 1 & suma_disp > 0,
                        participacion_norm / suma_disp, 0)
  ) |>
  ungroup() |>
  arrange(DESCRIP_PCIA, pais, mes) |>
  group_by(DESCRIP_PCIA, pais) |>
  mutate(
    var_tcr = if_else(
      disp == 1 & lag(disp) == 1,
      (tcr_bilateral / lag(tcr_bilateral)) ^ pond_adj,
      1
    )
  ) |>
  ungroup() |>
  group_by(DESCRIP_PCIA, mes) |>
  summarise(itcrm_factor = prod(var_tcr, na.rm = TRUE), .groups = "drop") |>
  arrange(DESCRIP_PCIA, mes) |>
  group_by(DESCRIP_PCIA) |>
  mutate(itcrm_nivel = cumprod(itcrm_factor) / first(cumprod(itcrm_factor)) * 100) |>
  ungroup()

# ============================================================
# CAMBIO DE BASE: DIC-2015 = 100
# ============================================================

base_pcias <- itcrm_pcias |>
  filter(mes == as.Date("2015-12-01")) |>
  select(DESCRIP_PCIA, base = itcrm_nivel)

itcrm_pcias <- itcrm_pcias |>
  left_join(base_pcias, by = "DESCRIP_PCIA") |>
  mutate(itcrm_nivel = itcrm_nivel / base * 100) |>
  select(-base)

# ============================================================
# RESULTADO FINAL: WIDE (una columna por provincia + BCRA)
# ============================================================

itcrm_wide <- itcrm_pcias |>
  filter(!is.na(DESCRIP_PCIA)) |>
  select(mes, DESCRIP_PCIA, itcrm_nivel) |>
  pivot_wider(names_from = DESCRIP_PCIA, values_from = itcrm_nivel) |>
  left_join(itcrm_bcra, by = "mes") |>
  arrange(mes)

itcrm_wide |>
  pivot_longer(
    cols      = -c(mes, itcrm_bcra),
    names_to  = "DESCRIP_PCIA",
    values_to = "itcrm_nivel"
  ) |>
  filter(!is.na(itcrm_nivel)) |>
  ggplot(aes(x = mes)) +
  geom_line(aes(y = itcrm_nivel, color = "Provincial"), linewidth = 0.7) +
  geom_line(aes(y = itcrm_bcra,  color = "BCRA"),       linewidth = 0.7) +
  scale_color_manual(values = c("Provincial" = "red", "BCRA" = "black")) +
  facet_wrap(~ DESCRIP_PCIA, scales = "free_y") +
  labs(
    title = "ITCRM por provincia vs BCRA",
    subtitle = "Base dic-2015 = 100",
    x = NULL,
    y = "Índice"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.title    = element_blank(),
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

serie_elegida <- "Santa Fe"

itcrm_wide |>
  select(mes, all_of(serie_elegida), itcrm_bcra) |>
  pivot_longer(
    cols      = -mes,
    names_to  = "serie",
    values_to = "valor"
  ) |>
  ggplot(aes(x = mes, y = valor, color = serie)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(
    values = c(setNames("steelblue", serie_elegida), "itcrm_bcra" = "black"),
    labels = c(setNames(paste("ITCRM", serie_elegida), serie_elegida), "itcrm_bcra" = "BCRA")
  ) +
  labs(
    title    = paste("ITCRM", serie_elegida, "vs BCRA"),
    subtitle = "Base dic-2015 = 100",
    x = NULL,
    y = "Índice"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# check

check <- resultado_santa_fe |>
  select(mes, itcrm_nivel, itcrm_bcra) |>
  left_join(
    itcrm_wide |> select(mes, sf_wide = `Santa Fe`),
    by = "mes"
  ) |>
  mutate(diff = itcrm_nivel - sf_wide)

# SISTEMA DE TIPO DE CAMBIO REGIONAL ####

# ============================================================
# MAPEO PROVINCIA -> REGIÓN
# ============================================================

mapa_regiones <- tibble(
  DESCRIP_PCIA = c(
    "Jujuy", "Salta", "Tucumán", "Catamarca", "Santiago del Estero", "La Rioja",
    "Misiones", "Corrientes", "Chaco", "Formosa",
    "Mendoza", "San Juan", "San Luis",
    "Buenos Aires", "Córdoba", "Santa Fe", "Entre Ríos", "La Pampa",
    "Neuquén", "Río Negro", "Chubut", "Santa Cruz", "Tierra del Fuego",
    "Ciudad Autónoma de Buenos Aires",
    "Extranjero", "Indeterminado", "Plataforma Continental"
  ),
  region = c(
    rep("NOA", 6),
    rep("NEA", 4),
    rep("Cuyo", 3),
    rep("Pampeana", 5),
    rep("Patagonia", 5),
    "GBA",
    rep("Otros", 3)
  )
)

# ============================================================
# PARTICIPACIONES POR REGIÓN
# ============================================================

participaciones_reg <- rubros_pulido |>
  mutate(
    DESCRIP_PCIA = case_when(
      # mismo case_when de normalización de nombres
      DESCRIP_PCIA %in% c("BUENOS AIRES",          "Buenos Aires")                   ~ "Buenos Aires",
      DESCRIP_PCIA %in% c("CATAMARCA",             "Catamarca")                      ~ "Catamarca",
      DESCRIP_PCIA %in% c("CHACO",                 "Chaco")                          ~ "Chaco",
      DESCRIP_PCIA %in% c("CHUBUT",                "Chubut")                         ~ "Chubut",
      DESCRIP_PCIA %in% c("CIUDAD AUTONOMA DE BUENOS AIRES", "Ciudad Autónoma de Buenos Aires") ~ "Ciudad Autónoma de Buenos Aires",
      DESCRIP_PCIA %in% c("CORDOBA",               "Córdoba")                        ~ "Córdoba",
      DESCRIP_PCIA %in% c("CORRIENTES",            "Corrientes")                     ~ "Corrientes",
      DESCRIP_PCIA %in% c("ENTRE RIOS",            "Entre Ríos")                     ~ "Entre Ríos",
      DESCRIP_PCIA %in% c("EXTRANJERO",            "Extranjero")                     ~ "Extranjero",
      DESCRIP_PCIA %in% c("FORMOSA",               "Formosa")                        ~ "Formosa",
      DESCRIP_PCIA %in% c("INDETERMINADO",         "Indeterminado")                  ~ "Indeterminado",
      DESCRIP_PCIA %in% c("JUJUY",                 "Jujuy")                          ~ "Jujuy",
      DESCRIP_PCIA %in% c("LA PAMPA",              "La Pampa")                       ~ "La Pampa",
      DESCRIP_PCIA %in% c("LA RIOJA",              "La Rioja")                       ~ "La Rioja",
      DESCRIP_PCIA %in% c("MENDOZA",               "Mendoza")                        ~ "Mendoza",
      DESCRIP_PCIA %in% c("MISIONES",              "Misiones")                       ~ "Misiones",
      DESCRIP_PCIA %in% c("NEUQUEN",               "Neuquén")                        ~ "Neuquén",
      DESCRIP_PCIA %in% c("PLATAFORMA CONTINENTAL","Plataforma Continental")         ~ "Plataforma Continental",
      DESCRIP_PCIA %in% c("RIO NEGRO",             "Río Negro")                      ~ "Río Negro",
      DESCRIP_PCIA %in% c("SALTA",                 "Salta")                          ~ "Salta",
      DESCRIP_PCIA %in% c("SAN JUAN",              "San Juan")                       ~ "San Juan",
      DESCRIP_PCIA %in% c("SAN LUIS",              "San Luis")                       ~ "San Luis",
      DESCRIP_PCIA %in% c("SANTA CRUZ",            "Santa Cruz")                     ~ "Santa Cruz",
      DESCRIP_PCIA %in% c("SANTA FE",              "Santa Fe")                       ~ "Santa Fe",
      DESCRIP_PCIA %in% c("SANTIAGO DEL ESTERO",   "Santiago del Estero")            ~ "Santiago del Estero",
      DESCRIP_PCIA %in% c("TIERRA DEL FUEGO",      "Tierra del Fuego")               ~ "Tierra del Fuego",
      DESCRIP_PCIA %in% c("TUCUMAN",               "Tucumán")                        ~ "Tucumán",
      TRUE ~ DESCRIP_PCIA
    )
  ) |>
  left_join(mapa_regiones, by = "DESCRIP_PCIA") |>
  filter(Rubro %in% c("MOA", "MOI", "PP", "CyE")) |>
  mutate(Año = as.integer(Año)) |>
  left_join(pais_master |> select(rubros_std, pais_match), by = "rubros_std") |>
  filter(!is.na(pais_match)) |>
  group_by(region, Año, pais = pais_match) |>
  summarise(exportaciones = sum(DOLARES_FOB, na.rm = TRUE), .groups = "drop") |>
  group_by(region, Año) |>
  mutate(participacion = exportaciones / sum(exportaciones)) |>
  ungroup()

# ============================================================
# PANEL + ITCRM POR REGIÓN (misma lógica, region como dimensión)
# ============================================================

panel_full_reg <- panel |>
  mutate(Año = as.integer(format(mes, "%Y"))) |>
  left_join(participaciones_reg, by = c("Año", "pais"),
            relationship = "many-to-many") |>
  left_join(er_cpi_arg, by = "mes") |>
  filter(!is.na(region)) |>    # agregá esta línea
  group_by(region, mes) |>
  mutate(participacion_norm = participacion / sum(participacion, na.rm = TRUE)) |>
  ungroup()

itcrm_reg <- panel_full_reg |>
  arrange(region, pais, mes) |>
  group_by(region, pais) |>
  mutate(
    tcr_bilateral = if_else(
      !is.na(er_arg) & !is.na(er) & !is.na(cpi) & !is.na(cpi_arg),
      (er_arg / er) * (cpi / cpi_arg),
      NA_real_
    ),
    disp = as.integer(!is.na(tcr_bilateral) & !is.na(lag(tcr_bilateral)))
  ) |>
  ungroup() |>
  group_by(region, mes) |>
  mutate(
    suma_disp = sum(participacion_norm * disp, na.rm = TRUE),
    pond_adj  = if_else(disp == 1 & suma_disp > 0,
                        participacion_norm / suma_disp, 0)
  ) |>
  ungroup() |>
  arrange(region, pais, mes) |>
  group_by(region, pais) |>
  mutate(
    var_tcr = if_else(
      disp == 1 & lag(disp) == 1,
      (tcr_bilateral / lag(tcr_bilateral)) ^ pond_adj,
      1
    )
  ) |>
  ungroup() |>
  group_by(region, mes) |>
  summarise(itcrm_factor = prod(var_tcr, na.rm = TRUE), .groups = "drop") |>
  arrange(region, mes) |>
  group_by(region) |>
  mutate(itcrm_nivel = cumprod(itcrm_factor) / first(cumprod(itcrm_factor)) * 100) |>
  ungroup()

# cambio de base dic-2015 = 100
base_reg <- itcrm_reg |>
  filter(mes == as.Date("2015-12-01")) |>
  select(region, base = itcrm_nivel)

itcrm_reg <- itcrm_reg |>
  left_join(base_reg, by = "region") |>
  mutate(itcrm_nivel = itcrm_nivel / base * 100) |>
  select(-base)

# wide final
itcrm_wide_reg <- itcrm_reg |>
  select(mes, region, itcrm_nivel) |>
  pivot_wider(names_from = region, values_from = itcrm_nivel) |>
  left_join(itcrm_bcra, by = "mes") |>
  arrange(mes)
itcrm_wide_reg |>
  pivot_longer(
    cols      = -c(mes, itcrm_bcra),
    names_to  = "region",
    values_to = "itcrm_nivel"
  ) |>
  filter(!is.na(itcrm_nivel)) |>
  ggplot(aes(x = mes)) +
  geom_line(aes(y = itcrm_nivel, color = "Regional"), linewidth = 0.7) +
  geom_line(aes(y = itcrm_bcra,  color = "BCRA"),     linewidth = 0.7) +
  scale_color_manual(values = c("Regional" = "red", "BCRA" = "black")) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title    = "ITCRM por región vs BCRA",
    subtitle = "Base dic-2015 = 100",
    x = NULL,
    y = "Índice"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.title    = element_blank(),
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )  
