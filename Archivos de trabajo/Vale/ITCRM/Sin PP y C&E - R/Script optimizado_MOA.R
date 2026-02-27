library(tidyverse)

options(digits = 3, scipen = 999)

rubros <- readxl::read_excel(
  "Datos.xlsx",
  sheet = "rubros",
  col_names = TRUE
)

er.cpi_arg <- readxl::read_excel(
  "Datos.xlsx",
  sheet = "Arg"
)

cpi <- read.csv("CPI all.csv")

er <- read.csv("ER all.csv")

##

paises_rubros <- sort(unique(rubros$DESCRIP_PAIS))
paises_cpi    <- sort(unique(cpi$COUNTRY))
paises_er     <- sort(unique(er$COUNTRY))

length(paises_rubros)
length(paises_cpi)
length(paises_er)

##  ####

library(dplyr)
library(stringr)
library(tibble)

# ============================================================
# 1) LIMPIEZA EN RUBROS: colapsar variantes a un nombre "rubros_std"
# ============================================================

rubros_pulido <- rubros %>%
  mutate(
    rubros_std = case_when(
      DESCRIP_PAIS %in% c("República Federal de Alemania", "República Federal De Alemania") ~ "Alemania",
      DESCRIP_PAIS == "Antillas Holandesas (territorio vinculado a Países Bajos)" ~ "Antillas Holandesas",
      DESCRIP_PAIS %in% c("Azerbaidzhan", "Azerbaiyán") ~ "Azerbaiyán",
      DESCRIP_PAIS %in% c("Belarús", "Bielorus") ~ "Bielorrusia",
      DESCRIP_PAIS %in% c("Cambodya", "Camboya (ex Kampuchea)") ~ "Camboya",
      DESCRIP_PAIS == "Camerun" ~ "Camerún",
      DESCRIP_PAIS %in% c("Rep. Democrática del Congo (ex Zaire)", "República Democrática Del Congo") ~ "República Democrática del Congo",
      DESCRIP_PAIS %in% c("Corea Republicana", "Corea, República de") ~ "Corea del Sur",
      DESCRIP_PAIS %in% c("Costa De Marfil", "Côte d' Ivoire (Costa de Marfil)") ~ "Costa de Marfil",
      DESCRIP_PAIS == "Emiratos Arabes Unidos" ~ "Emiratos Árabes Unidos",
      DESCRIP_PAIS == "Etiopia" ~ "Etiopía",
      DESCRIP_PAIS == "Grenada" ~ "Granada",
      DESCRIP_PAIS %in% c("Hong Kong (Región administrativa especial de China)",
                          "Hong Kong Región Administrativa Especial de (China)") ~ "Hong Kong",
      DESCRIP_PAIS == "Iran" ~ "Irán",
      DESCRIP_PAIS == "Japon" ~ "Japón",
      DESCRIP_PAIS == "Libano" ~ "Líbano",
      DESCRIP_PAIS == "Libia Jamahiriya Árabe" ~ "Libia",
      DESCRIP_PAIS == "Macao Región Administrativa Especial de (China)" ~ "Macao",
      DESCRIP_PAIS == "Macedonia (ex República Yugoslava de)" ~ "Macedonia",
      DESCRIP_PAIS == "Mali" ~ "Malí",
      DESCRIP_PAIS == "Niger" ~ "Níger",
      DESCRIP_PAIS == "Nueva Zelandia" ~ "Nueva Zelanda",
      DESCRIP_PAIS == "Oman" ~ "Omán",
      DESCRIP_PAIS == "Paquistan" ~ "Paquistán",
      DESCRIP_PAIS == "Puerto Rico (Estado Asociado)" ~ "Puerto Rico",
      DESCRIP_PAIS == "Rusia Federación de" ~ "Rusia",
      DESCRIP_PAIS %in% c("Saint Kitts y Nevis - (San Cristóbal Y Nevis)", "San Cristóbal Y Nevis") ~ "San Cristóbal y Nevis",
      DESCRIP_PAIS == "Samoa  Occidental" ~ "Samoa",
      DESCRIP_PAIS == "Taiwan" ~ "Taiwán",
      DESCRIP_PAIS %in% c("Tanzania", "Tanzania, República Unida de") ~ "Tanzania",
      DESCRIP_PAIS == "Tunez" ~ "Túnez",
      # DESCRIP_PAIS %in% c("Colonia (Uruguay)", "Florida (Uruguay)", "Libertad (Uruguay)",
      #                     "Nueva Palmira (Uruguay)", "Río Negro (Uruguay)",
      #                     "Zona Franca Zonamerica (Uruguay)", "Zonamerica (ex Montevideo) (Uruguay)") ~ "Uruguay",
      # DESCRIP_PAIS %in% c("Iquique (Chile)", "Punta Arenas (Chile)", "Zona Franca Iquique (Chile)") ~ "Chile",
      # DESCRIP_PAIS == "Estados Unidos - Puerto Rico" ~ "Estados Unidos",
      # DESCRIP_PAIS == "Viet Nam" ~ "Vietnam",
      DESCRIP_PAIS == "República de Yemen" ~ "Yemen",
      TRUE ~ DESCRIP_PAIS
    )
  )

# ============================================================
# 2) MAPEO RUBROS (ES) -> IMF (EN) : pais_match
#    Esto es tu "mapeo_paises" pero en sentido útil para CPI/ER
# ============================================================

mapeo_rubros_a_imf <- tribble(
  ~rubros_std, ~pais_match,
  
  "Afganistán", "Afghanistan, Islamic Republic of",
  "Albania", "Albania",
  "Alemania", "Germany",
  "Andorra", "Andorra, Principality of",
  "Angola", "Angola",
  "Antigua Y Barbuda", "Antigua and Barbuda",
  "Antillas Holandesas", "Netherlands Antilles",
  "Arabia Saudita", "Saudi Arabia",
  "Argelia", "Algeria",
  "Armenia", "Armenia, Republic of",
  "Aruba", "Aruba, Kingdom of the Netherlands",
  "Australia", "Australia",
  "Austria", "Austria",
  "Azerbaiyán", "Azerbaijan, Republic of",
  "Bahamas", "Bahamas, The",
  "Bahrein", "Bahrain, Kingdom of",
  "Bangladesh", "Bangladesh",
  "Barbados", "Barbados",
  "Bielorrusia", "Belarus, Republic of",
  "Bélgica", "Belgium",
  "Belice", "Belize",
  "Benin", "Benin",
  "Bolivia", "Bolivia",
  "Bosnia y Herzegovina", "Bosnia and Herzegovina",
  "Botswana", "Botswana",
  "Brasil", "Brazil",
  "Brunei Darussalam", "Brunei Darussalam",
  "Bulgaria", "Bulgaria",
  "Burkina Faso", "Burkina Faso",
  "Burundi", "Burundi",
  "Cabo Verde", "Cabo Verde",
  "Camboya", "Cambodia",
  "Camerún", "Cameroon",
  "Canadá", "Canada",
  "Chad", "Chad",
  "Chile", "Chile",
  "China", "China, People's Republic of",
  "Chipre", "Cyprus",
  "Colombia", "Colombia",
  "Comoras", "Comoros, Union of the",
  "Congo", "Congo, Republic of",
  "República Democrática del Congo", "Congo, Democratic Republic of the",
  "Costa de Marfil", "Côte d'Ivoire",
  "Costa Rica", "Costa Rica",
  "Croacia", "Croatia, Republic of",
  "Cuba", "Cuba",
  "Curazao", "Curaçao, Kingdom of the Netherlands",
  "Dinamarca", "Denmark",
  "Djibouti", "Djibouti",
  "Dominica", "Dominica",
  "República Dominicana", "Dominican Republic",
  "Ecuador", "Ecuador",
  "Egipto", "Egypt, Arab Republic of",
  "El Salvador", "El Salvador",
  "Emiratos Árabes Unidos", "United Arab Emirates",
  "Eritrea", "Eritrea, The State of",
  "Eslovaquia", "Slovak Republic",
  "Eslovenia", "Slovenia, Republic of",
  "España", "Spain",
  "Estados Unidos", "United States",
  "Estonia", "Estonia, Republic of",
  "Etiopía", "Ethiopia, The Federal Democratic Republic of",
  "Fiji", "Fiji, Republic of",
  "Filipinas", "Philippines",
  "Finlandia", "Finland",
  "Francia", "France",
  "Gabón", "Gabon",
  "Gambia", "Gambia, The",
  "Georgia", "Georgia",
  "Ghana", "Ghana",
  "Granada", "Grenada",
  "Grecia", "Greece",
  "Guatemala", "Guatemala",
  "Guinea", "Guinea",
  "Guinea Bissau", "Guinea-Bissau",
  # OJO: en tu lista aparece "Guiena ecuatorial" mal escrito. Normalizo a lo correcto:
  "Guinea Ecuatorial", "Equatorial Guinea, Republic of",
  "Guyana", "Guyana",
  "Haití", "Haiti",
  "Honduras", "Honduras",
  "Hong Kong", "Hong Kong Special Administrative Region, People's Republic of China",
  "Hungría", "Hungary",
  "India", "India",
  "Indonesia", "Indonesia",
  "Irán", "Iran, Islamic Republic of",
  "Iraq", "Iraq",
  "Irlanda", "Ireland",
  "Islandia", "Iceland",
  "Israel", "Israel",
  "Italia", "Italy",
  "Jamaica", "Jamaica",
  "Japón", "Japan",
  "Jordania", "Jordan",
  "Kazajstán", "Kazakhstan, Republic of",
  "Kenya", "Kenya",
  "Corea del Sur", "Korea, Republic of",
  "Kuwait", "Kuwait",
  "Lesotho", "Lesotho, Kingdom of",
  "Letonia", "Latvia, Republic of",
  "Líbano", "Lebanon",
  "Liberia", "Liberia",
  "Libia", "Libya",
  "Lituania", "Lithuania, Republic of",
  "Luxemburgo", "Luxembourg",
  "Macao", "Macao Special Administrative Region, People's Republic of China",
  "Macedonia", "North Macedonia, Republic of",
  "Madagascar", "Madagascar, Republic of",
  "Malasia", "Malaysia",
  "Malawi", "Malawi",
  "Maldivas", "Maldives",
  "Malí", "Mali",
  "Malta", "Malta",
  "Marruecos", "Morocco",
  "Mauricio", "Mauritius",
  "Mauritania", "Mauritania, Islamic Republic of",
  "México", "Mexico",
  "Moldavia, República de", "Moldova, Republic of",
  "Mongolia", "Mongolia",
  "Montenegro", "Montenegro",
  "Mozambique", "Mozambique, Republic of",
  "Myanmar", "Myanmar",
  "Namibia", "Namibia",
  "Nepal", "Nepal",
  "Nicaragua", "Nicaragua",
  "Níger", "Niger",
  "Nigeria", "Nigeria",
  "Noruega", "Norway",
  "Nueva Zelanda", "New Zealand",
  "Omán", "Oman",
  "Países Bajos", "Netherlands, The",
  "Panamá", "Panama",
  "Papua  Nueva Guinea", "Papua New Guinea",
  "Paquistán", "Pakistan",
  "Paraguay", "Paraguay",
  "Perú", "Peru",
  "Polonia", "Poland, Republic of",
  "Portugal", "Portugal",
  "Puerto Rico", "Puerto Rico",
  "Qatar", "Qatar",
  "Reino Unido", "United Kingdom",
  "República Centroafricana", "Central African Republic",
  "República Checa", "Czech Republic",
  "Rumania", "Romania",
  "Rusia", "Russian Federation",
  "Rwanda", "Rwanda",
  "San Cristóbal y Nevis", "St. Kitts and Nevis",
  "San Vicente Y Las Granadinas", "St. Vincent and the Grenadines",
  "Santa Lucía", "St. Lucia",
  "Santo Tomé y Príncipe", "São Tomé and Príncipe, Democratic Republic of",
  "Senegal", "Senegal",
  "Serbia", "Serbia, Republic of",
  "Seychelles", "Seychelles",
  "Sierra Leona", "Sierra Leone",
  "Singapur", "Singapore",
  "Siria", "Syrian Arab Republic",
  "Sri Lanka", "Sri Lanka",
  "Sudáfrica", "South Africa",
  "Sudán", "Sudan",
  "SUDAN DEL SUR", "South Sudan, Republic of",
  "Suecia", "Sweden",
  "Suiza", "Switzerland",
  "Suriname", "Suriname",
  "Tailandia", "Thailand",
  "Taiwán", "Taiwan Province of China",
  "Tanzania", "Tanzania, United Republic of",
  "Togo", "Togo",
  "Trinidad Y Tobago", "Trinidad and Tobago",
  "Túnez", "Tunisia",
  "Turquía", "Türkiye, Republic of",
  "Ucrania", "Ukraine",
  "Uganda", "Uganda",
  "Uruguay", "Uruguay",
  "Uzbekistán", "Uzbekistan, Republic of",
  "Venezuela", "Venezuela, República Bolivariana de",
  "Vietnam", "Vietnam",
  "Yemen", "Yemen, Republic of",
  "Zambia", "Zambia",
  "Zimbabwe", "Zimbabwe"
)

# ============================================================
# 3) EURO: definimos qué paises toman ER de Euro Area (EA)
#    (CPI siempre del país)
# ============================================================

euro_members <- c(
  "Austria","Belgium","Estonia, Republic of","Finland","France","Germany","Greece",
  "Ireland","Italy","Latvia, Republic of","Lithuania, Republic of","Luxembourg",
  "Malta","Netherlands, The","Portugal","Slovak Republic","Slovenia, Republic of","Spain"
)

# ============================================================
# 4) ARMAR pais_master COMPLETO (ya no es “ejemplo”)
# ============================================================

pais_master <- mapeo_rubros_a_imf %>%
  distinct(rubros_std, pais_match) %>%
  mutate(
    er_source = if_else(pais_match %in% euro_members, "Euro Area (EA)", pais_match)
  )

# ============================================================
# 5) GENERAR LAS 3 BASES CON LA LLAVE DE MATCH
#    IMPORTANTE: NO FILTRO. Dejo NA para ver qué falta.
# ============================================================

rubros_fix <- rubros_pulido %>%
  left_join(pais_master, by = "rubros_std")

cpi_fix <- cpi %>%
  mutate(pais_match = COUNTRY)

er_fix <- er %>%
  mutate(er_source = COUNTRY)

# ============================================================
# 6) CONTROLES DE UNIVERSO
# ============================================================

cat("Rubros (std) únicos:", length(unique(rubros_pulido$rubros_std)), "\n")
cat("Rubros con pais_match:", sum(!is.na(rubros_fix$pais_match)) / nrow(rubros_fix) * 100, "% de filas\n")
cat("Países rubros con match:", length(unique(na.omit(rubros_fix$pais_match))), "\n")

# Países que aparecen en rubros pero todavía sin match (para completar mapeo si queda algo suelto)
faltan_en_mapeo <- rubros_fix %>%
  filter(is.na(pais_match)) %>%
  distinct(rubros_std) %>%
  arrange(rubros_std)

faltan_en_mapeo

## 

cpi_long <- cpi_fix %>%
  pivot_longer(
    cols = matches("^X\\d{4}\\.M\\d{2}$"),
    names_to  = "periodo",
    values_to = "cpi"
  ) %>%
  mutate(
    anio = as.integer(substr(periodo, 2, 5)),
    mes  = as.integer(substr(periodo, 8, 9)),
    mes  = as.Date(sprintf("%d-%02d-01", anio, mes)),
    pais = COUNTRY
  ) %>%
  select(pais, mes, cpi)

er_long <- er_fix %>%
  pivot_longer(
    cols = matches("^X\\d{4}\\.M\\d{2}$"),
    names_to  = "periodo",
    values_to = "er"
  ) %>%
  mutate(
    anio = as.integer(substr(periodo, 2, 5)),
    mesn = as.integer(substr(periodo, 8, 9)),
    mes  = as.Date(sprintf("%d-%02d-01", anio, mesn)),
    pais = COUNTRY
  ) %>%
  select(er_source, mes, er, pais)

##

# Universos
paises_cpi <- distinct(cpi_long, pais)
paises_er  <- distinct(er_long, pais)

# Tabla de presencia
audit_paises <- full_join(
  paises_cpi %>% mutate(en_cpi = "SI"),
  paises_er  %>% mutate(en_er  = "SI"),
  by = "pais"
) %>%
  mutate(
    en_cpi = if_else(is.na(en_cpi), "NO", en_cpi),
    en_er  = if_else(is.na(en_er),  "NO", en_er)
  )

# Quedarse solo con los que NO están en ambos
audit_paises_faltantes <- audit_paises %>%
  filter(!(en_cpi == "SI" & en_er == "SI")) %>%
  arrange(pais)

audit_paises_faltantes


##
cpi_long <- cpi_fix %>%
  select(COUNTRY, starts_with("X")) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to  = "mes_raw",
    values_to = "cpi"
  ) %>%
  mutate(
    # X2001.M01 -> 2001-01-01
    mes = as.Date(
      paste0(
        str_sub(mes_raw, 2, 5), "-",
        str_sub(mes_raw, 8, 9), "-01"
      )
    ),
    pais = COUNTRY
  ) %>%
  select(mes, pais, cpi) %>%
  arrange(pais, mes)

n_distinct(cpi_long$pais)
range(cpi_long$mes)

er_long <- er_fix %>%
  select(COUNTRY, starts_with("X")) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to  = "mes_raw",
    values_to = "er"
  ) %>%
  mutate(
    mes = as.Date(
      paste0(
        str_sub(mes_raw, 2, 5), "-",
        str_sub(mes_raw, 8, 9), "-01"
      )
    ),
    er_source = COUNTRY
  ) %>%
  select(mes, er_source, er) %>%
  arrange(er_source, mes)

unique(er_long$er_source)[grepl("Euro", unique(er_long$er_source))]

# puente pais <- er_source
map_er <- pais_master %>%
  distinct(
    pais = pais_match,
    er_source
  )

er_by_pais <- er_long %>%
  inner_join(map_er, by = "er_source") %>%
  select(mes, pais, er)

panel_mes_pais <- cpi_long %>%
  inner_join(
    er_by_pais,
    by = c("mes", "pais")
  ) %>%
  select(mes, pais, er, cpi) %>%
  arrange(pais, mes)

length(unique(panel_mes_pais$pais))
unique(panel_mes_pais$pais)

## 

rubros_filtrado <- rubros %>%
  filter(Rubro %in% c("MOA")) %>%
  mutate(Año = as.integer(Año)) %>%
  group_by(Año, DESCRIP_PAIS) %>%
  summarise(
    exportaciones_fob = sum(DOLARES_FOB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    total_anual = sum(exportaciones_fob, na.rm = TRUE),
    participacion = exportaciones_fob / total_anual
  ) %>%
  ungroup() %>%
  arrange(Año, desc(participacion))

unique(rubros_filtrado$DESCRIP_PAIS)

map_rubros_a_panel <- rubros_pulido %>%
  select(DESCRIP_PAIS, rubros_std) %>%
  distinct() %>%
  left_join(
    pais_master,
    by = "rubros_std"
  ) %>%
  select(
    DESCRIP_PAIS,
    pais_cpi_er = pais_match
  )

rubros_filtrado1 <- rubros_filtrado %>%
  left_join(
    map_rubros_a_panel,
    by = "DESCRIP_PAIS"
  )

# ¿Cuáles sí matchean con CPI/ER?

rubros_filtrado1 %>%
  filter(!is.na(pais_cpi_er)) %>%
  distinct(pais_cpi_er) %>%
  arrange(pais_cpi_er)

# ¿Cuáles no tienen CPI/ER? (normal)
  
rubros_filtrado1 %>%
  filter(is.na(pais_cpi_er)) %>%
  distinct(DESCRIP_PAIS) %>%
  arrange(DESCRIP_PAIS)  

resumen_anual <- rubros_filtrado1 %>%
    mutate(
      en_cpi_er = !is.na(pais_cpi_er)
    ) %>%
    group_by(Año) %>%
    summarise(
      share_cpi_er    = sum(participacion[en_cpi_er], na.rm = TRUE),
      share_no_cpi_er = sum(participacion[!en_cpi_er], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Año) %>%
    mutate(
      suma_control = share_cpi_er + share_no_cpi_er
    )

resumen_anual
  
# JUNTANDO ER Y CPI CON PARTICIPACION ####

# afdsa ####
# 1) PARTICIPACIONES ANUALES POR PAÍS (MOA + MOI)
#    Universo: países que matchean con CPI/ER
#    Llave final: (Año, pais)

participaciones_anuales <- rubros_filtrado1 %>%
  filter(!is.na(pais_cpi_er)) %>%
  group_by(
    Año,
    pais = pais_cpi_er
  ) %>%
  summarise(
    participacion = sum(participacion, na.rm = TRUE),
    .groups = "drop"
  )

# afdsa ####
# 1.a) CHECK: unicidad de la llave (Año, pais)
#      Debe devolver 0 filas

participaciones_anuales %>%
  count(Año, pais) %>%
  filter(n > 1)


# afdsa ####
# 2) PANEL MENSUAL CPI–ER CON AÑO EXPLÍCITO

panel_mes_pais1 <- panel_mes_pais %>%
  mutate(
    Año = as.integer(format(mes, "%Y"))
  )


# afdsa ####
# 2.a) CHECK: solapamiento temporal

range(panel_mes_pais1$Año, na.rm = TRUE)
range(participaciones_anuales$Año, na.rm = TRUE)


# afdsa ####
# 3) JOIN FINAL: PANEL MENSUAL + PARTICIPACIÓN ANUAL
#    (join 1–a–muchos, correcto)

panel_mes_pais1 <- panel_mes_pais1 %>%
  left_join(
    participaciones_anuales,
    by = c("Año", "pais")
  )


# afdsa ####
# 4) CHECK FUERTE: MASA ANUAL DEL PANEL
#    (suma de participaciones del universo efectivo del panel)

check_panel <- panel_mes_pais1 %>%
  filter(!is.na(participacion)) %>%
  distinct(Año, pais, participacion) %>%  # quita repetición mensual
  group_by(Año) %>%
  summarise(
    suma_participacion = sum(participacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Año)

check_panel


# afdsa ####
# 5) CHECK CRUZADO CONTRA RUBROS
#    (la masa del panel debe coincidir con la masa CPI/ER efectiva)

share_cpi_er_panel <- rubros_filtrado1 %>%
  filter(!is.na(pais_cpi_er)) %>%
  semi_join(
    panel_mes_pais1 %>% distinct(Año, pais),
    by = c("Año", "pais_cpi_er" = "pais")
  ) %>%
  group_by(Año) %>%
  summarise(
    share_cpi_er_panel = sum(participacion, na.rm = TRUE),
    .groups = "drop"
  )

left_join(check_panel, share_cpi_er_panel, by = "Año") %>%
  mutate(
    diff = suma_participacion - share_cpi_er_panel
  )

### INCORORARNDO ARGENTINA ####

panel_mes_pais1

str(er.cpi_arg)

er_cpi_arg_fix <- er.cpi_arg %>%
  rename(
    mes     = ...1,
    cpi_arg = IPC_ARG,
    er_arg  = TC_ARG
  ) %>%
  mutate(
    mes = as.Date(mes)
  )

panel_mes_pais2 <- panel_mes_pais1 %>%
  left_join(
    er_cpi_arg_fix,
    by = "mes"
  )

panel_mes_pais2 %>%
  summarise(
    miss_cpi_arg = sum(is.na(cpi_arg)),
    miss_er_arg  = sum(is.na(er_arg))
  )


## CAMBIO DE BASE | CPI base 2002 = 100 (todos) ####

# Base Argentina
base_cpi_arg_2002 <- panel_mes_pais2 %>%
  filter(format(mes, "%Y") == "2002") %>%
  summarise(b = mean(cpi_arg, na.rm = TRUE)) %>%
  pull(b)

# Base por país
base_cpi_pais_2002 <- panel_mes_pais2 %>%
  filter(format(mes, "%Y") == "2002") %>%
  group_by(pais) %>%
  summarise(b = mean(cpi, na.rm = TRUE), .groups = "drop")

#

panel_mes_pais3 <- panel_mes_pais2 %>%
  left_join(base_cpi_pais_2002, by = "pais") %>%
  mutate(
    cpi_2002 = cpi / b * 100,
    cpi_arg_2002 = cpi_arg / base_cpi_arg_2002 * 100
  ) %>%
  select(-b)

### CHECK DE CAMBIO DE BASE CORRECTO ####

panel_mes_pais3 %>%
  filter(format(mes, "%Y") == "2002") %>%
  summarise(
    arg = mean(cpi_arg_2002, na.rm = TRUE),
    otros = mean(cpi_2002, na.rm = TRUE)
  )

#

panel_mes_pais4 <- panel_mes_pais3 %>%
  group_by(mes) %>%
  mutate(
    participacion_norm =
      participacion / sum(participacion, na.rm = TRUE)
  ) %>%
  ungroup()

#### check tiene que dar 1 ####
panel_mes_pais4 %>%
  filter(!is.na(participacion_norm)) %>%
  group_by(mes) %>%
  summarise(suma = sum(participacion_norm)) %>%
  summarise(
    min = min(suma),
    max = max(suma)
  )


panel_mes_pais5 <- panel_mes_pais4 %>%
  mutate(
    tcrm_bilateral =
      (er_arg / er) * (cpi_2002 / cpi_arg_2002)
  )

summary(panel_mes_pais5$tcrm_bilateral)


itcrm <- panel_mes_pais5 %>%
  filter(!is.na(participacion_norm),
         !is.na(tcrm_bilateral)) %>%
  group_by(mes) %>%
  summarise(
    itcrm = sum(participacion_norm * tcrm_bilateral),
    .groups = "drop"
  )

base_itcrm_2002 <- itcrm %>%
  filter(format(mes, "%Y") == "2002") %>%
  summarise(b = mean(itcrm, na.rm = TRUE)) %>%
  pull(b)

itcrm_final <- itcrm %>%
  mutate(
    itcrm_2002_100 = itcrm / base_itcrm_2002 * 100
  )

# incorporando el de argentina ####

itcrm_bcra <- readxl::read_excel("Datos.xlsx",
                                 sheet = "itcrm_bcra")

itcrm_bcra_fix <- itcrm_bcra %>%
  rename(
    mes = ...1
  ) %>%
  mutate(
    mes = as.Date(mes)
  )

ggplot() +
  geom_line(
    data = itcrm_final,
    aes(x = mes, y = itcrm_2002_100),
    color = "steelblue",
    linewidth = 0.9
  ) +
  geom_line(
    data = itcrm_bcra_fix,
    aes(x = mes, y = itcrm_bcra_2002),
    color = "black",
    linewidth = 0.9
  ) +
  labs(
    title = "ITCRM Argentina – Comparación",
    subtitle = "Azul: ITCRM propio (base 2002 = 100) | Negro: ITCRM BCRA",
    x = NULL,
    y = "Índice"
  ) +
  theme_minimal()

# agregando indice geométrico

itcrm_geo <- panel_mes_pais5 %>%
  filter(
    !is.na(participacion_norm),
    !is.na(tcrm_bilateral),
    tcrm_bilateral > 0
  ) %>%
  group_by(mes) %>%
  summarise(
    itcrm_geo = exp(
      sum(participacion_norm * log(tcrm_bilateral), na.rm = TRUE)
    ),
    .groups = "drop"
  )

base_itcrm_geo_2002 <- itcrm_geo %>%
  filter(format(mes, "%Y") == "2002") %>%
  summarise(b = mean(itcrm_geo, na.rm = TRUE)) %>%
  pull(b)

itcrm_geo_final <- itcrm_geo %>%
  mutate(
    itcrm_geo_2002_100 = itcrm_geo / base_itcrm_geo_2002 * 100
  )

base_bcra_2002 <- itcrm_bcra_fix %>%
  filter(format(mes, "%Y") == "2002") %>%
  summarise(b = mean(itcrm_bcra, na.rm = TRUE)) %>%
  pull(b)

itcrm_bcra_fix <- itcrm_bcra_fix %>%
  mutate(
    itcrm_bcra_2002 = itcrm_bcra / base_bcra_2002 * 100
  )

library(ggplot2)

ggplot() +
  geom_line(
    data = itcrm_final,
    aes(x = mes, y = itcrm_2002_100),
    color = "steelblue",
    linewidth = 0.9
  ) +
  geom_line(
    data = itcrm_geo_final,
    aes(x = mes, y = itcrm_geo_2002_100),
    color = "firebrick",
    linewidth = 0.9
  ) +
  geom_line(
    data = itcrm_bcra_fix,
    aes(x = mes, y = itcrm_bcra_2002),
    color = "black",
    linewidth = 0.9
  ) +
  labs(
    title = "ITCRM Argentina – Comparación",
    subtitle = "Azul: aritmético | Rojo: geométrico | Negro: BCRA",
    x = NULL,
    y = "Índice (base 2002 = 100)"
  ) +
  theme_minimal()
