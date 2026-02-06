library(tidyverse)
options(digits = 3, scipen = 999)

rubros <- readxl::read_excel("Datos.xlsx",
                             sheet = "rubros",
                             col_names = T)

# ARREGLO DE NOMBRES ####

rubros_pulido <- rubros %>%
  mutate(DESCRIP_PAIS = case_when(
    # Alemania
    DESCRIP_PAIS %in% c("República Federal de Alemania", "República Federal De Alemania") ~ "Alemania",
    
    # Antillas Holandesas
    DESCRIP_PAIS == "Antillas Holandesas (territorio vinculado a Países Bajos)" ~ "Antillas Holandesas",
    
    # Azerbaiyán
    DESCRIP_PAIS %in% c("Azerbaidzhan", "Azerbaiyán") ~ "Azerbaiyán",
    
    # Bielorrusia
    DESCRIP_PAIS %in% c("Belarús", "Bielorus") ~ "Bielorrusia",
    
    # Camboya
    DESCRIP_PAIS %in% c("Cambodya", "Camboya (ex Kampuchea)") ~ "Camboya",
    
    # Camerún
    DESCRIP_PAIS == "Camerun" ~ "Camerún",
    
    # Congo
    DESCRIP_PAIS %in% c("Rep. Democrática del Congo (ex Zaire)", 
                        "República Democrática Del Congo") ~ "República Democrática del Congo",
    
    # Corea del Sur
    DESCRIP_PAIS %in% c("Corea Republicana", "Corea, República de") ~ "Corea del Sur",
    
    # Costa de Marfil
    DESCRIP_PAIS %in% c("Costa De Marfil", "Côte d' Ivoire (Costa de Marfil)") ~ "Costa de Marfil",
    
    # Emiratos Árabes Unidos
    DESCRIP_PAIS == "Emiratos Arabes Unidos" ~ "Emiratos Árabes Unidos",
    
    # Etiopía
    DESCRIP_PAIS == "Etiopia" ~ "Etiopía",
    
    # Grenada/Granada
    DESCRIP_PAIS == "Grenada" ~ "Granada",
    
    # Hong Kong
    DESCRIP_PAIS %in% c("Hong Kong (Región administrativa especial de China)",
                        "Hong Kong Región Administrativa Especial de (China)") ~ "Hong Kong",
    
    # Irán
    DESCRIP_PAIS == "Iran" ~ "Irán",
    
    # Japón
    DESCRIP_PAIS == "Japon" ~ "Japón",
    
    # Líbano
    DESCRIP_PAIS == "Libano" ~ "Líbano",
    
    # Libia
    DESCRIP_PAIS == "Libia Jamahiriya Árabe" ~ "Libia",
    
    # Macao
    DESCRIP_PAIS == "Macao Región Administrativa Especial de (China)" ~ "Macao",
    
    # Macedonia
    DESCRIP_PAIS == "Macedonia (ex República Yugoslava de)" ~ "Macedonia",
    
    # Malí
    DESCRIP_PAIS == "Mali" ~ "Malí",
    
    # Níger
    DESCRIP_PAIS == "Niger" ~ "Níger",
    
    # Nueva Zelanda
    DESCRIP_PAIS == "Nueva Zelandia" ~ "Nueva Zelanda",
    
    # Omán
    DESCRIP_PAIS == "Oman" ~ "Omán",
    
    # Pakistán
    DESCRIP_PAIS == "Paquistan" ~ "Paquistán",
    
    # Puerto Rico
    DESCRIP_PAIS == "Puerto Rico (Estado Asociado)" ~ "Puerto Rico",
    
    # Rumania
    DESCRIP_PAIS == "Rumania" ~ "Rumania",
    
    # Rusia
    DESCRIP_PAIS == "Rusia Federación de" ~ "Rusia",
    
    # San Cristóbal y Nevis
    DESCRIP_PAIS %in% c("Saint Kitts y Nevis - (San Cristóbal Y Nevis)", 
                        "San Cristóbal Y Nevis") ~ "San Cristóbal y Nevis",
    
    # Samoa
    DESCRIP_PAIS == "Samoa  Occidental" ~ "Samoa",
    
    # Serbia y Montenegro
    DESCRIP_PAIS == "Federación Serbia Y Montenegro" ~ "Serbia y Montenegro",
    
    # Taiwan
    DESCRIP_PAIS == "Taiwan" ~ "Taiwán",
    
    # Tanzania
    DESCRIP_PAIS %in% c("Tanzania", "Tanzania, República Unida de") ~ "Tanzania",
    
    # Territorios vinculados a Francia
    DESCRIP_PAIS %in% c("Teritorios vinculados a Francia", 
                        "Territorios Franceses",
                        "Territorios vinculados a Francia",
                        "Territorios Vinculados a Francia",
                        "Territorios Vinculados a Francia Del Sur") ~ "Territorios Franceses",
    
    # Territorios vinculados a Reino Unido
    DESCRIP_PAIS %in% c("Territorios Británicos",
                        "Territorios vinculados al Reino Unido de Gran Bretaña e Irlanda del No") ~ "Territorios Británicos",
    
    # Territorios vinculados a Países Bajos
    DESCRIP_PAIS == "Territorios Holandeses" ~ "Territorios Holandeses",
    
    # Túnez
    DESCRIP_PAIS == "Tunez" ~ "Túnez",
    
    # Uruguay (zonas francas y localidades)
    DESCRIP_PAIS %in% c("Colonia (Uruguay)", "Florida (Uruguay)", 
                        "Libertad (Uruguay)", "Nueva Palmira (Uruguay)",
                        "Río Negro (Uruguay)", "Zona Franca Zonamerica (Uruguay)",
                        "Zonamerica (ex Montevideo) (Uruguay)") ~ "Uruguay",
    
    # Chile (zonas francas y localidades)
    DESCRIP_PAIS %in% c("Iquique (Chile)", "Punta Arenas (Chile)",
                        "Zona Franca Iquique (Chile)") ~ "Chile",
    
    # Estados Unidos
    DESCRIP_PAIS == "Estados Unidos - Puerto Rico" ~ "Estados Unidos",
    
    # Vietnam
    DESCRIP_PAIS == "Viet Nam" ~ "Vietnam",
    
    # Yemen
    DESCRIP_PAIS == "República de Yemen" ~ "Yemen",
    
    # Si no coincide con ninguno, mantener el nombre original
    TRUE ~ DESCRIP_PAIS
  ))

# Verificar cambios
rubros %>% 
  count(DESCRIP_PAIS) %>% 
  arrange(DESCRIP_PAIS)

# CALCULO DE TOTALES POR AÑO SEGÚN RUBRO ####

total_rubros_año <- rubros_pulido %>%
  group_by(Año, Rubro) %>%
  summarise(DOLARES_FOB = sum(DOLARES_FOB, na.rm = TRUE),
            .groups = 'drop') %>%
  pivot_wider(names_from = Rubro, 
              values_from = DOLARES_FOB,
              values_fill = 0) %>%
  mutate(
    TOTAL_AÑO = PP + MOA + MOI + CyE,
    PART_PP = (PP / TOTAL_AÑO) * 100,
    PART_MOA = (MOA / TOTAL_AÑO) * 100,
    PART_MOI = (MOI / TOTAL_AÑO) * 100,
    PART_CyE = (CyE / TOTAL_AÑO) * 100,
    MOI_MOA = MOI + MOA,
    PART_MOI_MOA = (MOI_MOA / TOTAL_AÑO) * 100
  ) %>%
  select(Año, PP, MOA, MOI, CyE, TOTAL_AÑO, PART_PP, PART_MOA, PART_MOI, PART_CyE, MOI_MOA, PART_MOI_MOA) %>%
  arrange(Año)

# CALCULO DE PARTICIPACIONES POR PAÍS POR AÑO SOBRE MOA + MOI ####

# Filtrar por MOI y MOA, y calcular participaciones por país y año
participaciones <- rubros_pulido %>%
  filter(Rubro %in% c("MOI", "MOA")) %>%
  group_by(Año, DESCRIP_PAIS) %>%
  summarise(DOLARES_FOB_PAIS = sum(DOLARES_FOB, na.rm = TRUE),
            .groups = 'drop') %>%
  group_by(Año) %>%
  mutate(DOLARES_FOB_TOTAL = sum(DOLARES_FOB_PAIS),
         Participacion = DOLARES_FOB_PAIS / DOLARES_FOB_TOTAL * 100) %>%
  ungroup() %>%
  select(Año, DESCRIP_PAIS, DOLARES_FOB_PAIS, Participacion) %>%
  arrange(Año, desc(Participacion))




# CONTROL: suma de moa y moi es igual a la suma de expo por país por año previamente filtrado por moa y moi. 

control <- total_rubros_año %>%
  select(Año, MOI_MOA) %>%
  left_join(
    participaciones %>%
      group_by(Año) %>%
      summarise(TOTAL_PARTICIPACIONES = sum(DOLARES_FOB_PAIS, na.rm = TRUE),
                .groups = 'drop'),
    by = "Año"
  ) %>%
  mutate(
    DIFERENCIA = MOI_MOA - TOTAL_PARTICIPACIONES,
    COINCIDE = abs(DIFERENCIA) < 0.01  # Tolerancia para errores de redondeo
  )

all(control$COINCIDE)

print(select(control, Año, COINCIDE), n = 200)

# # Ver solo los años que NO coinciden (si los hay)
# control %>% filter(!COINCIDE)

table(participaciones$DESCRIP_PAIS)

## IMPORTANDO PAÍSES ####

pais_orden <- readxl::read_excel("Datos.xlsx",
                             sheet = "pais_orden",
                             col_names = T)

# ORDENANDO Y SELECIONANDO PAISES ####

## PASO 1

# VIENDO QUÉ PAÍSES ESTÁN DESDE LOS DATOS BRUTOS EN COMPARACIÓN CON LOS ORIGINALMENTE INCORPORADOS

# Crear tabla de comparación directamente
comparacion <- pais_orden %>%
  rename(pais_orden = orden) %>%
  mutate(
    # Ver si el país de pais_orden está en participaciones
    pais_participaciones = if_else(
      pais_orden %in% unique(participaciones$DESCRIP_PAIS),
      pais_orden,
      NA_character_
    )
  )

# Ver los que NO coinciden (están en pais_orden pero no en participaciones)
no_coinciden <- comparacion %>%
  filter(is.na(pais_participaciones))

print(no_coinciden, n = Inf)

# Ver los países de participaciones que NO están en pais_orden
paises_faltantes <- unique(participaciones$DESCRIP_PAIS)[
  !unique(participaciones$DESCRIP_PAIS) %in% pais_orden$orden
]

print(paises_faltantes)

# 2 PASO 

# Crear mapeo manual de nombres
mapeo_paises <- tibble::tribble(
  ~pais_participaciones, ~pais_orden,
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
  "Corea del Sur", "Korea, Republic of",
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
  "Guinea Ecuatorial", "Guiena ecuatorial",
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
  "Islas Marshall", NA_character_,  # No está en pais_orden
  "Israel", "Israel",
  "Italia", "Italy",
  "Jamaica", "Jamaica",
  "Japón", "Japan",
  "Jordania", "Jordan",
  "Kazajstán", "Kazakhstan, Republic of",
  "Kenya", "Kenya",
  "Kuwait", "Kuwait",
  "Laos", NA_character_,  # No está en pais_orden
  "Letonia", "Latvia, Republic of",
  "Líbano", "Lebanon",
  "Liberia", "Liberia",
  "Libia", "Libya",
  "Lituania", "Lithuania, Republic of",
  "Luxemburgo", "Luxembourg",
  "Macao", "Macao Special Administrative Region, People's Republic of China",
  "Macedonia", "North Macedonia, Republic of",
  "Madagascar", "Madagascar",
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
  "Palestina", NA_character_,  # No está en pais_orden
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
  "República Democrática del Congo", "Congo, Democratic Republic of the",
  "Rumania", "Romania",
  "Rusia", "Russian Federation",
  "Rwanda", "Rwanda",
  "Samoa", NA_character_,  # No está en pais_orden
  "San Cristóbal y Nevis", "St. Kitts and Nevis",
  "San Vicente Y Las Granadinas", "St. Vincent and the Grenadines",
  "Santa Lucía", "St. Lucia",
  "Santo Tomé y Príncipe", "São Tomé and Príncipe, Democratic Republic of",
  "Senegal", "Senegal",
  "Serbia", "Serbia, Republic of",
  "Serbia y Montenegro", NA_character_,  # Ya no existe como país
  "Seychelles", "Seychelles",
  "Sierra Leona", "Sierra Leone",
  "Singapur", "Singapore",
  "Siria", "Syrian Arab Republic",
  "Sri Lanka", "Sri Lanka",
  "Sudáfrica", "South Africa",
  "Sudán", "Sudan",
  "Suecia", "Sweden",
  "Suiza", "Switzerland",
  "Suriname", "Suriname",
  "Swazilandia", NA_character_,  # No está en pais_orden
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
  "Zimbabwe", "Zimbabwe",
  # Países/territorios especiales que no están en pais_orden
  "Territorios Franceses", NA_character_,
  "Territorios Holandeses", NA_character_,
  "Territorios Británicos", NA_character_,
  "Territorios Vinculados a España", NA_character_,
  "Territorios vinculados a Estados Unidos", NA_character_,
  "Territorios Autónomos Palestinos (Gaza Y Jericó)", NA_character_,
  "Exportaciones e Importaciones Simplificadas", NA_character_,
  "Indeterminado (Continente)", NA_character_,
  "Indeterminado (Europa)", NA_character_,
  "Indeterminado (América)", NA_character_,
  "Indeterminado ( Asia )", NA_character_,
  "Indeterminado (África)", NA_character_,
  "Indeterminado (Oceanía)", NA_character_
)

# Aplicar el mapeo a la tabla comparacion
comparacion_final <- pais_orden %>%
  rename(pais_orden = orden) %>%
  left_join(mapeo_paises, by = "pais_orden") %>%
  filter(!is.na(pais_participaciones)) %>%
  select(pais_orden, pais_participaciones)

# Ver resultado
comparacion_final

# Verificar cuántos países quedaron
nrow(comparacion_final)

# TABLA FINAL ####

tabla_participaciones <- participaciones %>%
  # Unir con comparacion_final para mapear nombres
  left_join(comparacion_final, by = c("DESCRIP_PAIS" = "pais_participaciones")) %>%
  # Filtrar solo los que tienen match
  filter(!is.na(pais_orden)) %>%
  select(Año, pais_orden, Participacion) %>%
  pivot_wider(
    names_from = pais_orden,
    values_from = Participacion,
    values_fill = 0
  ) %>%
  # Reordenar las columnas según el orden de comparacion_final
  select(Año, all_of(comparacion_final$pais_orden))

# Ver resultado
tabla_participaciones

# Verificar
dim(tabla_participaciones)
colnames(tabla_participaciones)[1:10]

rowSums(tabla_participaciones[,-1])

# VERIFICACION ####

# Países que están en participaciones pero NO en comparacion_final
paises_excluidos <- unique(participaciones$DESCRIP_PAIS)[
  !unique(participaciones$DESCRIP_PAIS) %in% comparacion_final$pais_participaciones
]

print(paises_excluidos)

# Ver cuánta participación representan los países excluidos por año
participacion_excluidos <- participaciones %>%
  filter(DESCRIP_PAIS %in% paises_excluidos) %>%
  group_by(Año) %>%
  summarise(Participacion_Excluidos = sum(Participacion)) %>%
  arrange(Año)

print(participacion_excluidos, n = 23)

# Verificar que suma 100 con los incluidos
control_100 <- data.frame(
  Año = tabla_participaciones$Año,
  Incluidos = rowSums(tabla_participaciones[,-1]),
  Excluidos = participacion_excluidos$Participacion_Excluidos,
  Total = rowSums(tabla_participaciones[,-1]) + participacion_excluidos$Participacion_Excluidos
)

control_100

# MENSUALIZANDO PARTICIPACIONES ####

# Crear versión mensual (multiplicar participaciones por 12)
tabla_participaciones_mensual <- tabla_participaciones %>%
  slice(rep(1:n(), each = 12)) %>%
  group_by(Año) %>%
  mutate(Mes = 1:12) %>%
  ungroup() %>%
  select(Año, Mes, everything())

# EXPORTANDO EXCEL ####

# # Exportar ambas hojas
# library(openxlsx)
# 
# # Crear libro de trabajo
# wb <- createWorkbook()
# 
# # Agregar hoja 1: Anual
# addWorksheet(wb, "Participacion_Anual")
# writeData(wb, "Participacion_Anual", tabla_participaciones)
# 
# # Agregar hoja 2: Mensual
# addWorksheet(wb, "Participacion_Mensual")
# writeData(wb, "Participacion_Mensual", tabla_participaciones_mensual)
# 
# # Guardar archivo
# saveWorkbook(wb, "Tabla participacion.xlsx", overwrite = TRUE)

## CPI ####


cpi <- read.csv("CPI all.csv")

cpi_long <- cpi %>%
  dplyr::filter(INDEX_TYPE == "Consumer price index (CPI)") %>%
  
  dplyr::select(
    country = COUNTRY,
    dplyr::starts_with("X")
  ) %>%
  
  tidyr::pivot_longer(
    cols = -country,
    names_to  = "periodo",
    values_to = "cpi"
  ) %>%
  
  dplyr::mutate(
    periodo = gsub("^X", "", periodo),          # 2001.M01
    year  = substr(periodo, 1, 4),
    month = substr(periodo, 7, 8),
    fecha = as.Date(paste(year, month, "01", sep = "-"))
  ) %>%
  
  dplyr::select(country, fecha, cpi) %>%
  dplyr::arrange(country, fecha)

## ER ####

er <- read.csv("ER all.csv")

er_long <- er %>%
  # quedarnos solo con el tipo de indicador relevante
  dplyr::filter(INDICATOR == "Domestic currency per US Dollar") %>%
  
  # columnas útiles
  dplyr::select(
    country = COUNTRY,
    dplyr::starts_with("X")
  ) %>%
  
  # pasar a formato largo
  tidyr::pivot_longer(
    cols = -country,
    names_to  = "periodo",
    values_to = "er"
  ) %>%
  
  # construir fecha
  dplyr::mutate(
    periodo = gsub("^X", "", periodo),   # 2001.M01
    year  = substr(periodo, 1, 4),
    month = substr(periodo, 7, 8),
    fecha = as.Date(paste(year, month, "01", sep = "-"))
  ) %>%
  
  dplyr::select(country, fecha, er) %>%
  dplyr::arrange(country, fecha)

## CONTROLES ####

paises_cpi <- sort(unique(cpi_long$country))
paises_er  <- sort(unique(er_long$country))

length(paises_cpi)
length(paises_er)

identical(paises_cpi, paises_er)

solo_en_cpi <- setdiff(paises_cpi, paises_er)
solo_en_er  <- setdiff(paises_er, paises_cpi)

solo_en_cpi
solo_en_er

tibble::tibble(
  pais = sort(union(paises_cpi, paises_er)),
  en_cpi = pais %in% paises_cpi,
  en_er  = pais %in% paises_er
)

agrep(paises_cpi, paises_er, value = TRUE, max.distance = 0.1)

unique(cpi_long$country)
unique(er_long$country)

# JUNTANDO CPI Y ER ####

# países comunes (match exacto)
paises_comunes <- intersect(
  unique(cpi_long$country),
  unique(er_long$country)
)

# quedarnos solo con esos países
cpi_f <- cpi_long %>%
  dplyr::filter(country %in% paises_comunes)

er_f <- er_long %>%
  dplyr::filter(country %in% paises_comunes)

# merge final
cpi_er <- cpi_f %>%
  dplyr::inner_join(
    er_f,
    by = c("country", "fecha")
  ) %>%
  dplyr::rename(
    pais = country,
    mes  = fecha
  ) %>%
  dplyr::select(pais, mes, er, cpi) %>%
  dplyr::arrange(pais, mes)

unique(cpi_er$pais)

# CHEQUEANDO CON LOS PAISES QUE TENEMOS EN EL EXCEL ####

# vector con la lista que pasaste
lista_objetivo <- c(
  "Afghanistan, Islamic Republic of","Albania","Algeria","Andorra, Principality of",
  "Angola","Antigua and Barbuda","Armenia, Republic of",
  "Aruba, Kingdom of the Netherlands","Australia","Austria",
  "Azerbaijan, Republic of","Bahamas, The","Bahrain, Kingdom of","Bangladesh",
  "Barbados","Belarus, Republic of","Belgium","Belize","Benin","Bolivia",
  "Bosnia and Herzegovina","Botswana","Brazil","Brunei Darussalam","Bulgaria",
  "Burkina Faso","Burundi","Cabo Verde","Cambodia","Cameroon","Canada",
  "Central African Republic","Chad","Chile","China, People's Republic of",
  "Colombia","Comoros, Union of the","Congo, Democratic Republic of the",
  "Congo, Republic of","Costa Rica","Côte d'Ivoire","Croatia, Republic of",
  "Curaçao, Kingdom of the Netherlands","Cyprus","Czech Republic","Denmark",
  "Djibouti","Dominica","Dominican Republic","Ecuador","Egypt, Arab Republic of",
  "El Salvador","Eritrea, The State of","Estonia, Republic of",
  "Ethiopia, The Federal Democratic Republic of","Fiji, Republic of","Finland",
  "France","Gabon","Gambia, The","Georgia","Germany","Ghana","Greece","Grenada",
  "Guatemala","Guinea","Guiena ecuatorial","Guinea-Bissau","Guyana","Haiti",
  "Honduras","Hong Kong Special Administrative Region, People's Republic of China",
  "Hungary","Iceland","India","Indonesia","Iran, Islamic Republic of","Iraq",
  "Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan, Republic of",
  "Kenya","Korea, Republic of","Kuwait","Latvia, Republic of","Lebanon",
  "Lesotho, Kingdom of","Liberia","Libya","Lithuania, Republic of","Luxembourg",
  "Macao Special Administrative Region, People's Republic of China","Madagascar",
  "Malawi","Malaysia","Maldives","Mali","Malta",
  "Mauritania, Islamic Republic of","Mauritius","Mexico","Moldova, Republic of",
  "Mongolia","Montenegro","Morocco","Mozambique, Republic of","Myanmar","Namibia",
  "Nepal","Netherlands Antilles","Netherlands, The","New Zealand","Nicaragua",
  "Niger","Nigeria","North Macedonia, Republic of","Norway","Oman","Pakistan",
  "Panama","Papua New Guinea","Paraguay","Peru","Philippines",
  "Poland, Republic of","Portugal","Puerto Rico","Qatar","Romania",
  "Russian Federation","Rwanda",
  "São Tomé and Príncipe, Democratic Republic of","Saudi Arabia","Senegal",
  "Serbia, Republic of","Seychelles","Sierra Leone","Singapore","Slovak Republic",
  "Slovenia, Republic of","Solomon Islands","South Africa",
  "South Sudan, Republic of","Spain","Sri Lanka","St. Kitts and Nevis","St. Lucia",
  "St. Vincent and the Grenadines","Sudan","Suriname","Sweden","Switzerland",
  "Syrian Arab Republic","Taiwan Province of China","Tanzania, United Republic of",
  "Thailand","Togo","Trinidad and Tobago","Tunisia","Türkiye, Republic of",
  "Uganda","Ukraine","United Arab Emirates","United Kingdom","United States",
  "Uruguay","Uzbekistan, Republic of","Venezuela, República Bolivariana de",
  "Vietnam","Yemen, Republic of","Zambia","Zimbabwe"
)

# países presentes en cada base
paises_cpi <- unique(cpi_long$country)
paises_er  <- unique(er_long$country)

# chequeo
control_paises <- data.frame(
  pais = lista_objetivo,
  en_cpi = lista_objetivo %in% paises_cpi,
  en_er  = lista_objetivo %in% paises_er
)

control_paises

