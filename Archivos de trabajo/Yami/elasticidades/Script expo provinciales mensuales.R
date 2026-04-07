library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

expo_prov_rubros_mensual <- read_excel("Expo prov rubros mensual.xlsx")

# 1. Crear variable de fecha
base <- expo_prov_rubros_mensual %>%
  mutate(fecha = paste0(Año, "-", Mes))

# 2. Lista de rubros
rubros <- unique(base$Rubro)

# 3. Crear una lista de dataframes (uno por rubro)
listas_rubros <- lapply(rubros, function(r) {
  
  base %>%
    filter(Rubro == r) %>%
    group_by(fecha, `Nombre Prov`) %>%
    summarise(FOB = sum(`FOB_dólar`, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = `Nombre Prov`,
      values_from = FOB,
      values_fill = 0
    ) %>%
    arrange(fecha)
})

# 4. Nombrar la lista (esto define las hojas del Excel)
names(listas_rubros) <- rubros

# 5. Exportar a Excel (cada elemento = una hoja)
write_xlsx(listas_rubros, "exportaciones_por_rubro.xlsx")
