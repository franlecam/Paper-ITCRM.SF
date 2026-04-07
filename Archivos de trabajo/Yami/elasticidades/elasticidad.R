library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(writexl)

itcrm_por_provincias <- read_excel("itcrm_por_provincias ALL RUBROS.xlsx")
# CAMBIAR NOMBRE DE ARCHIVO SEGUN LO QUE SE DESEE TRABAJAR, TAL QUE:
# itcrm_por_provincias ALL RUBROS.xlsx
# itcrm_por_provincias MOA.xlsx
# itcrm_por_provincias MOI.xlsx
# itcrm_por_provincias MOA MOI.xlsx


exportaciones_provincias <- read_excel("exportaciones_por_rubro.xlsx", sheet = "ALL RUBROS")
#CAMBIAR NOMBRE DE SHEET SEGUN LO QUE SE DESEE TRABAJAR, TAL QUE: ALL RUBROS, MOA, MOI, MOA MOI


# 1. PASAR ITCRM A FORMATO LARGO
itcrm_long <- itcrm_por_provincias %>%
  pivot_longer(
    cols = -mes,
    names_to = "provincia",
    values_to = "itcrm"
  )

# 2. PASAR EXPORTACIONES A FORMATO LARGO
expo_long <- exportaciones_provincias %>%
  mutate(fecha = as.Date(paste0(fecha, "-01"))) %>%
  pivot_longer(
    cols = -fecha,
    names_to = "provincia",
    values_to = "exportaciones"
  )

# 3. UNIR BASES
base_reg <- itcrm_long %>%
  rename(fecha = mes) %>%
  inner_join(expo_long, by = c("fecha", "provincia"))

# 4. LIMPIAR (opcional pero recomendado)
base_reg <- base_reg %>%
  filter(!is.na(exportaciones), !is.na(itcrm))

# 5. CORRER 24 REGRESIONES (una por provincia)
resultados <- base_reg %>%
  group_by(provincia) %>%
  nest() %>%
  mutate(
    modelo = map(data, ~ lm(log(exportaciones) ~ log(itcrm), data = .x)),
    tidy = map(modelo, tidy),
    glance = map(modelo, glance)
  )

# 6. VER COEFICIENTES
coeficientes <- resultados %>%
  unnest(tidy)

# 7. VER R2 y métricas
metricas <- resultados %>%
  unnest(glance)


# 1. COEFICIENTES (filtrar solo lo importante)
coeficientes_final <- coeficientes %>%
  select(provincia, term, estimate, std.error, statistic, p.value)

# 2. MÉTRICAS DEL MODELO
metricas_final <- metricas %>%
  select(provincia, r.squared, adj.r.squared, nobs)

# 3. (OPCIONAL) QUEDARTE SOLO CON EL COEFICIENTE DEL ITCRM
elasticidades <- coeficientes_final %>%
  filter(term == "log(itcrm)") %>%
  select(provincia, estimate, p.value) %>%
  rename(
    elasticidad = estimate,
    p_valor = p.value
  ) %>%
  left_join(metricas_final, by = "provincia")

# 4. EXPORTAR TODO EN UN SOLO EXCEL (múltiples hojas)
write_xlsx(
  list(
    "Elasticidades" = elasticidades,
    "Coeficientes" = coeficientes_final,
    "Metricas" = metricas_final
  ),
  "resultados_regresiones_itcrm.xlsx"
)

