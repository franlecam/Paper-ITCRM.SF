#LIBRERIAS####
library(readxl)
library(dynlm)
library(ggplot2)
library(broom)
library(dplyr)
library(tidyr)
library(openxlsx)
#DATA ####
datos <- read_excel("data.xlsx",
                    sheet = "Sheet1",
                    range = "A3:W279")

attach(datos)
#DESCRIPTIVAS ####
## COEFICIENTES DE CORRELACION ####
### X_ARG VS ITCRM_ARG ####
cor(X_ARG    , ITCRM_ARG)
cor(X_PP_ARG , ITCRM_ARG)
cor(X_MOA_ARG, ITCRM_ARG)
cor(X_MOI_ARG, ITCRM_ARG)
cor(X_CYE_ARG, ITCRM_ARG)

### X_SFE VS ITCRM_ARG ####
cor(X_SFE     , ITCRM_ARG)
cor(X_PP_SFE  , ITCRM_ARG)
cor(X_MOA_SFE , ITCRM_ARG)
cor(X_MOI_SFE , ITCRM_ARG)
cor(X_CYE_SFE , ITCRM_ARG)

### X_SFE VS ITCRM_SFE ####
cor(X_SFE     , ITCRM_SFE)
cor(X_PP_SFE  , ITCRM_SFE)
cor(X_MOA_SFE , ITCRM_SFE)
cor(X_MOI_SFE , ITCRM_SFE)
cor(X_CYE_SFE , ITCRM_SFE)















## GRAFICOS DE DISPERSION ####
scatter_plot <- function(data, x_var, y_var, x_lab, y_lab) {
  
  ggplot(data, aes(x = {{ x_var }}, y = {{ y_var }})) +
    geom_point(
      color = "grey30",
      size = 2,
      alpha = 0.7
    ) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      color = "black",
      linewidth = 0.8
    ) +
    labs(
      x = x_lab,
      y = y_lab
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank()
    )
}

### X_SFE vs ITCRM_SFE ####
scatter_plot(
  data  = datos,
  x_var = X_SFE,      
  y_var = ITCRM_SFE,
  x_lab = "Exportaciones de Santa Fe",
  y_lab = "ITCRM Santa Fe"
)

scatter_plot(
  data  = datos,
  x_var = X_MOA_SFE,      
  y_var = ITCRM_SFE,
  x_lab = "Exportaciones MOA de Santa Fe",
  y_lab = "ITCRM Santa Fe"
)

scatter_plot(
  data  = datos,
  x_var = X_MOI_SFE,      
  y_var = ITCRM_SFE,
  x_lab = "Exportaciones MOI de Santa Fe",
  y_lab = "ITCRM Santa Fe"
)

scatter_plot(
  data  = datos,
  x_var = X_PP_SFE,      
  y_var = ITCRM_SFE,
  x_lab = "Exportaciones PP de Santa Fe",
  y_lab = "ITCRM Santa Fe"
)
# MODELOS SIMPLES ####
## X_ARG VS ITCRM_ARG ####
reg_X_ARG_ITCRM_ARG     <- lm(log(X_ARG)     ~ log(ITCRM_ARG))
reg_X_PP_ARG_ITCRM_ARG  <- lm(log(X_PP_ARG)  ~ log(ITCRM_ARG))
reg_X_MOA_ARG_ITCRM_ARG <- lm(log(X_MOA_ARG) ~ log(ITCRM_ARG))
reg_X_MOI_ARG_ITCRM_ARG <- lm(log(X_MOI_ARG) ~ log(ITCRM_ARG))
reg_X_CYE_ARG_ITCRM_ARG <- lm(log(X_CYE_ARG) ~ log(ITCRM_ARG))

summary(reg_X_ARG_ITCRM_ARG)
summary(reg_X_PP_ARG_ITCRM_ARG)
summary(reg_X_MOA_ARG_ITCRM_ARG)
summary(reg_X_MOI_ARG_ITCRM_ARG)
summary(reg_X_CYE_ARG_ITCRM_ARG)

## X_SFE VS ITCRM_SFE ####
reg_X_SFE_ITCRM_ARG     <- lm(log(X_SFE)     ~ log(ITCRM_ARG))
reg_X_PP_SFE_ITCRM_ARG  <- lm(log(X_PP_SFE)  ~ log(ITCRM_ARG))
reg_X_MOA_SFE_ITCRM_ARG <- lm(log(X_MOA_SFE) ~ log(ITCRM_ARG))
reg_X_MOI_SFE_ITCRM_ARG <- lm(log(X_MOI_SFE) ~ log(ITCRM_ARG))
reg_X_CYE_SFE_ITCRM_ARG <- lm(log(X_CYE_SFE) ~ log(ITCRM_ARG))

summary(reg_X_SFE_ITCRM_ARG)
summary(reg_X_PP_SFE_ITCRM_ARG)
summary(reg_X_MOA_SFE_ITCRM_ARG)
summary(reg_X_MOI_SFE_ITCRM_ARG)
summary(reg_X_CYE_SFE_ITCRM_ARG)

## X_SFE VS ITCRM_SFE ####
reg_X_SFE_ITCRM_SFE     <- lm(log(X_SFE)     ~ log(ITCRM_SFE))
reg_X_PP_SFE_ITCRM_SFE  <- lm(log(X_PP_SFE)  ~ log(ITCRM_SFE))
reg_X_MOA_SFE_ITCRM_SFE <- lm(log(X_MOA_SFE) ~ log(ITCRM_SFE))
reg_X_MOI_SFE_ITCRM_SFE <- lm(log(X_MOI_SFE) ~ log(ITCRM_SFE))
reg_X_CYE_SFE_ITCRM_SFE <- lm(log(X_CYE_SFE) ~ log(ITCRM_SFE))

summary(reg_X_SFE_ITCRM_SFE)
summary(reg_X_PP_SFE_ITCRM_SFE)
summary(reg_X_MOA_SFE_ITCRM_SFE)
summary(reg_X_MOI_SFE_ITCRM_SFE)
summary(reg_X_CYE_SFE_ITCRM_SFE)

# MODELO MULTIPLES ####
## X_ARG VS ITCRM_ARG + GEBC####
reg_X_ARG_ITCRM_ARG_GEBC     <- lm(log(X_ARG)     ~ log(ITCRM_ARG) + log(GEBC))
reg_X_PP_ARG_ITCRM_ARG_GEBC  <- lm(log(X_PP_ARG)  ~ log(ITCRM_ARG) + log(GEBC))
reg_X_MOA_ARG_ITCRM_ARG_GEBC <- lm(log(X_MOA_ARG) ~ log(ITCRM_ARG) + log(GEBC))
reg_X_MOI_ARG_ITCRM_ARG_GEBC <- lm(log(X_MOI_ARG) ~ log(ITCRM_ARG) + log(GEBC))
reg_X_CYE_ARG_ITCRM_ARG_GEBC <- lm(log(X_CYE_ARG) ~ log(ITCRM_ARG) + log(GEBC))

summary(reg_X_ARG_ITCRM_ARG_GEBC)
summary(reg_X_PP_ARG_ITCRM_ARG_GEBC)
summary(reg_X_MOA_ARG_ITCRM_ARG_GEBC)
summary(reg_X_MOI_ARG_ITCRM_ARG_GEBC)
summary(reg_X_CYE_ARG_ITCRM_ARG_GEBC)

## X_SFE VS ITCRM_ARG + GEBC####
reg_X_SFE_ITCRM_ARG_GEBC     <- lm(log(X_SFE)     ~ log(ITCRM_ARG) + log(GEBC))
reg_X_PP_SFE_ITCRM_ARG_GEBC  <- lm(log(X_PP_SFE)  ~ log(ITCRM_ARG) + log(GEBC))
reg_X_MOA_SFE_ITCRM_ARG_GEBC <- lm(log(X_MOA_SFE) ~ log(ITCRM_ARG) + log(GEBC))
reg_X_MOI_SFE_ITCRM_ARG_GEBC <- lm(log(X_MOI_SFE) ~ log(ITCRM_ARG) + log(GEBC))
reg_X_CYE_SFE_ITCRM_ARG_GEBC <- lm(log(X_CYE_SFE) ~ log(ITCRM_ARG) + log(GEBC))

summary(reg_X_SFE_ITCRM_ARG_GEBC)
summary(reg_X_PP_SFE_ITCRM_ARG_GEBC)
summary(reg_X_MOA_SFE_ITCRM_ARG_GEBC)
summary(reg_X_MOI_SFE_ITCRM_ARG_GEBC)
summary(reg_X_CYE_SFE_ITCRM_ARG_GEBC)

## X_SFE VS ITCRM_SFE + GEBC####
reg_X_SFE_ITCRM_SFE_GEBC     <- lm(log(X_SFE)     ~ log(ITCRM_SFE) + log(GEBC))
reg_X_PP_SFE_ITCRM_SFE_GEBC  <- lm(log(X_PP_SFE)  ~ log(ITCRM_SFE) + log(GEBC))
reg_X_MOA_SFE_ITCRM_SFE_GEBC <- lm(log(X_MOA_SFE) ~ log(ITCRM_SFE) + log(GEBC))
reg_X_MOI_SFE_ITCRM_SFE_GEBC <- lm(log(X_MOI_SFE) ~ log(ITCRM_SFE) + log(GEBC))
reg_X_CYE_SFE_ITCRM_SFE_GEBC <- lm(log(X_CYE_SFE) ~ log(ITCRM_SFE) + log(GEBC))

summary(reg_X_SFE_ITCRM_SFE_GEBC)
summary(reg_X_PP_SFE_ITCRM_SFE_GEBC)
summary(reg_X_MOA_SFE_ITCRM_SFE_GEBC)
summary(reg_X_MOI_SFE_ITCRM_SFE_GEBC)
summary(reg_X_CYE_SFE_ITCRM_SFE_GEBC)

## X_ARG VS ITCRM_ARG + API ####
reg_X_ARG_ITCRM_ARG_API     <- lm(log(X_ARG)     ~ log(ITCRM_ARG) + log(API))
reg_X_PP_ARG_ITCRM_ARG_API  <- lm(log(X_PP_ARG)  ~ log(ITCRM_ARG) + log(API))
reg_X_MOA_ARG_ITCRM_ARG_API <- lm(log(X_MOA_ARG) ~ log(ITCRM_ARG) + log(API))
reg_X_MOI_ARG_ITCRM_ARG_API <- lm(log(X_MOI_ARG) ~ log(ITCRM_ARG) + log(API))
reg_X_CYE_ARG_ITCRM_ARG_API <- lm(log(X_CYE_ARG) ~ log(ITCRM_ARG) + log(API))

summary(reg_X_ARG_ITCRM_ARG_API)
summary(reg_X_PP_ARG_ITCRM_ARG_API)
summary(reg_X_MOA_ARG_ITCRM_ARG_API)
summary(reg_X_MOI_ARG_ITCRM_ARG_API)
summary(reg_X_CYE_ARG_ITCRM_ARG_API)

## X_SFE VS ITCRM_ARG + API ####
reg_X_SFE_ITCRM_ARG_API     <- lm(log(X_SFE)     ~ log(ITCRM_ARG) + log(API))
reg_X_PP_SFE_ITCRM_ARG_API  <- lm(log(X_PP_SFE)  ~ log(ITCRM_ARG) + log(API))
reg_X_MOA_SFE_ITCRM_ARG_API <- lm(log(X_MOA_SFE) ~ log(ITCRM_ARG) + log(API))
reg_X_MOI_SFE_ITCRM_ARG_API <- lm(log(X_MOI_SFE) ~ log(ITCRM_ARG) + log(API))
reg_X_CYE_SFE_ITCRM_ARG_API <- lm(log(X_CYE_SFE) ~ log(ITCRM_ARG) + log(API))

summary(reg_X_SFE_ITCRM_ARG_API)
summary(reg_X_PP_SFE_ITCRM_ARG_API)
summary(reg_X_MOA_SFE_ITCRM_ARG_API)
summary(reg_X_MOI_SFE_ITCRM_ARG_API)
summary(reg_X_CYE_SFE_ITCRM_ARG_API)

## X_SFE VS ITCRM_SFE + API ####
reg_X_SFE_ITCRM_SFE_API     <- lm(log(X_SFE)     ~ log(ITCRM_SFE) + log(API))
reg_X_PP_SFE_ITCRM_SFE_API  <- lm(log(X_PP_SFE)  ~ log(ITCRM_SFE) + log(API))
reg_X_MOA_SFE_ITCRM_SFE_API <- lm(log(X_MOA_SFE) ~ log(ITCRM_SFE) + log(API))
reg_X_MOI_SFE_ITCRM_SFE_API <- lm(log(X_MOI_SFE) ~ log(ITCRM_SFE) + log(API))
reg_X_CYE_SFE_ITCRM_SFE_API <- lm(log(X_CYE_SFE) ~ log(ITCRM_SFE) + log(API))

summary(reg_X_SFE_ITCRM_SFE_API)
summary(reg_X_PP_SFE_ITCRM_SFE_API)
summary(reg_X_MOA_SFE_ITCRM_SFE_API)
summary(reg_X_MOI_SFE_ITCRM_SFE_API)
summary(reg_X_CYE_SFE_ITCRM_SFE_API)

## X_ARG VS ITCRM_ARG + GEBC + API ####
reg_X_ARG_ITCRM_ARG_G_E     <- lm(log(X_ARG)     ~ log(ITCRM_ARG) + log(GEBC) + log(API))
reg_X_PP_ARG_ITCRM_ARG_G_E  <- lm(log(X_PP_ARG)  ~ log(ITCRM_ARG) + log(GEBC) + log(API))
reg_X_MOA_ARG_ITCRM_ARG_G_E <- lm(log(X_MOA_ARG) ~ log(ITCRM_ARG) + log(GEBC) + log(API))
reg_X_MOI_ARG_ITCRM_ARG_G_E <- lm(log(X_MOI_ARG) ~ log(ITCRM_ARG) + log(GEBC) + log(API))
reg_X_CYE_ARG_ITCRM_ARG_G_E <- lm(log(X_CYE_ARG) ~ log(ITCRM_ARG) + log(GEBC) + log(API))

summary(reg_X_ARG_ITCRM_ARG_G_E)
summary(reg_X_PP_ARG_ITCRM_ARG_G_E)
summary(reg_X_MOA_ARG_ITCRM_ARG_G_E)
summary(reg_X_MOI_ARG_ITCRM_ARG_G_E)
summary(reg_X_CYE_ARG_ITCRM_ARG_G_E)

## X_SFE VS ITCRM_ARG + GEBC + API ####
reg_X_SFE_ITCRM_ARG_G_E     <- lm(log(X_SFE)     ~ log(ITCRM_ARG) + log(GEBC) + log(API))
reg_X_PP_SFE_ITCRM_ARG_G_E  <- lm(log(X_PP_SFE)  ~ log(ITCRM_ARG) + log(GEBC) + log(API))
reg_X_MOA_SFE_ITCRM_ARG_G_E <- lm(log(X_MOA_SFE) ~ log(ITCRM_ARG) + log(GEBC) + log(API))
reg_X_MOI_SFE_ITCRM_ARG_G_E <- lm(log(X_MOI_SFE) ~ log(ITCRM_ARG) + log(GEBC) + log(API))
reg_X_CYE_SFE_ITCRM_ARG_G_E <- lm(log(X_CYE_SFE) ~ log(ITCRM_ARG) + log(GEBC) + log(API))

summary(reg_X_SFE_ITCRM_ARG_G_E)
summary(reg_X_PP_SFE_ITCRM_ARG_G_E)
summary(reg_X_MOA_SFE_ITCRM_ARG_G_E)
summary(reg_X_MOI_SFE_ITCRM_ARG_G_E)
summary(reg_X_CYE_SFE_ITCRM_ARG_G_E)

## X_SFE VS ITCRM_SFE + GEBC + API ####
reg_X_SFE_ITCRM_SFE_G_E     <- lm(log(X_SFE)     ~ log(ITCRM_SFE) + log(GEBC) + log(API))
reg_X_PP_SFE_ITCRM_SFE_G_E  <- lm(log(X_PP_SFE)  ~ log(ITCRM_SFE) + log(GEBC) + log(API))
reg_X_MOA_SFE_ITCRM_SFE_G_E <- lm(log(X_MOA_SFE) ~ log(ITCRM_SFE) + log(GEBC) + log(API))
reg_X_MOI_SFE_ITCRM_SFE_G_E <- lm(log(X_MOI_SFE) ~ log(ITCRM_SFE) + log(GEBC) + log(API))
reg_X_CYE_SFE_ITCRM_SFE_G_E <- lm(log(X_CYE_SFE) ~ log(ITCRM_SFE) + log(GEBC) + log(API))

summary(reg_X_SFE_ITCRM_SFE_G_E)
summary(reg_X_PP_SFE_ITCRM_SFE_G_E)
summary(reg_X_MOA_SFE_ITCRM_SFE_G_E)
summary(reg_X_MOI_SFE_ITCRM_SFE_G_E)
summary(reg_X_CYE_SFE_ITCRM_SFE_G_E)

# TABLA DE MODELOS ####
## MODELOS PARA TOTAL DE EXPORTACIONES ####
modelos_X <- list(
  # === MODELOS SIMPLES X ===
  "ARG | ITCRM ARG"       = reg_X_ARG_ITCRM_ARG,
  "SFE | ITCRM ARG"       = reg_X_SFE_ITCRM_ARG,
  "SFE | ITCRM SFE"       = reg_X_SFE_ITCRM_SFE,
  
  # === MODELOS MULTIPLES X + GEBC ===
  "ARG | ITCRM ARG + GEBC"    = reg_X_ARG_ITCRM_ARG_GEBC,
  "SFE | ITCRM ARG + GEBC"    = reg_X_SFE_ITCRM_ARG_GEBC,
  "SFE | ITCRM SFE + GEBC"    = reg_X_SFE_ITCRM_SFE_GEBC,
  
  # === MODELOS MULTIPLES X + API ===
  "ARG | ITCRM ARG + API"    = reg_X_ARG_ITCRM_ARG_API,
  "SFE | ITCRM ARG + API"    = reg_X_SFE_ITCRM_ARG_API,
  "SFE | ITCRM SFE + API"    = reg_X_SFE_ITCRM_SFE_API,
  
  # === MODELOS MULTIPLES X + GEBC + API ===
  "ARG | ITCRM ARG + GEBC + API"    = reg_X_ARG_ITCRM_ARG_G_E,
  "SFE | ITCRM ARG + GEBC + API"    = reg_X_SFE_ITCRM_ARG_G_E,
  "SFE | ITCRM SFE + GEBC + API"    = reg_X_SFE_ITCRM_SFE_G_E
  )

coef_tabla <- bind_rows(
  lapply(names(modelos_X), function(nm) {
    tidy(modelos_X[[nm]]) %>%
      mutate(
        modelo = nm,
        stars = case_when(
          p.value < 0.01 ~ "***",
          p.value < 0.05 ~ "**",
          p.value < 0.10 ~ "*",
          TRUE ~ ""
        ),
        coef = sprintf("%.3f%s", estimate, stars)
      ) %>%
      select(modelo, term, coef)
  })
)

tabla_reg <- coef_tabla %>%
  pivot_wider(
    names_from  = modelo,
    values_from = coef
  )

r2_adj <- tibble(
  term = "R² ajustado",
  !!!setNames(
    lapply(modelos_X, function(m)
      as.character(round(summary(m)$adj.r.squared, 3))
    ),
    names(modelos_X)
  )
)


tabla_final <- bind_rows(tabla_reg, r2_adj)

write.xlsx(
  tabla_final,
  file = "regresiones_X_completas.xlsx",
  overwrite = TRUE
)

## MODELOS PARA EXPORTACIONES MOA ####
modelos_X_MOA <- list(
  # === MODELOS SIMPLES X ===
  "ARG | ITCRM ARG"       = reg_X_MOA_ARG_ITCRM_ARG,
  "SFE | ITCRM ARG"       = reg_X_MOA_SFE_ITCRM_ARG,
  "SFE | ITCRM SFE"       = reg_X_MOA_SFE_ITCRM_SFE,
  
  # === MODELOS MULTIPLES X + GEBC ===
  "ARG | ITCRM ARG + GEBC"    = reg_X_MOA_ARG_ITCRM_ARG_GEBC,
  "SFE | ITCRM ARG + GEBC"    = reg_X_MOA_SFE_ITCRM_ARG_GEBC,
  "SFE | ITCRM SFE + GEBC"    = reg_X_MOA_SFE_ITCRM_SFE_GEBC,
  
  # === MODELOS MULTIPLES X + API ===
  "ARG | ITCRM ARG + API"    = reg_X_MOA_ARG_ITCRM_ARG_API,
  "SFE | ITCRM ARG + API"    = reg_X_MOA_SFE_ITCRM_ARG_API,
  "SFE | ITCRM SFE + API"    = reg_X_MOA_SFE_ITCRM_SFE_API,
  
  # === MODELOS MULTIPLES X + GEBC + API ===
  "ARG | ITCRM ARG + GEBC + API"    = reg_X_MOA_ARG_ITCRM_ARG_G_E,
  "SFE | ITCRM ARG + GEBC + API"    = reg_X_MOA_SFE_ITCRM_ARG_G_E,
  "SFE | ITCRM SFE + GEBC + API"    = reg_X_MOA_SFE_ITCRM_SFE_G_E
)

coef_tabla <- bind_rows(
  lapply(names(modelos_X_MOA), function(nm) {
    tidy(modelos_X_MOA[[nm]]) %>%
      mutate(
        modelo = nm,
        stars = case_when(
          p.value < 0.01 ~ "***",
          p.value < 0.05 ~ "**",
          p.value < 0.10 ~ "*",
          TRUE ~ ""
        ),
        coef = sprintf("%.3f%s", estimate, stars)
      ) %>%
      select(modelo, term, coef)
  })
)

tabla_reg <- coef_tabla %>%
  pivot_wider(
    names_from  = modelo,
    values_from = coef
  )

r2_adj <- tibble(
  term = "R² ajustado",
  !!!setNames(
    lapply(modelos_X_MOA, function(m)
      as.character(round(summary(m)$adj.r.squared, 3))
    ),
    names(modelos_X_MOA)
  )
)


tabla_final <- bind_rows(tabla_reg, r2_adj)

write.xlsx(
  tabla_final,
  file = "regresiones_X_MOA_completas.xlsx",
  overwrite = TRUE
)

## MODELOS PARA EXPORTACIONES MOI ####
modelos_X_MOI <- list(
  # === MODELOS SIMPLES X ===
  "ARG | ITCRM ARG"       = reg_X_MOI_ARG_ITCRM_ARG,
  "SFE | ITCRM ARG"       = reg_X_MOI_SFE_ITCRM_ARG,
  "SFE | ITCRM SFE"       = reg_X_MOI_SFE_ITCRM_SFE,
  
  # === MODELOS MULTIPLES X + GEBC ===
  "ARG | ITCRM ARG + GEBC"    = reg_X_MOI_ARG_ITCRM_ARG_GEBC,
  "SFE | ITCRM ARG + GEBC"    = reg_X_MOI_SFE_ITCRM_ARG_GEBC,
  "SFE | ITCRM SFE + GEBC"    = reg_X_MOI_SFE_ITCRM_SFE_GEBC,
  
  # === MODELOS MULTIPLES X + API ===
  "ARG | ITCRM ARG + API"    = reg_X_MOI_ARG_ITCRM_ARG_API,
  "SFE | ITCRM ARG + API"    = reg_X_MOI_SFE_ITCRM_ARG_API,
  "SFE | ITCRM SFE + API"    = reg_X_MOI_SFE_ITCRM_SFE_API,
  
  # === MODELOS MULTIPLES X + GEBC + API ===
  "ARG | ITCRM ARG + GEBC + API"    = reg_X_MOI_ARG_ITCRM_ARG_G_E,
  "SFE | ITCRM ARG + GEBC + API"    = reg_X_MOI_SFE_ITCRM_ARG_G_E,
  "SFE | ITCRM SFE + GEBC + API"    = reg_X_MOI_SFE_ITCRM_SFE_G_E
)

coef_tabla <- bind_rows(
  lapply(names(modelos_X_MOI), function(nm) {
    tidy(modelos_X_MOI[[nm]]) %>%
      mutate(
        modelo = nm,
        stars = case_when(
          p.value < 0.01 ~ "***",
          p.value < 0.05 ~ "**",
          p.value < 0.10 ~ "*",
          TRUE ~ ""
        ),
        coef = sprintf("%.3f%s", estimate, stars)
      ) %>%
      select(modelo, term, coef)
  })
)

tabla_reg <- coef_tabla %>%
  pivot_wider(
    names_from  = modelo,
    values_from = coef
  )

r2_adj <- tibble(
  term = "R² ajustado",
  !!!setNames(
    lapply(modelos_X_MOI, function(m)
      as.character(round(summary(m)$adj.r.squared, 3))
    ),
    names(modelos_X_MOI)
  )
)


tabla_final <- bind_rows(tabla_reg, r2_adj)

write.xlsx(
  tabla_final,
  file = "regresiones_X_MOI_completas.xlsx",
  overwrite = TRUE
)
