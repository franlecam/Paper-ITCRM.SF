#Librerias####
library(readxl)
library(dynlm)
#Datos####
datos <- read_excel("datos.xlsx",
                    sheet = "Tabelle2",
                    range = "K2:T278")

attach(datos)
#DESCRIPTIVAS ####
## X SIN FILTRAR ####
cor(X, ITCRM_ARG_BCRA)
cor(X, ITCRM_SFE_BCRA)
cor(X, ITCRM_SFE_berre)

plot(X, ITCRM_ARG_BCRA)
plot(X, ITCRM_SFE_BCRA)
plot(X, ITCRM_SFE_berre)
## X FILTRADAS ####
cor(X_FIL, ITCRM_ARG_BCRA)
cor(X_FIL, ITCRM_SFE_BCRA)
cor(X_FIL, ITCRM_SFE_berre)

plot(X_FIL, ITCRM_ARG_BCRA)
plot(X_FIL, ITCRM_SFE_BCRA)
plot(X_FIL, ITCRM_SFE_berre)
# MODELOS SIMPLES####
## X SIN FILTRAR####
reg_ARG       <- lm(log(X) ~ log(ITCRM_ARG_BCRA))
reg_SFE_BCRA  <- lm(log(X) ~ log(ITCRM_SFE_BCRA))
reg_SFE_Berre <- lm(log(X) ~ log(ITCRM_SFE_berre))

summary(reg_ARG)
summary(reg_SFE_BCRA)
summary(reg_SFE_Berre)

## X FILTRADAS####
reg_ARG_fil       <- lm(log(X_FIL) ~ log(ITCRM_ARG_BCRA))
reg_SFE_BCRA_fil  <- lm(log(X_FIL) ~ log(ITCRM_SFE_BCRA))
reg_SFE_Berre_fil <- lm(log(X_FIL) ~ log(ITCRM_SFE_berre))

summary(reg_ARG_fil)
summary(reg_SFE_BCRA_fil)
summary(reg_SFE_Berre_fil)
# MODELOS MULTIPPLES####
## X SIN FILTRAR | GEBC####
reg_ARG_mult       <- lm(log(X) ~ log(ITCRM_ARG_BCRA) + log(GEBC))
reg_SFE_BCRA_mult  <- lm(log(X) ~ log(ITCRM_SFE_BCRA) + log(GEBC))
reg_SFE_Berre_mult <- lm(log(X) ~ log(ITCRM_SFE_berre) + log(GEBC))

summary(reg_ARG_mult)
summary(reg_SFE_BCRA_mult)
summary(reg_SFE_Berre_mult)
## X FILTRADAS | GEBC####
reg_ARG_mult_fil       <- lm(log(X_FIL) ~ log(ITCRM_ARG_BCRA)  + log(GEBC))
reg_SFE_BCRA_mult_fil  <- lm(log(X_FIL) ~ log(ITCRM_SFE_BCRA)  + log(GEBC))
reg_SFE_Berre_mult_fil <- lm(log(X_FIL) ~ log(ITCRM_SFE_berre) + log(GEBC))

summary(reg_ARG_mult_fil)
summary(reg_SFE_BCRA_mult_fil)
summary(reg_SFE_Berre_mult_fil)

## X SIN FILTRAR | GEBC + VMT####
reg_ARG_mult_2       <- lm(log(X) ~ log(ITCRM_ARG_BCRA) + log(GEBC)  + log(VMT))
reg_SFE_BCRA_mult_2  <- lm(log(X) ~ log(ITCRM_SFE_BCRA) + log(GEBC)  + log(VMT))
reg_SFE_Berre_mult_2 <- lm(log(X) ~ log(ITCRM_SFE_berre) + log(GEBC) + log(VMT))

summary(reg_ARG_mult_2)
summary(reg_SFE_BCRA_mult_2)
summary(reg_SFE_Berre_mult_2)
## X FILTRADAS | GEBC + VMT####
reg_ARG_mult_fil_2       <- lm(log(X) ~ log(ITCRM_ARG_BCRA)  + log(GEBC) + log(VMT))
reg_SFE_BCRA_mult_fil_2  <- lm(log(X) ~ log(ITCRM_SFE_BCRA)  + log(GEBC) + log(VMT))
reg_SFE_Berre_mult_fil_2 <- lm(log(X) ~ log(ITCRM_SFE_berre) + log(GEBC) + log(VMT))

summary(reg_ARG_mult_fil_2)
summary(reg_SFE_BCRA_mult_fil_2)
summary(reg_SFE_Berre_mult_fil_2)

## X SIN FILTRAR | API (AGRICULTURAL PRICE INDEX, THE PINK SHEET) ####
reg_ARG_mult_3       <- lm(log(X) ~ log(ITCRM_ARG_BCRA) + log(API))
reg_SFE_BCRA_mult_3  <- lm(log(X) ~ log(ITCRM_SFE_BCRA) + log(API))
reg_SFE_Berre_mult_3 <- lm(log(X) ~ log(ITCRM_SFE_berre) + log(API))

summary(reg_ARG_mult_3)
summary(reg_SFE_BCRA_mult_3)
summary(reg_SFE_Berre_mult_3)

## X SIN FILTRAR | API + GEBC ####
reg_ARG_mult_4       <- lm(log(X) ~ log(ITCRM_ARG_BCRA) + log(GEBC)  + log(API))
reg_SFE_BCRA_mult_4  <- lm(log(X) ~ log(ITCRM_SFE_BCRA) + log(GEBC)  + log(API))
reg_SFE_Berre_mult_4 <- lm(log(X) ~ log(ITCRM_SFE_berre) + log(GEBC) + log(API))

summary(reg_ARG_mult_4)
summary(reg_SFE_BCRA_mult_4)
summary(reg_SFE_Berre_mult_4)

install.packages(c("broom", "dplyr", "tidyr", "openxlsx"))
library(broom)
library(dplyr)
library(tidyr)
library(openxlsx)

modelos <- list(
  
  # === MODELOS SIMPLES ===
  "ARG | ITCRM ARG BCRA"        = reg_ARG,
  "SFE | ITCRM SFE BCRA"        = reg_SFE_BCRA,
  "SFE | ITCRM SFE Berre"       = reg_SFE_Berre,
  
  # === X FILTRADAS ===
  "ARG FIL | ITCRM ARG BCRA"    = reg_ARG_fil,
  "SFE FIL | ITCRM SFE BCRA"    = reg_SFE_BCRA_fil,
  "SFE FIL | ITCRM SFE Berre"   = reg_SFE_Berre_fil,
  
  # === MULTIPLES | GEBC ===
  "ARG | ITCRM ARG BCRA + GEBC"      = reg_ARG_mult,
  "SFE | ITCRM SFE BCRA + GEBC"      = reg_SFE_BCRA_mult,
  "SFE | ITCRM SFE Berre + GEBC"  = reg_SFE_Berre_mult,
  
  # === FILTRADAS | GEBC ===
  "ARG FIL | ITCRM ARG BCRA + GEBC"  = reg_ARG_mult_fil,
  "SFE FIL | ITCRM SFE BCRA + GEBC"  = reg_SFE_BCRA_mult_fil,
  "SFE FIL | ITCRM SFE Berre + GEBC" = reg_SFE_Berre_mult_fil,
  
  # === GEBC + VMT ===
  "ARG | ITCRM ARG BCRA + GEBC + VMT" = reg_ARG_mult_2,
  "SFE | ITCRM SFE BCRA + GEBC + VMT" = reg_SFE_BCRA_mult_2,
  "SFE | ITCRM SFE Berre + GEBC + VMT" = reg_SFE_Berre_mult_2,
  
  # === API ===
  "ARG | ITCRM ARG BCRA + API"       = reg_ARG_mult_3,
  "SFE | ITCRM SFE BCRA + API"       = reg_SFE_BCRA_mult_3,
  "SFE | ITCRM SFE Berre + API"   = reg_SFE_Berre_mult_3,
  
  # === API + GEBC ===
  "ARG | ITCRM ARG BCRA + API + GEBC" = reg_ARG_mult_4,
  "SFE | ITCRM SFE BCRA + API + GEBC" = reg_SFE_BCRA_mult_4,
  "SFE | ITCRM SFE Berre + API + GEBC" = reg_SFE_Berre_mult_4
)

coef_tabla <- bind_rows(
  lapply(names(modelos), function(nm) {
    tidy(modelos[[nm]]) %>%
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
    lapply(modelos, function(m)
      as.character(round(summary(m)$adj.r.squared, 3))
    ),
    names(modelos)
  )
)


tabla_final <- bind_rows(tabla_reg, r2_adj)

write.xlsx(
  tabla_final,
  file = "regresiones_completas.xlsx",
  overwrite = TRUE
)
