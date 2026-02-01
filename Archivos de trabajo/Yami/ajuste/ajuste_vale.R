#Librerias####
library(readxl)
library(dynlm)
#Datos####
datos <- read_excel("datos.xlsx",
                    sheet = "Tabelle2",
                    range = "K2:S278")

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
reg_ARG_mult_fil       <- lm(log(X) ~ log(ITCRM_ARG_BCRA)  + log(GEBC))
reg_SFE_BCRA_mult_fil  <- lm(log(X) ~ log(ITCRM_SFE_BCRA)  + log(GEBC))
reg_SFE_Berre_mult_fil <- lm(log(X) ~ log(ITCRM_SFE_berre) + log(GEBC))

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
