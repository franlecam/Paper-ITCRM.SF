library(readxl)
library(dynlm)

datos <- read_excel("datos.xlsx", range = "K1:O286")

attach(datos)
reg_ARG <- lm(log(X_FOB_FILT) ~ log(ITCRM_ARGENTINA_BCRA)
             # + log(ICA_SFE)
              )

reg_SFE <- lm(log(X_FOB_FILT) ~ log(ITCRM_SANTA_FE_met_BCRA)
            #  + log(ICA_SFE)
              )

summary(reg_ARG)
cor(X_FOB_FILT, ITCRM_ARGENTINA_BCRA)

summary(reg_SFE)
cor(X_FOB_FILT, ITCRM_SANTA_FE_met_BCRA)


