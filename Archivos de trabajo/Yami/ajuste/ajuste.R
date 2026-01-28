library(readxl)
library(dynlm)

datos <- read_excel("datos.xlsx",
                    sheet = "Tabelle2",
                    range = "K1:O286")

attach(datos)
reg_ARG <- lm(log(X_FOB) ~ log(ITCRM_ARGENTINA_BCRA)
             # + log(ICA_SFE)
              )

reg_SFE <- lm(log(X_FOB) ~ log(ITCRM_SANTA_FE_met_BCRA)
            #  + log(ICA_SFE)
              )

summary(reg_ARG)
cor(X_FOB, ITCRM_ARGENTINA_BCRA)^2

summary(reg_SFE)
cor(X_FOB, ITCRM_SANTA_FE_met_BCRA)


regF_ARG <- lm(log(X_FOB_FILT) ~ log(ITCRM_ARGENTINA_BCRA)
              # + log(ICA_SFE)
)

regF_SFE <- lm(log(X_FOB_FILT) ~ log(ITCRM_SANTA_FE_met_BCRA)
              #  + log(ICA_SFE)
)

summary(regF_ARG)
summary(regF_SFE)
