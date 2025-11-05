ITCRB <- readxl::read_excel("Construyendo ITCRM_mensual_met_Berretoni.xlsx",
                            sheet= "ITCRB",
                            range = "A2:AU290")

library(tidyverse)
library(zoo)
library(ggplot2)

ITCRB_long <- ITCRB %>%
  mutate(fecha = gsub("M", "", fecha),            # elimina la "M"
         fecha = as.yearmon(fecha, "%Y.%m")) %>%  # ahora sí: "2001.01"
  pivot_longer(cols = -fecha, names_to = "pais", values_to = "valor") %>%
  mutate(
    valor = ifelse(valor == 0, NA, valor)     # reemplaza 0 por NA
  )


ggplot(ITCRB_long, aes(x = fecha, y = valor)) +
  geom_line(color = "darkgreen", linewidth = 0.4) +
  facet_wrap(~ pais, scales = "free_y", ncol = 5) +
  scale_x_yearmon(format = "%Y") +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(color = "black"),       # todo texto en negro
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal()


