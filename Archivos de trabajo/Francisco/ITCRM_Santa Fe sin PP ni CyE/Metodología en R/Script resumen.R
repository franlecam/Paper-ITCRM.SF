
panel_mes_pais1

# suma de participaciones por año ####

print(panel_mes_pais1 %>%
  distinct(Año, pais, participacion) %>%
  group_by(Año) %>%
  summarise(
    total = sum(participacion, na.rm = TRUE),
    .groups = "drop"
  ), n = 23)


# rescalando ponderaciones ####

panel_mes_pais2 <- panel_mes_pais1 %>%
  group_by(mes) %>%
  mutate(
    participacion_norm =
      participacion / sum(participacion, na.rm = TRUE)
  ) %>%
  ungroup()

## AGREGANDO ARGENTIAN ####

er_cpi_arg_fix <- er.cpi_arg %>%
  rename(
    mes     = ...1,
    cpi_arg = IPC_ARG,
    er_arg  = TC_ARG
  ) %>%
  mutate(
    mes = as.Date(mes)
  )

panel_mes_pais3 <- panel_mes_pais2 %>%
  left_join(
    er_cpi_arg_fix,
    by = "mes"
  )

#### check ####

panel_mes_pais3 %>%
  summarise(
    miss_cpi_arg = sum(is.na(cpi_arg)),
    miss_er_arg  = sum(is.na(er_arg))
  )

# AGREGANDO   ####

panel_mes_pais4 <- panel_mes_pais3 %>%
  mutate(
    tc_bilateral = er_arg / er,
    
    tcr_bilateral = if_else(
      !is.na(er_arg) & !is.na(er) & !is.na(cpi) & !is.na(cpi_arg),
      (er_arg / er) * (cpi / cpi_arg),
      NA_real_
    )
  )

panel_mes_pais5 <- panel_mes_pais4 %>%
  arrange(pais, mes) %>%
  group_by(pais) %>%
  mutate(
    tcr_disp_t_t1 = as.integer(
      !is.na(tcr_bilateral) &
        !is.na(dplyr::lag(tcr_bilateral))
    )
  ) %>%
  ungroup()


panel_mes_pais6 <- panel_mes_pais5 %>%
  arrange(pais, mes) %>%
  group_by(pais) %>%
  mutate(
    var_tcr_bil = if_else(
      tcr_disp_t_t1 == 1 &
        !is.na(participacion_norm) &
        is.finite(participacion_norm),
      (tcr_bilateral / dplyr::lag(tcr_bilateral)) ^ participacion_norm,
      NA_real_
    )
  ) %>%
  ungroup()

panel_mes_pais7 <- panel_mes_pais6 %>%
  group_by(mes) %>%
  mutate(
    suma_participacion_disp =
      sum(
        participacion_norm * tcr_disp_t_t1,
        na.rm = TRUE
      )
  ) %>%
  ungroup() %>%
  mutate(
    ponderador_adj = if_else(
      tcr_disp_t_t1 == 1 & suma_participacion_disp > 0,
      participacion_norm / suma_participacion_disp,
      0
    )
  ) 

panel_mes_pais8 <- panel_mes_pais7 %>%
  arrange(pais, mes) %>%
  group_by(pais) %>%
  mutate(
    var_tcr_bil_adj = if_else(
      tcr_disp_t_t1 == 1 & dplyr::lag(tcr_disp_t_t1) == 1,
      (tcr_bilateral / dplyr::lag(tcr_bilateral)) ^ ponderador_adj,
      1
    )
  ) %>%
  ungroup()

itcrm <- panel_mes_pais8 %>%
  group_by(mes) %>%
  summarise(
    itcrm_factor = prod(var_tcr_bil_adj, na.rm = TRUE),
    .groups = "drop"
  )

itcrm_final <- itcrm %>%
  arrange(mes) %>%
  mutate(
    itcrm_nivel = 100 * cumprod(itcrm_factor)
  )


### grafico ####
itcrm_bcra <- readxl::read_excel("C:/Users/SFC/OneDrive/Escritorio/Paper-ITCRM.SF/Archivos de trabajo/Yami/itcrm bcra.xlsx",
                                 col_names = T) 

itcrm_graf <- itcrm_final %>%
  left_join(itcrm_bcra, by = "mes")

ggplot(itcrm_graf, aes(x = mes)) +
  geom_line(
    aes(y = itcrm_nivel, color = "ITCRM SFE"),
    linewidth = 0.9
  ) +
  geom_line(
    aes(y = ITCRM, color = "ITCRM ARG"),
    linewidth = 0.9,
  ) +
  labs(
    title = "ITCRM – Comparación",
    x = NULL,
    y = "Índice"
  ) +
  scale_color_manual(
    values = c("ITCRM SFE" = "steelblue", "ITCRM ARG" = "black")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())



# correr hasta acá####
itcrm_vale <- readxl::read_excel(
  
  # "C:/Repositorios/Paper-ITCRM.SF/Archivos de trabajo/Vale/ITCRM/Sin PP y C&E/ITCRM_metodología BCRA_SIN PP NI CYE.xlsx",
  
  # path yami:                               
  "C:/Users/SFC/OneDrive/Escritorio/Paper-ITCRM.SF/Archivos de trabajo/Vale/ITCRM/Sin PP y C&E/ITCRM_metodología BCRA_SIN PP NI CYE.xlsx" ,                            
                   sheet= "ITCRM", 
                   range = "a1:k301", col_names = T) %>%
  select(1,9) %>%
  rename(
    mes = 1,
    itcrm_vale = 2
  )

itcrm_comp <- itcrm_final %>%
  left_join(itcrm_vale, by = "mes")

ggplot(itcrm_comp, aes(x = mes)) +
  geom_line(
    aes(y = itcrm_nivel, color = "ITCRM R"),
    linewidth = 0.9
  ) +
  geom_line(
    aes(y = itcrm_vale, color = "ITCRM Vale"),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  # geom_line(
  #   aes(y = bcra, color = "ITCRM bcra"),
  #   linewidth = 0.9,
  #   linetype = "dashed"
  # ) +
  labs(
    title = "ITCRM Argentina – Comparación",
    subtitle = "Cálculo propio vs. Vale",
    x = NULL,
    y = "Índice"
  ) +
  scale_color_manual(
    values = c("ITCRM R" = "steelblue", "ITCRM Vale" = "black")
               # "ITCRM bcra" = "red")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# check

prueba <- itcrm_comp %>%
  arrange(mes) %>%
  mutate(
    d_r    = log(itcrm_nivel) - log(lag(itcrm_nivel)),
    d_vale = log(itcrm_vale)  - log(lag(itcrm_vale))
  )

cor(prueba$d_r, prueba$d_vale, use = "complete.obs")

base_2015_12 <- itcrm_comp %>%
  filter(mes == as.Date("2015-12-01")) %>%
  summarise(
    base_r    = itcrm_nivel,
    base_vale = itcrm_vale,
    base_bcra = bcra
  )

itcrm_comp_base2015 <- itcrm_comp %>%
  mutate(
    itcrm_r_2015    = itcrm_nivel / base_2015_12$base_r * 100,
    itcrm_vale_2015 = itcrm_vale  / base_2015_12$base_vale * 100,
    itcrm_bcra_2015 = bcra        / base_2015_12$base_bcra * 100
  )

ggplot(itcrm_comp_base2015, aes(x = mes)) +
  geom_line(
    aes(y = itcrm_r_2015, color = "ITCRM R"),
    linewidth = 0.9
  ) +
  geom_line(
    aes(y = itcrm_vale_2015, color = "ITCRM Vale"),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  geom_line(
    aes(y = itcrm_bcra_2015, color = "ITCRM BCRA"),
    linewidth = 0.9,
    linetype = "dotted"
  ) +
  labs(
    title = "ITCRM Argentina – Comparación",
    subtitle = "Índices rebasados a diciembre 2015 = 100",
    x = NULL,
    y = "Índice (dic-2015 = 100)"
  ) +
  scale_color_manual(
    values = c(
      "ITCRM R"     = "steelblue",
      "ITCRM Vale"  = "black",
      "ITCRM BCRA"  = "red"
    )
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# base 2024 ####

base_2024 <- itcrm_comp %>%
  filter(format(mes, "%Y") == "2024") %>%
  summarise(
    base_r    = mean(itcrm_nivel, na.rm = TRUE),
    base_vale = mean(itcrm_vale,  na.rm = TRUE),
    base_bcra = mean(bcra,        na.rm = TRUE)
  )

itcrm_comp_base2024 <- itcrm_comp %>%
  mutate(
    itcrm_r_2024    = itcrm_nivel / base_2024$base_r * 100,
    itcrm_vale_2024 = itcrm_vale  / base_2024$base_vale * 100,
    itcrm_bcra_2024 = bcra        / base_2024$base_bcra * 100
  )

ggplot(itcrm_comp_base2024, aes(x = mes)) +
  geom_line(
    aes(y = itcrm_r_2024, color = "ITCRM R"),
    linewidth = 0.9
  ) +
  geom_line(
    aes(y = itcrm_vale_2024, color = "ITCRM Vale"),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  geom_line(
    aes(y = itcrm_bcra_2024, color = "ITCRM BCRA"),
    linewidth = 0.9,
    linetype = "dotted"
  ) +
  labs(
    title = "ITCRM Argentina – Comparación",
    subtitle = "Índices rebasados a promedio 2024 = 100",
    x = NULL,
    y = "Índice (promedio 2024 = 100)"
  ) +
  scale_color_manual(
    values = c(
      "ITCRM R"     = "steelblue",
      "ITCRM Vale"  = "black",
      "ITCRM BCRA"  = "red"
    )
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# descargar ITCRM - agregado por valentín
library(openxlsx)
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "ITCRM")
openxlsx::writeData(wb, "ITCRM", itcrm_final)
openxlsx::saveWorkbook(wb,
                       "C:/Users/vcorvalan/Desktop/Trabajo/Paper-ITCRM.SF/Archivos de trabajo/Vale/ITCRM.xlsx",
                       overwrite = TRUE)






