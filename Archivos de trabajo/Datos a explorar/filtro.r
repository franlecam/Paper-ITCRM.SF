# OBJETIVO ####
# El objetivo de este codigo es filtrar una base de INDEC de exportaciones de modo tal de
# quedarnos solamente con las exportaciones con origen Santa Fe, pudiendo filtrar entre 
# MOA y MOI, y destino

## LIBRERIAS ####
library(readxl)
library(dplyr)

## Leer base de datos ####
# Primero leemos cada hoja
data1 <- 
  readxl::read_excel(
    "C:/Users/vcorvalan/Desktop/Trabajo/Paper/Paper-ITCRM.SF/Archivos de trabajo/Datos a explorar/DB_X_SFE_nmc_dest.xlsx",
     sheet = "datos origen_2002-2011")

data2 <- 
  readxl::read_excel(
    "C:/Users/vcorvalan/Desktop/Trabajo/Paper/Paper-ITCRM.SF/Archivos de trabajo/Datos a explorar/DB_X_SFE_nmc_dest.xlsx",
    sheet = "datos origen_2012-2022")

data3 <- 
  readxl::read_excel(
    "C:/Users/vcorvalan/Desktop/Trabajo/Paper/Paper-ITCRM.SF/Archivos de trabajo/Datos a explorar/DB_X_SFE_nmc_dest.xlsx",
    sheet = "datos origen_2023-2024")

# Luego unimos en un sólo objeto
data <- 
  dplyr::bind_rows(data1, data2, data3)

# Filtramos para quedarnos sólo con los datos de SFE
data_sfe <-
  data |> 
  dplyr::filter(DESCRIP_PCIA == c("Santa Fe","SANTA FE")) |> 
  dplyr::select(CANIO, DESCRIP_RUBRO,DESCRIP_PAIS, DOLARES_FOB)

## CHECK EXPORTACIONES ANUALES PROVINCIA SFE ####
check_X <- 
  data_sfe |> 
  dplyr::group_by(CANIO) |> 
  dplyr::summarise(sum(DOLARES_FOB))

unique(data$DESCRIP_PCIA)
