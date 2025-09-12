## EL OBJETIVO DE ESTE CODIGO ES TOMAR LAS BASES DE DATOS DE CPI DE FMI Y QUEDARNOS SOLO CON LOS PAISES QUE NOS INTERESAN

dataset_IMF_CPI <- read.csv("C:/Users/SFC/OneDrive/Escritorio/Paper-ITCRM.SF/Archivos de trabajo/Datos/dataset_IMF_CPI.csv")

mis_paises <- c(
  "Saudi Arabia", "Algeria", "Australia", "Bangladesh", "Belgium", "Brazil", "Chile",
  "China, People's Republic of", "Colombia", "Korea, Republic of", "Denmark", "Ecuador", "Egypt, Arab Republic of", "Spain", "United States",
  "Philippines", "France", "Greece", "India", "Indonesia", "Iran, Islamic Republic of", "Ireland", "Italy",
  "Jordan", "Latvia, Republic of", "Malaysia", "Malta", "Morocco", "Mexico", "Netherlands, The",
  "Paraguay", "Pakistan", "Peru", "Poland, Republic of", "United Kingdom", "Dominican Republic",
  "Germany", "Russian Federation", "South Africa", "Thailand", "Tunisia", "Türkiye, Republic of", "Uruguay",
  "Venezuela, República Bolivariana de", "Vietnam"
)

library(dplyr)
dataset_filtrado <- dataset_IMF_CPI %>%
  filter(COUNTRY %in% mis_paises,
         TYPE_OF_TRANSFORMATION == "Index")

library(writexl)
write_xlsx(dataset_filtrado,"C:/Users/SFC/OneDrive/Escritorio/Paper-ITCRM.SF/Archivos de trabajo/Datos/dataset_limpio_IMF_CPI.xlsx")
