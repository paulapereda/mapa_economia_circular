library(tidyverse)
library(janitor)
library(readxl)
library(here)


nacional <- read_xls(here('data', 'proyectos_ec.xls'), sheet = "nacional") %>% 
  clean_names() %>% 
  mutate(sector = recode(sector,
                         'Ganadería ' = "Ganadería"),
         estado_finalizado_en_ejecucion_solicitado = recode(estado_finalizado_en_ejecucion_solicitado,
                                                            'en ejecución' = "En ejecución",
                                                            'En ejecución' = "En ejecución",
                                                            'finalizado' = "Finalizado",
                                                            'tratamiento Parlamento' = "Tratamiento Parlamento"),
         instituciones_participantes = recode(instituciones_participantes,
                                              'MVOTMA y MGAP' = "MVOTMA/MGAP"))

saveRDS(nacional, 'data/nacional.rds')

mundial <- read_xls(here('data', 'proyectos_ec.xls'), sheet = "mundo") %>% 
  clean_names()
