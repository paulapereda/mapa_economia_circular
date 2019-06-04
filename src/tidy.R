source(file.path('~/../vburguete', 'codigueras', 'R', 'corrige_paises.R'))            

library(tidyverse)
library(janitor)

proyectos_ec <- readxl::read_xlsx(here::here('data', 'proyectos_ec.xlsx')) %>% 
  clean_names() %>% 
  left_join(iso3, by =  c("pais" = "final")) %>% 
  mutate(ISO93 = ifelse(pais == "Escocia", "GBR", ISO93),
         ISO93 = ifelse(pais == "Inglaterra", "GBR", ISO93))

# nacional <- read_xls(here('data', 'proyectos_ec.xls'), sheet = "nacional") %>% 
#   clean_names() %>% 
#   mutate(sector = recode(sector,
#                          'Ganadería ' = "Ganadería"),
#          estado_finalizado_en_ejecucion_solicitado = recode(estado_finalizado_en_ejecucion_solicitado,
#                                                             'en ejecución' = "En ejecución",
#                                                             'En ejecución' = "En ejecución",
#                                                             'finalizado' = "Finalizado",
#                                                             'tratamiento Parlamento' = "Tratamiento Parlamento"),
#          instituciones_participantes = recode(instituciones_participantes,
#                                               'MVOTMA y MGAP' = "MVOTMA/MGAP"))
# 
# saveRDS(nacional, 'data/nacional.rds')
# 
# mundial <- read_xls(here('data', 'proyectos_ec.xls'), sheet = "mundo") %>% 
#   clean_names()
