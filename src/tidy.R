source(file.path('~/../vburguete', 'codigueras', 'R', 'corrige_paises.R'))            

library(tidyverse)
library(janitor)

coords <- read_csv("data/coords.csv")

proyectos_ec <- readxl::read_xlsx(here::here('data', 'proyectos_ec.xlsx')) %>% 
  clean_names() %>% 
  left_join(iso3, by =  c("pais" = "final")) %>% 
  mutate(ISO93 = ifelse(pais == "Escocia", "GBR", ISO93),
         ISO93 = ifelse(pais == "Inglaterra", "GBR", ISO93),
         ISO99 = ifelse(pais == "Holanda", "NLD", ISO93)) %>% 
  left_join(select(coords, `Alpha-3 code`, lat, lon), by = c("ISO93" = "Alpha-3 code"))

aux <- proyectos_ec %>% 
  group_by(ISO93) %>% 
  mutate(country = paste0(pais, collapse = " "),
         concepto = paste0(vision, collapse = "\n")) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(country = recode(country,
                          "Alemania Alemania" = "Alemania",
                          "Austria Austria" = "Austria",
                          "Corea del Sur Corea del Sur" = "Corea del Sur",
                          "Dinamarca Dinamarca" = "Dinamarca",
                          "España España" = "España",
                          "Finlandia Finlandia"  = "Finlandia",
                          "Francia Francia" = "Francia",
                          "Luxemburgo Luxemburgo" = "Luxemburgo",  
                          "Holanda" = "Países Bajos",
                          "Suecia Suecia" = "Suecia"))%>% 
  select(ISO93, country, concepto) 

proyectos_ec <- proyectos_ec %>% 
  left_join(select(aux, ISO93, country, concepto))

saveRDS(proyectos_ec, 'data/proyectos_ec.rds')

proyectos_ec_uy <- readxl::read_xlsx(here::here('data', 'ec_uy.xlsx')) %>% 
  clean_names() %>% 
  select(-proyecto_number_si_aplica, -responsable_coordinador, -observaciones) %>% 
  mutate(sector = recode(sector,
                         'Ganadería ' = "Ganadería")) %>% 
  mutate(estado_finalizado_en_ejecucion_solicitado = recode(estado_finalizado_en_ejecucion_solicitado,
                                                            'en ejecución' = "En ejecución",
                                                            'En ejecución' = "En ejecución",
                                                            'En Ejecución' = 	"En ejecución",
                                                            'finalizado' = "Finalizado",
                                                            'tratamiento Parlamento' = "Tratamiento Parlamento")) %>% 
  mutate(instituciones_participantes = recode(instituciones_participantes,
                                              'MVOTMA y MGAP' = "MVOTMA/MGAP"))

