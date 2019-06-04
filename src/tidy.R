source(file.path('~/../vburguete', 'codigueras', 'R', 'corrige_paises.R'))            

library(tidyverse)
library(janitor)

coords <- read_csv("data/coords.csv")

proyectos_ec <- readxl::read_xlsx(here::here('data', 'proyectos_ec.xlsx')) %>% 
  clean_names() %>% 
  left_join(iso3, by =  c("pais" = "final")) %>% 
  mutate(ISO93 = ifelse(pais == "Escocia", "GBR", ISO93),
         ISO93 = ifelse(pais == "Inglaterra", "GBR", ISO93)) %>% 
  left_join(select(coords, `Alpha-3 code`, lat, lon), by = c("ISO93" = "Alpha-3 code"))


