library(tidyverse)
library(janitor)
library(readxl)
library(here)

source(file.path('~/../vburguete', 'codigueras', 'R', 'corrige_paises.R'))

coords <- read_csv("data/coords.csv")

# Proyectos Economía Circular POR PAÍSES

proyectos_ec <- read_xlsx(here('data', 'proyectos_ec.xlsx')) %>% 
  clean_names() %>% 
  left_join(iso3, by =  c("pais" = "final")) %>% 
  mutate(ISO93 = ifelse(pais == "Escocia", "GBR", ISO93),
         ISO93 = ifelse(pais == "Inglaterra", "GBR", ISO93),
         ISO99 = ifelse(pais == "Holanda", "NLD", ISO93)) %>% 
  filter(vision != "Plan de acción para una economía circular en Europa (2015)") %>% 
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

# Proyectos Economía Circular POR CIUDADES

aux_2 <- tribble(
  ~ ciudad,                 ~ lat,          ~ lon,  ~ pais,
  "Amsterdam",             52.370216,    4.895168, "Holanda",
  "Amtres",                39.225449,   -8.266333, "Portugal",      
  "Aquitania",             44.246206,   -0.183998, "Francia",
  "Arun",                  51.481869,    0.424960, "Inglaterra",  
  "Baix Camp",             41.144254,    1.035543, "España", 
  "Barcelona",             41.385063,    2.173404, "España",
  "Boulder (Colorado)",    40.014984, -105.270546, "Estados Unidos",
  "Castle Morpeth",        55.171772,   -1.686301, "Inglaterra",
  "Chicago",               41.878113,  -87.629799, "Estados Unidos",
  "Cork",                  51.896893,   -8.486316, "Irlanda",
  "Cupello",               42.069241,   14.667807, "Italia",
  "Gironde",               44.849667,   -0.450237, "Francia",
  "Kerry",                 52.059792,   -9.506940, "Irlanda",
  "Limerick",              52.663860,   -8.626770, "Irlanda",  
  "Lipor",                 41.198605,   -8.548224, "Portugal",
  "Londres",               51.507351,   -0.127758, "Inglaterra",
  "Madison",               43.073051,  -89.401230, "Estados Unidos",
  "Milán",                 45.464203,    9.189982, "Italia",
  "Montejurra",            42.630617,   -2.044794, "España",
  "Monza",                 45.583130,    9.272970,    "Italia",            
  "Niort",                 46.323860,   -0.457580, "Francia",
  "Nueva York",            40.712776,  -74.005974, "Estados Unidos",
  "Oporto",                41.150150,   -8.610320, "Portugal",
  "Padova",                45.406433,   11.876761,  "Italia",
  "Paris",                48.856613,     2.352222,        "Francia",
  "Peterborough",          52.569500,   -0.240530, "Inglaterra",
  "Región de Bruselas",    50.850342,    4.351710, "Bélgica",
  "San Francisco",         37.774929, -122.419418, "Estados Unidos",
  "San José (California)", 37.334789, -121.888138, "Estados Unidos",
  "SIVOM de Bapaume",      50.103250,    2.850984, "Francia",
  "Taiwan",                23.697809,  120.960518, "Taiwán",
  "Wyecycle",               52.418788,  -1.247349, "Inglaterra")

proyectos_ec_ciudades <- read_xlsx(here('data', 'proyectos_ec.xlsx'), sheet = "ciudades") %>% 
  left_join(aux_2)

saveRDS(proyectos_ec_ciudades, 'data/proyectos_ec_ciudades.rds')

# Proyectos Economía Circular PARA URUGUAY

proyectos_ec_uy <- read_xlsx(here('data', 'proyectos_ec.xlsx'), sheet = "uy") %>%
 mutate(Impacto = str_to_sentence(Impacto))

saveRDS(proyectos_ec_uy, 'data/proyectos_ec_uy.rds')
