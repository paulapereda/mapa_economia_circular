source(file.path('~/../vburguete', 'codigueras', 'R', 'corrige_paises.R'))                                        

# metadata (meta)
# fuente archivo primario:   http://databank.worldbank.org/data/reports.aspx?source=wdi-database-archives-%28beta%29#
region <- file.path('~/../vburguete', 'datos_repositorio', 'cadenas_logisticas', 'meta.xlsx') %>% 
  read_excel() %>% 
  janitor::clean_names() %>% 
  transmute(ISO93 = code, long_name, region)


#Crea un vector auxiliar con la columna del data frame a corregir
aux1 <- base_id %>% 
  select(pais = pais_destino) %>%
  distinct()

#Pasa todo a mayúscula y quita acentuaciones, ademas crea join con casos particulares a corregir
aux1 <- aux1 %>%
  mutate(intermedio = pais %>%
           toupper %>%
           tildes) %>%
  left_join(excep, by = c("intermedio" = "pais")) %>%
  mutate(final = case_when(is.na(final) == T ~ pais,
                           T ~ final)) %>%
  select(-intermedio)

#Corrige el nombre de los países según patrones
for (i in 1:length(patrones)) {
  aux1 <- aux1 %>%
    mutate(final = str_replace_all(final, patrones[[i]], finales[[i]]))
}

#Une el data frame auxiliar al data frame original. 
# Se agrega una columna con los nombres corregidos y otra con el código ISO
base_id <- base_id %>% 
  left_join(aux1, by = c("pais_destino" = "pais")) %>%
  left_join(iso3, by = "final") %>%
  select(-pais_destino) %>%
  select(pais = final, everything())

# Incorpora región
base_id <- base_id %>% 
  left_join(region %>% 
              filter(is.na(region) != TRUE) %>% 
              select(-long_name), 
            by = "ISO93") %>% 
  select(pais_destino = pais, 
         everything())