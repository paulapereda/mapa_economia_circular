# Librerías
library(readxl)
library(readr)

library(stringr)
library(dplyr)
library(tidyr)
library(DT)

library(ggplot2)
library(treemapify)
library(leaflet)

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)

library(estilotu)
set_estilotu()


################### Shiny Dashboard App ####################
# Bases
path1 <- file.path('~', '..', 'vburguete', 'datos_repositorio', 'internacionalizacion')
path2 <- file.path('~', '..', 'vburguete', 'datos_repositorio', 'acuerdos')

import_princ_2017 <- file.path(path1, 'import_princ_2017.rds') %>% read_rds
export_uru_2017 <- file.path(path1, 'export_uru_2017.rds') %>% read_rds
export_princ_2017 <- file.path(path1, 'export_princ_2017.rds') %>% read_rds

# # Mapa
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
# system("unzip world_shape_file.zip")
library(rgdal)
world_spdf=readOGR( dsn= getwd() , layer="TM_WORLD_BORDERS_SIMPL-0.3")
# countries <- world_spdf@data %>% select(-AREA, -POP2005, -REGION, -SUBREGION)
# country <- world_spdf@data

paises <- file.path(path1, 'paises.xlsx') %>% read_excel %>% 
  mutate(FINAL = toupper(final))
country <- file.path(path1, 'country.rds') %>% read_rds %>% 
  filter(is.na(M49) != TRUE)
WTO <- file.path(path2, 'acuerdos_completo.rds') %>% read_rds %>% 
     # file.path(path2, 'acuerdos.rds') %>% read_rds %>% 
  mutate(pais = case_when(pais == "Chinese Taipei" ~ "China", TRUE ~ pais),
         members = case_when(members == "CHINASE TAIPEI" ~ "CHINA", TRUE ~ members)) %>% 
  rename(PAIS = pais) %>%
  left_join(country,# %>% transmute(M49, ISO3, ingles, final), 
            by = c("members" = "NAME"))
paises <- file.path(path1, 'paises.xlsx') %>% read_excel %>% 
  mutate(final = final %>% toupper)

aranceles_uruguay <- file.path(path1, 'aranceles_uruguay.rds') %>% read_rds
aranceles_competidores <- file.path(path1, 'aranceles_competidores.rds') %>% read_rds
aranceles_4 <- file.path(path1, 'aranceles_4.rds') %>% read_rds
aranceles_6 <- file.path(path1, 'aranceles_6.rds') %>% read_rds
aranceles_uru_4 <- file.path(path1, 'aranceles_uru_4.rds') %>% read_rds
aranceles_uru_6 <- file.path(path1, 'aranceles_uru_6.rds') %>% read_rds


ncm <- file.path(path1, "ncm.rds") %>% read_rds %>% mutate(codigo = as.character(codigo)) %>% 
  mutate(codigo = case_when(nchar(codigo)==3 ~ paste("0", codigo, sep = ""), TRUE ~ codigo))


# codiguera_productos <- productos.tu()
# ncm <- codiguera_ncm()

## Inputs
GRUPO = sort(unique(export_princ_2017$GRUPO))
PAIS = sort(unique(export_uru_2017$DESTINO))
DESTINO = sort(unique(import_princ_2017$PAIS))
PAIS.ACUERDOS <- WTO %>% transmute(members) %>% arrange(members) %>% unique %>% 
  filter(!members %in% c("EASTERN AND SOUTHERN AFRICA STATES INTERIM EPA",
                         "EC ENLARGEMENT (INDETERMINADO)",
                         "EC (INDETERMINADO) ENLARGEMENT",
                         "EC TREATY",
                         "EU (INDETERMINADO) ENLARGEMENT",
                         "EUROPEAN UNION"))


## Función para lectura de mapa 
choropleth_world <- function(df) {
  mapa <- world_spdf
  mapa@data <- mapa@data %>% 
    rename(pais = NAME) %>% 
    left_join(country %>% transmute(pais, NAME), by = "pais") %>% 
    left_join(df) %>% 
    mutate(country_label = if_else(is.na(country_label) == TRUE, "No-socios",
                                   as.character(country_label))) %>%
    ungroup %>% 
    transmute(FIPS, ISO2, ISO3, UN, NAME, AREA, POP2005, REGION, SUBREGION, LON, LAT, country_label) %>% 
    unique
  
  pal <- colorFactor(palette = c("#2974B4", "#F7941E",  "#BACB33"),                               # previewColors(pal, c("País", "Socios", "No-socios"))
                     levels = c("País", "Socios", "No-socios"))
  leaflet(mapa,
          options = leafletOptions(minZoom = 1.5)) %>%
    addTiles() %>% 
    addPolygons(stroke = TRUE, 
                # smoothFactor = 0.2, 
                fillOpacity = 0.8,
                weight = 1,
                fillColor = ~pal(country_label),
                label = ~NAME,
                layerId = ~NAME,
                color =  "#BACB33") %>% 
    # label = labels) #%>%
    addLegend("topright", pal = pal, values = ~country_label,
              title = "Programas",
              opacity = 1)
}

## User Interface - UI

dbheader <- dashboardHeader(
  title = "Análisis comercio internacional",
  titleWidth = 450,
  tags$li(a(href = 'https://www.transformauruguay.gub.uy',
            img(src = "https://www.transformauruguay.gub.uy/media/images/logo_sntpc.svg?timestamp=20170908142947",
                height="30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"))

sidebar <- dashboardSidebar(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")),
  disable = FALSE,
  sidebarMenu(menuItem('Acuerdos comerciales', tabName = 'acuerdos', icon = icon('info'))
              ))

body <- dashboardBody(
  estilo_tu,
  tabItems(
    tabItem(tabName = "acuerdos",
            fluidRow(tabBox(title = "Alianzas comerciales",
                            width = 12,
                            tabPanel(title = "Socios",
                                     fluidPage(column(6, 
                                                      selectInput('acuerdos.1', 'Seleccione país:', choices = PAIS.ACUERDOS, selected = 'AUSTRALIA'))),
                                     fluidPage(column(2),
                                               column(9,
                                                      withSpinner(
                                                        leafletOutput("map", height="600px", width = "1000px"),
                                                        absolutePanel(top = 10, right = 10, fixed = TRUE,
                                                                      tags$div(style = "opacity: 0.50; background: #2974B4; padding: 5px; ",
                                                                               helpText("Alianzas comerciales"), textOutput("text"))),
                                                        type = 4, color = "#F7941E", size = 0.4),
                                                      br(), br(), br(), br())),
                                     ########### 
                                     fluidPage(column(7),
                                               column(5, uiOutput('acuerdos.2'), br())),
                                     ########### 
                                     
                                     fluidPage(column(6, dataTableOutput("tabla_acuerdos1")),
                                               column(1),
                                               column(5, dataTableOutput("tabla_acuerdos2")))
                                     ))))
  
    ))


ui <- dashboardPage(dbheader, sidebar, body,
                     skin = "black")

# Server
server <- function(input, output) {
  
  
  ########### Opción 1: con selector de acuerdo
  output$acuerdos.2 <- renderUI ({
    acuerdos.3 <-
      WTO %>%
      filter(members == input$acuerdos.1) %>%
      pull(var = Agreement) %>%
      unique()
    pickerInput('acuerdos.4', 'Seleccione acuerdo:',
                choices = acuerdos.3,
                options = list(`actions-box` = TRUE),
                selected = acuerdos.3[],
                multiple = TRUE)
  })

  dataset_acuerdos2 <- reactive({
    WTO %>%
      filter(Agreement %in% input$acuerdos.4) %>%
      transmute(Agreement, members) %>%
      filter(members != input$acuerdos.1) %>%
      arrange(members) %>%
      unique
  })

  output$tabla_acuerdos2 <- renderDataTable({
    datatable(
      dataset_acuerdos2() %>%
        rename(Acuerdos = Agreement,
               `Países miembros` = members) %>%
        arrange(Acuerdos),
      rownames = FALSE,
      filter = "top",
      options = list(
        paging = TRUE,
        searching = TRUE,
        columnDefs = list(list(
          className = 'dt-center',
          targets = 1))))
  })
  
  
  dataset_map_pais <- reactive({
    WTO %>% 
      filter(members == as.character(input$acuerdos.1)) %>% 
      mutate(country_label = "País") %>% 
      mutate(country_label = as.factor(country_label)) %>%
      select(-Agreement, -Date, -year, -Type, -provisions, -AC, -LE, -class) %>%
      unique
  })
  
  dataset_map_socio <- reactive({
    WTO %>% 
      left_join(dataset_acuerdos2() %>% 
                  mutate(country_label = "Socios"),
                by = c("members", "Agreement")) %>%
      mutate(country_label = as.factor(country_label)) %>%
      select(-Agreement, -Date, -year, -Type, -provisions, -AC, -LE, -class) %>%
      filter(is.na(country_label) != TRUE) %>% 
      unique
  })
  
  dataset_map <- reactive({
    dataset_map_pais() %>% 
      rbind(dataset_map_socio())
  })
  
  output$map <- renderLeaflet({
    choropleth_world(dataset_map())
  })
  
  ##
  
  
}

# Run app
shinyApp(ui, server)

