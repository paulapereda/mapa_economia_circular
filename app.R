library(shinydashboard)
library(tidyverse)
library(htmltools)
library(estilotu)
library(leaflet)
library(shiny)

proyectos_ec <- readRDS('data/proyectos_ec.rds') %>% 
  mutate(tamanio = 10) %>% 
  select(pais, tipo, lat, lon, concepto, tamanio) %>% 
  mutate(tipo = case_when(
                tipo == "Hoja de Rutas" ~ "Hoja de Ruta",
                T ~ tipo))

proyectos_ec_uy <- readRDS('data/proyectos_ec_uy.rds') 

proyectos_ec_ciudades <- readRDS('data/proyectos_ec_ciudades.rds') %>% 
  mutate(tamanio = 5) %>% 
  mutate(concepto = vision) %>% 
  select(- vision) %>% 
  mutate(pais = ciudad) %>% 
  select(pais, tipo, lat, lon, concepto, tamanio)

proyectos_ec <- proyectos_ec %>% 
  bind_rows(proyectos_ec_ciudades)

pal <- colorFactor(c(AZUL, VERDE, AMARILLO, NARANJA), domain = proyectos_ec$tipo)

## ui.R ##

## UI CONFIG

## Header
header <- dashboardHeader(title = "Uruguay: plan de acción de economía circular",
                          titleWidth = 500,
                          tags$li(a(href = 'https://www.transformauruguay.gub.uy',
                                    img(src = "https://www.transformauruguay.gub.uy/media/images/logo_sntpc.svg?timestamp=20170908142947",
                                        height="30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown")
                          )

# Sidebar content
sidebar <- dashboardSidebar(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")),
  sidebarMenu(
    menuItem(text = "Mapamundi", tabName = "Mapamundi", icon = icon("map")),
    menuItem(text = "En Uruguay", tabName = "en_uruguay", icon = icon("table")),
    menuItem(text = "About", tabName = "about", icon = icon("heart"))
  )
)

## Body content
body <-   
  dashboardBody(
    estilo_tu, 
    tabItems(
      
      # Front Page
      
      # First sidebar tab - R-Ladies
      
      tabItem(tabName = "Mapamundi",
              selected = TRUE, 
              leafletOutput('map', height = 700)
      ),
      
      tabItem(tabName = "en_uruguay",
              DT::dataTableOutput('table') 
      ),
      
      tabItem(tabName = "about",
              
              fluidPage(
                h1(strong("About")),
                p("Esta app fue desarrollada por ",
                  a("Tranforma Uruguay.", href = "https://www.transformauruguay.gub.uy/es/"), 
                  br(),
                  "Podés encontrar la fuente del código ",
                  a("aquí.", href = "https://github.com/paulapereda/mapa_economia_circular")))
      )))




ui <- dashboardPage(header, sidebar, body)

icons <- awesomeIcons(icon = "whatever",
                      iconColor = "black",
                      library = "ion",
                      markerColor = "green")

global_popups <- paste0("<b>", proyectos_ec$pais, "</b>", "<br/>",
                        proyectos_ec$concepto
)

server <- function(input, output) { 
  
  
  output$map <- renderLeaflet({
    leaflet(data = proyectos_ec, options = leafletOptions(maxZoom = 19)) %>% 
      addTiles() %>%
      addCircleMarkers(~lon, ~lat, 
                       color = ~ pal(tipo), 
                       popup = global_popups, 
                       radius = ~ tamanio,
                       stroke = FALSE,
                       fillOpacity = 1) %>%
      addLegend("bottomright",
                pal = pal, 
                values = ~ tipo,
                title = "Tipo",
                opacity = 1)
  })
  
  
  output$table <- DT::renderDataTable({
    DT::datatable(proyectos_ec_uy, 
                  rownames = FALSE)  
  })

}


shinyApp(ui, server)