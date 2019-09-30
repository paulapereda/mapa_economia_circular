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
header <- dashboardHeader(title = "", titleWidth = 0)

# Sidebar content
sidebar <- dashboardSidebar(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")),
  sidebarMenu(
    menuItem(text = "Mapamundi", tabName = "Mapamundi", icon = icon("map")),
    menuItem(text = "En Uruguay", tabName = "en_uruguay", icon = icon("table")))
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