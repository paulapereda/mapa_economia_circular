library(shinydashboard)
library(htmltools)
library(estilotu)
library(leaflet)
library(shiny)

tor::load_rds('data/')

## ui.R ##

## UI CONFIG

## Header
header <- dashboardHeader(
                          #title = "Estrategias de Economía Circular",
                          titleWidth = 400)

# Sidebar content
sidebar <- dashboardSidebar(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")),
  sidebarMenu(
    menuItem(text = "Lugares", tabName = "Lugares", icon = icon("dashboard")),
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
      tabItem(tabName = "Lugares",
              selected = TRUE, 
              leafletOutput('map', height = 700)
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

global_popups <- paste0("<b>", proyectos_ec$country, "</b>", "<br/>",
                        proyectos_ec$concepto
)

server <- function(input, output) { 
  
  output$map <- renderLeaflet({
    leaflet(data = proyectos_ec, options = leafletOptions(maxZoom = 8)) %>% 
      addTiles() %>%
      addAwesomeMarkers(~lon, ~lat, icon = icons, popup = global_popups)
  })

  
  
}


shinyApp(ui, server)