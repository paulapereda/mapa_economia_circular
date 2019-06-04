library(shinydashboard)
library(htmltools)
library(leaflet)
library(shiny)

tor::load_rds('data/')

## ui.R ##

## UI CONFIG

## Header
header <- dashboardHeader(title = "Estrategias de Economía Circular")

# Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Lugares", tabName = "Lugares", icon = icon("dashboard")),
    menuItem(text = "About", tabName = "about", icon = icon("heart"))
  )
)

## Body content
body <-   
  dashboardBody(
    tabItems(
      
      # Front Page
      
      # First sidebar tab - R-Ladies
      tabItem(tabName = "Lugares",
              selected = TRUE, 
              leafletOutput('map', height = 700)
      ),
      
      tabItem(tabName = "about",
              
              fluidPage(
                h1(strong("About:")),
                p("This app was developed by ",
                  a("R-Ladies.", href = "http://www.rladies.org"), 
                  "You can find the source code",
                  a("here.", href = "https://github.com/rladies/rshinylady")),
                
                img(src = "R-LadiesGlobal_RBG_online_LogoWithText.png", height = 300, width = 300)
                
              )
      )))




ui <- dashboardPage(skin = "purple", header, sidebar, body)

icons <- awesomeIcons(icon = "whatever",
                      iconColor = "black",
                      library = "ion",
                      markerColor = "green")



server <- function(input, output) { 
  
  output$map <- renderLeaflet({
    leaflet(data = proyectos_ec) %>% 
      addTiles() %>%
      addAwesomeMarkers(~lon, ~lat, icon = icons)
  })

  
  
}


shinyApp(ui, server)