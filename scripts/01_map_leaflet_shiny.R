library(shiny)
library(shinyWidgets)
library(magrittr)
library(leaflet)
library(DT)
library(dplyr)
library(stringr)

# read in fellow data
fellows <-
  read.csv(
    "/Users/nathalie_gerstner/Documents/ownCloud/Map_JHS_AddOnFellows/data/fellow_network_new.csv",
    sep = ";",
    dec = ","
  )


# format popup text
fellows <- fellows %>% 
  mutate(popup_text = paste(sep = "<br/>",
                            "<b>",fellows$Name,"</b>",
                            fellows$Jahrgang,
                            fellows$Affiliation_Institut, 
                            fellows$Profile))
# add column to group alumni and current fellows
fellows <- fellows %>%
  mutate(Status = if_else(str_detect(Affiliation_Institut, "Alumn"), 
                          true = "Alumni", 
                          false = "Current Fellow"))
# Create a palette that maps factor levels to colors
pal <- colorFactor(c("navy", "red"), domain = c("Alumni", "Current Fellow"))


ui <- shinyUI(fluidPage(
  titlePanel("Joachim Herz Add-on Network"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 pickerInput(
                   "cohort",
                   label = "Cohort",
                   choices = unique(fellows$Jahrgang),
                   selected = unique(fellows$Jahrgang),
                   options = list(`actions-box` = TRUE),
                   multiple = T
                 ), 
                 pickerInput(
                   "status",
                   label = "Add-on Fellow Status",
                   choices = unique(fellows$Status),
                   selected = unique(fellows$Status),
                   options = list(`actions-box` = TRUE),
                   multiple = T
                 ), 
                 searchInput(
                   inputId = "city",
                   label = "City", 
                   placeholder = "Enter a city name",
                   btnSearch = icon("search"), 
                   btnReset = icon("remove"),
                   width = "100%"
                 ),
                 searchInput(
                   inputId = "interest",
                   label = "Research Interest", 
                   placeholder = "Enter a research topic",
                   btnSearch = icon("search"), 
                   btnReset = icon("remove"),
                   width = "100%"
                 )
                 ),
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel(
        "Add-on Fellow Locations",
        leafletOutput("leafletmap", width = "1000px", height = "500px"),
        dataTableOutput("tbl")
      )
    ))
  )
))



server <- function(input, output) {
 
  map_data_react <- reactive({
    
    fellows <- fellows %>% 
      filter(Jahrgang %in% input$cohort) %>%
      filter(Status %in% input$status) 
    
    if(input$city != ""){
      fellows <- fellows %>%
        filter(str_detect(Affiliation_Institut,input$city))
    }
    
    if(input$interest != ""){
      fellows <- fellows %>%
        filter(str_detect(Profile,input$interest))
    }
    
    return(fellows)
    
  })
  
  
  output$leafletmap <- renderLeaflet({
    
    fellow_data <- map_data_react()  # Add this
    
    fellow_data %>% leaflet() %>%
      # addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # addTiles %>%
      addCircleMarkers(
        ~ longitude , 
        ~ latitude,
        popup =  ~ popup_text,
        color = ~ pal(Status),
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.8,
        popupOptions = popupOptions(closeButton = FALSE),
        clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)
      ) %>%
      addLegend(pal = pal, 
                values = ~Status, 
                opacity = 1,
                title = "Add-on Fellow Status")
  })
  
  output$tbl <- DT::renderDataTable({
    DT::datatable(
      map_data_react() %>%
        select(Name, Jahrgang, Affiliation_Institut, Affiliation_Stadt, Profile),
      extensions = "Scroller",
      style = "bootstrap",
      class = "compact",
      width = "100%",
      options = list(
        deferRender = TRUE,
        scrollY = 300,
        scroller = TRUE,
        dom = 'tp'
      )
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
