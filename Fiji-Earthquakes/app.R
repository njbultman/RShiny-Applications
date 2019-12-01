#Define necessary packages for application
if(!require("pacman")) install.packages("pacman")
pacman::p_load(leaflet,
               shiny,
               htmlwidgets,
               tidyverse,
               shinycssloaders,
               datasets)

# Define UI
ui <- navbarPage(title = "Fiji Earthquakes Analysis",
    
    tabPanel(title = "Map",

      sidebarLayout = NULL,
      
        mainPanel(
           leafletOutput("quakemap") %>%
               withSpinner()
      )
    ),
  
  tabPanel("Relationship Testing",
      
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput(inputId = "xaxis",
                    label = "X Axis",
                    choices = c("Depth" = "depth",
                                "Magnitutde" = "mag",
                                "Stations" = "stations"),
                    selected = "mag"),
      
        selectInput(inputId = "yaxis",
                    label = "Y Axis",
                    choices = c("Depth" = "depth",
                                "Magnitutde" = "mag",
                                "Stations" = "stations"),
                    selected = "stations"),
        
        selectInput(inputId = "color",
                    label = "Color",
                    choices = c("Depth" = "depth",
                                "Magnitutde" = "mag",
                                "Stations" = "stations"),
                    selected = "depth")
      ),
      mainPanel(
        plotOutput(outputId = "scatter") %>% withSpinner()
      )
    )
  )
)

# Define server
server <- function(input, output) {
    
    #Load data
    data <- read.csv("./data/quakes.csv")

    #Interactive Map
    output$quakemap <- renderLeaflet({
            
            leaflet() %>%
                addProviderTiles(provider = "Stamen.Watercolor") %>%
                addCircles(lng = data$long, lat = data$lat)

    })
    
    #Scatter plot
    output$scatter <- renderPlot({
      
      ggplot(data, aes_string(x = input$xaxis, y = input$yaxis, color = input$color)) +
        geom_point()
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
