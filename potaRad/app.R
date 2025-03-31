#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(potaplan)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
# hamcall <- "KE4MKG"
# 
# parksRefs <- "US-2177"
# distance <- 100
# grid = "EM83aw"
# 
# dfx <- ParksRad(grid, distance)
# 
# activity_dfx <-
#   lapply(parksRefs, function(x) getWorkCount(hamcall, x)) |>
#   dplyr::bind_rows()
# 
# activity_dfx <- activity_dfx |> dplyr::rowwise() |>
#   dplyr::mutate(total = cw + data + phone,
#                 activated = activations > 0)
# 
# dfx$parks_filtered <- merge(dfx$parks_filtered, activity_dfx)


#map <- potaplanner_radius(hamcall = "KE4MKG", grid = "EM83aw", distance = 100)
  
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Pota Planner"),

    
    sidebarLayout(
        sidebarPanel(
            sliderInput("radius",
                        "Radius of search:",
                        min = 1,
                        max = 500,
                        value = 100),
            textInput("call",
                        "Callsign:",
                        value = "KE4MKG"),
            textInput("maidenhead",
                      "Maidenhead (up to 6 digits):",
                      value = "EM83aw"),
            actionButton("recalc", "New points")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           p(),
           leafletOutput("mymap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dfx <- eventReactive(input$recalc, {
  hamcall = input$call
  grid = input$maidenhead
  distance = input$radius
  
  
  
  dfx = ParksRad(grid, distance)
  #library(leaflet)
  
  hamcall <- toupper(hamcall)
  
  parksRefs <- dfx$parks_filtered$properties.reference
  
  activity_dfx <- 
    lapply(parksRefs, function(x) getWorkCount(hamcall, x)) |>
    dplyr::bind_rows()
  
  activity_dfx <- activity_dfx |> dplyr::rowwise() |>
    dplyr::mutate(total = cw + data + phone,
                  activated = activations > 0)
  
  dfx$parks_filtered <- merge(dfx$parks_filtered, activity_dfx)

  
  }, ignoreNULL = FALSE)
  

  output$mymap <- renderLeaflet({
    dfx1 <- dfx()
    #browser()
    leaflet::leaflet(width = 700) |>
      leaflet::addProviderTiles("OpenStreetMap.Mapnik") |>
      leaflet::addCircleMarkers(
        data = dfx1[dfx1$activated,],
        color = "#377eb8",
        fillOpacity = 0.6,
        radius = ~sqrt(activations) + 3,
        popup = ~ paste0(properties.name, " activations = ", activations)
      ) |>
      leaflet::addCircleMarkers(
        data = dfx1[!dfx1$activated,],
        color = "#e41a1c",
        radius = 4,
        fillOpacity = 0.6,
        popup = ~ properties.name
      ) 
      
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
