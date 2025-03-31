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
library(DT)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Pota Planner - Parks within Radius"),
  
  
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),
      helpText("1. Enter your callsign and Maidenhead grid location."),
      helpText("2. Set the search radius in kilometers."),
      helpText("3. Click 'Get Parks' to find POTA locations."),
      helpText("4. View results on the map and in the table. Clicking on map dots will show park and total activations. Red indicates park is not activated by call sign. You can sort the table using arrow controls at top of columns."),
      helpText("5. Click 'Download CSV' to save the data."),
      hr(),
      sliderInput(
        "radius",
        "Radius of search (km):",
        min = 10,
        max = 200,
        value = 50
      ),
      textInput("call", "Callsign:", value = "KE4MKG"),
      textInput("maidenhead", "Maidenhead (up to 6 digits):", value = "EM83aw"),
      actionButton("recalc", "Get Parks"),
      hr(),
      downloadButton("downloadData", "Download CSV")
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(leafletOutput("mymap"), DTOutput('tbl'))
  ))

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

  output$tbl = renderDT({
    dfx1 <- dfx()
    
    dfx1$distance_km <-signif(dfx1$distance_km,3)
    dfx2 <- as.data.frame(dfx1)[,!names(dfx1) == "geometry"]
    datatable(dfx2, options = list(lengthChange = TRUE))
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ParksData-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      dfx1 <- dfx()
      dfx1$distance_km <- signif(dfx1$distance_km, 3)
      dfx2 <- as.data.frame(dfx1)[,!names(dfx1) == "geometry"]
      write.csv(dfx2, file, row.names = FALSE)
    }
  )
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
