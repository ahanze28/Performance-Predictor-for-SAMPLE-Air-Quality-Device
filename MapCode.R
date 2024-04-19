# Load required libraries
library(shiny)
library(leaflet)

# Define UI for application
ui <- fluidPage(
  titlePanel("AMOS Performance in Top 40 most populated U.S. Cities"),
  leafletOutput("map")
)

# Define server logic
server <- function(input, output) {
  output$map <- renderLeaflet({
    # Create a basic map of the United States
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = -98.5833, lat = 39.8333, zoom = 4)  # Set initial view to center of US
    
    # Data of the top 40 cities (latitude and longitude coordinates)
    cities <- data.frame(
      City = c("New York City", "Los Angeles", "Chicago", "Houston", "Phoenix", 
               "Philadelphia", "San Antonio", "San Diego", "Dallas", "San Jose",
               "Austin", "Jacksonville", "Fort Worth", "Columbus", "Charlotte",
               "San Francisco", "Indianapolis", "Seattle", "Denver", "Washington",
               "Boston", "El Paso", "Nashville", "Detroit", "Oklahoma City",
               "Portland", "Las Vegas", "Memphis", "Louisville", "Baltimore",
               "Milwaukee", "Albuquerque", "Tucson", "Fresno", "Sacramento",
               "Kansas City", "Long Beach", "Mesa", "Atlanta", "Colorado Springs"),
      Lat = c(40.7128, 34.0522, 41.8781, 29.7604, 33.4484, 
              39.9526, 29.4241, 32.7157, 32.7767, 37.3382,
              30.2672, 30.3322, 32.7555, 39.9612, 35.2271,
              37.7749, 39.7684, 47.6062, 39.7392, 38.8951,
              42.3601, 31.7619, 36.1627, 42.3314, 35.4676,
              45.5051, 36.1699, 35.1495, 38.2527, 39.2904,
              43.0389, 35.0844, 32.2226, 36.7372, 38.5816,
              39.0997, 33.7879, 33.4152, 34.0522, 38.8339),
      Lon = c(-74.0060, -118.2437, -87.6298, -95.3698, -112.0740, 
              -75.1652, -98.4936, -117.1611, -96.7969, -121.8863,
              -97.7431, -81.6557, -97.3308, -82.9988, -80.8431,
              -122.4194, -86.1581, -122.3321, -104.9903, -77.0364,
              -71.0589, -106.4850, -86.7816, -83.0458, -97.5164,
              -122.6750, -115.1398, -89.9711, -85.7585, -76.6122,
              -87.9065, -106.6504, -110.9747, -119.7871, -121.4944,
              -94.5786, -118.1892, -111.8315, -84.3880, -104.8214)
    )
    
    # Calculate the values for the first equation and store them in an array
    calculation_array_1 <- sapply(cities$Lat, function(lat) (lat * 0.9) - 23.5)
    
    # Set display values based on calculation_array_1 value
    display_values <- ifelse(abs(calculation_array_1) <= 15, 4,
                             ifelse(abs(calculation_array_1 - 30) <= 15, 3,
                                    ifelse(abs(calculation_array_1 - 60) <= 15, 2,
                                           ifelse(abs(calculation_array_1 - 90) <= 15, 1, NA))))
    
    # Replace NA with 0
    display_values[is.na(display_values)] <- 0
    
    # Add the display values to the cities data frame
    cities$Display <- display_values
    
    map <- map %>%
      addCircleMarkers(
        data = cities,
        lng = ~Lon,
        lat = ~Lat,
        popup = ~paste("City:", City, "<br>",
                       "Mount Setting:", Display),
        radius = 5,
        fillOpacity = 0.8,
        color = "green"  # Change dot color to green
      )
    
    map
  })
}

# Run the application
shinyApp(ui = ui, server = server)
