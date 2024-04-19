library(shiny)
library(readr)
library(leaflet)

# List CSV files in directory
file_list <- list.files("~/Spring2023/Weather/2020 Winter/", pattern = "\\.csv$", full.names = TRUE)

# Extract file names without extension
file_names <- tools::file_path_sans_ext(basename(file_list))

# Create an empty list to store data frames
weather_data_list <- list()

# Read each CSV file and store the data frame in the list
for (i in seq_along(file_list)) {
  weather_data_list[[file_names[i]]] <- read.csv(file_list[i])
}

# Update the city information with the top 20 most populated cities
city_info <- data.frame(
  CSV_File = file_names,
  City = c("New York City, NY", "Los Angeles, CA", "Chicago, IL", "Houston, TX", "Phoenix, AZ", 
           "Philadelphia, PA", "San Antonio, TX", "San Diego, CA", "Dallas, TX", "San Jose, CA", "Austin, TX",
           "Jacksonville, FL", "Fort Worth, TX", "Columbus, OH", "Charlotte, NC", "Indianapolis, IN",
           "Seattle, WA", "Denver, CO", "Washington D.C", "San Francisco, CA"),
  Lat = c(40.7128, 34.0522, 41.8781, 29.7604, 33.4484, 
          39.9526, 29.4241, 32.7157, 32.7767, 37.3382, 30.2672,
          30.3322, 32.7555, 39.9612, 35.2271, 39.7684,
          47.6062, 39.7392, 38.8951, 37.7749),
  Lon = c(-74.0060, -118.2437, -87.6298, -95.3698, -112.0740, 
          -75.1652, -98.4936, -117.1611, -96.7969, -121.8863, -97.7431,
          -81.6557, -97.3308, -82.9988, -80.8431, -86.1581,
          -122.3321, -104.9903, -77.0364,  -122.4194)
)


# Define UI for application
ui <- fluidPage(
  titlePanel("SAMPLE Predicted Performance in the Winter of 2020"),
  fluidRow(
    column(width = 4,
           # Dropdown menu for selecting cities
           selectInput("city", "Select a City", choices = setNames(city_info$City, city_info$City),
                       selected = city_info$City[1]),
           # Output plot
           plotOutput("plots", height = "600px")  # Adjust the height of the plots
    ),
    column(width = 8,
           # Leaflet map
           leafletOutput("map", height = "600px")  # Adjust the height of the map
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Get selected CSV file based on city selection
  selected_file <- reactive({
    city_info$CSV_File[city_info$City == input$city]
  })
  
  # Generate plots based on selected CSV file
  output$plots <- renderPlot({
    # Access data frame corresponding to selected file
    selected_data <- weather_data_list[[selected_file()]]
    
    # Convert temperature from Celsius to Fahrenheit
    selected_data$Temperature <- selected_data$Temperature * 9/5 + 32
    
    # Define initial variables and calculations
    n <- nrow(selected_data)
    E <- numeric(n)
    V <- numeric(n)
    Eused <- numeric(n)
    Eint <- numeric(n)
    Vint <- numeric(n)
    
    #Define Draws and Charges
    Boarddraw <- 0.064 #A
    Pumpdraw <- 0.092 #A
    RTdraw <- 0.08 #A
    Fandraw <- 0.388 #A
    SolarChargeRateC <- 763.25 #C
    SolarChargeRate <- 0.105 #V
    DischargeRate <- -0.0216 #V
    
    #Set Initial Charge and Voltage
    Vint[1] <- 14.16  #[V]
    Eint[1] <- 55535*Vint[1]/3600  #[W-hr]
    
    for (i in 1:(n - 1)) {  # Modified loop to avoid index out of bounds
      
      # Extract data
      amb_temp <- selected_data$Temperature[i]
      hour <- selected_data$Hour[i]
      dni <- selected_data$DNI[i]
      
      # Convert amb_temp to Fahrenheit
      amb_temp <- amb_temp * 9/5 + 32
      
      # Check for missing values
      if (is.na(amb_temp) || is.na(hour) || is.na(dni)) {
        next
      }
      
      # Calculate Ihour based on temperature
      if (amb_temp > 32 && amb_temp < 120) {
        Ihour <- Boarddraw + Pumpdraw + RTdraw
      } else if (amb_temp > 120) {
        Ihour <- Boarddraw + Pumpdraw + RTdraw + Fandraw 
      } else if (amb_temp < 32) {
        Ihour <- Boarddraw + Pumpdraw + RTdraw
      }
      
      # Check for solar charging
      if (dni > 0) {
        Chg <- SolarChargeRate*(dni/1000)
        EnChg <- SolarChargeRateC * Vint[i] / 3600
      } else {
        Chg <- 0
        EnChg <- 0
      }
      
      # Calculate energy used based on charge/discharge
      Eused[i] <- (Vint[i] * Ihour*t) #(W-hr)
      E[i] <- Eint[i] - Eused[i] + EnChg #(W-hr)
      
      # Check energy bounds
      E[i] <- ifelse(E[i] < 0, 0, E[i])
      
      # Update initial energy for the next iteration
      Eint[i + 1] <- E[i]
      
      # Calculate New Voltage
      Vint[i + 1] <- Vint[i] + DischargeRate + Chg
      
      # Check voltage bounds
      Vint[i + 1] <- ifelse(Vint[i + 1] > 14.16, 14.16, Vint[i + 1])
      Vint[i + 1] <- ifelse(Vint[i + 1] < 11.41, 0, Vint[i + 1])
      
      V[i] <- Vint[i + 1]
    }
    
    # Plotting temperature, energy consumption, and voltage
    par(mfrow = c(3,1))
    
    # Plot Temperature vs. Time
    plot(selected_data$Hour, selected_data$Temperature, type = 'l', col = 'red', 
         xlab = 'Time (hours)', ylab = 'Temperature (°F)',  # Changed label to Fahrenheit
         main = paste("Temperature vs. Time for", input$city),
         xlim = c(0, 335))
    
    # Plot Energy Consumption vs. Time
    plot(selected_data$Hour, E, type = 'l', col = 'blue', 
         xlab = 'Time (hours)', ylab = 'Energy Used (Watt-hours)',
         main = paste("Energy Consumption vs. Time for", input$city))
    
    # Plot Voltage vs. Time
    plot(selected_data$Hour, V, type = 'l', col = 'blue', 
         xlab = 'Time (hours)', ylab = 'Voltage (V)',
         main = paste("Voltage vs. Time for", input$city),
         ylim = c(11, 15))
  })
  
  # Generate map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -97, lat = 38, zoom = 4) %>%
      addCircleMarkers(data = city_info[city_info$City == input$city, ],
                       lng = ~Lon, lat = ~Lat, color = "red", fillOpacity = 1, radius = 6) %>%
      addCircleMarkers(data = city_info[-which(city_info$City == input$city), ],
                       lng = ~Lon, lat = ~Lat, color = "green", fillOpacity = 1, radius = 6)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
