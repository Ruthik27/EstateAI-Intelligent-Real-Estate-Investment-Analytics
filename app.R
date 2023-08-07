
#install.packages("shiny")
#install.packages("leaflet")
#install.packages("readr")


#setwd("F:/SEM3/STAT6365/FINAL_PROJECT/Airbnb_Perman/")

library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)


# Define UI for the application
ui <- fluidPage(
  includeCSS("./www/css/dark.css"),
  titlePanel("| Airbnb Dashboard 2023 |"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Select city:", choices = "Asheville"),
      sliderInput("log_price", "Price Range", min = 0, max = 3000, value = c(0, 3000)),
      radioButtons("host_identity_verified", "Is host identity verified?", choices = c("Verified", "Not Verified"), selected = "Not Verified"),
      numericInput("accommodates", "accommodates:", value = 1, min = 1, max = 16),
      radioButtons("cleaning_fee", "Cleaning Fee:", choices = c("Applicable", "Not Applicable"), selected = "Applicable"),
      selectInput("property_type", "Property Type:", choices = c("All", "Apartment", "Bed & Breakfast", "House", "Condominium", "Townhouse", "Camper/RV", "Dorm", "Guesthouse", "Loft", "Bungalow", "Boutique hotel", "Other", "Villa", "Guest suite", "Hostel"), multiple = TRUE, selected = c("All")),
      selectInput("bed_type", "Bed Type:", choices = c("All", "Real Bed", "Couch", "Futon", "Pull-out Sofa", "Airbed"), multiple = TRUE, selected = c("All")),
      selectInput("room_type", "Room Type:", choices = c("All", "Shared room", "Entire home/apt", "Private room", "Hotel room"), multiple = TRUE, selected = c("All")),
      sliderInput("bathrooms", "Bathrooms Available Size :", min = 0, max = 5, value = 5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Description", 
                 h3("Information"),
                 tags$hr(),
                 p("The Shiny app provides an interactive dashboard for exploring Airbnb data for the year 2023. Users can select a city and a price range to filter the available listings. The dashboard prominently features a histogram that displays the distribution of log-transformed prices, with customizable bin sizes, and is color-coded with a gradient from purple to blue. Accommodations are also visually represented, and the dark-themed user interface, accented with shades of purple, enhances visual appeal. Overall, the app offers a comprehensive and user-friendly way to analyze and understand trends in Airbnb pricing and accommodation types."),
                 tags$hr(),
                 p("For best results:"),
                 p("Use full screen."),
                 p("You can zoom into the map to find the exact Airbnb and click on any location."),
                 p("This action will display details about the specific Airbnb listing below the map."),
                 p("Try to make some edits in the Filter section when you shift to 'Geo Plots and Details' Tab - to reload the map locations, as they are not in Dynamic view.")
        ),
        tabPanel("Geo Plots and Details",
                 h3("Plots and Filters"),
                 leafletOutput("map"),
                 tags$hr(),
                 tags$div(textOutput("total_locations"), style = "font-family: Cambria; font-size: 15px; text-align: center"),
                 tags$hr(),
                 wellPanel(
                   titlePanel("Selected Hotel Details:"),
                   tags$div(
                     class = "flex-container",
                     style = "display: flex; justify-content: space-between;",
                     tags$div(
                       style = "width: 70%;",  # Adjust this as needed
                       textOutput("selected_name"),  # Individual div for Name
                       textOutput("selected_id"),  # Individual div for ID
                       textOutput("selected_neighbourhood"),  # Individual div for Neighbourhood
                       textOutput("selected_log_price"),  # Individual div for Price
                       textOutput("selected_zipcode"),  # Individual div for Minimum Nights Stay
                       textOutput("selected_review_scores_rating"),  # Individual div for Review Scores Rating
                       textOutput("selected_amenities")  # Individual div for Amenities
                     ),
                     tags$div(
                       uiOutput("selected_thumbnail_url"),  # Individual div for Thumbnail URL
                     )
                   )
                 )
        ),
        tabPanel("Price Distribution and Accommodations",
                 h3("Additional Filters"),
                 plotlyOutput("barplot"), # Bar plot for price distribution
                 tags$hr(),
                 textOutput("highest_price"), # Text output for highest price
                 tags$hr(),
                 plotlyOutput("accommodation_plot") # Plotly output for accommodation distribution
        )
      )
    )
  )
)

# Define server logic required to draw a map
server <- function(input, output, session) {
  
  # Assuming you have a CSV file named "output_file.csv" with the necessary columns.
  data <- read.csv("./data/short.csv")
  

  
  
  # Populate city selection dropdown with unique city names
  updateSelectInput(session, "city", choices = unique(data$city))
  
  data$cleaning_fee <- as.logical(data$cleaning_fee)
  #data$host_identity_verified <- as.logical(data$host_identity_verified)
  
  data$host_identity_verified <- data$host_identity_verified == "t"
  

  data$cleaning_fee <- as.logical(data$cleaning_fee)
  data$host_identity_verified <- data$host_identity_verified == "t"
  
  
  
  # Create a reactive subset of data based on selected city, price range, minimum nights, room type, and availability
  # Create a reactive subset of data based on selected city, price range, minimum nights, room type, and availability
  reactive_subset <- reactive({
    req(input$city)
    filtered_data <- data %>%
      filter(city == input$city) %>%
      filter(log_price >= input$log_price[1] & log_price <= input$log_price[2]) %>%
      filter(accommodates >= input$accommodates) %>%
      filter(bathrooms <= input$bathrooms)
    
    if (!"All" %in% input$room_type) {
      filtered_data <- filtered_data %>%
        filter(room_type %in% input$room_type)
    }
    
    if (!"All" %in% input$bed_type) {
      filtered_data <- filtered_data %>%
        filter(bed_type %in% input$bed_type)
    }
    
    if (!"All" %in% input$property_type) {
      filtered_data <- filtered_data %>%
        filter(property_type %in% input$property_type)
    }
    
    # Adjusted cleaning_fee filter
    if (input$cleaning_fee == "Applicable") {
      filtered_data <- filtered_data %>%
        filter(cleaning_fee == TRUE)
    } else if (input$cleaning_fee == "Not Applicable") {
      filtered_data <- filtered_data %>%
        filter(cleaning_fee == FALSE)
    }
    
    if (input$host_identity_verified == "Verified") {
      filtered_data <- filtered_data %>%
        filter(host_identity_verified == TRUE)
    } else if (input$host_identity_verified == "Not Verified") {
      filtered_data <- filtered_data %>%
        filter(host_identity_verified == FALSE)
    }
    
    
    

    return(filtered_data)
  })
  
  #################################################
  output$map <- renderLeaflet({
    leaflet_data <- reactive_subset()
    leaflet(leaflet_data) %>%
      setView(lng = mean(leaflet_data$longitude), lat = mean(leaflet_data$latitude), zoom = 4) %>%
      addTiles()
  })
  
  output$barplot <- renderPlotly({
    leaflet_data <- reactive_subset()
    
    # If no data is available, return an empty plot
    if(nrow(leaflet_data) == 0) {
      plotly_empty()
      return()
    }
    
    # Create a histogram using Plotly
    plot_ly(leaflet_data, x = ~log_price, type = "histogram", nbinsx = 60,  # Double the number of bins
            marker = list(color = colorRampPalette(c("lightblue", "purple"))(60))) %>%
      layout(title = "Price Distribution",
             font = list(color = "white"),
             paper_bgcolor = "#383838",
             plot_bgcolor = "black",
             xaxis = list(title = "Log Price"),
             yaxis = list(title = "Frequency"))
  })
  

  output$highest_price <- renderText({
    leaflet_data <- reactive_subset()
    highest_price <- max(leaflet_data$log_price, na.rm = TRUE)
    paste("Highest Price:", highest_price)
  })
  
  ##############################################
  

  output$accommodation_plot <- renderPlotly({
    leaflet_data <- reactive_subset()
    
    # If no data is available, return an empty plot
    if(nrow(leaflet_data) == 0) {
      plotly_empty()
      return()
    }
    
    # Create a table of accommodations
    accommodations <- table(leaflet_data$accommodates)
    
    # Create a pie chart using plotly
    plot_ly(
      labels = names(accommodations), 
      values = as.numeric(accommodations), 
      type = "pie", 
      marker = list(colors = colorRampPalette(c("lightblue", "purple"))(length(accommodations))),
      textinfo = "label+percent",
      insidetextorientation = "radial"
    ) %>%
      layout(title = "Accommodation Distribution",
             font = list(color = "white"),
             paper_bgcolor = "#383838",
             plot_bgcolor = "black")
  })
  
  
  
  #################################################
  
  
  # Update markers on map when subset changes
  observe({
    leaflet_data <- reactive_subset()
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = leaflet_data, 
                 ~longitude, 
                 ~latitude, 
                 popup = ~name, 
                 group = "Locations",
                 layerId = ~id)  # assigning layerId as id of the location
  })
  
  output$total_locations <- renderText({
    leaflet_data <- reactive_subset()
    total_options <- nrow(leaflet_data)
    paste(total_options, "options available according to given search criteria.")
  })
  
  # Initialize the outputs
  output$selected_name <- renderText("")
  output$selected_id <- renderText("")
  output$selected_neighbourhood <- renderText("")
  output$selected_log_price <- renderText("")
  output$selected_zipcode <- renderText("")
  output$selected_thumbnail_url <- renderText("")
  output$selected_review_scores_rating <- renderText("")
  output$selected_amenities <- renderText("")
  
  
  # Show selected location details when a marker is clicked
  
  observeEvent(input$map_marker_click, {
    marker_id <- input$map_marker_click$id
    selected <- reactive_subset()[which(reactive_subset()$id == marker_id),]
    output$selected_name <- renderText(paste("Name:", selected$name))  # Display Name
    output$selected_id <- renderText(paste("ID:", selected$id))  # Display ID
    output$selected_neighbourhood <- renderText(paste("Neighbourhood:", selected$neighbourhood))  # Display Neighbourhood
    output$selected_log_price <- renderText(paste("Price:", selected$log_price))  # Display Price
    output$selected_zipcode <- renderText(paste("Zipcode:", selected$minimum_nights))  # Display Price
    output$selected_thumbnail_url <- renderUI({
      if(!is.null(input$map_marker_click)){
        marker_id <- input$map_marker_click$id
        selected <- reactive_subset()[which(reactive_subset()$id == marker_id),]
        if(!is.null(selected$thumbnail_url)){
          tags$img(src=selected$thumbnail_url, height=300, width=300)
        }
      }
    })    
    output$selected_review_scores_rating <- renderText(paste("Reviews Ratings:", selected$review_scores_rating))
    output$selected_amenities <- renderText(paste("Amenities:", selected$amenities))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

