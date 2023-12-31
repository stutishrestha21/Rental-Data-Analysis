library(tidyverse)
library(tidytext)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(leaflet)
library(caret)
library(class)
library(viridis)
rm(list = ls())

#setwd("")

df <- read.csv("data/hotels_motels.csv") %>%
  separate(the_geom, into = c("type", "longitude", "latitude"), sep = " ")

# remove parentheses from latitude and longitude columns
df$latitude <- gsub("\\(|\\)", "", df$latitude)
df$longitude <- gsub("\\(|\\)", "", df$longitude)

# convert latitude and longitude columns to numeric values
df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude) 


df$street_name <- sapply(str_split(df$BusinessAddress, "\\s"), function(x) {
  if (length(x) >= 3) {
    paste(x[2:(length(x) - 1)], collapse = " ")
  } else {
    ""
  }
})


df2 <- df %>%
  group_by(Zip, BusinessType) %>%
  summarise(NumBusinessType = n())

#Converting this date to year
df$Year <- str_sub(df$BusinessStartDate,1,4)

# Define the UI
ui <- fluidPage(
  titlePanel("Stays in New Orleans"),
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
               textOutput("intro_text")),
      tabPanel("Map of Hotels, Motels, and Rentals",
               fluidRow(
                 column(12, textOutput('map_comment')),
                 column(12, leafletOutput("map"))
               )),
      tabPanel("Count by Business Type",
               fluidRow(
                 column(12, textOutput('busn_type_comment')),
                 column(12, plotOutput("busn_type_plot", width = '800px',
                                       height = '600px'))
               )),
      tabPanel("Business Start Date",
               fluidRow(
                 column(12, textOutput('start_date_comment')),
                 column(12, plotOutput("start_date", width = '800px',
                                       height = '600px'))
               )),
      tabPanel("Major Players in the Business",
               fluidRow(
                 column(12, textOutput('major_players_comment')),
                 column(12, plotOutput("major_players", width = '800px',
                                       height = '600px'))
               )),
      tabPanel("Popular zip code",
               fluidRow(
                 column(12, textOutput('zip_comment')),
                 column(3,
                        selectInput(
                          "Year_select","Select a year",
                          choices = c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
                                      "2010","2011","2012","2013","2014","2015","2016","2017","2018","2019",
                                      "2020","2021","2022","2023"))),
                 column(9, plotOutput("zip_data"))
               )),
      tabPanel("LLM Prediction Model",
               fluidRow(
                 column(12, textOutput("llm_model")),
                 column(12, verbatimTextOutput("llm_citation"))
               )),
      tabPanel("Our Prediction Model",
               fluidRow(
                 column(12, textOutput('knn_model_comment')),
                 column(6,textInput("latitude_input", "Latitude:",
                                    placeholder = "Enter latitude...")),
                 column(6, textInput("longitude_input", "Longitude:",
                                     placeholder = "Enter longitude..."))),
               fluidRow(
                 column(12, actionButton("predict_button", "Predict"))),
               fluidRow(
                 column(12, verbatimTextOutput("predicted_output"))
                 )),
      tabPanel("Map of ZIP Codes",
               fluidRow(
                 column(12, textOutput('map2_comment'))),
               fluidRow(
                 column(12, leafletOutput("map2")))),
      tabPanel("Number of Businesses on Each Street",
               fluidRow(
                 column(12, textOutput('business_graph_comment')),
              fluidRow(
        # Street name selection
                column(12, selectInput("street_select", "Select Street Name",
                                       choices = unique(df$street_name)),
                column(12, plotOutput("business_graph"))))))
  )))   

# Define the server
server <- function(input, output) {
  
  #Intro paragraph explaining project
  output$intro_text <-renderText({
    "
    
    
   
   We found the data on data.gov. It contains information on hotels and motels in the LA neighborhood of New Orleans. 
   We created a shiny app to begin the analysis. 
   We divided latitude and longitude, which made it easier for us to locate various places on the map. 
   To determine the number of hotels and motels operating there, we then counted different company categories and created a bar graph. 
   We used a company start date and kept track of how many companies launched over the course of a decade in order to conduct further analyses. 
   To determine the terms that were utilized in the business type, we used emotional analysis. 
   The most repeated words were then counted once we had separated the words. 
   When we examine the top 5 companies in the Major Player in the Business graph, 
   we can see that Sonder is the largest chain when compared to the others. Additionally, 
   we developed a prediction model based on latitude and longitude that requests the user's input in order 
   to inform them of the location of each organization.
   


"
  })
  
  # Count by busn type
  output$busn_type_plot <- renderPlot({
    # pivot showing count of each busn type
    busn_type <- df %>%
      group_by(BusinessType) %>%
      summarise(count = n())
    
    ggplot(busn_type, aes(x = BusinessType, y = count)) +
      geom_bar(stat = "identity",fill="deeppink3",color="black") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,
                                       size = 10),
            plot.title = element_text(size = 20)) +
      labs(x = "Business Type", y = "Count", title = "Business Type Counts") +
      ggtitle("Business Type Counts")
    
  })
  
  # Map of Places
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = df, lat = ~latitude, lng = ~longitude)
  })
  
  # Business Start Date
  output$start_date <- renderPlot({
    start_date <- df %>%
      mutate(Year = lubridate::year(BusinessStartDate)) %>%
      group_by(Year, BusinessType) %>%
      summarise(count = n()) %>%
      arrange(Year, BusinessType) %>%
      group_by(BusinessType) %>%
      mutate(cumulative_count = cumsum(count))
    
    ggplot(start_date, aes(x = Year, y = cumulative_count,
                           color = BusinessType, group = BusinessType)) +
      geom_line(linewidth = 1.1) +
      scale_color_viridis(discrete = TRUE, option = "H") +
      labs(x = "Business Start Date", y = "Count") +
      ggtitle("Trend of New Business Start Dates")
  })
  
  # The Major Players in NOLA
  output$major_players <- renderPlot({
    word_counts <- df %>%
      unnest_tokens(word, BusinessName) %>%
      count(word) %>%
      arrange(desc(n))
    
    string_values <- c('sonder', 'marriott', 'hilton', 'holiday', 'hyatt')
    majors <- subset(word_counts, word %in% string_values)
    
    ggplot(majors, aes(x = word, y = n)) +
      geom_bar(stat = "identity",fill= "peachpuff3", color="peachpuff3")
  })
  
  
  
  # Popular zip code and start date (which zip code got popular??)
  output$zip_data <- renderPlot({
    popular_zip <- df %>%
      filter(Year %in% input$Year_select)%>%
      group_by(Zip, BusinessStartDate) %>%
      summarise(count = n())
    
    ggplot(popular_zip, aes(x = Zip, y = count)) +
      geom_bar(stat = "identity",fill="pink",color="gray") +
      ggtitle("Count of Hotels in Each Zip Code")+
      labs(title = "Interactive Bar Chart", x = "Zip Code", y = "Count") +
      theme_minimal()
  })
  
  
  #Model
  
  
  output$llm_model <- renderText({
    data <- df
    data$year <- substr(data$BusinessStartDate, 1, 4)
    
    # Split data into training and testing sets
    set.seed(123)
    trainIndex <- createDataPartition(data$BusinessType, p = 0.8, list = FALSE)
    training <- data[trainIndex, ]
    testing <- data[-trainIndex, ]
    
    # Split the response variables
    training$latitude <- as.numeric(training$latitude)
    training$longitude <- as.numeric(training$longitude)
    
    # Convert 'year' to character in both training and testing datasets
    training$year <- as.numeric(training$year)
    testing$year <- as.numeric(testing$year)
    
    # Train the model separately for latitude and longitude
    model_latitude <- train(latitude ~ year + BusinessType,
                            method = "rpart",
                            data = training)
    
    model_longitude <- train(longitude ~ year + BusinessType,
                             method = "rpart",
                             data = training)
    
    # Predict the future locations for each BusinessType
    predictions_latitude <- predict(model_latitude, newdata = testing)
    predictions_longitude <- predict(model_longitude, newdata = testing)
    
    # Specify the latitude and longitude
    lat <- predictions_latitude[1]
    lng <- predictions_longitude[1]
    
    # Create a string with the values of lat and lng
    result <- paste("Latitude: ", lat, "\nLongitude: ", lng)
    
    # Return the result as the output text
    return(result)
  })
  output$llm_citation <- renderText({
    paste('APA Citation from Bing AI',
    '"Built In. (n.d.). Regression Trees: How to Get Started. Built In.
    https://builtin.com/data-science/regression-tree"',
    '"Carnegie Mellon University. (n.d.). Lecture 10: Regression Trees.
    Carnegie Mellon University. https://www.stat.cmu.edu/~cshalizi/350-2006/lecture-10.pdf"',
    'Explanation',
    '"Regression trees are a variant of decision trees that aim to
    predict outcomes we consider real numbers — such as the optimal prescription
    dosage, the cost of gas next year or the number of expected Covid cases this
    winter. Regression trees divide the data into subsets, that is, branches,
    nodes, and leaves. Like decision trees, regression trees select splits that
    decrease the dispersion of target attribute values. Thus, the target attribute
    values can be predicted from their mean values in the leaves."
    ', sep = "\n")
  })
  
  observeEvent(input$predict_button, {
    
    dataset <- df[, c("latitude", "longitude", "BusinessType")]
    
    # Split the dataset into training and testing sets
    set.seed(123) # Set seed for reproducibility
    train1_indices <- sample(nrow(dataset), nrow(dataset)*0.7) # 70% for training
    train1_data <- dataset[train1_indices, ]
    test1_data <- dataset[-train1_indices, ]
    
    # Set the number of neighbors for the KNN model
    k <- 3
    
    # Train the KNN model
    knn_model <- knn(train1_data[, c("latitude", "longitude")],
                     test1_data[, c("latitude", "longitude")],
                     train1_data$BusinessType, k)
    
    # Evaluate the model
    accuracy <- sum(knn_model == test1_data$BusinessType) / 
      length(test1_data$BusinessType)
    
    cat("Accuracy:", accuracy, "\n")
    
    # Make predictions on new data
    # Get user input
    latitude <- as.numeric(input$latitude_input)
    longitude <- as.numeric(input$longitude_input)
    
    # Validate input
    if (is.na(latitude) || is.na(longitude)) {
      output$predicted_output <- renderPrint("Invalid input. Please enter numeric values.")
    } else {
      # Make predictions on new data
      new_data <- data.frame(latitude = latitude,
                             longitude = longitude)
      predicted_types <- knn(train1_data[, c("latitude", "longitude")], new_data,
                             train1_data$BusinessType, k)
      
      # Display predicted business types
      output$predicted_output <- renderPrint({
        cat("Predicted Business Type:", predicted_types, "\n")
        predicted_types
      })
    }})
  
  output$map_comment <- renderText({
    'This is the map of the businesses that is in the data using different zip codes.'
  })
  
  output$busn_type_comment <- renderText({
    'We have different business types in our data. So we have sorted out business 
    type and we counted how many business there are in each type. The highest business type 
    is 4701- Hotels(EXC Casino Hotels) and Motels. They have total of around 235. 
    The lowest in the 2060 Casino Hotels and 2302 Theater Co and Dinner Theater with
    less than 10 in total.'
  })
  
  output$start_date_comment <- renderText({
    'We have a graph here that shows business types and when they were started.
    In the x axis we have business start date from 1980 before till 2020 in the gap
    of 20 years apart. As we know from The " COunt bu Business Type" that the hotels
    and motels were the ones taht were started before anyone else and till 2020
    its the highest that is being started. Bed and Breakfast INNS has also started 
    being popular among the customers. As fpr 1980 till 2020, we can see vast differences.
    Travelers accomodation started for only few years. Before 2000 it stoped its openings.'
  })
  
  output$major_players_comment <- renderText({
    'We did text analysis and found out top 5 business chain in this data. As we can see
    that Sounder has the highest chain and hilton, holiday and hyatt has the similar 
    amount of chain in New Orleans.'
  })
  
  output$zip_comment <- renderText({
    'This is the interactive graph where we can select years and see which zip code 
    is famous among hotels and motels. We selected all the datas after 2000 till 2023 
    to do this analysis. We can see that in 2000 70115 is the most popular and in 2023
    70112 is the most popular.'
  })
  
  output$knn_model_comment <- renderText({
    'This model is using K-nearest neighbor to determine what business type the 
    user inputed coordinates are most likely to be. You put in your coordinates,
    and the models uses the closest businesses and determines the business type. 
    The coordinates for New Orleans are approx. 29.9511, -90.0715, so the model
    is most accurate if you input similar coordinates.'
  })
  
  output$map2_comment <- renderText({
    'The map displays thr zip code from the Zip column found in our dataset and
    when we click on one of teh zip codes on the map, we can see the type of
    business you can find in that area.'
  })
  
  output$business_graph_comment <- renderText({
    'It is a bar chart that displays the street name and the total number of
    businessess in that street.For this specific chart,we had to seperate the
    street name from the business address column and add a new column of the
    street names. The y axis shows the total number of businessess and the x 
    axis shows the street name.'
  })
  
# Server function
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4) %>%
      # Customize the map as desired, e.g., add markers or polygons
      addMarkers(data = df, lng = ~longitude, lat = ~latitude, popup = ~BusinessType)
  })

  output$business_graph <- renderPlot({
    filtered_data <- subset(df, street_name == input$street_select)
    
    street_counts <- filtered_data %>%
      group_by(street_name) %>%
      summarise(num_streets = n())
    
    ggplot(data = street_counts, aes(x = street_name, y = num_streets)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Street Name", y = "Number of Occurrences") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
