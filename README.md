
# Rental Data Analysis 📈

We will analyze the hotels_motels data from data.gov for this project. In order to determine whether there may be a relationship in our data, we will look at the business type, date, location, and zip.
 


## Contributors 📝

- Eirik Andersen
- Stuti Shrestha
- Mansi Gujadhur




## Library 📚
    1.  tidyverse
    2.  lubridate
    3.  ggplot2
    4.  dplyr
    5.  DT
    6.  leaflet
    7.  viridis
    8.  shiny
    9. rsconnect


## Analysis Code 🧐


    # pivot showing count of each busn type
    busn_type <- df %>%
      group_by(BusinessType) %>%
      summarise(count = n())
    
    ggplot(busn_type, aes(x = BusinessType, y = count)) +
      geom_bar(stat = "identity",fill="deeppink3",color="black") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            plot.title = element_text(size = 20)) +
      labs(x = "Business Type", y = "Count", title = "Business Type Counts") +
      ggtitle("Business Type Counts")
    
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

    #Popular zip code and start date (which zip code got popular)
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
## Shinny Code ✨

    ui <- fluidPage(
    titlePanel("Stays in New Orleans"),
    mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
               textOutput("intro_text")),
      tabPanel("Map of Hotels, Motels, and Rentals",
               fluidRow(
                 column(12, leafletOutput("map"))
               )),
        tabPanel("LLM Prediction Model",
               fluidRow(
                 column(12, textOutput("llm_model")),
                 column(12, textOutput("llm_citation"))
               )),
        tabPanel("Our Prediction Model",
               fluidRow(
                 column(6,textInput("latitude_input", "Latitude:",
                                    placeholder = "Enter latitude...")),
                 column(6, textInput("longitude_input", "Longitude:",
                                     placeholder = "Enter longitude..."))),
               fluidRow(
                 column(12, actionButton("predict_button", "Predict"))),
               fluidRow(
                 column(12, verbatimTextOutput("predicted_output"))
               ))

## Geo Spatial leaflet 📍
    #Map of Places
    output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = df, lat = ~latitude, lng = ~longitude)
    })
## Prediction ride model 🔮
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
    '
    # APA Citation from Bing AI
    "Built In. (n.d.). Regression Trees: How to Get Started. Built In.
    https://builtin.com/data-science/regression-tree"
    
    "Carnegie Mellon University. (n.d.). Lecture 10: Regression Trees.
    Carnegie Mellon University. https://www.stat.cmu.edu/~cshalizi/350-2006/lecture-10.pdf"
    
    # Explanation
    "Regression trees are a variant of decision trees that aim to
    predict outcomes we consider real numbers — such as the optimal prescription
    dosage, the cost of gas next year or the number of expected Covid cases this
    winter. Regression trees divide the data into subsets, that is, branches,
    nodes, and leaves. Like decision trees, regression trees select splits that
    decrease the dispersion of target attribute values. Thus, the target attribute
    values can be predicted from their mean values in the leaves."
    '
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
## Links 🖇️

[github](https://github.com/ehando/final_project)

[shinnyApp](https://ehando.shinyapps.io/final_project/)


