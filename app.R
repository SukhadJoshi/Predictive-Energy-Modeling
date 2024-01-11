#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)  # For plottingr
#setwd("D:/IDS_Final/Final_app/")
#final_cleaned_app <- read.csv("D:/IDS_Final/Final_app/Final_app.csv")
final_cleaned_app <- read.csv("Final_app.csv")

model3 <- lm(daily_total_usage ~ in.sqft + in.bedrooms + in.building_america_climate_zone + in.clothes_dryer + in.clothes_washer + in.cooking_range + in.county + in.federal_poverty_level + daily_temperature + daily_humidity, data = final_cleaned)

# Assuming your model_lm and train data are available
str(final_cleaned_app)
# UI
ui <- fluidPage(
  titlePanel("Energy Prediction App"),
  sidebarLayout(
    sidebarPanel(
      # Add input controls for each predictor
      sliderInput("in_sqft", "Square Footage", min = min(train$in.sqft), max = max(train$in.sqft), value = mean(train$in.sqft)),
      sliderInput("in_bedrooms", "Number of bedrooms", min = min(train$in.bedrooms), max = max(train$in.bedrooms), value = mean(train$in.bedrooms)),
      selectInput("in_building_america_climate_zone", "Climate Zone", choices = unique(train$in.building_america_climate_zone), selected = (unique(train$in.building_america_climate_zone)[1])),
      sliderInput("daily_temperature", "Temperature", min = min(train$daily_temperature), max = max(train$daily_temperature), value = mean(train$daily_temperature)),
      selectInput("in_clothes_dryer", "Clothes Dryer Type", choices = (unique(train$in.clothes_dryer)), selected = (unique(train$in.clothes_dryer)[1])),
      selectInput("in_clothes_washer", "Clothes Washer Type", choices = (unique(train$in.clothes_washer)), selected = (unique(train$in.clothes_washer)[1])),
      selectInput("in_cooking_range", "Cooking Range", choices = (unique(train$in.cooking_range)), selected = (unique(train$in.cooking_range)[1])),
      selectInput("in_county", "County", choices = (unique(train$in.county)), selected = (unique(train$in.county)[1])),
      selectInput("in_poverty_level", "Poverty Level", choices = (unique(train$in.federal_poverty_level)), selected = (unique(train$in.federal_poverty_level)[1])),
      sliderInput("daily_humidity", "Humidity", min = min(train$daily_humidity), max = max(train$daily_humidity), value = mean(train$daily_humidity))),
    
    
    # Repeat this for other predictors or use selectInput for categorical variables
    # selectInput("in_bedrooms", "Number of Bedrooms", choices = unique(train$in.bedrooms), selected = unique(train$in.bedrooms)[1]),
    # Add more controls as needed
    actionButton("predictButton", "Predict")
  ),
  mainPanel(
    # Display the predicted total usage
    h4("Predicted Total Energy Usage:"),
    textOutput("prediction"),
    
    # Display the prediction plot
    plotOutput("predictionPlot")
  )
)

# Server
server <- function(input, output) {
  # Function to run the model and make predictions
  predict_energy <- eventReactive(input$predictButton, {
    # Create a new data frame with user inputs
    new_data <- data.frame(
      in.sqft = input$in_sqft,
      in.bedrooms = input$in_bedrooms,
      in.building_america_climate_zone = input$in_building_america_climate_zone,
      daily_temperature = input$daily_temperature,
      in.clothes_dryer = input$in_clothes_dryer,
      in.clothes_washer = input$in_clothes_washer,
      in.cooking_range = input$in_cooking_range,
      in.county = input$in_county,
      in.federal_poverty_level = input$in_poverty_level,
      daily_humidity = input$daily_humidity
      # Add more variables as needed
    )
    
    # Make predictions
    predictions <- predict(model3, new_data)
    return(predictions)
  })
  
  # Output the predicted total usage
  output$prediction <- renderText({
    paste("Predicted Total Energy Usage: ", round(predict_energy(), 2))
  })
  
  
}

# Run the app
shinyApp(ui, server)