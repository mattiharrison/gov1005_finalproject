library(shiny)
library(fs)
library(tidyverse)
library(stringr)
library(ggplot2)
library(janitor)
library(rsconnect)
library(shinythemes)
library(plotly)
library(scales)

data <- readRDS("la_ct.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Population and the effect on Housing in LA and Connecticut"),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", htmlOutput("summary")),
                  tabPanel("Graphs", plotlyOutput("graphs")),
                  tabPanel("Models", plotlyOutput("models")),
                  tabPanel("Insights", plotOutput("insights")))
    ))


# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  output$summary <- renderUI({
    
    str1  <- paste("Summary")
    str2  <- paste("This app shows the population in different LA zipcodes and Connecticut towns in 2010 and their corresponding price for a single family home.")
    str3  <- paste("Instructions") 
    str4  <- paste("Click through the tabs to see the data in different ways and use the drop-down menus to go between different characteristics.")
    str5  <- paste("How to read the graphs")
    str6  <- paste("??????")
    str7  <- paste("Sources")
    str8  <- paste("Census data was provided by Data.gov and home prices were from Zillow. https://catalog.data.gov/dataset/2010-census-populations-by-zip-code https://catalog.data.gov/dataset/2010-population-by-town https://www.zillow.com/research/data/")
    str9  <- paste("Notes")
    str10 <- paste("The data had to be cleaned to be merged by zipcode. Zipcodes with 0 as a population were removed from the dataset. Home prices were selected from December of 2012 to keep it consistent across economic policies such as interest rates and recovery from the Great Recession.")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6), h3(str7), p(str8), h3(str9), p(str10)))
  })
  
  output$graphs <- renderPlot({
    data %>% 
      ggplot(aes(x = total_population, 
                 y = average_household_size, 
                 color = median_age)) + 
      geom_point() + 
      labs(title = "Population in LA and Average Household Size, using zipcode data", 
           subtitle = "As people get older they have a smaller household size", 
           x = "Population per Zipcode", 
           y = "Average Household Size", 
           color = "Median Age per Zipode")
  })
  
  output$models <- renderPlot({
    specific %>% 
      ggplot(aes_string(x = input$characteristic, color = input$characteristic)) + geom_bar() + 
      coord_polar("y", start=0) + ggtitle("New Jersey District 3, by characteristic")
  
  })
  
  output$insights <- renderUI({
    
    str1 <- paste("Summary")
    str2 <- paste("This app shows the interviewees that Upshot used in their poll.")
    str3 <- paste("Instructions") 
    str4 <- paste("Click through the tabs to see the data in different ways and use the drop-down menu to go between different characteristics.")
    str5 <- paste("How to read the graphs")
    str6 <- paste("The first plot is a bar graph of the total interviews that Upshot conducted to create their data source.  This includes all states and districts. 
                  The graphs shows that they interviewed white people above 65 the most, this could potentially have created a bias dataset from the beginning.
                  The pie chart just shows New Jersey's 3rd district.  The graph is by count, not percentage, so the full circles were the most frequent.  In this case
                  the most interviewed people were white and between 50 and 64.  This shows the lack of diversity in the polling data and shows the discrepency in US polling data.
                  New Jersey's 3rd district demographics show that the district is much younger with an average age of 43, but it is 75% white.  This shows that the aging variable
                  could lead to bias, but race/ethnicity bias is not present.")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))})
}

# Run the application 
shinyApp(ui = ui, server = server)