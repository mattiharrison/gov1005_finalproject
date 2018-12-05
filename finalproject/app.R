library(shiny)
library(shinyjs)
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

##LA population
plot_1 <- all %>% 
  ggplot(aes(x = zipcode, y = total_population)) + geom_col() + theme(axis.text.x = element_blank()) + 
  labs(title = "Populations in Specific LA areas", x = "", y = "Population")
#LA prices 
plot_2 <- all %>%
  ggplot(aes(x = zipcode, y = x2010_12_x)) + geom_col() + theme(axis.text.x = element_blank()) + 
  labs(title = "Prices for Single Family Homes in LA", x = "", y = "Home Prices")

##CT population
plot_3 <- all %>% 
  ggplot(aes(x = city, y = population)) + geom_col() + theme(axis.text.x = element_blank()) + 
  labs(title = "Populations in Connecticut cities", x = "", y = "Population")
#CT prices
plot_4 <- all %>%
  ggplot(aes(x = city, y = x2010_12_y)) + geom_col() + theme(axis.text.x = element_blank()) + 
  labs(title = "Prices for Single Family Homes in CT", x = "", y = "Home Prices")

#Population and household size in LA, depending on age
##Population density for LA
plot_5 <- all %>% 
  ggplot(aes(x = total_population, y = average_household_size, color = median_age)) + geom_point() + labs(title = "As the Population in an LA zipcode increases, the household size increases", subtitle = "As people get older they have a smaller household size", x = "Population per Zipcode", y = "Average Household Size", color = "Median Age per Zipode")

#Population and number of open houses, added pricing of homes
##does population correspond to more vacant homes
plot_6 <- all %>%
  ggplot(aes(x = population, y = vacant_housing_units , color = x2010_12_y)) + geom_point() +
  labs(title = "As the population increases, the number of vacant homes increases in CT", 
       x = "Population", y = "Vacant Homes", color = "Home Prices")

#Population and home prices in LA, colored by average home size
plot_7 <- all %>%
  ggplot(aes(x = total_population, y = x2010_12_x, color = average_household_size)) + 
  geom_point() +
  labs(title = "As population in LA zipcodes increase, the home prices decrease", 
       subtitle = "The most expensive homes have the smallest household size.",
       x = "Population", y = "Home Prices", color = "Average household size")

#Population and home prices in CT, colored by total number of homes
plot_8 <- all %>%
  ggplot(aes(x = population, y = x2010_12_y, color = total_housing_units)) + geom_point() +
  labs(title = "As population in CT towns increases, the home prices decrease",
       subtitle = "As population increases the total number of available homes increases",
       x = "Population", y = "Home Prices", color = "Total houses")

# Define UI for application that draws a histogram
ui <- navbarPage("Population and its Effect on Single-Family Home Prices in LA and Connecticut", theme = shinytheme("sandstone"),
    
    # Show a plot of the generated distribution
   mainPanel(
      
    tabsetPanel(type = "tabs",
                 tabPanel("Summary", htmlOutput("summary")),
                 tabPanel("Graphs", plotlyOutput("graph"),
                          sidebarPanel(
                            selectInput(
                              inputId = "graph",
                              label = "Choose a graph:",
                              choices = c("LA population" = "plot_1", 
                                          "LA home prices" = "plot_2",
                                          "CT population" = "plot_3", 
                                          "CT home prices" = "plot_4",
                                          "Population Density in LA" = "plot_5", 
                                          "Available Homes in CT" = "plot_6",
                                          "Populations and Corresponding Home Prices in LA" = "plot_7", 
                                          "Populations and Corresponding Home Prices in CT cities" = "plot_8"),
                              selected = "plot_1"
                            ))),
                 tabPanel("Models", plotlyOutput("models")),
                 tabPanel("Insights", htmlOutput("insights")))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  output$summary <- renderUI({
    
    str1  <- paste("Summary")
    str2  <- paste("This app shows the population in different LA zipcodes and Connecticut towns in 2010 and their corresponding price for a single family home.")
    str3  <- paste("Instructions") 
    str4  <- paste("Click through the tabs to see the data in different ways and use the drop-down menus to go between different characteristics.")
    str5  <- paste("Notes")
    str6  <- paste("The data had to be cleaned to be merged by zipcode. Zipcodes with 0 as a population were removed from the dataset. Home prices were selected from December of 2012 to keep it consistent across economic policies such as interest rates and recovery from the Great Recession.")
    str7  <- paste("Sources")
    str8  <- paste("Census data was provided by Data.gov and home prices were from Zillow, specifically single-family homes. 
                   https://catalog.data.gov/dataset/2010-census-populations-by-zip-code, 
                   https://catalog.data.gov/dataset/2010-population-by-town,
                   https://www.zillow.com/research/data/")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6), h3(str7), p(str8)))
  })
 
  output$graph <- reactive({
    switch(input$graph,
           "LA population" = plot_1,
           "LA home prices" = plot_2,
           "CT population" = plot_3,
           "CT home prices" = plot_4)
    
  })
  
  output$models <- ???????({
    leaflet(options = 
              leafletOptions(dragging = FALSE,
                             minZoom = 6, 
                             maxZoom = 12))  %>% 
      addProviderTiles("CartoDB")  %>% 
      setView(lng = -118.2437, lat = 34.0522, zoom = 9)
    
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