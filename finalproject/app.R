library(shiny)
library(fs)
library(tidyverse)
library(plotly)
library(shinythemes)
library(leaflet)

data <- readRDS("la_ct.rds")

# Define UI for application that draws a histogram
ui <- navbarPage("Population and its Effect on Single-Family Home Prices in LA and Connecticut", theme = shinytheme("flatly"),
    
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
                 tabPanel("Models", leafletOutput("models")),
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
  
  output$models <- renderLeaflet({
    leaflet(options = 
              leafletOptions(dragging = FALSE,
                             minZoom = 6, 
                             maxZoom = 12))  %>% 
      addProviderTiles("CartoDB")  %>% 
      setView(lng = -118.2437, lat = 34.0522, zoom = 9)
    
  })
  
  output$insights <- renderUI({
    
    str1 <- paste("Los Angeles")
    str2 <- paste("As population size for a specific zipcode increases, the household size increases, but as people get older their household size shrinks.  
                  This makes sense, children are leaving their parents homes and moving to homes with roommates or their families. As population increases, the price of homes decreases. 
                  This also makes sense because more homes have to fit in a smaller area to accomidate for the large population. Specifically the most expensive homes have the smallest number of occupants.")
    str3 <- paste("Connecticut") 
    str4 <- paste("As population increases in Connecticut, the more number of vacant homes there are in those specific cities. 
                  One reason for this is that companies are developing the city, thinking that even more people are going to move there. 
                  They are being preemptive in their business decisions. As the population increases, the prices of homes decrease.")
    str5 <- paste("Relating the Areas")
    str6 <- paste("In both LA and all of Connecticut, as the population increases, the prices of homes decrease. The communities have to accomodate the growing proportion of people relative to the total space of the community.
                  Other reasons for this include the age of the areas, the average number of households, and specific jobs that are offered in those communities. ")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))})
}

  

# Run the application 
shinyApp(ui = ui, server = server)