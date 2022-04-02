#loading library
library(shiny)
library(tidyverse)
library(plotly)
library(DT)

#Data Import
veg = read.csv('kalimati_tarkari_dataset_cleaned.csv')
#View(veg)

#setting required data types
veg$Commodity <- as.factor(veg$Commodity)
veg$Date <- as.Date(veg$Date)

# Defining UI 
ui <- fluidPage(

    # Application title
    titlePanel("Vegetables Price Over Time"),

    # Sidebar for inputs 
    sidebarLayout(
        sidebarPanel(
            selectInput("commodity","Select Vegetable", unique(veg$Commodity)),
            dateRangeInput("date","Select Date Range", start = "2013-06-16", 
                           end = "2021-05-13", min = "2013-06-16", 
                           max = "2021-05-13")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Plot",plotlyOutput("lineplot")),
            tabPanel("Table",DTOutput('table'))
          )
        )
    )
)

# Define server logic required
server <- function(input, output) {
  
    rval_commodity <- reactive({
      veg %>% 
        filter(Commodity == input$commodity, 
               Date >= input$date[1],
               Date <= input$date[2])
    })
    output$lineplot <-plotly::renderPlotly({
      rval_commodity() %>% 
      ggplot(aes(x= Date, y= Average))+
        geom_line()
      plotly::ggplotly()
    })
    
    output$table <- DT::renderDT({
      rval_commodity() %>%
          arrange(desc(Average)) %>% 
            select(Commodity,Date,Average_Price =Average) %>% 
              top_n(10)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
