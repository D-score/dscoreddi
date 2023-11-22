library(shiny)
library(plotly)
library(dscore)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Test application for speed"),

        mainPanel(
         plotOutput("ggplottest"),
         plotlyOutput("plotlytest")
        )
    #)
)
