library(shiny)
library(plotly)
library(dscore)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Test application for speed"),

    sidebarLayout(
    	sidebarPanel(width = 2,
    							 numericInput("testval", "TestButton", min = 0, max = 10, value = 5)),
    	mainPanel(
    		plotlyOutput("plotlytest")
    	)
    )
    
)
