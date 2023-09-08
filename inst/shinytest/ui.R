library(shiny)
library(ggplot2)
library(dscore)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Test application for speed"),

        mainPanel(
            plotOutput("distPlot")
        )
    #)
)
