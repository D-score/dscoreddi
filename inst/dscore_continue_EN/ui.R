#

library(shiny)
library(shinyBS)

# Define UI for application that draws a histogram
shinyUI(

    fluidPage(

    # Application title
    titlePanel("Dutch Development instrument - adaptieve demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            p("This application selects the best items based on age and percentile (regularly is P90 is used and average is about P50)."),
            br(),

            numericInput("agemos", "Age child in months", min = 0, max = 50, value = 8),
            bsTooltip("agemos", "Milestones are selected based on age. Age can be adjusted for gestation.", placement = "top", trigger = "hover", options = list(container = "body")),
            br(),

            sliderInput("refperc",
                        "Percentile for milestones",
                        min = 0, max = 100,step = 10,
                        value = 90),
            bsTooltip("refperc", "Usually P90 is used. The P50 means that 50% of the children can pass the milestone at this age.", placement = "top", options = list(container = "body")),
            br(),
            numericInput("suggest", "Number of milestones to select", min = 0, max = 12, value = 6)


          #  p("Hier kunnen we ook kenmerken die in eerdere contacten al zijn uitgevraagd ophalen. Deze kunnen we dan (1) uitsluiten als nieuwe suggesties en (2) gebruiken om de D-scores te bepalen van vorige contactmenten en te plotten in het D-score plot.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("DDI 0-15m",
                         plotOutput("VWOplot1", height = 800),
                         p("** = Repeat milestone; (M) = Caregiver report; -rl = right and left together"),
                         p("For each mileston the bar marks the P10 (left), P50 (middle), P90 (right). De vertical line marks the age of the current child. Milestones that fit the input settings are colored.")
                         ),
                tabPanel("DDI 12-48m",
                         plotOutput("VWOplot2", height = 800),
                         p("(M) = Caregiver report; -rl = right and left together"),
                         p("For each mileston the bar marks the P10 (left), P50 (middle), P90 (right). De vertical line marks the age of the current child. Milestones that fit the input settings are colored.")
                         ),
                tabPanel("D-score curve", plotOutput("Dscoreplot", height = 800))
            )
        )
    )
))
