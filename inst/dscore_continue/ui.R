#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(

    fluidPage(

    # Application title
    titlePanel("Van Wiechen continue - adaptieve demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h3("Instellingen:"),
            p("Hieronder kun je aangeven welk percentiel het uitgangspunt is voor de aanbevolen kenmerken om mee te beginnen, hiervoor wordt rekening gehouden met de leeftijd van het kind."),
            sliderInput("refperc",
                        "Percentiel voor start-kenmerken",
                        min = 0, max = 100,
                        value = 50),
            p("Het aantal aanbevolen kenmerken kun je hieronder aangeven. Kenmerken die het beste passen bij de instellingen zijn blauw gekleurd in het plot"),
            numericInput("suggest", "Aantal suggesties", min = 0, max = 10, value = 3),
            br(),
            h3("BDS input:"),
            p("Dit is input die we uiteindelijk vanuit het DD willen inlezen."),
            numericInput("agemos", "Leeftijd in maanden", min = 0, max = 50, value = 6),
            br(),
            p("Hier kunnen we ook kenmerken die in eerdere contacten al zijn uitgevraagd ophalen. Deze kunnen we dan (1) uitsluiten als nieuwe suggesties en (2) gebruiken om de D-scores te bepalen van vorige contactmenten en te plotten in het D-score plot.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("VWO 0-15m", plotOutput("VWOplot1", height = 800),
                         p("** = Kenmerk herhalen; (M) = mededeling; -rl = rechts en links samen")),
                tabPanel("VWO 12-48m", plotOutput("VWOplot2", height = 800),
                         p("(M) = mededeling; -rl = rechts en links samen")),
                tabPanel("D-score curve", plotOutput("Dscoreplot", height = 800))
            )
        )
    )
))
