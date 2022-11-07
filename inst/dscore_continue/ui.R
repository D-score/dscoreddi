#

library(shiny)
library(shinyBS)

# Define UI for application that draws a histogram
shinyUI(

    fluidPage(

    # Application title
    titlePanel("Van Wiechen continue - adaptieve demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 2,
            p("Deze applicatie selecteert op basis van leeftijd en percentiel (regulier is ongeveer 90 en gemiddeld is ongeveer 50) het wenste aantal aanbevolen van Wiechen kenmerken om af te nemen in blauw."),
            br(),

            numericInput("agemos", "Leeftijd kind in maanden", min = 0, max = 50, value = 6),
            bsTooltip("agemos", "Op basis van deze leeftijd worden kenmerken aanbevolen. Bij vroeggeboorte kun je leeftijd corrigeren.", placement = "top", trigger = "hover", options = list(container = "body")),
            br(),

            sliderInput("refperc",
                        "Percentiel voor kenmerken",
                        min = 0, max = 100,step = 10,
                        value = 50),
            bsTooltip("refperc", "Gewoonlijk worden kenmerken op het 90e percentiel afgenomen. Het 50e percentiel houdt in dat 50% van de kinderen het kenmerk op deze leeftijd kan.", placement = "top", options = list(container = "body")),
            br(),
            numericInput("suggest", "Het aantal kenmerken om af te nemen", min = 0, max = 12, value = 6)


          #  p("Hier kunnen we ook kenmerken die in eerdere contacten al zijn uitgevraagd ophalen. Deze kunnen we dan (1) uitsluiten als nieuwe suggesties en (2) gebruiken om de D-scores te bepalen van vorige contactmenten en te plotten in het D-score plot.")
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 10,
            tabsetPanel(
                tabPanel("VWO 0-15m",
                         plotOutput("VWOplot1", height = 800),
                         p("** = Kenmerk herhalen; (M) = mededeling; -rl = rechts en links samen"),
                         p("Voor elk kenmerk geeft de balk de P10 (links), P50 (middenstip), P90 (rechts). De verticale hulplijn staat bij de leeftijd van het kind. Kenmerken die passen bij de instellingen zijn gekleurd.")
                         ),
                tabPanel("VWO 12-48m",
                         plotOutput("VWOplot2", height = 800),
                         p("(M) = mededeling; -rl = rechts en links samen"),
                         p("Voor elk kenmerk geeft de balk de P10 (links), P50 (middenstip), P90 (rechts). De verticale hulplijn staat bij de leeftijd van het kind. Kenmerken die passen bij de instellingen zijn gekleurd.")
                         ),
                tabPanel("VWO Fijne motoriek",
                         plotOutput("VWOfinemotor", height = 800),
                         p("** = Kenmerk herhalen; (M) = mededeling; -rl = rechts en links samen"),
                         p("Voor elk kenmerk geeft de balk de P10 (links), P50 (middenstip), P90 (rechts). De verticale hulplijn staat bij de leeftijd van het kind. Kenmerken die passen bij de instellingen zijn gekleurd.")
                ),
                tabPanel("VWO Communicatie",
                         plotOutput("VWOcommunication", height = 800),
                         p("(M) = mededeling; -rl = rechts en links samen"),
                         p("Voor elk kenmerk geeft de balk de P10 (links), P50 (middenstip), P90 (rechts). De verticale hulplijn staat bij de leeftijd van het kind. Kenmerken die passen bij de instellingen zijn gekleurd.")
                ),
                tabPanel("VWO Grove motoriek",
                         plotOutput("VWOgrovemotor", height = 800),
                         p("** = Kenmerk herhalen; (M) = mededeling; -rl = rechts en links samen"),
                         p("Voor elk kenmerk geeft de balk de P10 (links), P50 (middenstip), P90 (rechts). De verticale hulplijn staat bij de leeftijd van het kind. Kenmerken die passen bij de instellingen zijn gekleurd.")
                ),
                tabPanel("D-score curve", plotOutput("Dscoreplot", height = 800))
            )
        )
    )
))
