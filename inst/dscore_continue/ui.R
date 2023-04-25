#

library(shiny)
library(shinyBS)

# Define UI for application that draws a histogram
shinyUI(

    fluidPage(

    # Application title
    titlePanel("Van Wiechen Continue - Tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 2,
            p("Dit is een tool om te helpen met het selecteren van Van Wiechen kenmerken voor flexibele contactmomenten. Hieronder kun je de leeftijd van het kind, het percentiel (gewoonlijk P90 - 90% van de kinderen behaalt het kenmerk), en het aantal gewenste kenmerken aanpassen."),
            br(),
            h2("Instellingen"),
            br(),

            numericInput("agemos", "Leeftijd kind in maanden", min = 0, max = 50, value = character(0)),
            bsTooltip("agemos", "Op basis van deze leeftijd worden kenmerken aanbevolen. Bij vroeggeboorte kun je leeftijd corrigeren.", placement = "top", trigger = "hover", options = list(container = "body")),
            br(),

            sliderInput("refperc",
                        "Percentiel voor kenmerken",
                        min = 0, max = 100,step = 5,
                        value = 90),
            bsTooltip("refperc", "Gewoonlijk worden kenmerken op het 90e percentiel afgenomen. Het 50e percentiel houdt in dat 50% van de kinderen het kenmerk op deze leeftijd kan.", placement = "top", options = list(container = "body")),
            br(),
            numericInput("suggest", "Het aantal kenmerken om af te nemen", min = 0, max = 12, value = 6),
            br(),
            br(),
            br(),
            p("Bron referenties:"), em("Van Buuren S (2014). Growth charts of human development. Stat Methods Med Res, 23(4), 346-
368.")


          #  p("Hier kunnen we ook kenmerken die in eerdere contacten al zijn uitgevraagd ophalen. Deze kunnen we dan (1) uitsluiten als nieuwe suggesties en (2) gebruiken om de D-scores te bepalen van vorige contactmenten en te plotten in het D-score plot.")
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 10,
            tabsetPanel(
                tabPanel("VWO 0-15m",
                         plotOutput("VWOplot1", height = 800),
                         tags$div(
                           tags$span("(M) = mededeling; -rl = rechts en links samen."), tags$br(),
                           tags$span("** = Kenmerk herhalen gedurende eerste 52 weken.", style = "color:#3399ff;"), tags$span("Kenmerken die passen bij de instellingen zijn gekleurd.", style= "color:#e6550d"),tags$br(), tags$span("Voor elk kenmerk geeft de balk de P10 (links), P50 (middenstip), P90 (rechts). De verticale hulplijn staat bij de leeftijd van het kind.")
                         )
                         ),
                tabPanel("VWO 12-48m",
                         plotOutput("VWOplot2", height = 800),
                         tags$div(
                           tags$span("(M) = mededeling; -rl = rechts en links samen."), tags$br(),
                           tags$span("Kenmerken die passen bij de instellingen zijn gekleurd.", style= "color:#e6550d"),tags$br(), tags$span("Voor elk kenmerk geeft de balk de P10 (links), P50 (middenstip), P90 (rechts). De verticale hulplijn staat bij de leeftijd van het kind.")
                         )
                         ),
                tabPanel("VWO Fijne motoriek",
                         plotOutput("VWOfinemotor", height = 800),
                         tags$div(
                           tags$span("(M) = mededeling; -rl = rechts en links samen."), tags$br(),
                           tags$span("Kenmerken die passen bij de instellingen zijn gekleurd.", style= "color:#e6550d"),tags$br(), tags$span("Voor elk kenmerk geeft de balk de P10 (links), P50 (middenstip), P90 (rechts). De verticale hulplijn staat bij de leeftijd van het kind.")
                         )
                ),
                tabPanel("VWO Communicatie",
                         plotOutput("VWOcommunication", height = 800),
                         tags$div(
                           tags$span("(M) = mededeling; -rl = rechts en links samen."), tags$br(),
                           tags$span("Kenmerken die passen bij de instellingen zijn gekleurd.", style= "color:#e6550d"),tags$br(), tags$span("Voor elk kenmerk geeft de balk de P10 (links), P50 (middenstip), P90 (rechts). De verticale hulplijn staat bij de leeftijd van het kind.")
                         )
                ),
                tabPanel("VWO Grove motoriek",
                         plotOutput("VWOgrovemotor", height = 800),
                         tags$div(
                          tags$span("(M) = mededeling; -rl = rechts en links samen."), tags$br(),
                          tags$span("** = Kenmerk herhalen gedurende eerste 52 weken.", style = "color:#3399ff;"), tags$span("Kenmerken die passen bij de instellingen zijn gekleurd.", style= "color:#e6550d"),tags$br(), tags$span("Voor elk kenmerk geeft de balk de P10 (links), P50 (middenstip), P90 (rechts). De verticale hulplijn staat bij de leeftijd van het kind.")
                         )

                )#,
               # tabPanel("D-score curve", plotOutput("Dscoreplot", height = 800))
            )
        )
    )
))
