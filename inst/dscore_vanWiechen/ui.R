#
# App that shows the probability to pass a milestone for each van Wiechen item for model versus observed data en separate plots per milestone
#


# Define UI for application that draws a histogram
ui <-
    dashboardPage(
    dashboardHeader(title = "D-score van Wiechen"
                    ),
    dashboardSidebar(
        sidebarMenuOutput("menu")
        ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "reference",
                    fluidRow(box(title = "Nederlandse referentie D-score",
                                 p("De Nederlandse referentie voor de D-score is ontwikkeld op de SMOCK data."),
                        plotOutput("refplot", width = 1000, height = 500),
                        height = 600, width = 1100
                    ))),

            tabItem(tabName = "p_a_model",
                    fluidRow(box(title = "Kans per kenmerk per leeftijd",
                                 p("De kans om het kenmerk te kunnen gegeven de leeftijd in maanden. Deze kans per leeftijd is gebaseerd op het Nederlandse model voor de D-score. Door over de curve te bewegen met de cursor, wordt de exacte kans per leeftijd weergegeven."),
                        width = 1100,
                                  column(width = 3, offset = 0,
                                         selectInput(
                                             inputId = "pam_list",
                                             label = "Item search",
                                             choices = ""
                                         ))
                    )),
                    fluidRow(box(
                        plotlyOutput("plots_p_a_model", width = 1000, height = 500),
                        height = 600, width = 1100,
                        fluidRow(
                            column(1, offset = 1, actionButton("prev6", "Prev")),
                            column(1, offset = 1, actionButton("run6", "Next"))
                        )
                    ))),
            tabItem(tabName = "p_a_obs",
                    fluidRow(box(title = "Geobserveerde data uit de SMOCK en MOM studie",
                                 p("De geobserveerde kans om het kenmerk te kunnen gegeven de leeftijd in maanden."), width = 1100,
                                  column(width = 3, offset = 0,
                                         selectInput(
                                             inputId = "pai_list",
                                             label = "Item search",
                                             choices = ""
                                         ))
                    )),
                    fluidRow(box(
                        imageOutput("plots_p_a_item", width = 1000, height = 500),
                        height = 600, width = 1100,
                        fluidRow(
                            column(1, offset = 1, actionButton("prev7", "Prev")),
                            column(1, offset = 1, actionButton("run7", "Next"))
                        )
                    ))),
            tabItem(tabName = "invoer",
                    fluidRow(
                    tabsetPanel(
                      tabPanel("Invoer",
                      box(width = 12,
                        numericInput("age1", "Leeftijd in maanden", min = 0, max = 52, step = 1, value = NULL)),
                        box(title = "0-12maanden",
                                 uiOutput("vwinvoerA"), width = 6),
                               box(title = "12-48maanden",
                                 uiOutput("vwinvoerB"), width = 6)
                      ),
                      tabPanel("Data",
                               box(tableOutput("vwingevoerd"))),
                      tabPanel("D-score",
                               box(
                               plotOutput("scoreplot"),
                               tableOutput("scoretab")
                               )
                      )
                    ))



                    ),
            tabItem(tabName = "about",
                    fluidRow(box(tags$h1("About"),
                                 width = 400,
                                 box(width= 300,
                                     tags$a(img(src="TNO_zwart.jpg", width="10%"),href="https://www.tno.nl/nl/"),
                                     tags$h4("Department Child Health")),
                                 box(width= 300,
                                     tags$h4("Iris Eekhout"),
                                     tags$div("Email: iris.eekhout@tno.nl"),
                                     tags$div("Website: ",tags$a("www.iriseekhout.com", href="https://www.iriseekhout.com")),
                                     tags$div("Github: ", tags$a("iriseekhout", href="https://github.com/iriseekhout")
                                     )))))

            )
        )
    )

