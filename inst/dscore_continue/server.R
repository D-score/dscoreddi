#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    #reference data, with additional column for start-items based on input$refperc en highlights voor geselecteerde start-items based on input$agemos en input$suggest.
    # ahv voorgaand afgenomen items uit BDS item_candidates aanpassen en dynamisch maken, dan kunnen alleen items gesuggereerd worden die nog niet eerder zijn afgenomen; of herhaald moeten worden afgenomen.
    # volgende stap is het aanpassen van de highlight variabele door de ingevoerde van wiechen kenmerken tijdens het consult.
    pref <- reactive({

                   james_post(host = host,
                             path = "get_percentile_plot_data",
                             itembank = itembank,
                             reference = expanded_reference,
                             age = input$agemos,
                             refperc = input$refperc,
                             suggest = input$suggest)$parsed

    })

    output$VWOplot1 <- renderPlot({
      vwo_plot(data = pref(), side = "sideA", age = input$age)
      james_post(host = host,
                 path = "get_vwo_plot",
                 data = pref(),
                 side = "sideA",
                 age = input$agemos)$parsed
    })


    output$VWOplot2 <- renderPlot({
      vwo_plot(data = pref(), side = "sideB", age = input$age)
      })




    output$Dscoreplot <- renderPlot({

        ggplot()+
            geom_line(data =references, aes(x = month, y = d, group = centile), color = "#e6550d", size = 0.8, alpha = 0.5) +
            geom_point(data = childdat, aes(x = month, y = d), size = 2)+
            geom_line(data = childdat, aes(x = month, y = d), size = 1)+

            ylab("D-score") +
            scale_x_continuous("Leeftijd (in maanden)", limits = c(0,36),
                               breaks = seq(0, 36, 3))

    })

})
