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
function(input, output, session) {

  #input uit digitaal dossier; dscores op verschillende leeftijden.
   childdat <- data.frame(month = c(1,2, 3, 6),
                          d = c(12,16,24,35))


   #ophalen uit JAMES? of gewoon dscore package
       references <- dscore::builtin_references %>%
         filter(pop == "dutch") %>%
         mutate(month = age * 12) %>%
         select(month, SDM2:SDP2) %>%
         filter(month <= 60) %>%
         pivot_longer(names_to = "centile", values_to = "d", cols = -month)



    output$ggplottest <- renderPlot({


                  ggplot()+
                      geom_line(data =references, aes(x = month, y = d, group = centile), color = "#e6550d", size = 0.8, alpha = 0.5) +
                      geom_point(data = childdat, aes(x = month, y = d), size = 2)+
                      geom_line(data = childdat, aes(x = month, y = d), size = 1)+
        geom_point(data = childdat2, aes(x = month, y = d), size = 2, color = "darkblue")+
        geom_line(data = childdat2, aes(x = month, y = d), size = 1, color = "darkblue")+
                      ylab("D-score") +
                      scale_x_continuous("Leeftijd (in maanden)", limits = c(0,36),
                                         breaks = seq(0, 36, 3))





    })

    output$plotlytest <- renderPlotly({

      plot_ly() %>%

        # Voeg lijnen toe van de referentiedata
        add_lines(data = references %>% filter(centile == "SDM2"), x = ~month, y = ~d, color = I("#e6550d"),
                  alpha = 0.5, line = list(shape = "linear"),
                  text = ~centile, hoverinfo = "text", showlegend = FALSE) %>%
        add_lines(data = references %>% filter(centile == "SDM1"), x = ~month, y = ~d, color = I("#e6550d"),
                  alpha = 0.5, line = list(shape = "linear"),
                  text = ~centile, hoverinfo = "text", showlegend = FALSE) %>%
        add_lines(data = references %>% filter(centile == "SD0"), x = ~month, y = ~d, color = I("#e6550d"),
                  alpha = 0.5, line = list(shape = "linear"),
                  text = ~centile, hoverinfo = "text", showlegend = FALSE) %>%
        add_lines(data = references %>% filter(centile == "SDP1"), x = ~month, y = ~d, color = I("#e6550d"),
                  alpha = 0.5, line = list(shape = "linear"),
                  text = ~centile, hoverinfo = "text", showlegend = FALSE) %>%
        add_lines(data = references %>% filter(centile == "SDP2"), x = ~month, y = ~d, color = I("#e6550d"),
                  alpha = 0.5, line = list(shape = "linear"),
                  text = ~centile, hoverinfo = "text", showlegend = FALSE) %>%

        # Voeg punten toe van de kinddata
        add_markers(data = childdat, x = ~month, y = ~d, mode = "markers") %>%

        # Voeg lijnen toe van de kinddata
        add_lines(data = childdat, x = ~month, y = ~d,
                  line = list(shape = "linear"), showlegend = FALSE) %>%

        # Voeg labels toe aan de y-as
        layout(yaxis = list(title = "D-score")) %>%

        # Pas de x-as aan
        layout(xaxis = list(title = "Leeftijd (in maanden)", range = c(0, 36),
                            tickvals = seq(0, 36, 3)))



    })


}
