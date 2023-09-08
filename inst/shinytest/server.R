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


    output$distPlot <- renderPlot({


           ggplot()+
               geom_line(data =references, aes(x = month, y = d, group = centile), color = "#e6550d", size = 0.8, alpha = 0.5) +
               geom_point(data = childdat, aes(x = month, y = d), size = 2)+
               geom_line(data = childdat, aes(x = month, y = d), size = 1)+

               ylab("D-score") +
               scale_x_continuous("Leeftijd (in maanden)", limits = c(0,36),
                                  breaks = seq(0, 36, 3))


          })

}
