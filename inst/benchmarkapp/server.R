#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(plotly)
# library(promises)
# library(future)

# Set future promises to be resolved in parallel if used
# plan(multisession)

vline <- function(x = 0, color = "red") {
	if (is.null(x) | is.na(x)) x <- 0
	list(
		type = "line",
		y0 = 0,
		y1 = 1,
		yref = "paper",
		x0 = x,
		x1 = x,
		line = list(color = color)
	)
}

# Define server logic required to draw a histogram
function(input, output, session) {

  #input uit digitaal dossier; dscores op verschillende leeftijden.
   childdat <- data.frame(month = c(1,2, 3, 6),
                          d = c(12,16,24,35))

   # Ophalen uit JAMES? of gewoon dscore package
       references <- dscore::builtin_references %>%
         filter(population == "dutch" & key == "dutch") %>%
         mutate(month = age * 12) %>%
         select(month, SDM2:SDP2) %>%
         filter(month <= 60) %>%
         pivot_longer(names_to = "centile", values_to = "d", cols = -month)

   # Declare reactive values
   # myReactives <- reactiveValues(val1 = input$val1, etc)

   # Output base plot
    output$plotlytest <- renderPlotly({
    	isolate({ # isolate makes it so that updates dont affect the original plot!
    		fm_dom <- ggplot()+
    			geom_line(data =references, aes(x = month, y = d, group = centile), color = "#e6550d", size = 0.8, alpha = 0.5) +
    			geom_point(data = childdat, aes(x = month, y = d), size = 2)+
    			geom_line(data = childdat, aes(x = month, y = d), size = 1)+
    		#	geom_vline(aes(xintercept = input$testval)) +
    			ylab("D-score") +
    			scale_x_continuous("Leeftijd (in maanden)", limits = c(0,36),
    												 breaks = seq(0, 36, 3))


    		ggplotly(fm_dom, tooltip = c("label")) %>%
    			toWebGL()
    		})


    })

    # Speed up by not completely redrawing on update.
    observeEvent(input$testval, {
    	plotlyProxy("plotlytest", session) %>%
    		plotlyProxyInvoke("relayout",
    											list(shapes = list(vline(input$testval))))
    	# plotly_json(myPlotly) is a useful way to figure out where to look! Note
    	# that in JSON when something is inside an object (eg curly brackets),
    	# that's the equivalant of a list() in R
    })


}
