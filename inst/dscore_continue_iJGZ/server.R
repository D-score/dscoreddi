#iJGZ
library(shiny)
library(plotly)


shinyServer(function(input, output, session) {

    #reference data, with additional column for start-items based on input$refperc en highlights voor geselecteerde start-items based on input$agemos en input$suggest.
    # ahv voorgaand afgenomen items uit BDS item_candidates aanpassen en dynamisch maken, dan kunnen alleen items gesuggereerd worden die nog niet eerder zijn afgenomen; of herhaald moeten worden afgenomen.
    # volgende stap is het aanpassen van de highlight variabele door de ingevoerde van wiechen kenmerken tijdens het consult.


#update input agemos with info from target data, if not available use input field. So update input field
  agemos_in <- reactive({
    agedays <- as.numeric(Sys.Date() - as.Date(tgt$psn$dob))
    agemos <- round(agedays / 365.25 * 12)
  })

  observe({
  updateNumericInput(inputId = "agemos",value = agemos_in())
  })

  #deze kan updaten tijdens
selected_items <-
  reactive({
    select_vwc(
      age = input$agemos/12,
      refperc = input$refperc,
      nsuggest = input$suggest,
      vw_history = vw_history
    )

  })

   output$VWOfinemotor <- renderPlotly({
      plot_vwc(
        data = dscoreddi::prefdat,
        domein = "Fijne motoriek",
        age = input$agemos/12,
        selected = selected_items(),
        vw_history = vw_history
      )
    })

   output$VWOcommunication <- renderPlotly({
     plot_vwc(
       data = dscoreddi::prefdat,
       domein = "Communicatie",
       age = input$agemos/12,
       selected = selected_items(),
       vw_history = vw_history
     )


    })

   output$VWOgrovemotor <- renderPlotly({
     plot_vwc(
       data = dscoreddi::prefdat,
       domein = "Grove motoriek",
       age = input$agemos/12,
       selected = selected_items(),
       vw_history = vw_history
     )
    })


 ####---- D-score demo plot
     output$dscoreplot <- renderPlotly({
         p <-ggplot()+
             geom_line(data = references, aes(x = month, y = d, group = centile), color = "#e6550d", size = 0.8, alpha = 0.5) +
             geom_point(data = d_history, aes(x = leeftijd, y = d, label1 = maand, label2 = d), size = 2)+
             geom_line(data = d_history, aes(x = leeftijd, y = d), size = 1)+

             ylab("D-score") +
             scale_x_continuous("Leeftijd (in maanden)", limits = c(0,60),
                                breaks = seq(0, 60, 3))

         ggplotly(p, tooltip = c("label1", "label2"))

     })

})
