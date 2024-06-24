#iJGZ
library(shiny)
library(plotly)


shinyServer(function(input, output, session) {


#update input agemos with info from target data, if not available use input field. So update input field
 # agemos_in <- reactive({
  #  agedays <- as.numeric(Sys.Date() - as.Date(tgt$psn$dob))
 #   agemos <- round(agedays / 365.25 * 12)
 # })

 # observe({
 # updateNumericInput(inputId = "agemos",value = agemos_in())
 # })



#deze kan updaten tijdens het plotten door input parameters refperc en suggest
selected_items <-
  reactive({
    vwc::select_vwc(
      age = agemos_in/12,
      refperc = input$refperc,
      nsuggest = input$suggest,
      vw_history = vw_history
    )

  })

   output$VWOfinemotor <- renderPlotly({
      vwc::plot_vwc(
        data = dscoreddi::prefdat,
        domein = "Fijne motoriek",
        age = agemos_in/12,
        selected = selected_items(),
        vw_history = vw_history
      )
    })


   output$VWOcommunication <- renderPlotly({
     vwc::plot_vwc(
       data = dscoreddi::prefdat,
       domein = "Communicatie",
       age = agemos_in/12,
       selected = selected_items(),
       vw_history = vw_history
     )


    })

   output$VWOgrovemotor <- renderPlotly({
     vwc::plot_vwc(
       data = dscoreddi::prefdat,
       domein = "Grove motoriek",
       age = agemos_in/12,
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
                                breaks = seq(0, 60, 3))+
           ggtitle("D-score")
         ggplotly(p, tooltip = c("label1", "label2"))
     })

})
