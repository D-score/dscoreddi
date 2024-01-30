#iJGZ
library(shiny)
library(plotly)


shinyServer(function(input, output) {

    #reference data, with additional column for start-items based on input$refperc en highlights voor geselecteerde start-items based on input$agemos en input$suggest.
    # ahv voorgaand afgenomen items uit BDS item_candidates aanpassen en dynamisch maken, dan kunnen alleen items gesuggereerd worden die nog niet eerder zijn afgenomen; of herhaald moeten worden afgenomen.
    # volgende stap is het aanpassen van de highlight variabele door de ingevoerde van wiechen kenmerken tijdens het consult.


  ## HIERONDER STAAT NU ALLES AL REACTIVE OBJECT, MAAR WELLICHT IS DIT HELEMAAL NIET NODIG EN KAN DIT IN DE GLOBAL GEZET WORDEN, OMDAT DE APP MET DEZE INFORMATIE GELADEN KAN WORDEN.
  # passed items uitlezen uit data.
  passed_items <- reactive({
     get_vwhistory(tgt$xyz) |>
      filter(value == 1) |>
      group_by(item) |>
      slice_max(age, n = 1) |>
      mutate(agemos = age *12) |>
      dplyr::select(agemos, item)
   })

  failed_items <- reactive({
    get_vwhistory(tgt$xyz) |>
      group_by(item) |>
      slice_max(value) |>
      filter(value == 0) |>
      slice_max(age, n = 1) |>
      mutate(agemos = age *12) |>
      dplyr::select(agemos, item)
  })



  dscoredata <- reactive({
    get_dhistory(tgt$xyz)
  })


#update input agemos with info from target data, if not available use input field. So update input field
  agemos_in <- reactive({
    agedays <- as.numeric(Sys.Date() - as.Date(tgt$psn$dob))
    agemos <- round(agedays / 365.25 * 12)
  })

  observe({
  updateNumericInput(inputId = "agemos",value = agemos_in())
  })




  # data voor VW continue plaatjes
      pref <- reactive({

      continuous_items <- continuous_item55 <- selected_items <- length(0)

      if(!is.na(input$agemos)){
        if(input$agemos < 15) {continuous_items <- prefdat %>% filter(nr %in% 52:54) %>% pull(item)}
        if(input$agemos >= 15){continuous_items <- length(0)}

        if(input$agemos < 3) {continuous_item55 <- prefdat %>% filter(nr == 55) %>% pull(item)}
        if(input$agemos >= 3){continuous_item55 <- length(0)}

        item_candidates <- prefdat %>%
          filter((sideA==1 | sideB ==1) & !nr %in% 52:54) %>%
          filter(!item %in% passed_items()$item) %>% #excluded passed items
          pull(item)

        itembank_candidates <- itembank_vwc %>%
          filter(item %in% item_candidates)

        selected_items <- dinstrument::dform1(itembank = itembank_candidates, ageband = input$agemos, reference = dscoreddi::expanded_reference, population = "dutch", leniency = input$refperc, n = input$suggest)$item
    }

            dscoreddi::prefdat %>%
              filter(sideA==1 | sideB ==1) %>% #only use selection voor VWO
              rename(kenmerk = labelNL) %>%
              left_join({bind_rows(passed_items(), failed_items())}, by = "item") %>%
            mutate(#already passed as grey
                   highlight = ifelse(item %in% passed_items()$item, "grey60", "black"),
                   #previously fail as salmon
                   highlight = ifelse(item %in% failed_items()$item, "salmon", highlight),
                   #continuous items blue.
                   highlight = ifelse(item %in% c(continuous_items, continuous_item55), "#3399ff", highlight ),
                   #suggested items as orange
                   highlight = ifelse(item %in% selected_items, "#e6550d", highlight),
                   #suggested and continuous items bold
                   #bold = ifelse(item %in% c(selected_items, continuous_items, continuous_item55), "bold", "plain"),
                   A10 = ifelse(nr %in% 52:54, 0, A10),
                   A50 = ifelse(nr %in% 52:54, 0, A50), #to not plot it but have it in the plot
                   A90 = ifelse(nr %in% 52:54, 12, A90))


    })

#####---- Plot Milestones for 0-15m


#####----- Plot milestones for Fine motor
   output$VWOfinemotor <- renderPlotly({

      fm_highlight <- pref() %>% filter(domein == "Fijne motoriek") %>%
        arrange(-nr) %>% pull(highlight) #added
   #   fm_bold <- pref() %>% filter(domein == "Fijne motoriek") %>%
   #     arrange(-nr) %>% pull(bold)

      fm_dom <-
        ggplot(pref() %>% filter(domein == "Fijne motoriek"), #selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight, label = kenmerk)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+
        geom_point(aes(x = reorder(labelNLn, -nr),  y= agemos, group = highlight, color = highlight, label = kenmerk), pch = 4)+
        scale_color_manual(values = linecolors)+ #added
        geom_hline(yintercept = as.numeric(input$agemos))+
        coord_flip()+
        theme(legend.position = "none"#,
             # axis.text.y = element_text(face = fm_bold, color = fm_highlight)
              ) +
        scale_y_continuous(name = "",
                           breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)),
                           minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50),
                           limits= c(0,50),
                           position = "right",
                           sec.axis= dup_axis(name = "Leeftijd (maanden)"),
                           expand = c(0,0.1))+
        xlab("")+
        ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")


      #plot met extra layout opties in plotly code
      ggplotly(fm_dom, tooltip = "label") %>%
        add_markers(data = NULL, inherit = TRUE, xaxis = "x2") %>%
        layout(
          xaxis = list(
            tickvals = xtick,
            ticktext = xtick_text),
          xaxis2 = list(
            overlaying = "x",
            tickvals = xtick,
            ticktext = xtick_text,
            range = c(0,50),
            side = "top",
            showgrid = FALSE,
            zeroline = FALSE,
            tickfont = list(size = 11)
          )
        )
    })

   output$VWOcommunication <- renderPlotly({

      cm_highlight <- pref() %>% filter(domein == "Communicatie") %>%
        arrange(-nr) %>% pull(highlight) #added
      #cm_bold <- pref() %>% filter(domein == "Communicatie") %>%
       # arrange(-nr) %>% pull(bold)

      cm_dom <-
        ggplot(pref() %>% filter(domein == "Communicatie"),#selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight, label = kenmerk)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+
        geom_point(aes(x = reorder(labelNLn, -nr),  y= agemos, group = highlight, color = highlight, label = kenmerk), pch = 4)+
        scale_color_manual(values = linecolors)+ #added
        geom_hline(yintercept = as.numeric(input$agemos))+
        xlab("") +
        theme(legend.position = "none"#,
              #axis.text.y = element_text(face = cm_bold, color = cm_highlight)
              ) +
        scale_y_continuous(name = "",
                           breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)),
                           minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50),
                           limits= c(0,50),
                           position = "right",
                           sec.axis= dup_axis(name = "Leeftijd (maanden)"),
                           expand = c(0,0.1))+
        coord_flip()+
        ggtitle("Communicatie")



      ggplotly(cm_dom, tooltip = "label")# %>%
       # add_markers(data = NULL, inherit = TRUE, xaxis = "x2") %>%
       # layout(
       #   xaxis = list(
        #     tickvals = xtick,
        #     ticktext = xtick_text),
        #   xaxis2 = list(
        #     overlaying = "x",
        #     tickvals = xtick,
        #     ticktext = xtick_text,
        #     range = c(0,50),
        #     side = "top",
        #     showgrid = FALSE,
        #     zeroline = FALSE,
        #     tickfont = list(size = 11)
        #   )
        # )


    })

   output$VWOgrovemotor <- renderPlotly({

      gm_highlight <- pref() %>% filter(domein == "Grove motoriek") %>%
        arrange(-nr) %>% pull(highlight) #added
      #gm_bold <- pref() %>% filter(domein == "Grove motoriek") %>%
      #  arrange(-nr) %>% pull(bold)

     gm_dom <-
        ggplot(pref() %>% filter(domein == "Grove motoriek"), #selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight, label = kenmerk)) +
       geom_point()+
       geom_errorbar(aes(ymin = A10, ymax = A90))+
      # geom_linerange(aes(ymin = cont1, ymax = cont2), lty = 2)+
       geom_point(aes(x = reorder(labelNLn, -nr),  y= agemos, group = highlight, color = highlight, label = kenmerk), pch = 4)+
       scale_color_manual(values = linecolors)+ #added
       geom_hline(yintercept = as.numeric(input$agemos))+
        xlab("") +
        theme(legend.position = "none"#,
              #axis.text.y = element_text(face = gm_bold, color = gm_highlight)
              ) +
       scale_y_continuous(name = "",
                          breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)),
                          minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50),
                          limits= c(0,50),
                          position = "right",
                          sec.axis= dup_axis(name = "Leeftijd (maanden)"),
                          expand = c(0,0.1))+
       coord_flip()+

        ggtitle("Grove motoriek")


     ggplotly(gm_dom, tooltip = "label")
      # ggplotly(gm_dom, tooltip = "label") %>%
      #   add_markers(data = NULL, inherit = TRUE, xaxis = "x2") %>%
      #   layout(
      #     xaxis = list(
      #       tickvals = xtick,
      #       ticktext = xtick_text),
      #     xaxis2 = list(
      #       overlaying = "x",
      #       tickvals = xtick,
      #       ticktext = xtick_text,
      #       range = c(0,50),
      #       side = "top",
      #       showgrid = FALSE,
      #       zeroline = FALSE,
      #       tickfont = list(size = 11)
      #     )
      #   )
    })


 ####---- D-score demo plot
     output$dscoreplot <- renderPlotly({

         p <-ggplot()+
             geom_line(data = references, aes(x = month, y = d, group = centile), color = "#e6550d", size = 0.8, alpha = 0.5) +
             geom_point(data = dscoredata(), aes(x = leeftijd, y = d, label1 = maand, label2 = d), size = 2)+
             geom_line(data = dscoredata(), aes(x = leeftijd, y = d), size = 1)+

             ylab("D-score") +
             scale_x_continuous("Leeftijd (in maanden)", limits = c(0,60),
                                breaks = seq(0, 60, 3))

         ggplotly(p, tooltip = c("label1", "label2"))

     })

})
