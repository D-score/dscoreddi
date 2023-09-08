library(shiny)

shinyServer(function(input, output) {

    #reference data, with additional column for start-items based on input$refperc en highlights voor geselecteerde start-items based on input$agemos en input$suggest.
    # ahv voorgaand afgenomen items uit BDS item_candidates aanpassen en dynamisch maken, dan kunnen alleen items gesuggereerd worden die nog niet eerder zijn afgenomen; of herhaald moeten worden afgenomen.
    # volgende stap is het aanpassen van de highlight variabele door de ingevoerde van wiechen kenmerken tijdens het consult.
    pref <- reactive({

      continuous_items <- continuous_item55 <- selected_items <- length(0)

      if(!is.na(input$agemos)){
        if(input$agemos < 15) {continuous_items <- prefdat %>% filter(nr %in% 52:54) %>% pull(item)}
        if(input$agemos >= 15){continuous_items <- length(0)}

        if(input$agemos < 3) {continuous_item55 <- prefdat %>% filter(nr == 55) %>% pull(item)}
        if(input$agemos >= 3){continuous_item55 <- length(0)}

        item_candidates <- prefdat %>%
          filter((sideA==1 | sideB ==1) & !nr %in% 52:54) %>%
          filter(!item %in% passed_items) %>% #excluded passed items
          pull(item)

        itembank_candidates <- itembank_vwc %>%
          filter(item %in% item_candidates)

        selected_items <- dinstrument::dform1(itembank = itembank_candidates, ageband = input$agemos, reference = dscoreddi::expanded_reference, population = "dutch", leniency = input$refperc, n = input$suggest)$item
    }

            dscoreddi::prefdat %>%
              filter(sideA==1 | sideB ==1) %>% #only use selection voor VWO
            mutate(
                   #high light continuous items blue.
                   highlight = ifelse(item %in% c(continuous_items, continuous_item55), "#3399ff", "black"),
                   highlight = ifelse(item %in% selected_items, "#e6550d", highlight),
                   #already passed as grey
                   highlight = ifelse(item %in% passed_items, "grey60", highlight),

                   bold = ifelse(item %in% c(selected_items, continuous_items, continuous_item55), "bold", "plain"),
                   ## dashed line for continuous items - remove estimated target age.
                   cont1 = ifelse(nr %in% 52:54, 0, NA),
                   cont2 = ifelse(nr %in% 52:54, 12, NA),
                   cont1 = ifelse(nr == 55, 1, cont1),
                   cont2 = ifelse(nr == 55, 3, cont2),
                   A10 = ifelse(nr %in% 52:54, NA, A10),
                   A50 = ifelse(nr %in% 52:54, NA, A50), #to not plot it but have it in the plot
                   A90 = ifelse(nr %in% 52:54, NA, A90))

            #1A80C4 - blue color
            #31a354 - green color
            #e6550d - red color
    })

#####---- Plot Milestones for 0-15m
    output$VWOplot1 <- renderPlot({

     #if(length(input$agemos)>0){ageline <- as.numeric(input$agemos)}

        ## 0-18m
        fm_highlight <- pref() %>% filter(domein == "Fijne motoriek"& sideA == 1) %>%
            arrange(-nr) %>% pull(highlight) #added
        fm_bold <- pref() %>% filter(domein == "Fijne motoriek"& sideA == 1) %>%
          arrange(-nr) %>% pull(bold)
        fm1 <-
            ggplot(pref() %>% filter(domein == "Fijne motoriek"& sideA == 1),  #selecteer milestons met A10 < 15 maanden
                   aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) + #changed
            geom_point()+
            geom_errorbar(aes(ymin = A10, ymax = A90))+
            scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60"))+ #added
            geom_hline(yintercept = as.numeric(input$agemos))+
            coord_flip()+ xlab("") +
            theme(legend.position = "none", axis.text.y = element_text(face = fm_bold, color = fm_highlight)) +
          scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,18),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
            ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")


        cm_highlight <- pref() %>% filter(domein == "Communicatie"& sideA == 1) %>%
            arrange(-nr) %>% pull(highlight) #added
        cm_bold <- pref() %>% filter(domein == "Communicatie"& sideA == 1) %>%
          arrange(-nr) %>% pull(bold)
        cm1 <-
            ggplot(pref() %>% filter(domein == "Communicatie"& sideA == 1),#selecteer milestons met A10 < 15 maanden
                   aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
            geom_point()+
            geom_errorbar(aes(ymin = A10, ymax = A90))+
            scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60"))+ #added
            geom_hline(yintercept = as.numeric(input$agemos))+
            coord_flip() + xlab("") +
            theme(legend.position = "none", axis.text.y = element_text(face = cm_bold, color = cm_highlight)) +
          scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,18),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
            ggtitle("Communicatie")


        gm_highlight <- pref() %>% filter(domein == "Grove motoriek"& sideA == 1) %>%
            arrange(-nr) %>% pull(highlight) #added
        #add additional highlight categorie voor de ** items.

        gm_bold <- pref() %>% filter(domein == "Grove motoriek"& sideA == 1) %>%
          arrange(-nr) %>% pull(bold)

        gm1 <-
            ggplot(pref() %>% filter(domein == "Grove motoriek" & sideA == 1), #selecteer milestons met A10 < 15 maanden
                   aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
            geom_point()+
            geom_errorbar(aes(ymin = A10, ymax = A90))+
            geom_linerange(aes(ymin = cont1, ymax = cont2), lty = 2)+
            scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "#3399ff"= "#3399ff", "grey60" = "grey60"))+ #added
            geom_hline(yintercept = as.numeric(input$agemos))+
            coord_flip()+
            ylab("Leeftijd (maanden)") + xlab("") +
            theme(legend.position = "none", axis.text.y = element_text(face = gm_bold, color = gm_highlight)) +
          scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,18),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
            ggtitle("Grove motoriek")

        allplotslist1 <- align_plots(fm1,cm1,gm1, align = "v")

        m1 <- arrangeGrob(grobs = allplotslist1, nrow = 3, ncol = 1, heights = c(11,10.5,15))
        grid.arrange(m1)


    })


#####---- Plot Milestones for 6-48m

    output$VWOplot2 <- renderPlot({

        fm_highlight <- pref() %>% filter(domein == "Fijne motoriek"& sideB == 1) %>%
            arrange(-nr) %>% pull(highlight) #added
        fm_bold <- pref() %>% filter(domein == "Fijne motoriek"& sideB == 1) %>%
          arrange(-nr) %>% pull(bold)

    fm2 <-
        ggplot(pref() %>% filter(domein == "Fijne motoriek"& sideB == 1), #selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+
        scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60"))+ #added
        geom_hline(yintercept = as.numeric(input$agemos))+
        coord_flip()+ xlab("") +
      scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(8,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+        theme(legend.position = "none", axis.text.y = element_text(face = fm_bold, color = fm_highlight)) +
        ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

    cm_highlight <- pref() %>% filter(domein == "Communicatie"& sideB == 1) %>%
        arrange(-nr) %>% pull(highlight) #added
    cm_bold <- pref() %>% filter(domein == "Communicatie"& sideB == 1) %>%
      arrange(-nr) %>% pull(bold)
    cm2 <-
        ggplot(pref() %>% filter(domein == "Communicatie"& sideB == 1),#selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+
        scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60"))+ #added
        geom_hline(yintercept = as.numeric(input$agemos))+
        coord_flip()+ xlab("") +
        theme(legend.position = "none", axis.text.y = element_text(face = cm_bold, color = cm_highlight)) +
      scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(8,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
        ggtitle("Communicatie")

    gm_highlight <- pref() %>% filter(domein == "Grove motoriek"& sideB == 1) %>%
        arrange(-nr) %>% pull(highlight) #added
    gm_bold <- pref() %>% filter(domein == "Grove motoriek"& sideB == 1) %>%
      arrange(-nr) %>% pull(bold)
    gm2 <-
        ggplot(pref() %>% filter(domein == "Grove motoriek" & sideB == 1), #selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+

        scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60"))+ #added
        geom_hline(yintercept = as.numeric(input$agemos))+
        coord_flip()+ xlab("") +
        theme(legend.position = "none", axis.text.y = element_text(face = gm_bold, color = gm_highlight)) +
      scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(8,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
        ggtitle("Grove motoriek")



    allplotslist <- align_plots(fm2,cm2,gm2, align = "v")

    m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(11.5,11,9))


    grid.arrange(m1)

})



#####----- Plot milestones for Fine motor
    output$VWOfinemotor <- renderPlot({

      fm_highlight <- pref() %>% filter(domein == "Fijne motoriek") %>%
        arrange(-nr) %>% pull(highlight) #added
      fm_bold <- pref() %>% filter(domein == "Fijne motoriek") %>%
        arrange(-nr) %>% pull(bold)

      fm_dom <-
        ggplot(pref() %>% filter(domein == "Fijne motoriek"), #selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+
        scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60"))+ #added
        geom_hline(yintercept = as.numeric(input$agemos))+
        coord_flip()+
        theme(legend.position = "none", axis.text.y = element_text(face = fm_bold, color = fm_highlight)) +
        scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
        xlab("")+
        ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

      fm_dom
    })

    output$VWOcommunication <- renderPlot({

      cm_highlight <- pref() %>% filter(domein == "Communicatie") %>%
        arrange(-nr) %>% pull(highlight) #added
      cm_bold <- pref() %>% filter(domein == "Communicatie") %>%
        arrange(-nr) %>% pull(bold)

      cm_dom <-
        ggplot(pref() %>% filter(domein == "Communicatie"),#selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+
        scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60"))+ #added
        geom_hline(yintercept = as.numeric(input$agemos))+
        coord_flip()+
        xlab("") +
        theme(legend.position = "none", axis.text.y = element_text(face = cm_bold, color = cm_highlight)) +
        scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
        ggtitle("Communicatie")

      cm_dom
    })

  output$VWOgrovemotor <- renderPlot({
      gm_highlight <- pref() %>% filter(domein == "Grove motoriek") %>%
        arrange(-nr) %>% pull(highlight) #added
      gm_bold <- pref() %>% filter(domein == "Grove motoriek") %>%
        arrange(-nr) %>% pull(bold)
     gm_dom <-
        ggplot(pref() %>% filter(domein == "Grove motoriek"), #selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+
        geom_linerange(aes(ymin = cont1, ymax = cont2), lty = 2)+
        scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "#3399ff"= "#3399ff", "grey60" = "grey60"))+ #added
        geom_hline(yintercept = as.numeric(input$agemos))+
        coord_flip()+
        xlab("") +
        theme(legend.position = "none", axis.text.y = element_text(face = gm_bold, color = gm_highlight)) +
       scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
        ggtitle("Grove motoriek")

      gm_dom


    })











 ####---- D-score demo plot
    # output$Dscoreplot <- renderPlot({
    #
    #     ggplot()+
    #         geom_line(data =references, aes(x = month, y = d, group = centile), color = "#e6550d", size = 0.8, alpha = 0.5) +
    #         geom_point(data = childdat, aes(x = month, y = d), size = 2)+
    #         geom_line(data = childdat, aes(x = month, y = d), size = 1)+
    #
    #         ylab("D-score") +
    #         scale_x_continuous("Leeftijd (in maanden)", limits = c(0,36),
    #                            breaks = seq(0, 36, 3))
    #
    # })

})
