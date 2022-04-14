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

         item_candidates <- prefdat %>% filter(sideA==1 | sideB ==1) %>% select(item)
        itembank_candidates <- itembank %>% filter(item %in% item_candidates[[1]])
        selected_items <- dinstrument::dform1(itembank = itembank_candidates, ageband = input$agemos, reference = expanded_reference, scalefactor = 2.099986, leniency = input$refperc, n = input$suggest)$item

            prefdat %>%
            mutate(highlight = ifelse(item %in% selected_items, "#e6550d", "black"),
                   bold = ifelse(item %in% selected_items, "bold", "plain"))

            #1A80C4 - blue color
            #31a354 - green color
            #e6550d - red color
    })

    #
    output$VWOplot1 <- renderPlot({

        ## 0-18m
        fm_highlight <- pref() %>% filter(domein == "Fijne motoriek"& sideA == 1) %>%
            arrange(-nr) %>% select(highlight) #added
        fm_bold <- pref() %>% filter(domein == "Fijne motoriek"& sideA == 1) %>%
          arrange(-nr) %>% select(bold)
        fm1 <-
            ggplot(pref() %>% filter(domein == "Fijne motoriek"& sideA == 1),  #selecteer milestons met A10 < 15 maanden
                   aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) + #changed
            geom_point()+
            geom_errorbar(aes(ymin = A10, ymax = A90))+
            scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d"))+ #added
            geom_hline(yintercept = input$agemos)+
            coord_flip()+ xlab("") +
            theme(legend.position = "none", axis.text.y = element_text(face = fm_bold[[1]], color = fm_highlight[[1]])) +
            scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18),position = "right")+
            ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")


        cm_highlight <- pref() %>% filter(domein == "Communicatie"& sideA == 1) %>%
            arrange(-nr) %>% select(highlight) #added
        cm_bold <- pref() %>% filter(domein == "Communicatie"& sideA == 1) %>%
          arrange(-nr) %>% select(bold)
        cm1 <-
            ggplot(pref() %>% filter(domein == "Communicatie"& sideA == 1),#selecteer milestons met A10 < 15 maanden
                   aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
            geom_point()+
            geom_errorbar(aes(ymin = A10, ymax = A90))+
            scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d"))+ #added
            geom_hline(yintercept = input$agemos)+
            coord_flip() + xlab("") +
            theme(legend.position = "none", axis.text.y = element_text(face = cm_bold[[1]], color = cm_highlight[[1]])) +
            scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18), position = "right")+
            ggtitle("Communicatie")


        gm_highlight <- pref() %>% filter(domein == "Grove motoriek"& sideA == 1) %>%
            arrange(-nr) %>% select(highlight) #added
        gm_bold <- pref() %>% filter(domein == "Grove motoriek"& sideA == 1) %>%
          arrange(-nr) %>% select(bold)

        gm1 <-
            ggplot(pref() %>% filter(domein == "Grove motoriek" & sideA == 1), #selecteer milestons met A10 < 15 maanden
                   aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
            geom_point()+
            geom_errorbar(aes(ymin = A10, ymax = A90))+
            scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d"))+ #added
            geom_hline(yintercept = input$agemos)+
            coord_flip()+
            ylab("Leeftijd (maanden)") + xlab("") +
            theme(legend.position = "none", axis.text.y = element_text(face = gm_bold[[1]], color = gm_highlight[[1]])) +
            scale_y_continuous(name = "Leeftijd (maanden)", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18),
                               sec.axis = sec_axis(trans = ~(.*1), breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3))))+
            ggtitle("Grove motoriek")

        allplotslist1 <- align_plots(fm1,cm1,gm1, align = "v")

        m1 <- arrangeGrob(grobs = allplotslist1, nrow = 3, ncol = 1, heights = c(11,9,15))
        grid.arrange(m1)


    })
    output$VWOplot2 <- renderPlot({

        fm_highlight <- pref() %>% filter(domein == "Fijne motoriek"& sideB == 1) %>%
            arrange(-nr) %>% select(highlight) #added
        fm_bold <- pref() %>% filter(domein == "Fijne motoriek"& sideB == 1) %>%
          arrange(-nr) %>% select(bold)

    fm2 <-
        ggplot(pref() %>% filter(domein == "Fijne motoriek"& sideB == 1), #selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+
        scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d"))+ #added
        geom_hline(yintercept = input$agemos)+
        coord_flip()+ xlab("") +
        theme(legend.position = "none", axis.text.y = element_text(face = fm_bold[[1]], color = fm_highlight[[1]])) +
        scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(8,50),position = "right")+
        ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")

    cm_highlight <- pref() %>% filter(domein == "Communicatie"& sideB == 1) %>%
        arrange(-nr) %>% select(highlight) #added
    cm_bold <- pref() %>% filter(domein == "Communicatie"& sideB == 1) %>%
      arrange(-nr) %>% select(bold)
    cm2 <-
        ggplot(pref() %>% filter(domein == "Communicatie"& sideB == 1),#selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+
        scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d"))+ #added
        geom_hline(yintercept = input$agemos)+
        coord_flip()+ xlab("") +
        theme(legend.position = "none", axis.text.y = element_text(face = cm_bold[[1]], color = cm_highlight[[1]])) +
        scale_y_continuous(name = "", breaks = seq(0,48,3), limits= c(8,50), position = "right")+
        ggtitle("Communicatie")

    gm_highlight <- pref() %>% filter(domein == "Grove motoriek"& sideB == 1) %>%
        arrange(-nr) %>% select(highlight) #added
    gm_bold <- pref() %>% filter(domein == "Grove motoriek"& sideB == 1) %>%
      arrange(-nr) %>% select(bold)
    gm2 <-
        ggplot(pref() %>% filter(domein == "Grove motoriek" & sideB == 1), #selecteer milestons met A10 < 15 maanden
               aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
        geom_point()+
        geom_errorbar(aes(ymin = A10, ymax = A90))+
        scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d"))+ #added
        geom_hline(yintercept = input$agemos)+
        coord_flip()+ xlab("") +
        theme(legend.position = "none", axis.text.y = element_text(face = gm_bold[[1]], color = gm_highlight[[1]])) +
        ylab("Leeftijd (maanden)") + xlab("") +
        scale_y_continuous(name = "Leeftijd (maanden)", breaks = seq(0,48,3), limits= c(8,50),
                           sec.axis = sec_axis(trans = ~(.*1), breaks = seq(0,48,3)))+
        ggtitle("Grove motoriek")



    allplotslist <- align_plots(fm2,cm2,gm2, align = "v")

    m1 <- arrangeGrob(grobs = allplotslist, nrow = 3, ncol = 1, heights = c(11.5,10,9))


    grid.arrange(m1)

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
