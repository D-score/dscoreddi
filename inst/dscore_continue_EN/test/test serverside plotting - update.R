# test server side
#global
library(dmetric)
library(dscore)
library(shiny)
library(dplyr)
library(gtools)
library(plotly)
library(tidyr)


# input

inputagemos <- 6
inputrefperc <- 90
inputsuggest <- 4

item_candidates <- prefdat %>% filter(sideA==1 | sideB ==1) %>% select(item)
itembank_candidates <- itembank %>% filter(item %in% item_candidates[[1]])
selected_items <- dinstrument::dform1(itembank = itembank_candidates, ageband = inputagemos, reference = expanded_reference, scalefactor = 2.099986, leniency = inputrefperc, n = inputsuggest)$item

  pref <-
  prefdat %>%
    mutate(highlight = ifelse(item %in% selected_items, "#1A80C4", "black"))




#})
### NU NOG STERRETJES IN DE PLOT WEERGEVEN
#output$distPlot <- renderPlot({

  ## 0-18m
  fm_highlight <- pref %>% filter(domein == "Fijne motoriek"& sideA == 1) %>%
    arrange(-nr) %>% select(highlight) #added

  fm1 <-
    ggplot(pref %>% filter(domein == "Fijne motoriek"& sideA == 1),  #selecteer milestons met A10 < 15 maanden
           aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) + #changed
    geom_point()+
    geom_errorbar(aes(ymin = A10, ymax = A90))+
    scale_color_manual(values = c("black" = "black", "#1A80C4" = "#1A80C4"))+ #added
    geom_hline(yintercept = inputagemos)+
    #geom_text(aes(x = reorder(labelNLn, -nr), y = (0), label = highlight))+
    coord_flip()+ xlab("") +
    theme(legend.position = "none", axis.text.y = element_text(color = fm_highlight[[1]])) +
    scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18),position = "right")+
    ggtitle("Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag")


  cm_highlight <- pref %>% filter(domein == "Communicatie"& sideA == 1) %>%
    arrange(-nr) %>% select(highlight) #added

  cm1 <-
    ggplot(pref %>% filter(domein == "Communicatie"& sideA == 1),#selecteer milestons met A10 < 15 maanden
           aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
    geom_point()+
    geom_errorbar(aes(ymin = A10, ymax = A90))+
    scale_color_manual(values = c("black" = "black", "#1A80C4" = "#1A80C4"))+ #added
    geom_hline(yintercept = inputagemos)+
        coord_flip() + xlab("") +
    theme(legend.position = "none", axis.text.y = element_text(color = cm_highlight[[1]])) +
    scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18), position = "right")+
    ggtitle("Communicatie")


  gm_highlight <- pref %>% filter(domein == "Grove motoriek"& sideA == 1) %>%
    arrange(-nr) %>% select(highlight) #added

  gm1 <-
    ggplot(pref %>% filter(domein == "Grove motoriek" & sideA == 1), #selecteer milestons met A10 < 15 maanden
           aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
    geom_point()+
    geom_errorbar(aes(ymin = A10, ymax = A90))+
    scale_color_manual(values = c("black" = "black", "#1A80C4" = "#1A80C4"))+ #added
    geom_hline(yintercept = inputagemos)+
    coord_flip()+
    ylab("Leeftijd (maanden)") + xlab("") +
    theme(legend.position = "none", axis.text.y = element_text(color = gm_highlight[[1]])) +
    scale_y_continuous(name = "Leeftijd (maanden)", breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3)), limits= c(0,18),
                       sec.axis = sec_axis(trans = ~(.*1), breaks = c(0,1,2,3,4,5,6,7,8,9,10, seq(12,48,3))))+
    ggtitle("Grove motoriek")




  allplotslist1 <- align_plots(fm1,cm1,gm1, align = "v")

  m1 <- arrangeGrob(grobs = allplotslist1, nrow = 3, ncol = 1, heights = c(11,9,15))
  grid.arrange(m1)
