# test server side

# input

inputagemos <- 6
inputrefperc <- 100


#pref <- reactive({
  #for start criterion
reference_percentile <- inputrefperc
if(reference_percentile == 100) reference_percentile <- 99
if(reference_percentile == 0) reference_percentile <- 1


  refdata <- calculate_age_equivalents(itembank = itembank, scalefactor = 2.099986, p = c(10, 50, 90, reference_percentile), reference = expanded_reference)
  colnames(refdata)[c(5,9)]<- c("Dstart", "Pstart")

  #load("data/itemtableVWO.rda")

  prefdat <- refdata %>%
    #pivot_longer(cols = c(everything(), -item), names_pattern = "(.)(.*)",
    #             names_to = c(".value", "percentiel")) %>%
    mutate(#percentiel = as.numeric(percentiel),
      domain = substr(item, 4,5),
      domein = plyr::revalue(domain, c("cm" = "Communicatie",
                                       "fm" = "Fijne motoriek",
                                       "gm" = "Grove motoriek"))) %>%
    #rename(leeftijd = A) %>%
    left_join(itemtableVWO %>% select(item, labelNL, ID.VWO2005, month), by = "item") %>%
    mutate(nr = as.numeric(substr(item, 7, 9)),
           nr = ifelse(nr == 136, 35, nr),
           nr = ifelse(nr == 148, 40, nr),
           nr = ifelse(nr == 168, 68.1, nr),
           nr = ifelse(nr == 268, 68.2, nr),
           domein = ifelse(nr == 6, "Fijne motoriek", domein),

           labelNLn = paste(nr, labelNL, sep = ". "),
           labelNLn = ifelse(nr %in% 52:55, paste(nr, labelNL, sep = ".* "), labelNLn),
           labelNLn = ifelse(month < 10, paste(labelNLn, month, sep = " |  "), paste(labelNLn, month, sep = " |") ),
           sideA = ifelse(nr %in% c(1:12, 29:38, 52:67), 1, 0),
           sideB = ifelse(nr %in% c(11:28, 37:51, 66:75, 68.1, 68.2), 1, 0),
           A10 = ifelse(is.na(A10), 0, A10),
           A50 = ifelse(is.na(A50), 0, A50),
           A90 = ifelse(is.na(A90), 0, A90)

    ) %>%
    arrange(nr) %>%
    mutate(closeness = abs(Pstart - inputagemos),
           closeness = ifelse(is.na(closeness), 999, closeness))

  item_candidates <- prefdat$item

  crit <- prefdat %>% filter(item %in% item_candidates) %>%
    summarise(nth(sort(closeness), 3))
  crit

  pref <-
  prefdat %>%
    mutate(highlight = ifelse(item %in% item_candidates & closeness <= crit[[1]], "#1A80C4", "black"))


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
