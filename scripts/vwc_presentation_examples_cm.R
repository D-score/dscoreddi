# VWC presentatie voorbeeld communicatie
library(dscore)
library(dplyr)
library(dscoreddi)
library(ggplot2)

theme_set(theme_light())

agemos <- NA
nsuggest <- 0
refperc <- NA

continuous_items <- continuous_item55 <- selected_items <- length(0)

if(!is.na(agemos)){
  if(agemos < 15) {continuous_items <- prefdat %>% filter(nr %in% 52:54) %>% pull(item)}
  if(agemos >= 15){continuous_items <- length(0)}

  if(agemos < 3) {continuous_item55 <- prefdat %>% filter(nr == 55) %>% pull(item)}
  if(agemos >= 3){continuous_item55 <- length(0)}

  item_candidates <- prefdat %>%
    filter((sideA==1 | sideB ==1) & !nr %in% 52:54) %>%
    # filter(!item %in% passed_items) %>% #excluded passed items
    pull(item)

  itembank_candidates <- itembank_vwc %>%
    filter(item %in% item_candidates)

  selected_items <- dinstrument::dform1(itembank = itembank_candidates, ageband = agemos, reference = dscoreddi::expanded_reference, population = "dutch", leniency = refperc, n = nsuggest)$item
}

pref <-
dscoreddi::prefdat %>%
  filter(sideA==1 | sideB ==1) %>% #only use selection voor VWO
  mutate(
    #high light continuous items blue.
    highlight = ifelse(item %in% c(continuous_items, continuous_item55), "#3399ff", "black"),
    highlight = ifelse(item %in% selected_items, "#e6550d", highlight),
    #already passed as grey
    # highlight = ifelse(item %in% passed_items, "grey60", highlight),

    bold = ifelse(item %in% c(selected_items, continuous_items, continuous_item55), "bold", "plain"),
    ## dashed line for continuous items - remove estimated target age.
    cont1 = ifelse(nr %in% 52:54, 0, NA),
    cont2 = ifelse(nr %in% 52:54, 12, NA),
    cont1 = ifelse(nr == 55, 1, cont1),
    cont2 = ifelse(nr == 55, 3, cont2),
    A10 = ifelse(nr %in% 52:54, NA, A10),
    A50 = ifelse(nr %in% 52:54, NA, A50), #to not plot it but have it in the plot
    A90 = ifelse(nr %in% 52:54, NA, A90))


cm_highlight <- pref %>% filter(domein == "Communicatie"& sideA == 1) %>%
  arrange(-nr) %>% pull(highlight) #added
cm_bold <- pref %>% filter(domein == "Communicatie"& sideA == 1) %>%
  arrange(-nr) %>% pull(bold)
cm1 <-
  ggplot(pref %>% filter(domein == "Communicatie"& sideA == 1),#selecteer milestons met A10 < 15 maanden
         aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
  geom_point(size = 2.2)+
  geom_errorbar(aes(ymin = A2, ymax = A98), linewidth = 1.2)+
  scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60"))+ #added
  geom_hline(yintercept = as.numeric(agemos))+
  coord_flip() + xlab("") +
  theme(legend.position = "none", axis.text.y = element_text(face = cm_bold, color = cm_highlight, size = 14), axis.text.x = element_text(size = 14)) +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,24),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))#+
 # ggtitle("Communicatie")

cm1

vwc::plot_vwc(data = pref %>% filter(domein == "Communicatie"& sideA == 1), domein = "Communicatie", age = 0.5,selected = "ddicmm032")



agemos <- 18
pref <- pref %>%
  mutate(highlight = ifelse(item == "ddicmm041", "#e6550d", "black"),
         bold = ifelse(item == "ddicmm041", "bold", "plain"))

cm_highlight <- pref %>% filter(domein == "Communicatie"& sideB == 1) %>%
  arrange(-nr) %>%  pull(highlight) #added
cm_bold <- pref %>% filter(domein == "Communicatie"& sideB == 1) %>%
  arrange(-nr) %>% pull(bold)
cm2 <-
  ggplot(pref %>% filter(domein == "Communicatie"& sideB == 1),#selecteer milestons met A10 < 15 maanden
         aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
  geom_point(size = 2.2)+
  geom_errorbar(aes(ymin = A10, ymax = A90), linewidth = 1.2)+
  scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60"))+ #added
  geom_hline(yintercept = as.numeric(agemos), size = 1.2)+
  coord_flip() + xlab("") +
  theme(legend.position = "none", axis.text.y = element_text(face = cm_bold, color = cm_highlight, size = 14), axis.text.x = element_text(size = 14)) +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(8,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))#+
# ggtitle("Communicatie")

cm2

