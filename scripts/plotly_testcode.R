##plotly_testcode
linecolors <-  c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60", "#3399ff" = "#3399ff", "maroon"="maroon")


continuous_items <- continuous_item55 <- selected_items <- passed_items <- failed_items <-  length(0)

fn_o <- system.file("extdata", "bds_v2.0", "smocc", "Iris_S.json", package = "jamesdemodata")
tgt <- read_bds(fn_o, append_ddi = TRUE)
tgt$psn$dob <- "2021-02-16"

get_vwhistory <- function(data){
    data|>
    filter(grepl("^ddi", yname)) |>
    dplyr::select(age, yname, y) |>
    rename(item = yname, value = y) |>
    mutate(item = ifelse(item == "ddigmd055" & (age > 0.5   / 365.25 & age < 42.5  / 365.25),
                         "ddigmd155", item),
           item = ifelse(item == "ddigmd055" & (age > 42.5   / 365.25 & age < 102.5  / 365.25),
                         "ddigmd255", item),
           item = ifelse(item == "ddigmd055" & (age > 105.5   / 365.25 & age < 146.5  / 365.25),
                         "ddigmd355", item),
           value = ifelse(item == "ddigmd055" & (age < 146.5 / 365.25 | age > 1),
                          NA, value),
           item = ifelse(item == "ddigmd068" & (age > 1.75 & age < 2.5),
                         "ddigmd168", item),
           item = ifelse(item == "ddigmd068" & (age > 2.5 & age < 3.5),
                         "ddigmd268", item),
           value = ifelse(item == "ddigmd068" & (age < 0.75 & age > 1.75),
                          NA, value)
      )
  }
passed_items <-
  get_vwhistory(tgt$xyz) |>
    filter(value == 1) |>
    group_by(item) |>
    slice_max(age, n = 1) |>
    mutate(agemos = age *12) |>
    dplyr::select(agemos, item)


failed_items <-
  get_vwhistory(tgt$xyz) |>
    group_by(item) |>
    slice_max(value) |>
    filter(value == 0) |>
    slice_max(age, n = 1) |>
    mutate(agemos = age *12) |>
    dplyr::select(agemos, item)




agemos <- 36
suggest <- 4
refperc <- 70
if(!is.na(agemos)){

  if(agemos < 15) {continuous_items <- prefdat %>% filter(nr %in% 52:54) %>% pull(item)}
  if(agemos >= 15){continuous_items <- length(0)}

  if(agemos < 3) {continuous_item55 <- prefdat %>% filter(nr == 55) %>% pull(item)}
  if(agemos >= 3){continuous_item55 <- length(0)}

  item_candidates <- prefdat %>%
    filter((sideA==1 | sideB ==1) & !nr %in% 52:54) %>%
    filter(!item %in% passed_items) %>% #excluded passed items
    pull(item)

  itembank_candidates <- itembank_vwc %>%
    filter(item %in% item_candidates)

  selected_items <- dinstrument::dform1(itembank = itembank_candidates, ageband = agemos, reference = dscoreddi::expanded_reference, population = "dutch", leniency = refperc, n = suggest)$item
}

pref <-
  dscoreddi::prefdat %>%
  filter(sideA==1 | sideB ==1) %>% #only use selection voor VWO
  mutate( #already passed as grey
    #already passed as grey
    highlight = ifelse(item %in% passed_items$item, "grey60", "black"),
    #previously fail as maroon
    highlight = ifelse(item %in% failed_items$item, "maroon", highlight),
    #continuous items blue.
    highlight = ifelse(item %in% c(continuous_items, continuous_item55), "#3399ff", highlight ),
    #suggested items as orange
    highlight = ifelse(item %in% selected_items, "#e6550d", highlight),
    #high light continuous items blue.
    highlight = ifelse(item %in% c(continuous_items, continuous_item55), "#3399ff", highlight ),

    bold = ifelse(item %in% c(selected_items, continuous_items, continuous_item55), "bold", "plain"),
    ## dashed line for continuous items - remove estimated target age.
    cont1 = ifelse(nr %in% 52:54, 0, NA),
    cont2 = ifelse(nr %in% 52:54, 12, NA),
    cont1 = ifelse(nr == 55, 1, cont1),
    cont2 = ifelse(nr == 55, 3, cont2),
    A10 = ifelse(nr %in% 52:54, 0, A10),
    A50 = ifelse(nr %in% 52:54, 12, A50), #to not plot it but have it in the plot
    A90 = ifelse(nr %in% 52:54, 12, A90),
    ltys = ifelse(nr %in% 52:55, 2, 1))

gm_highlight <- pref %>% filter(domein == "Grove motoriek") %>%
  arrange(-nr) %>% pull(highlight) #added
gm_bold <- pref %>% filter(domein == "Grove motoriek") %>%
  arrange(-nr) %>% pull(bold)

gm_dom <-
  ggplot(pref %>% filter(domein == "Grove motoriek"), #selecteer milestons met A10 < 15 maanden
         aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight, label = labelNL)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90, linetype = ltys))+
  scale_linetype_identity()+
  #geom_linerange(aes(ymin = cont1, ymax = cont2), lty = 2)+
  scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60", "#3399ff" = "#3399ff", "salmon"="salmon"))+ #added
  geom_hline(yintercept = as.numeric(agemos))+
   xlab("") +
  theme(legend.position = "none", axis.text.y = element_text(face = gm_bold, color = gm_highlight)) +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,50),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
  coord_flip()+

  ggtitle("Grove motoriek")

bold_labels <- pref %>% filter(domein == "Grove motoriek" & bold == "bold") %>% pull(labelNLn)

major <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3))
minor <- c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50)
xtick <- c(major, minor) %>% sort
xtick_text <- ifelse(xtick %in% major, xtick, "")

# No Problem
gg2list(gm_dom)
pbase <- ggplotly(gm_dom, tooltip = "label")

pbase$x$data[[1]]

 data[2].marker.color

str(plotly_build(pbase)$x$data)


pbase %>%
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


gm_dom %>%
  ggplotly(layerData = 2, originalData = FALSE) %>%
  plotly_data()




##basic plot for gm:
dscoreddi::prefdat %>%
  rename(kenmerk = labelNL) %>%
  filter(sideA==1 | sideB ==1) %>%
  filter(domein == "Grove motoriek") %>%
  mutate(labelNLn = forcats::fct_reorder(labelNLn, desc(nr))) %>%
  arrange(desc(nr)) %>%
  plot_ly(x = ~A50, y= ~labelNLn, hoverinfo = "text") %>%
    add_segments(x = ~A10, y = ~labelNLn,
                 xend = ~A90, yend = ~labelNLn,
                 color = I("black"),
                 showlegend = FALSE
    )%>%
  #deze veroorzaakt nu oranje stippen!!! dat moet weg!!!
 add_markers(xaxis = "x2", inherit = TRUE, data = NULL) %>%

  add_markers(
    x = ~A10,
    y = ~labelNLn,
    color = I("black"),
    text = ~kenmerk,
    marker = list(symbol = "line-ns",
                  size = 3,
                  line = list(
                    color = "black",
                    width = 2
                  )
    ),
    showlegend = FALSE

    ) %>%
  add_markers(
    x = ~A50,
    y = ~labelNLn,
    color = I("black"),
    text = ~kenmerk,
    showlegend = FALSE
  ) %>%
  add_markers(
    x = ~A90,
    y = ~labelNLn,
    color = I("black"),
    text = ~kenmerk,
    marker = list(symbol = "line-ns",
                  size = 3,
                  line = list(
                    color = "black",
                    width = 2
                    )
                  ),
    showlegend = FALSE
    ) %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = xtick,
      zeroline = FALSE,
      showgrid = TRUE,
      range = c(0,50),
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
    ),
    yaxis = list(
      title = ""
    )
  )
