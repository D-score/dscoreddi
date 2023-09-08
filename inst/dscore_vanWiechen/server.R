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
server <- function(input, output, session) {


    ## menu----------
    output$menu <- renderMenu({
        sidebarMenu(
            id = "mytabs",
            menuItem("D-score referentie",
                     tabName = "reference",
                     icon = icon("area-chart")),
            menuItem("Model kans",
                     tabName = "p_a_model",
                     icon = icon("line-chart")),
            menuItem("Real data",
                     tabName = "p_a_obs",
                     icon = icon("line-chart")),
            menuItem("Van Wiechen scoren",
                     tabName = "invoer",
                     icon = icon("check")),
            menuItem("About",
                     tabName = "about",
                     icon = icon("info"))

        )
    })

    ##dscor reference -------------
    output$refplot <- renderPlot({

        ggplot(references, aes(x = month, y = d, group = centile))+
            geom_line( color = "blue", size = 1) +
            ylab("D-score") +
            scale_x_continuous("Leeftijd (in maanden)", limits = c(0,36),
                               breaks = seq(0, 36, 6))

    })
    ##p_a_model --------------

    pam_ls = reactive({
        mixedsort(unique(pref$item_wlab))
    })

    count_pam <- reactiveVal(1)

    observeEvent(input$run6, {
        old_count = count_pam()
        count_pam(old_count + 1)
    })

    observeEvent(input$prev6, {
        old_count = count_pam()
        count_pam(old_count - 1)
    })
    observeEvent(input$pam_list, {
        count_pam(which(pam_ls() == input$pam_list))
    })

    observe({
        if (length(count_pam()) > 0) {
            updateSelectInput(session,
                              "pam_list",
                              choices = pam_ls(),
                              selected = pam_ls()[count_pam()])
        }
        if (length(count_pam()) == 0) {
            updateSelectInput(session,
                              "pam_list",
                              choices = pam_ls(),
                              selected = pam_ls()[1])
        }
    })

    cdata <- session$clientData

    output$plots_p_a_model <- renderPlotly({


        plotdata <- pref %>% filter(item == substr(pam_ls()[which(pam_ls() == input$pam_list)], 1, 9))
        the_label  <- plotdata$labelNL[1]
        if(is.na(the_label)) the_label <- plotdata$label[1]
        the_label <- substr(the_label, 1L, 60L)
        item_name <- plotdata$ID.VWO2005[1]
        if(is.na(item_name)) item_name <- plotdata$item[1]



           plot <- ggplot(plotdata, aes(x = Leeftijd, y = percentile))+
            geom_line() +
            annotate("text", x = 1, y = 2, hjust = 0,
                     label = item_name,
                     family = "Courier", fontface = "bold")+
            annotate("text", x = 30, y = 2, hjust = 0, label = the_label)+
               scale_x_continuous("Leeftijd (in maanden)", limits = c(0,36),
                                  breaks = seq(0, 36, 6)) +
               scale_y_continuous("% pass", breaks = seq(0, 100, 20),
                                  limits = c(0, 100))


           ggplotly(plot,width = cdata$output_pid_width, height = cdata$output_pid_height)


    })







    ##p_a_obs --------------

    pai_ls = reactive({
        mixedsort(unique(pass$item_wlab))
    })

    count_pai <- reactiveVal(1)

    observeEvent(input$run7, {
        old_count = count_pai()
        count_pai(old_count + 1)
    })

    observeEvent(input$prev7, {
        old_count = count_pai()
        count_pai(old_count - 1)
    })
    observeEvent(input$pai_list, {
        count_pai(which(pai_ls() == input$pai_list))
    })

    observe({
        if (length(count_pai()) > 0) {
            updateSelectInput(session,
                              "pai_list",
                              choices = pai_ls(),
                              selected = pai_ls()[count_pai()])
        }
        if (length(count_pai()) == 0) {
            updateSelectInput(session,
                              "pai_list",
                              choices = pai_ls(),
                              selected = pai_ls()[1])
        }
    })

    output$plots_p_a_item <- renderPlot({

      selecteditem <- substr(pai_ls()[which(pai_ls() == input$pai_list)], 1, 9)
        print(dmetric::plot_p_a_item(pass = pass,
                                     items = selecteditem,
                                     xlim = c(0, 36))
        )
    })


###invoer---------------------



    output$vwinvoerA <- renderUI({
      lapply(as.list(itembankA$item), function(q){
        radioButtons(inputId = q,
                     label = itembank[itembank$item==q,"labelNLn"],
                     choiceNames = c("-", "+"),
                     choiceValues = c(0,1),
                     selected = character(0),
                     #selected = character(0) ,
                     inline = TRUE)
      })

    })


    output$vwinvoerB <- renderUI({
      lapply(as.list(itembankB$item), function(q){
        radioButtons(inputId = q,
                     label = itembank[itembank$item==q,"labelNLn"],
                     choiceNames = c("-", "+"),
                     choiceValues = c(0,1),
                     selected = character(0),
                     #selected = character(0) ,
                     inline = TRUE)
      })

    })


    itemleveldata <- reactive({
      uitlist <- lapply(as.list(itembank$item), function(q){
        answer <- input[[q]]
        #reverse score if recode == ja
        if(length(answer)>0) answer <- as.numeric(answer)
        if(length(answer)==0) {
          answer <- NA}

        answer

      })
      names(uitlist) <- itembank$item
      as.data.frame(uitlist)

    })

output$vwingevoerd <- renderTable(
  {
  df <-
  itemleveldata() %>%
    pivot_longer(everything(), names_to = "item", values_to = "score") %>%
    drop_na(score) %>%
    left_join(itembank) %>%
    select(labelNL, score)

  df
})

wvdscore <- reactive({
  dfd <- data.frame(age = input$age1/12, itemleveldata())

  dscore::dscore(data = dfd, items = names(itemleveldata()), population = "dutch",
                key = "dutch")


})

resulttable <- reactive({
  tab <- wvdscore()
  colnames(tab) <- c("Age", "n", "p", "D-score", "sem", "daz")
  tab
})

output$scoretab <- function() {
  resulttable() %>%
    knitr::kable("html") %>%
    kable_styling("striped", full_width = F) %>%
    footnote(general = "Age = age in years (decimal); n = number of available milestones; p = percentage of passed milestones; D -score = development score; sem = standard error of measurement; daz = D-score adjusted for age",
             general_title = "Note:")
}

output$scoreplot <- renderPlot({

  r <- dscore::builtin_references %>%
    filter(pop == "dutch") %>%
    mutate(age = age *12) %>%
    select(age, SDM2, SD0, SDP2)

  ggplot(wvdscore(), aes(a*12, d)) +
    geom_point(shape = 16, size = 2, show.legend = FALSE, alpha = .75 , color = 'navyblue') +
    labs(x = "Age (Months)", y = "D-score") +
    ggtitle(paste("D-score voor ingevoerde van Wiechenschema"))+
    theme_light() +
    annotate("polygon", x = c(r$age, rev(r$age)),
             y = c(r$SDM2, rev(r$SDP2)), alpha = 0.1, fill = "green") +
    annotate("line", x = r$age, y = r$SDM2, lwd = 0.3, alpha = 0.2, color = "green") +
    annotate("line", x = r$age, y = r$SDP2, lwd = 0.3, alpha = 0.2, color = "green") +
    annotate("line", x = r$age, y = r$SD0, lwd = 0.5, alpha = 0.2, color = "green")

})





}
